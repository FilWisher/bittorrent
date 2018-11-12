{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}

module BitTorrent.Monad where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger

import Data.List (intercalate)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Network.Socket            as NS
import qualified Network.Socket.ByteString as NSB

import qualified Data.Serialize as S

import BitTorrent.Action
import BitTorrent.ActionHandlers
import BitTorrent.Services
import BitTorrent.StateMachine
import BitTorrent.Protocol
import BitTorrent.Connection
import BitTorrent.Exceptions

import Data.BitField (bits)
import qualified Data.ByteString as BS

import Data.Bits ((.&.), bit, popCount)
import Data.Word

import qualified Data.Text as T
import System.IO.Error

data Config = Config
    { configChan      :: TChan Action
    , configId        :: NodeID
    , configConnCount :: Int
    , configReqCount  :: Int
    }

newtype BitTorrentM a = BitTorrentM
    { unBitTorrentM :: ReaderT Config (LoggingT (StateT StateMachine IO)) a
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadReader Config
        , MonadState StateMachine
        , MonadThrow
        , MonadCatch
        , MonadLogger
        , MonadIO
        )

runBitTorrentM :: Int -> InfoHash -> [(Word16, Word32)] -> TChan Action -> BitTorrentM a -> IO a
runBitTorrentM n infohash peers ch m =
    evalStateT 
        (runStderrLoggingT
            (runReaderT 
                (unBitTorrentM m) 
                (Config ch "TODO: generate random peer ID" 22 5)))
        (initialState n infohash peers)
    
instance EventEmitter Action BitTorrentM where
    emit action = do
        ch <- configChan <$> ask
        liftIO (atomically $ writeTChan ch action) 

instance TCPCommunicator BitTorrentM where
    sendTCP socket buf = void . liftIO $ NSB.send socket buf
    recvTCP socket     = liftIO $ NSB.recv socket 1024

-- | Listen for messages, decode them, and then emit them as received message
--   actions on a channel.
connectionListener :: (MonadIO m, MonadThrow m) => TChan Action -> NS.Socket -> NodeID -> m ()
connectionListener ch socket peerid = do
    m <- S.runGet S.get <$> liftIO (NSB.recv socket 1024)
    case m of
        Left err -> throwM (ParseMessageException err)
        Right msg -> liftIO . atomically $ writeTChan ch (IncomingMessage peerid msg)

-- | Interpreter the action.
update :: Action -> BitTorrentM ()
update (IncomingSocket socket) = do
    logDebugN "IncomingSocket"
    configId <$> ask >>= incomingHandshake socket
update (OutgoingSocket infohash socket) = do
    logDebugN ("Outgoing socket for: " <> (T.pack . show $ infohash))
    selfid <- configId <$> ask
    handle onError $ do
        outgoingHandshake socket infohash selfid
        logInfoN "Successful handshake"
    where
        onError :: HandshakeParseError -> BitTorrentM ()
        onError (HandshakeParseError err) = do
            logDebugN (T.pack $ "BitTorrent.Monad.HandshakeParseError: " ++ show err)
        
update (HandshakeComplete infohash peerid socket) = do
    logDebugN ("Handshake complete with peer: " <> (T.pack $ show peerid))
    ch <- configChan <$> ask
    conn <- liftIO $ createConnection socket peerid (connectionListener ch socket peerid)
    modify $ transition (EvNewConnection peerid conn)
update (ConnectionClosed peerid) = do
    logDebugN ("Connection closed to " <> (T.pack $ show peerid))
    c <- getConnection peerid <$> get
    case c of
        -- XXX: This shouldn't happen. It should probably be logged.
        Nothing   -> return ()
        Just conn -> do
            liftIO $ closeConnection conn
            modify $ transition (EvRemoveConnection peerid)
update (IncomingMessage peerid msg) = do
    logDebugN ("Message received:" <> T.pack (show msg))
    case msg of
        KeepAlive                -> modify $ transition (EvMessageReceived peerid msg)
        Choke                    -> modify $ transition (EvMessageReceived peerid msg)
        Unchoke                  -> modify $ transition (EvMessageReceived peerid msg)
        Interested               -> modify $ transition (EvMessageReceived peerid msg)
        NotInterested            -> modify $ transition (EvMessageReceived peerid msg)
        Have idx                 -> return ()
        BitField bitfield        -> modify $ transition (EvMessageReceived peerid msg)
        -- XXX: For now, ignore requests. Implement a greedy node for simplicity.
        Request idx offset len -> return ()
        Piece idx offset buf   -> do
            modify $ transition (EvNewPiece idx offset buf)
            need <- stateMachineNeed <$> get
            when (popCount need == 0) $
                (emit Complete)
        Cancel idx offset len  -> error "TODO: implement me"
        Port port                -> error "TODO: implement me"
update Tick = do
    logDebugN "Tick"
    configConnCount <$> ask >>= ensureConnectionCount
    configReqCount  <$> ask >>= ensureRequestCount
    -- TODO: ensure there are min(N,length(bitfield) - popCount(bitField)) open requests for pieces
-- XXX: Instead of trying to be clever, use the Network.Socket.getAddrInfo
--      function to resolve addresses and connect to them.
-- XXX: Refactor this to take a list of (port:addr) pairs so we can try to connect to multiple.
-- XXX: Need to timeout so it doesn't hang. Also need to do this asynchronously so we don't just
--      pause waiting on connections and can process other events.
update (Connect port ip) = do
    infohash <- stateMachineInfoHash <$> get
    maddr <- liftIO $ resolve port ip
    case maddr of
        Nothing -> logDebugN "Could not resolve"
        Just addr -> do
            socket <- liftIO $ NS.socket (NS.addrFamily addr) (NS.addrSocketType addr) (NS.addrProtocol addr)
            ms <- liftIO $ flip timeout 1000000 $
                try (NS.connect socket $ NS.addrAddress addr)
            case ms of
                Nothing -> do
                    logDebugN $ T.pack $ "Could not connect to " ++ show (NS.hostAddressToTuple ip) ++ ":" ++ show port
                    liftIO $ NS.close socket
                Just (Left e) | isDoesNotExistError e -> do
                    logDebugN (T.pack $ "Exception from socket: " ++ show e)
                    liftIO $ NS.close socket
                Just (Left e) | otherwise -> throwM e
                Just (Right _) -> do
                    logInfoN (T.pack $ "Connected to " ++ show socket)
                    emit (OutgoingSocket infohash socket)
    where
        resolve :: Word16 -> Word32 -> IO (Maybe NS.AddrInfo)
        resolve port addr = do
            addrs <- NS.getAddrInfo 
                (Just NS.defaultHints) 
                (Just . showHost $ NS.hostAddressToTuple addr) 
                (Just $ show port)
            case addrs of
                (addr:_) -> return (Just addr)
                _        -> return Nothing
            
        showHost :: (Word8,Word8,Word8,Word8) -> String
        showHost (a,b,c,d) = intercalate "." $ map show [a,b,c,d]
        timeout :: IO a -> Int -> IO (Maybe a)
        timeout f i = do
            withAsync (threadDelay i) $ \a1 ->
                withAsync f $ \a2 ->
                    fmap 
                        (either (const Nothing) Just)
                        (waitEitherCancel a1 a2)
update (NewRequest conn idx offset len) = do
    logDebugN ("NewRequest for " <> T.pack (show idx))
    modify $ transition (EvRequest idx)
    sendTCP (connSocket conn) 
        . S.runPut 
        . S.put 
        $ Request idx offset len
update (CancelRequest conn idx offset len) = do
    logDebugN ("CancelRequest for " <> T.pack (show idx))
    modify $ transition (EvCancel idx)
    sendTCP (connSocket conn)
        . S.runPut
        . S.put
        $ Cancel idx offset len
update Complete = do
    logDebugN "Complete"
    file <- extractFile . stateMachinePiecePool <$> get
    liftIO $ BS.writeFile "/tmp/received" file
    where
        step ch buf = maybe buf (`BS.append` buf) ch

ensureConnectionCount
    :: (MonadLogger m, EventEmitter Action m, MonadState StateMachine m)
    => Int -> m ()
ensureConnectionCount min = do
    count <- numberOfConnections <$> get
    infohash <- stateMachineInfoHash <$> get
    if count >= min
        then logInfoN "All OK"
        else do
            dead <- stateMachineDeadPeers <$> get
            peers <- take (min - count) . getGoodPeers <$> get
            modify $ transition (EvConnectPeers peers)
            logDebugN . T.pack $ "Adding: " ++ show (length peers) ++ " peers"
            mapM_ (emit . uncurry Connect) peers

ensureRequestCount 
    :: (MonadLogger m, EventEmitter Action m, MonadState StateMachine m)
    => Int -> m ()
ensureRequestCount min = do
    count <- numberOfRequests <$> get
    if count >= min
        then logInfoN "Enough requests"
        else do
            need <- prioritizePieces (min - count) <$> get
            conns <- connectionPool need <$> get
            mapM_ (uncurry request) (matchPieceConnection need conns)
    where
        request n Nothing = logErrorN . T.pack $ "No connection available for piece: " ++ show n
        request n (Just conn) = emit $ NewRequest conn (fromIntegral n) 0 1024
