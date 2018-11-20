{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE BangPatterns               #-}

module BitTorrent.Monad where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger

import Data.List (intercalate)

import Control.Concurrent (threadDelay, myThreadId)
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Network.Socket            as NS
import qualified Network.Socket.ByteString as NSB

import qualified Data.Serialize as S

import BitTorrent.Action
import BitTorrent.Services
import BitTorrent.StateMachine
import BitTorrent.Protocol
import BitTorrent.Connection
import BitTorrent.Exceptions

import Data.BitField (bits)
import qualified Data.ByteString as BS

import Data.Bits ((.&.), bit, popCount, testBit)
import Data.Word
import Data.List

import Process

import qualified Data.Text as T
import System.IO.Error

data Config = Config
    { configChan         :: TChan Action
    , configId           :: NodeID
    , configConnCount    :: Int
    , configReqCount     :: Int
    , configStateMachine :: TVar StateMachine
    }

data BitTorrentState = BitTorrentState
    { btStateMachine :: TVar StateMachine
    }

instance HasActionChan Config where
    getActionChan = configChan

newtype BitTorrentM a = BitTorrentM
    { unBitTorrentM :: LoggingT (Process Config) a
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadReader Config
        , MonadThrow
        , MonadCatch
        , MonadLogger
        , MonadIO
        )

instance MonadState StateMachine BitTorrentM where
    state f = do
        tvar <- configStateMachine <$> ask
        liftIO . atomically $ do
            st <- readTVar tvar
            let (v, st') = f st
            writeTVar tvar st'
            return v
            

runBitTorrentM :: Int -> InfoHash -> [(Word16, Word32)] -> BitTorrentM a -> IO a
runBitTorrentM n infohash peers proc = do
    ch <- liftIO $ newTChanIO
    st <- liftIO . newTVarIO $ initialState n infohash peers
    runProcess 
        (Config ch "TODO: generate random peer ID" 22 5 st)
        (runStderrLoggingT (unBitTorrentM proc))

forkBitTorrentM :: Config -> BitTorrentM a -> IO (Async a)
forkBitTorrentM config proc = runProcessAsync config (runStderrLoggingT $ unBitTorrentM proc)

instance Forkable BitTorrentM where
    forkM proc = do
        conf <- ask
        liftIO $ forkBitTorrentM conf proc

instance TCPCommunicator BitTorrentM where
    sendTCP socket buf = void . liftIO $ NSB.send socket buf
    recvTCP socket     = liftIO $ NSB.recv socket 1024

data ListenerConfig = ListenerConfig
    { listenerSocket :: NS.Socket
    , listenerChan   :: TChan Action
    }

instance HasActionChan ListenerConfig where
    getActionChan = listenerChan

instance TCPCommunicator (Process c) where
    sendTCP socket buf = void . liftIO $ NSB.send socket buf
    recvTCP socket     = liftIO $ NSB.recv socket 1024
    
type ListenerM a = Process ListenerConfig a

connectionListenerP :: NodeID -> ListenerM ()
connectionListenerP peerid = do
    sock <- listenerSocket <$> ask
    emsg <- S.runGet S.get <$> recvTCP sock
    case emsg of
        Left err -> throwM (ParseMessageException err)
        Right msg -> do
            emit (IncomingMessage peerid msg)
            connectionListenerP peerid

debug :: T.Text -> BitTorrentM ()
debug str = do
    tid <- liftIO myThreadId
    logDebugN (T.pack (show tid) <> ": " <> str)

info :: T.Text -> BitTorrentM ()
info str = do
    tid <- liftIO myThreadId
    logInfoN (T.pack (show tid) <> ": " <> str)


-- | Interpreter the action.
update :: Action -> BitTorrentM ()
update (IncomingSocket socket) = do
    debug "IncomingSocket"
    configId <$> ask >>= incomingHandshake socket
update (OutgoingSocket infohash socket) = do
    debug ("Outgoing socket for: " <> (T.pack . show $ infohash))
    selfid <- configId <$> ask
    handle onError $ do
        outgoingHandshake socket infohash selfid
        info "Successful handshake"
    where
        onError :: HandshakeParseError -> BitTorrentM ()
        onError (HandshakeParseError err) =
            debug (T.pack $ "BitTorrent.Monad.HandshakeParseError: " ++ show err)
update (ConnectionClosed peerid) = do
    debug ("Connection closed to " <> (T.pack $ show peerid))
    c <- getConnection peerid <$> get
    case c of
        -- XXX: This shouldn't happen. It should probably be logged.
        Nothing   -> return ()
        Just conn -> do
            liftIO $ closeConnection conn
            modify $ transition (EvRemoveConnection peerid)
update (IncomingMessage peerid msg) = do
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
        Cancel idx offset len -> return ()
        Port port             -> return ()
update Tick = do
    debug "Tick"
    configConnCount <$> ask >>= ensureConnectionCount
    debug "Connection count done"
    configReqCount  <$> ask >>= ensureRequestCount
    debug "Request count done"
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
        Nothing -> debug "Could not resolve"
        Just addr -> do
            socket <- liftIO $ NS.socket (NS.addrFamily addr) (NS.addrSocketType addr) (NS.addrProtocol addr)
            ms <- liftIO $ flip timeout 1000000 $
                try (NS.connect socket $ NS.addrAddress addr)
            case ms of
                Nothing -> do
                    debug $ T.pack $ "Could not connect to " ++ show (NS.hostAddressToTuple ip) ++ ":" ++ show port
                    liftIO $ NS.close socket
                Just (Left e) | isDoesNotExistError e -> do
                    debug (T.pack $ "Exception from socket: " ++ show e)
                    liftIO $ NS.close socket
                Just (Left e) | otherwise -> throwM e
                Just (Right _) -> do
                    info (T.pack $ "Connected to " ++ show socket)
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
    debug ("NewRequest for " <> T.pack (show idx))
    modify $ transition (EvRequest idx)
    sendTCP (connSocket conn) 
        . S.runPut 
        . S.put 
        $ Request idx offset len
update (CancelRequest conn idx offset len) = do
    debug ("CancelRequest for " <> T.pack (show idx))
    modify $ transition (EvCancel idx)
    sendTCP (connSocket conn)
        . S.runPut
        . S.put
        $ Cancel idx offset len
update Complete = do
    debug "Complete"
    file <- extractFile . stateMachinePiecePool <$> get
    liftIO $ BS.writeFile "/tmp/received" file
    where
        step ch buf = maybe buf (`BS.append` buf) ch

ensureConnectionCount :: Int -> BitTorrentM ()
ensureConnectionCount min = do
    count <- numberOfConnections <$> get
    infohash <- stateMachineInfoHash <$> get
    if count >= min
        then info "All OK"
        else do
            st <- get
            let dead = stateMachineDeadPeers st
                peers = take (min - count) (getGoodPeers st)
            modify $ transition (EvConnectPeers peers)
            debug . T.pack $ "Adding: " ++ show (length peers) ++ " peers"
            mapM_ (emit . uncurry Connect) peers

ensureRequestCount :: Int -> BitTorrentM ()
ensureRequestCount min = do
    count <- numberOfRequests <$> get
    if count >= min
        then info "Enough requests"
        else do
            need <- prioritizePieces (min - count) <$> get
            debug "Done prioritizing"
            conns <- connectionPool need <$> get
            debug "Done getting conns"

            let matched = matchPieceConnection need conns
                matchedConns = map (\n -> (n, snd <$> find (flip testBit n . fst) matched)) (bits need)
                
            debug "Done matchin"
            -- XXX: Needs refactored, this is inelegant.

            forM_ matchedConns $ \(n, mc) -> do
                case mc of
                    Nothing -> debug "No matching connection"
                    Just conn@Connection{..} -> 
                        if choked connSelf
                            then do
                                debug "Informing interest"
                                sendTCP connSocket $ S.encode Interested
                            else do
                                debug ("Sending request for " <> T.pack (show n))
                                emit $ NewRequest conn (fromIntegral n) 0 1024
                
            debug "Done sending"
    where

        getConns :: [(Int, Maybe Connection)] -> [(Int, Connection)]
        getConns ((_,Nothing):xs) = getConns xs
        getConns ((n,Just x):xs) = (n,x):getConns xs


data HandshakeParseError = HandshakeParseError String
    deriving (Show)
instance Exception HandshakeParseError

incomingHandshake :: NS.Socket -> NodeID -> BitTorrentM ()
incomingHandshake socket selfid = do
    hs <- S.decode <$> recvTCP socket
    case hs of
        Left err -> throwM (HandshakeParseError err)
        Right (Handshake infohash peerid) -> do
            sendTCP socket $ S.encode (Handshake infohash selfid)
            completeHandshake infohash peerid socket

outgoingHandshake :: NS.Socket -> InfoHash -> NodeID -> BitTorrentM ()
outgoingHandshake socket infohash selfid = do
    sendTCP socket $ S.encode (Handshake infohash selfid)
    buf <- recvTCP socket
    case S.decode buf of
        Left err -> do
            debug (T.pack $ show buf)
            throwM (HandshakeParseError err)
        Right (Handshake infohash peerid) ->
            completeHandshake infohash peerid socket

completeHandshake :: InfoHash -> NodeID -> NS.Socket -> BitTorrentM ()
completeHandshake infohash peerid socket = do
    debug ("Handshake complete with peer: " <> (T.pack $ show peerid))
    ch <- configChan <$> ask
    conn <- liftIO $ createConnection socket peerid (runProcess (ListenerConfig socket ch) (connectionListenerP peerid))
    modify $ transition (EvNewConnection peerid conn)
