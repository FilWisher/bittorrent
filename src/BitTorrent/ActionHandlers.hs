{-# LANGUAGE FlexibleContexts #-}

module BitTorrent.ActionHandlers where

import Control.Monad.Catch
import Control.Exception

import qualified Data.Serialize            as S
import qualified Network.Socket            as NS
import qualified Network.Socket.ByteString as NSB

import BitTorrent.Services
import BitTorrent.Action
import BitTorrent.Protocol

data HandshakeParseError = HandshakeParseError String
    deriving (Show)
instance Exception HandshakeParseError

incomingHandshake
    :: (Monad m, MonadThrow m, EventEmitter Action m, TCPCommunicator m)
    => NS.Socket -> NodeID -> m ()
incomingHandshake socket selfid = do
    hs <- S.decode <$> recvTCP socket
    case hs of
        Left err -> throwM (HandshakeParseError err)
        Right (Handshake infohash peerid) -> do
            sendTCP socket $ S.encode (Handshake infohash selfid)
            emit (HandshakeComplete infohash peerid socket)

outgoingHandshake
    :: (Monad m, MonadThrow m, EventEmitter Action m, TCPCommunicator m)
    => NS.Socket -> InfoHash -> NodeID -> m ()
outgoingHandshake socket infohash selfid = do
    sendTCP socket $ S.encode (Handshake infohash selfid)
    hs <- S.decode <$> recvTCP socket
    case hs of
        Left err -> throwM (HandshakeParseError err)
        Right (Handshake infohash peerid) -> 
            emit (HandshakeComplete infohash peerid socket)