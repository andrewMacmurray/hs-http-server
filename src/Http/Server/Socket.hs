module Http.Server.Socket
  ( ServerSocket(..)
  , Socket(..)
  , listenOn
  ) where

import           Control.Monad             (void)
import qualified Data.ByteString           as B
import qualified Network                   as N
import qualified Network.Socket            as NS
import qualified Network.Socket.ByteString as NSB

data ServerSocket = ServerSocket
  { accept :: IO Socket
  , close' :: IO ()
  }

data Socket = Socket
  { send    :: B.ByteString -> IO ()
  , receive :: Int -> IO B.ByteString
  , close   :: IO ()
  }

listenOn :: Int -> IO ServerSocket
listenOn port = fromBoundNetworkSocket <$> bindNetworkSocket port

bindNetworkSocket :: Int -> IO NS.Socket
bindNetworkSocket = N.listenOn . N.PortNumber . fromIntegral

fromBoundNetworkSocket :: NS.Socket -> ServerSocket
fromBoundNetworkSocket socket =
  ServerSocket
  { accept = (fromNetworkSocket . fst) <$> NS.accept socket
  , close' = NS.close socket
  }

fromNetworkSocket :: NS.Socket -> Socket
fromNetworkSocket socket =
  Socket
  { send = \maxBytes -> void $ NSB.send socket maxBytes
  , receive = NSB.recv socket
  , close = NS.close socket
  }
