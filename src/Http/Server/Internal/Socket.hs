module Http.Server.Internal.Socket
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
  { accept      :: IO Socket
  , closeServer :: IO ()
  }

data Socket = Socket
  { send    :: B.ByteString -> IO ()
  , receive :: Int -> IO B.ByteString
  , close   :: IO ()
  }

listenOn :: Int -> IO ServerSocket
listenOn port = fromBoundNetworkSocket <$> bindTo port

bindTo :: Int -> IO NS.Socket
bindTo = N.listenOn . N.PortNumber . fromIntegral

fromBoundNetworkSocket :: NS.Socket -> ServerSocket
fromBoundNetworkSocket socket =
  ServerSocket
  { accept = (fromNetworkSocket . fst) <$> NS.accept socket
  , closeServer = NS.close socket
  }

fromNetworkSocket :: NS.Socket -> Socket
fromNetworkSocket socket =
  Socket
  { send = void . NSB.send socket
  , receive = NSB.recv socket
  , close = NS.close socket
  }
