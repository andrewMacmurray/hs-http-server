module Http.Server.Middleware.Static where

import           Control.Monad.Reader         (ask)
import           Control.Monad.Trans          (MonadIO, liftIO)
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as C
import qualified Http.Server.Handler          as H
import qualified Http.Server.Internal.Request as Req
import           Prelude                      hiding (readFile)

serveFile :: String -> H.Handler
serveFile publicDir = do
  Req.uri <$> ask >>= (readFile . toPath publicDir) >>= H.respondBS

toPath :: String -> B.ByteString -> String
toPath publicDir uri = C.unpack $ C.pack publicDir <> uri

readFile :: MonadIO m => String -> m B.ByteString
readFile = liftIO . B.readFile
