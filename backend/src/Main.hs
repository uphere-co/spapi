module Main where

import API

import Data.Proxy (Proxy(..))
import System.Environment
import Servant.Utils.StaticFiles (serveDirectoryFileServer)
import Servant.Server (serve)
import Network.Wai.Handler.Warp (run)
-- import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)
-- import Network.Wai.Middleware.ETag

-- staticRoot = staticApp . defaultWebAppSettings


main = do
  (d:_) <- getArgs
  putStrLn "Serving on localhost:8080/static/, visit http://localhost:8080/static/index.html"
  run 8080 $
    serve api (serveDirectoryFileServer d)
  -- etagcontext <- defaultETagContext False
  -- run 3456 $ etag etagcontext NoMaxAge $ serve api (serveDirectoryFileServer d)


  -- staticPolicy (addBase "servant") $ serve api (serveDirectory d)
  -- staticRoot d $ serve api (serveDirectory d)

api :: Proxy API
api = Proxy

