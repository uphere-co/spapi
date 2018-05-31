module Main where

import API

import Data.Proxy (Proxy(..))
import System.Environment
import Servant.Utils.StaticFiles (serveDirectory)
import Servant.Server (serve)
-- import Network.Wai.Dispatch (dispatch)
import Network.Wai.Handler.Warp (run)
-- import Network.Wai.Middleware.Static (static,staticPolicy,addBase)
-- import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)
import Network.Wai.Middleware.ETag

-- staticRoot = staticApp . defaultWebAppSettings


main = do
  (d:_) <- getArgs
  putStrLn "Serving on localhost:8080/static/, visit http://localhost:8080/static/index.html"
  -- run 8080 $ static $
  --   serve api (serveDirectory d)
  etagcontext <- defaultETagContext False
  run 3456 $ etag etagcontext NoMaxAge $ serve api (serveDirectory d)


  -- staticPolicy (addBase "servant") $ serve api (serveDirectory d)   --  $ staticRoot d

  -- staticRoot d $ serve api (serveDirectory d)

api :: Proxy API
api = Proxy

-- main :: IO ()
-- main = putStrLn "Hello, Haskell!"
