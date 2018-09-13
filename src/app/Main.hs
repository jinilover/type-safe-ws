module Main where

import Servant

import Services
import ApiType
import Data
import Config
import ConfigType
import Network.Wai.Handler.Warp

main :: IO ()
main = loadAppConfig "src/resources/appl.cfg" >>= ((`run` app) . port)

server :: Server UserAPI
server = return . sortBy

userApi :: Proxy UserAPI
userApi = Proxy

app :: Application
app = serve userApi server
