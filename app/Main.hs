{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.UTF8 as BU
import Data.Either
import Data.Time
import Database
import Lucid
import Network.HTTP.Types
    ( Status (statusCode, statusMessage)
    )
import Network.Wai
    ( Application
    , Middleware
    , rawPathInfo
    , requestMethod
    , responseStatus
    )
import Network.Wai.Handler.Warp (run)

-- import OldWay
import Routes
import Text.Printf (printf)
import Todo

main :: IO ()
main = do
    setupDB
    putStrLn $ "Listening on http://localhost:" ++ show port
    -- putStrLn $ TL.unpack $ renderText form
    putStrLn "AppNew"
    run port $ midLog app -- run :: Port -> Application -> IO ()
    -- putStrLn "AppOrig"
    -- run port $ midLog appOrig -- run :: Port -> Application -> IO ()
    putStrLn ""
  where
    port = 3000

-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app :: Application
app req respond =
    do
        routes <- fromRight (return $ errorResponseByteString $ (" woops #404#\n" :: Html ())) $ myRouteWai req $ myRoutes req
        respond (routes)

midLog :: Middleware
midLog midApp req res = do
    start <- getCurrentTime
    let
        method = BU.toString $ requestMethod req
        path = BU.toString $ rawPathInfo req
    let res' h = do
            end <- getCurrentTime
            let
                diff = show $ diffUTCTime start end
                status = responseStatus h
                code = statusCode status
            let message = BU.toString $ statusMessage status
            printf "%d %s %s %s %s \n" code method path diff message
            res h
    midApp req res'
