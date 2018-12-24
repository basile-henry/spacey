{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM

import           Data.String.Interpolate.IsString
import           Data.Text                            (Text)
import qualified Data.Text                            as Text
import qualified Data.Text.IO                         as Text

import           Control.Concurrent.STM.TChan
import           Control.Exception
import           Data.IORef
import           System.IO

import qualified Network.Wai                          as Wai
import qualified Network.Wai.Handler.Warp             as Warp
import qualified Network.Wai.Handler.WebSockets       as WaiWs
import qualified Network.Wai.Middleware.Gzip          as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import qualified Network.Wai.Middleware.Static        as Wai
import qualified Network.WebSockets                   as WS
import           Web.Scotty                           (ActionM, ScottyM)
import qualified Web.Scotty                           as Sc

data Control = Control Int Btn State deriving (Show, Eq)

data Btn
  = Left
  | Shoot
  | Right
  deriving (Show, Eq)

instance Sc.Parsable Btn where
  parseParam "left"  = Prelude.Right Main.Left
  parseParam "shoot" = Prelude.Right Shoot
  parseParam "right" = Prelude.Right Main.Right
  parseParam btn     = Prelude.Left $ "Not a button: " <> btn

data State
  = Up
  | Down
  deriving (Show, Eq)

instance Sc.Parsable State where
  parseParam "up"   = Prelude.Right Up
  parseParam "down" = Prelude.Right Down
  parseParam state  = Prelude.Left $ "Not a state: " <> state

port :: Int
port = 1234

url :: Text
url = "localhost:" <> Text.pack (show port)

main :: IO ()
main = do
  let settings = Warp.setPort port Warp.defaultSettings

  count <- liftIO $ newIORef 0
  chan <- newTChanIO

  sapp <- Sc.scottyApp $ scottyApp count chan

  Warp.runSettings settings $
    WaiWs.websocketsOr WS.defaultConnectionOptions (wsapp count chan) sapp

scottyApp :: IORef Int -> TChan Control -> ScottyM ()
scottyApp countRef chan = do
  Sc.middleware $ Wai.gzip $ Wai.def { Wai.gzipFiles = Wai.GzipCompress }
  Sc.middleware Wai.logStdoutDev
  Sc.middleware $ Wai.staticPolicy (Wai.noDots Wai.>-> Wai.addBase ".")

  Sc.get "/" $ Sc.redirect "/site/index.html"

  Sc.get "/controller" $ do
    count <- liftIO $ atomicModifyIORef' countRef (\x -> (succ x`mod` 4, x))
    controller count

  Sc.put "/click/:index/:btn/:state" $ do
    index <- Sc.param "index"
    btn   <- Sc.param "btn"
    state <- Sc.param "state"
    liftIO . atomically . writeTChan chan $ Control index btn state

  Sc.get "/favicon.ico" $ pure ()

wsapp :: IORef Int -> TChan Control -> WS.ServerApp
wsapp count chan pending = do
  Text.putStrLn "ws connected"
  conn <- WS.acceptRequest pending

  handle
    (\e -> hPrint stderr (e :: WS.ConnectionException)) $ do

    WS.forkPingThread conn 30

    forever $ do
      Control index btn state <- atomically $ readTChan chan
      WS.sendTextData conn
        ([i|{ "index": #{ index }, "button": "#{ btn }", "state": "#{ state }" }|] :: Text)

controller :: Int -> ActionM ()
controller index = Sc.html [i|
<!DOCTYPE html>
<html>
  <head>
    <title>Spacey Control</title>
    <style>
      body { display: flex; }
      div { flex: 1; }
      img { width: 100%; }
    </style>
    <script language="javascript">
      console.log(#{ index });

      function send(btn, state) {
        var xmlHttp = new XMLHttpRequest();
        var url = "/click/#{ index }/" + btn + "/" + state;
        xmlHttp.open( "PUT", url, true );
        xmlHttp.send( null );
      };

      function pointerUp(btn) {
        send(btn, "up");
      };

      function pointerDown(btn) {
        send(btn, "down");
      };
    </script>
    <meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1, user-scalable=0">
  </head>
  <body>
    <div>
      <img src="res/left.png"
           ontouchstart=pointerDown("left")
           ontouchend=pointerUp("left")
           ontouchcancel=pointerUp("left")
           oncontextmenu="return false"
           /></div>
    <div>
      <img src="res/shoot.png"
           ontouchstart=pointerDown("shoot")
           ontouchend=pointerUp("shoot")
           ontouchcancel=pointerUp("shoot")
           oncontextmenu="return false"
           /></div>
    <div>
      <img src="res/right.png"
           ontouchstart=pointerDown("right")
           ontouchend=pointerUp("right")
           ontouchcancel=pointerUp("right")
           oncontextmenu="return false"
           /></div>
  </body>
</html>
|]
