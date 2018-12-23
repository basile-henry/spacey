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
import qualified Network.WebSockets                   as WS
import           Web.Scotty                           (ActionM, ScottyM)
import qualified Web.Scotty                           as Sc

data Control = Control Int Btn deriving (Show, Eq)

data Btn
  = Left
  | Shoot
  | Right
  deriving (Show, Eq)

instance Sc.Parsable Btn where
  parseParam "left"  = Prelude.Right Main.Left
  parseParam "shoot" = Prelude.Right Shoot
  parseParam "right" = Prelude.Right Main.Right
  parseParam btn     = Prelude.Left $ "Not a button " <> btn

port :: Int
port = 1234

url :: Text
url = "localhost:" <> Text.pack (show port)

main :: IO ()
main = do
  let port = 1234
      settings = Warp.setPort port Warp.defaultSettings

  count <- liftIO $ newIORef 0
  chan <- newTChanIO

  sapp <- Sc.scottyApp $ scottyApp count chan

  Warp.runSettings settings $
    WaiWs.websocketsOr WS.defaultConnectionOptions (wsapp count chan) sapp

scottyApp :: IORef Int -> TChan Control -> ScottyM ()
scottyApp countRef chan = do
  Sc.middleware $ Wai.gzip $ Wai.def { Wai.gzipFiles = Wai.GzipCompress }
  Sc.middleware Wai.logStdoutDev

  Sc.get "/" $ do
    count <- liftIO $ atomicModifyIORef' countRef (\x -> (succ x, x))
    controller count

  Sc.put "/click/:index/:btn" $ do
    index <- Sc.param "index"
    btn   <- Sc.param "btn"
    liftIO . atomically . writeTChan chan $ Control index btn

  Sc.get "/favicon.ico" $ pure ()

  mapM_
    (\img -> Sc.get (Sc.literal $ "/res/" <> img) $ Sc.file ("res/" <> img))
    [ "left.png"
    , "right.png"
    , "shoot.png"
    ]

wsapp :: IORef Int -> TChan Control -> WS.ServerApp
wsapp count chan pending = do
  Text.putStrLn "ws connected"
  conn <- WS.acceptRequest pending

  handle
    (\e -> do
      hPrint stderr (e :: WS.ConnectionException)
      atomicWriteIORef count 0) $ do

    WS.forkPingThread conn 30

    forever $ do
      Control index btn <- atomically $ readTChan chan
      WS.sendTextData conn ([i|
        { index : #{ index }
        , button : # { btn }
        }|] :: Text)

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
      function sendBtnClick(btn) {
        var xmlHttp = new XMLHttpRequest();
        var url = "/click/#{ index }/" + btn;
        console.log(url);
        xmlHttp.open( "PUT", url, true );
        xmlHttp.send( null );
      };
    </script>
  </head>
  <body>
    <div><img src="res/left.png"  onpointerdown=sendBtnClick("left")  /></div>
    <div><img src="res/shoot.png" onpointerdown=sendBtnClick("shoot") /></div>
    <div><img src="res/right.png" onpointerdown=sendBtnClick("right") /></div>
  </body>
</html>
|]
