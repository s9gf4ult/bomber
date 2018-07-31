module Bomber
    ( dropBombs
    ) where

import Bomber.Stat
import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad
import Data.Foldable
import Data.IORef
import Data.Monoid
import Network.Connection
import Network.HTTP.Client
import Network.HTTP.Client.OpenSSL
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status
import OpenSSL.Session
import Options.Applicative

data Backend = Raw | Tls | OpenSSL
  deriving (Show, Read)

data Opts = Opts
  { poolSize :: Int
  , simRequests :: Int
  , target :: String
  , backend :: Backend
  , validateCert :: Bool
  }

parseOpts :: IO Opts
parseOpts = execParser $ info (helper <*> parser) infoMod
  where
    parser = Opts
      <$> (option auto (short 'p' <> help "Pool size") <|> pure 10)
      <*> (option auto (short 'r' <> help "Simultaneous requests") <|> pure 10)
      <*> (strOption (short 't' <> help "Tagret URI"))
      <*> (option auto (short 'b' <> help "Backend") <|> pure Raw)
      <*> (switch (long "validate-cert" <> help "Validate SSL certificates (no by default)"))
    infoMod = progDesc "Sends multiple requests to same address"

getManager :: Opts -> IO Manager
getManager opts = do
  let
    settings' = case backend opts of
      Raw     -> defaultManagerSettings
      Tls     ->
        mkManagerSettings (TLSSettingsSimple (not $ validateCert opts) False False) Nothing
      OpenSSL -> opensslManagerSettings $ do
        ctx <- context
        unless (validateCert opts) $ do
          contextSetVerificationMode ctx VerifyNone
        return ctx
    settings = settings'
      { managerConnCount = poolSize opts }
    wrap = case backend opts of
      OpenSSL -> withOpenSSL
      _       -> id
  wrap $ newManager settings

dropBombs :: IO ()
dropBombs = do
  opts <- parseOpts
  manager <- getManager opts
  req <- parseRequest $ target opts
  stat <- newIORef defStat
  let
    worker = do
      updateStat stat $ sTryingConnect +~ 1
      try go >>= \case
        Left e -> case fromException e of
          Just (_ :: AsyncException) -> return ()
          Nothing                    -> do
            updateStat stat
              $ (sTryingConnect -~ 1)
              . (sExceptions +~ 1)
            worker
        Right resp -> do
          let
            code = statusCode $ responseStatus resp
            status = if
              | code >= 500 -> s5xx +~ 1
              | code >= 400 -> s4xx +~ 1
              | code >= 200 -> s2xx +~ 1
              | otherwise   -> sxxx +~ 1
          updateStat stat
            $ (sTryingConnect -~ 1)
            . status
          worker
    go = httpLbs req manager
  workers <- sequence $ replicate (simRequests opts) (forkIO worker)
  let
    sleep = forever $ do
      printStat =<< readIORef stat
      threadDelay 1e6
    killWorkers = for_ workers $ \w -> do
      killThread w
  finally sleep killWorkers
