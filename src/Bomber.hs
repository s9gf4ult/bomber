module Bomber
    ( dropBombs
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Foldable
import Data.Monoid
import Network.Connection
import Network.HTTP.Client
import Network.HTTP.Client.OpenSSL
import Network.HTTP.Client.TLS
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
  let
    worker = try go >>= \case
      Left e -> case fromException e of
        Just (_ :: AsyncException) -> return ()
        Nothing                    -> worker
      Right _ -> worker
    go = void $ httpLbs req manager
  workers <- sequence $ replicate (simRequests opts) (forkIO worker)
  let
    sleep = forever $ threadDelay 1e6
    killWorkers = for_ workers $ \w -> do
      killThread w
  finally sleep killWorkers
