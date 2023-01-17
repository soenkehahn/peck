{-# LANGUAGE DerivingStrategies #-}

module Peck.Error where

import Control.Exception
import Data.Maybe
import System.Exit
import System.IO

data PeckError = PeckError
  { exitCode :: Maybe ExitCode,
    message :: String
  }
  deriving stock (Show, Eq)

instance Exception PeckError

peckError :: String -> PeckError
peckError = PeckError Nothing

handleErrors :: IO () -> IO ExitCode
handleErrors action = do
  catch (action >> return ExitSuccess) $ \(PeckError exitCode message) -> do
    hPutStrLn stderr message
    return $ fromMaybe (ExitFailure 1) exitCode
