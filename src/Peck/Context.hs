module Peck.Context where

import System.IO
import Prelude hiding (log)

newtype Context = Context
  { log :: String -> IO ()
  }

production :: Context
production =
  Context
    { log = hPutStrLn stderr
    }
