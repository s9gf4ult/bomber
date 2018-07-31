module Bomber.Stat where

import Control.Lens
import Data.IORef
import Data.Text.IO as T
import Text.Shakespeare.Text

data Stats = Stats
  { _sTryingConnect :: !Int
  , _sAttempts      :: !Int
  , _sExceptions    :: !Int
  , _s2xx           :: !Int
  , _s4xx           :: !Int
  , _s5xx           :: !Int
  , _sxxx           :: !Int
  }

makeLenses ''Stats

defStat :: Stats
defStat = Stats 0 0 0 0 0 0 0

printStat :: Stats -> IO ()
printStat Stats{..} = T.putStrLn
  [st|Connecting: #{_sTryingConnect}, Attempts: #{_sAttempts}, Exceptions: #{_sExceptions}, 2xx: #{_s2xx}, 4xx: #{_s4xx}, 5xx: #{_s5xx}, xxx: #{_sxxx}|]

updateStat :: IORef Stats -> (Stats -> Stats) -> IO ()
updateStat ref f = atomicModifyIORef' ref ((,()) <$> f)
