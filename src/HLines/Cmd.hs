module HLines.Cmd where

import HLines.Type
import System.Console.CmdArgs

options =
  Options
    { files = def &= args &= typ "FILES/DIRS"
    , ignore_ = def &= help "Ignore pattern"
    , language = def &= help "Count lines by language"
    } &=
  verbosity &=
  help "Counts your source code" &=
  summary "hlines v1.0.0" &=
  details
    [ "hlines help you count your source code"
    , ""
    , "To count your javascript code lines under src folder"
    , "  hlint src --language=javascript"
    ]

mode = cmdArgsMode options
