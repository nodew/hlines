module HLines.Cmd where

import HLines.Type
import System.Console.CmdArgs

options =
  Options
    { files = def &= args &= typ "FILES/DIRS"
    , include = def &= help "Include dirs"
    , exclude = def &= help "Exclude dirs"
    , match = def &= help "Match filename to search"
    , language = def &= name "lang" &= typ "Lang" &= help "Language to search"
    , extension = def &= name "ext" &= typ "EXT" &= help "File extensions to search"
    , listBy = enum [ListByLang &= help "List result by lang - DEFAULT", ListByFile &= help "List result by file"]
    , sortBy =
        enum
          [ SortByCode &= help "Sort result by code lines - DEFAULT"
          , SortByFile &= help "Sort result by file counts"
          , SortByComment &= help "Sort result by comment lines"
          , SortByBlank &= help "Sort result by blank lines"
          ]
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
