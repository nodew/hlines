{-# LANGUAGE OverloadedStrings #-}

module HLines.Languages where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HashMap
import HLines.Types

-- Language configuration

languages :: [Language]
languages =
  [ Language
      { name = "Ada",
        extensions = [".ada", ".adb", ".ads"],
        lineComments = ["--"],
        multiLineComments = []
      },
    Language
      { name = "Agda",
        extensions = [".agda"],
        lineComments = ["--"],
        multiLineComments =
          [ BlockCommentStyle "{-" "-}"
          ]
      },
    Language
      { name = "Assembly",
        extensions = [".s", ".asm"],
        lineComments = [";"],
        multiLineComments =
          [ BlockCommentStyle "/*" "*/"
          ]
      },
    Language
      { name = "Haskell",
        extensions = [".hs", ".lhs"],
        lineComments = ["--"],
        multiLineComments =
          [ BlockCommentStyle "{-" "-}"
          ]
      },
    Language
      { name = "PHP",
        extensions = [".php"],
        lineComments = ["#", "//"],
        multiLineComments =
          [ BlockCommentStyle "/*" "*/"
          ]
      },
    Language
      { name = "Perl",
        extensions = [".pl", ".pm"],
        lineComments = ["#"],
        multiLineComments = []
      },
    Language
      { name = "Python",
        extensions = [".py", ".pyw", ".pyi"],
        lineComments = ["#"],
        multiLineComments =
          [ BlockCommentStyle "\"\"\"" "\"\"\""
          ]
      },
    Language
      { name = "C",
        extensions = [".c", ".h"],
        lineComments = ["//"],
        multiLineComments =
          [ BlockCommentStyle "/*" "*/"
          ]
      },
    Language
      { name = "C++",
        extensions = [".cpp", ".hpp"],
        lineComments = ["//"],
        multiLineComments =
          [ BlockCommentStyle "/*" "*/"
          ]
      },
    Language
      { name = "C#",
        extensions = [".cs", ".csx"],
        lineComments = ["//"],
        multiLineComments =
          [ BlockCommentStyle "/*" "*/"
          ]
      },
    Language
      { name = "Java",
        extensions = [".java"],
        lineComments = ["//"],
        multiLineComments =
          [ BlockCommentStyle "/*" "*/"
          ]
      },
    Language
      { name = "JavaScript",
        extensions = [".js", ".mjs"],
        lineComments = ["//"],
        multiLineComments =
          [ BlockCommentStyle "/*" "*/"
          ]
      },
    Language
      { name = "TypeScript Typings",
        extensions = [".d.ts"],
        lineComments = ["//"],
        multiLineComments =
          [ BlockCommentStyle "/*" "*/"
          ]
      },
    Language
      { name = "TypeScript",
        extensions = [".ts"],
        lineComments = ["//"],
        multiLineComments =
          [ BlockCommentStyle "/*" "*/"
          ]
      },
    Language
      { name = "F#",
        extensions = [".fs", ".fsi", ".fsx", ".fsscript"],
        lineComments = ["//"],
        multiLineComments =
          [ BlockCommentStyle "(*" "*)"
          ]
      },
    Language
      { name = "Go",
        extensions = [".go"],
        lineComments = ["//"],
        multiLineComments =
          [ BlockCommentStyle "/*" "*/"
          ]
      },
    Language
      { name = "Julia",
        extensions = [".jl"],
        lineComments = ["#"],
        multiLineComments =
          [ BlockCommentStyle "#=" "=#"
          ]
      },
    Language
      { name = "Kotlin",
        extensions = [".kt", ".kts"],
        lineComments = ["//"],
        multiLineComments =
          [ BlockCommentStyle "/*" "*/"
          ]
      },
    Language
      { name = "MATLAB",
        extensions = [".m"],
        lineComments = ["%"],
        multiLineComments =
          [ BlockCommentStyle "%{" "}%"
          ]
      },
    Language
      { name = "OCaml",
        extensions = [".ml", ".mli"],
        lineComments = [],
        multiLineComments =
          [ BlockCommentStyle "(*" "*)"
          ]
      },
    Language
      { name = "R",
        extensions = [".r"],
        lineComments = ["#"],
        multiLineComments = []
      },
    Language
      { name = "Ruby",
        extensions = [".rb"],
        lineComments = ["#"],
        multiLineComments =
          [ BlockCommentStyle "=begin" "=end"
          ]
      },
    Language
      { name = "Rust",
        extensions = [".rs"],
        lineComments = ["//"],
        multiLineComments =
          [ BlockCommentStyle "/*" "*/"
          ]
      },
    Language
      { name = "Shell",
        extensions = [".sh"],
        lineComments = ["#"],
        multiLineComments = []
      },
    Language
      { name = "Scala",
        extensions = [".sc", ".scala"],
        lineComments = ["//"],
        multiLineComments =
          [ BlockCommentStyle "/*" "*/"
          ]
      },
    Language
      { name = "SQL",
        extensions = [".sql", ".dml", ".ddl", ".dql"],
        lineComments = ["--"],
        multiLineComments = 
          [ BlockCommentStyle "/*" "*/"
          ]
      },
    Language
      { name = "Swift",
        extensions = [".swift"],
        lineComments = ["//"],
        multiLineComments =
          [ BlockCommentStyle "/*" "*/"
          ]
      },
    Language
      { name = "HTML",
        extensions = [".html"],
        lineComments = [],
        multiLineComments =
          [ BlockCommentStyle "<!--" "-->"
          ]
      },
    Language
      { name = "CSS",
        extensions = [".css"],
        lineComments = [],
        multiLineComments =
          [ BlockCommentStyle "/*" "*/"
          ]
      },
    Language
      { name = "YAML",
        extensions = [".yaml", ".yml"],
        lineComments = ["#"],
        multiLineComments = []
      },
    Language
      { name = "JSON",
        extensions = [".json"],
        lineComments = [],
        multiLineComments = []
      },
    Language
      { name = "XML",
        extensions = [".xml"],
        lineComments = [],
        multiLineComments =
          [ BlockCommentStyle "<!--" "-->"
          ]
      },
    Language
      { name = "Markdown",
        extensions = [".md"],
        lineComments = [],
        multiLineComments = []
      },
    Language
      { name = "TCL",
        extensions = [".tcl"],
        lineComments = ["#"],
        multiLineComments = []
      },
    Language
      { name = "TOML",
        extensions = [".toml"],
        lineComments = ["#"],
        multiLineComments = []
      },
    Language
      { name = "Dockerfile",
        extensions = ["Dockerfile"],
        lineComments = [],
        multiLineComments = []
      },
    Language
      { name = "Makefile",
        extensions = ["Makefile"],
        lineComments = ["#"],
        multiLineComments = []
      }
  ]

extentionToLangMap :: HashMap.HashMap ByteString Language
extentionToLangMap = HashMap.fromList $ do
  lang <- languages
  ext <- extensions lang
  return (ext, lang)

-- Get a language configuration from a file extension
getLanguageFromExtension :: ByteString -> Maybe Language
getLanguageFromExtension ext = HashMap.lookup ext extentionToLangMap
