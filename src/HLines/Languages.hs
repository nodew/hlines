{-# LANGUAGE OverloadedStrings #-}
module HLines.Languages where
    
import HLines.Types
import Data.Text (Text)

-- Language configuration

languages :: [Language]
languages = [
    Language {
        name = "Haskell",
        extensions = [".hs", ".lhs"],
        lineComments = ["--"],
        multiLineComments = [
            BlockCommentStyle "{-" "-}"
        ]
    },
    Language {
        name = "Python",
        extensions = [".py"],
        lineComments = ["#"],
        multiLineComments = [
            BlockCommentStyle "\"\"\"" "\"\"\""
        ]
    },
    Language {
        name = "C",
        extensions = [".c", ".h"],
        lineComments = ["//"],
        multiLineComments = [
            BlockCommentStyle "/*" "*/"
        ]
    },
    Language {
        name = "C++",
        extensions = [".cpp", ".hpp"],
        lineComments = ["//"],
        multiLineComments = [
            BlockCommentStyle "/*" "*/"
        ]
    },
    Language {
        name = "Java",
        extensions = [".java"],
        lineComments = ["//"],
        multiLineComments = [
            BlockCommentStyle "/*" "*/"
        ]
    },
    Language {
        name = "JavaScript",
        extensions = [".js"],
        lineComments = ["//"],
        multiLineComments = [
            BlockCommentStyle "/*" "*/"
        ]
    },
    Language {
        name = "TypeScript Typings",
        extensions = [".d.ts"],
        lineComments = ["//"],
        multiLineComments = [
            BlockCommentStyle "/*" "*/"
        ]
    },
    Language {
        name = "TypeScript",
        extensions = [".ts"],
        lineComments = ["//"],
        multiLineComments = [
            BlockCommentStyle "/*" "*/"
        ]
    },
    Language {
        name = "Rust",
        extensions = [".rs"],
        lineComments = ["//"],
        multiLineComments = [
            BlockCommentStyle "/*" "*/"
        ]
    },
    Language {
        name = "Ruby",
        extensions = [".rb"],
        lineComments = ["#"],
        multiLineComments = [
            BlockCommentStyle "=begin" "=end"
        ]
    },
    Language {
        name = "Shell",
        extensions = [".sh"],
        lineComments = ["#"],
        multiLineComments = []
    },
    Language {
        name = "SQL",
        extensions = ["sql"],
        lineComments = ["--"],
        multiLineComments = []
    },
    Language {
        name = "HTML",
        extensions = [".html"],
        lineComments = [],
        multiLineComments = [
            BlockCommentStyle "<!--" "-->"
        ]
    },
    Language {
        name = "CSS",
        extensions = [".css"],
        lineComments = [],
        multiLineComments = [
            BlockCommentStyle "/*" "*/"
        ]
    },
    Language {
        name = "YAML",
        extensions = [".yaml", ".yml"],
        lineComments = ["#"],
        multiLineComments = []
    },
    Language {
        name = "JSON",
        extensions = [".json"],
        lineComments = [],
        multiLineComments = []
    },
    Language {
        name = "XML",
        extensions = [".xml"],
        lineComments = [],
        multiLineComments = [
            BlockCommentStyle "<!--" "-->"
        ]
    },
    Language {
        name = "Markdown",
        extensions = [".md"],
        lineComments = [],
        multiLineComments = []
    },
    Language {
        name = "Unkown",
        extensions = [],
        lineComments = [],
        multiLineComments = []
    }]

-- Get a language configuration from a file extension
getLanguageFromExtension :: Text -> Maybe Language
getLanguageFromExtension ext = case filter (\l -> ext `elem` extensions l) languages of
    [] -> Nothing
    (l:_) -> Just l
