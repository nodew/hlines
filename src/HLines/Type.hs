{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module HLines.Type where

import System.FilePath.GlobPattern
import Control.Lens
import Data.Data
import Data.Text as T
import Data.Typeable

data Language
  = ActionScript
  | Ada
  | Agda
  | AmbientTalk
  | Asp
  | AspNet
  | Assembly
  | Autoconf
  | Awk
  | Batch
  | BourneShell
  | C
  | CCppHeader
  | CMake
  | CSharp
  | CShell
  | Clojure
  | CoffeeScript
  | ColdFusion
  | ColdFusionScript
  | Coq
  | Cpp
  | Css
  | CUDA
  | CUDAHeader
  | D
  | Dart
  | DeviceTree
  | Docker
  | Elixir
  | Elm
  | Erlang
  | Forth
  | FortranLegacy
  | FortranModern
  | FSharp
  | Gherkin
  | Glsl
  | Go
  | Groovy
  | Handlebars
  | Haskell
  | Hex
  | Html
  | INI
  | Idris
  | IntelHex
  | Isabelle
  | Jai
  | Java
  | JavaScript
  | Json
  | Jsx
  | Julia
  | Kotlin
  | Less
  | LinkerScript
  | Lean
  | Lisp
  | Lua
  | Make
  | Makefile
  | Markdown
  | Mustache
  | Nim
  | Nix
  | OCaml
  | ObjectiveC
  | ObjectiveCpp
  | OpenCl
  | Oz
  | Pascal
  | Perl
  | PHP
  | Polly
  | PowerShell
  | Prolog
  | Protobuf
  | Puppet
  | PureScript
  | Pyret
  | Python
  | Qcl
  | Qml
  | R
  | Razor
  | ReStructuredText
  | Ruby
  | RubyHtml
  | Rust
  | SaltStack
  | Sass
  | Scala
  | Sml
  | Sql
  | Stylus
  | Swift
  | Tcl
  | Terraform
  | Tex
  | Text
  | Toml
  | TypeScript
  | Tsx
  | UnrealScript
  | VimScript
  | Wolfram
  | XML
  | Yacc
  | Yaml
  | Zig
  | Zsh
  | Haxe
  | Unknown
  deriving (Show, Data, Eq, Ord)

data ListBy
  = ListByLang
  | ListByFile
  deriving (Show, Eq, Data)

data SortBy
  = SortByFile
  | SortByComment
  | SortByCode
  | SortByBlank
  deriving (Show, Eq, Data)

data Options = Options
  { files :: [FilePath]
  , include :: Maybe [GlobPattern]
  , exclude :: Maybe [GlobPattern]
  , match :: Maybe [GlobPattern]
  , language :: Maybe [Language]
  , extension :: Maybe [String]
  , listBy :: ListBy
  , sortBy :: SortBy
  } deriving (Show, Data, Typeable)

data Count = Count
  { _code :: Int
  , _blank :: Int
  , _comment :: Int
  , _total :: Int
  } deriving (Show)

makeLenses ''Count

data Comment = Comment
  { single :: [T.Text]
  , multi :: [(T.Text, T.Text)]
  }
