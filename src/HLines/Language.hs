module HLines.Language where
{-
  The rules of Language's extension and comment are all copied from https://github.com/cgag/loc
-}
import Data.Text as T

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
  deriving (Show, Eq, Ord)

data Comment = Comment
  { single :: [T.Text]
  , multi :: [(T.Text, T.Text)]
  }
getLangFromExt :: T.Text -> Language
getLangFromExt ext
  | _of ["4th", "forth", "fr", "frt", "fth", "f83", "fb", "fpm", "e4", "rx", "ft"] = Forth
  | _of ["ada", "adb", "ads", "pad"] = Ada
  | _of ["agda"] = Agda
  | _of ["as"] = ActionScript
  | _of ["at"] = AmbientTalk
  | _of ["awk"] = Awk
  | _of ["bat", "btm", "cmd"] = Batch
  | _of ["c", "ec", "pgc"] = C
  | _of ["cc", "cpp", "cxx", "c++", "pcc"] = Cpp
  | _of ["cfc"] = ColdFusionScript
  | _of ["cmake"] = CMake
  | _of ["cl"] = OpenCl
  | _of ["coffee"] = CoffeeScript
  | _of ["cs"] = CSharp
  | _of ["csh"] = CShell
  | _of ["css", "pcss", "sss", "postcss"] = Css
  | _of ["cu"] = CUDA
  | _of ["cuh"] = CUDAHeader
  | _is "d" = D
  | _is "dart" = Dart
  | _of ["dts", "dtsi"] = DeviceTree
  | _is "docker" = Docker
  | _of ["el", "lisp", "lsp", "scm", "ss", "rkt"] = Lisp
  | _of ["ex", "exs"] = Elixir
  | _is "elm" = Elm
  | _of ["erl", "hrl"] = Erlang
  | _is "feature" = Gherkin
  | _of ["fs", "fsx"] = FSharp
  | _of ["vert", "tesc", "tese", "geom", "frag", "comp"] = Glsl
  | _is "go" = Go
  | _is "groovy" = Groovy
  | _of ["h", "hh", "hpp", "hxx"] = CCppHeader
  | _of ["hbs", "handlebars"] = Handlebars
  | _is "hs" = Haskell
  | _is "html" = Html
  | _of ["idr", "lidr"] = Idris
  | _is "ini" = INI
  | _is "jai" = Jai
  | _is "java" = Java
  | _is "jl" = Julia
  | _of ["js", "mjs"] = JavaScript
  | _is "jsx" = Jsx
  | _of ["kt", "kts"] = Kotlin
  | _is "lds" = LinkerScript
  | _of ["lean", "hlean"] = Lean
  | _is "less" = Less
  | _is "lua" = Lua
  | _is "m" = ObjectiveC
  | _of ["ml", "mli"] = OCaml
  | _of ["nb", "wl"] = Wolfram
  | _is "sh" = BourneShell
  | _of ["asa", "asp"] = Asp
  | _of ["asax", "ascx", "asmx", "aspx", "master", "sitemap", "webinfo"] = AspNet
  | _is "in" = Autoconf
  | _of ["clj", "cljs", "cljc"] = Clojure
  | _of ["f", "for", "ftn", "f77", "pfo"] = FortranLegacy
  | _of ["f03", "f08", "f90", "f95"] = FortranModern
  | _of ["makefile", "mk"] = Makefile
  | _is "mm" = ObjectiveCpp
  | _is "nim" = Nim
  | _is "nix" = Nix
  | _is "php" = PHP
  | _of ["pl", "pm"] = Perl
  | _is "pp" = Puppet
  | _is "qcl" = Qcl
  | _is "qml" = Qml
  | _is "cshtml" = Razor
  | _is "mustache" = Mustache
  | _is "oz" = Oz
  | _of ["p", "pro"] = Prolog
  | _is "pas" = Pascal
  | _is "hex" = Hex
  | _is "ihex" = IntelHex
  | _is "json" = Json
  | _of ["markdown", "md"] = Markdown
  | _is "rst" = ReStructuredText
  | _of ["text", "txt"] = Text
  | _is "polly" = Polly
  | _of ["ps1", "psd1", "psm1"] = PowerShell
  | _is "proto" = Protobuf
  | _is "purs" = PureScript
  | _is "arr" = Pyret
  | _is "py" = Python
  | _is "r" = R
  | _of ["rake", "rb"] = Ruby
  | _of ["rhtml", "erb"] = RubyHtml
  | _is "rs" = Rust
  | _of ["s", "asm"] = Assembly
  | _of ["sass", "scss"] = Sass
  | _of ["sc", "scala"] = Scala
  | _is "sls" = SaltStack
  | _is "sml" = Sml
  | _is "sql" = Sql
  | _is "styl" = Stylus
  | _is "swift" = Swift
  | _is "tcl" = Tcl
  | _is "tf" = Terraform
  | _of ["tex", "sty"] = Tex
  | _is "toml" = Toml
  | _is "ts" = TypeScript
  | _is "tsx" = Tsx
  | _is "thy" = Isabelle
  | _of ["uc", "uci", "upkg"] = UnrealScript
  | _is "v" = Coq
  | _is "vim" = VimScript
  | _is "xml" = XML
  | _of ["yaml", "yml"] = Yaml
  | _is "y" = Yacc
  | _is "zig" = Zig
  | _is "zsh" = Zsh
  | _is "hx" = Haxe
  | otherwise = Unknown
  where
    ext' = T.drop 1 ext
    _of = elem ext'
    _is = (==) ext'

noComment = Comment [] []
cStyle = Comment ["//"] [("/*", "*/")]
htmlStyle = Comment [] [("<!--", "-->")]
mlStyle = Comment [] [("(*", "*)")]
prologStyle = Comment [] [("/*", "*/")]
shStyle = Comment ["#"] []

getCommentStyle :: Language -> Comment
getCommentStyle lang
  | _is Ada = Comment ["--"] []
  | _is Batch = Comment ["REM"] []
  | _is FortranModern = Comment ["!"] []
  | _is INI = Comment [";"] []
  | _is VimScript = Comment ["\""] []
  | _is Terraform = Comment ["#"] [("/*", "*/")]
  | _is Nix = Comment ["#"] [("/*", "*/")]
  | _is Assembly = Comment ["#"] [("/*", "*/")]
  | _is CMake = Comment ["#"] [("#[[", "]]")]
  | _is CoffeeScript = Comment ["#"] [("###", "###")]
  | _is D = Comment ["//"] [("/*", "*/")]
  | _is Docker = Comment ["#"] []
  | _is Forth = Comment ["\\"] [("(", ")")]
  | _is FSharp = Comment ["//"] [("(*", "*)")]
  | _is Julia = Comment ["#"] [("#=", "=#")]
  | _is Lisp = Comment [";"] [("#|", "|#")]
  | _is Lean = Comment ["--"] [("/-", "-/")]
  | _is Lua = Comment ["--"] [("--[[", "]]")]
  | _is Perl = Comment ["#"] [("=pod", "=cut")]
  | _is Puppet = Comment ["#"] []
  | _is Pyret = Comment ["#"] [("#|", "|#")]
  | _is Python = Comment ["#"] [("'''", "'''")]
  | _is Ruby = Comment ["#"] [("=begin", "=end")]
  | _is Sql = Comment ["--"] [("/*", "*/")]
  | _is ColdFusion = Comment [] [("<!---", "--->")]
  | _is Mustache = Comment [] [("{{!", "}}")]
  | _is Asp = Comment ["'", "REM"] []
  | _is AspNet = Comment [] [("<!--", "-->"), ("<%--", "-->")]
  | _is Autoconf = Comment ["#", "dnl"] []
  | _is Clojure = Comment [";", "#"] []
  | _is FortranLegacy = Comment ["c", "C", "!", "*"] []
  | _is Handlebars = Comment [] [("<!--", "-->"), ("{{!", "}}")]
  | _is PHP = Comment ["#", "//"] [("/*", "*/")]
  | _is PowerShell = Comment ["#"] [("<#", "#>")]
  | _is Isabelle = Comment ["--"] [("{*", "*}"), ("(*", "*)"), ("‹", "›"), ("\\<open>", "\\<close>")]
  | _is Razor = Comment [] [("<!--", "-->"), ("@*", "*@")]
  | _is Pascal = Comment ["//", "(*"] [("{", "}")]
  | _of [Protobuf, Zig] = Comment ["//"] []
  | _of [Erlang, Tex] = Comment ["%"] []
  | _of [Text, Markdown, Json, IntelHex, Hex, ReStructuredText] = noComment
  | _of [Haskell, Idris, Agda, PureScript, Elm] = Comment ["--"] [("{-", "-}")]
  | _of [Oz, Prolog] = prologStyle
  | _of [Coq, Sml, Wolfram, OCaml] = mlStyle
  | _of [Html, Polly, RubyHtml, XML] = htmlStyle
  | _of [BourneShell, Make, Awk, CShell, Gherkin, Makefile, Nim, R, SaltStack, Tcl, Toml, Yaml, Zsh, Elixir] = shStyle
  | _of
     [ AmbientTalk
     , C
     , CCppHeader
     , Rust
     , Yacc
     , ActionScript
     , ColdFusionScript
     , Css
     , Cpp
     , CUDA
     , CUDAHeader
     , CSharp
     , Dart
     , DeviceTree
     , Glsl
     , Go
     , Jai
     , Java
     , JavaScript
     , Jsx
     , Kotlin
     , Less
     , LinkerScript
     , ObjectiveC
     , ObjectiveCpp
     , OpenCl
     , Qcl
     , Sass
     , Scala
     , Swift
     , TypeScript
     , Tsx
     , UnrealScript
     , Stylus
     , Qml
     , Haxe
     , Groovy
     ] = cStyle
  | otherwise = noComment
  where
    _of = elem lang
    _is = (==) lang

