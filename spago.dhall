{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "arrays"
  , "css"
  , "datetime"
  , "dom-indexed"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "free"
  , "halogen"
  , "halogen-css"
  , "integers"
  , "maybe"
  , "newtype"
  , "now"
  , "partial"
  , "prelude"
  , "random"
  , "simple-json"
  , "strings"
  , "test-unit"
  , "tuples"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
