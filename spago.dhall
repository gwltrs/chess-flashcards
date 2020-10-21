{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff-promise"
  , "console"
  , "css"
  , "effect"
  , "halogen"
  , "halogen-css"
  , "psci-support"
  , "simple-json"
  , "spec"
  , "test-unit"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
