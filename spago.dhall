{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "alga"
, dependencies =
  [ "console"
  , "effect"
  , "folds"
  , "lists"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
