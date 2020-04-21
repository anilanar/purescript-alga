module Test.Algebra.Graph.LabelledSpec where

import Prelude
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "purescript-spec" do
    describe "Attributes" do
      it "awesome" do
        let
          isAwesome = true
        isAwesome `shouldEqual` true
