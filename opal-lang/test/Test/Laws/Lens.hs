module Test.Laws.Lens
  ( lens,
  )
where

import Hedgehog (Gen, forAll, (===))

import Test.Core (TestName, TestTree, property, testGroup, testProp)

--------------------------------------------------------------------------------

lens ::
  (Applicative f, Eq (f a), Show (f a), Eq (f b), Show (f b), Eq a, Show a, Eq b, Show b) =>
  -- | Name of the lens
  TestName ->
  -- | The getter function
  (a -> f b) ->
  -- | The setting function
  (a -> b -> a) ->
  -- | Generator for the "outer" structure
  Gen a ->
  -- | Generator for the "inner" structure
  Gen b ->
  TestTree
lens name view set outer inner =
  testGroup
    name
    [ testProp "view l (set l v s) ≡ v" $ property do
        struct <- forAll outer
        field <- forAll inner
        view (set struct field) === pure field
    , testProp "set l (view l s) s ≡ s" $ property do
        struct <- forAll outer
        fmap (set struct) (view struct) === pure struct
    , testProp "set l v' (set l v s) ≡ set l v' s" $ property do
        struct <- forAll outer
        field0 <- forAll inner
        field1 <- forAll inner
        set (set struct field0) field1 === set struct field1
    ]