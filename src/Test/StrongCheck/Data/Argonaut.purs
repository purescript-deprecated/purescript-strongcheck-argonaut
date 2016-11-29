{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Test.StrongCheck.Data.Argonaut
  ( ArbJson
  , runArbJson
  , genJson
  , ArbJCursor
  , runArbJCursor
  , genJCursor
  ) where

import Prelude

import Control.Lazy (defer)

import Data.Argonaut (Json, JCursor(..), jsonEmptyObject, (~>), (:=), fromArray, fromString, fromNumber, fromBoolean, jsonNull)
import Data.Array as A

import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.StrongCheck.Gen (Gen, arrayOf, oneOf, resize, sized)

newtype ArbJson = ArbJson Json

runArbJson ∷ ArbJson → Json
runArbJson (ArbJson m) = m

instance arbitraryArbResult ∷ Arbitrary ArbJson where
  arbitrary = ArbJson <$> genJson

genJson ∷ Gen Json
genJson = sized genJson'
  where
  genJson' size
    | size > 3 = genJson' 3
    | size > 0 = resize (size - 1) (oneOf genArray [genObject])
    | otherwise =
        oneOf
          (pure jsonNull)
          [ fromBoolean <$> arbitrary
          , fromNumber <$> arbitrary
          , fromString <$> arbitrary
          ]
  genArray = fromArray <$> arrayOf (defer \_ -> genJson)
  genObject = A.foldM extendObj jsonEmptyObject =<< arrayOf arbitrary
    where
    extendObj ∷ Json → String → Gen Json
    extendObj obj k = do
      val ← defer \_ → genJson
      pure $ k := val ~> obj

newtype ArbJCursor = ArbJCursor JCursor

runArbJCursor ∷ ArbJCursor → JCursor
runArbJCursor (ArbJCursor j) = j

instance arbJCursor ∷ Arbitrary ArbJCursor where
  arbitrary = ArbJCursor <$> genJCursor

genJCursor ∷ Gen JCursor
genJCursor = sized genJCursor'
  where
  genJCursor' size
    | size > 5 = genJCursor' 5
    | size > 0 = resize (size - 1) (oneOf genField [genIndex])
    | otherwise = pure JCursorTop
  genField = JField <$> arbitrary <*> defer \_ → genJCursor
  genIndex = JIndex <$> arbitrary <*> defer \_ → genJCursor
