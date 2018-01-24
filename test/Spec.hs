module Main where

import Control.Monad (void)
import RealmSpec
import MSetSpec

main :: IO ()
main = void RealmSpec.tests >> void MSetSpec.tests

