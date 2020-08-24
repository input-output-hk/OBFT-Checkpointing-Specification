module FederationTests where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Domain
import Federation

import qualified Data.Map as M

federationUnitTests = testGroup "Federation unit tests" 
  [ testCase "expected checkpoints #1" expectedCheckpoints1
  ]

expectedCheckpoints1 :: Assertion
expectedCheckpoints1 =
  let 
    federationSize = 10
    requiredMajority = 6
    blockInterval = 3
    testDurationInSlots = 25

    powBlocks = M.fromList
      [ (1,  [(powBlockRef 1 "a", [0..9])])
      , (3,  [(powBlockRef 2 "b", [0..9])])
      , (4,  [(powBlockRef 3 "c", [0..7])])
      , (6,  [(powBlockRef 4 "d", [3..9])])
      , (7,  [(powBlockRef 5 "e", [4..9])])
      , (10, [(powBlockRef 6 "f", [0..5]), (powBlockRef 6 "g", [6..9])])
      , (11, [(powBlockRef 7 "h", [0..9])])
      , (13, [(powBlockRef 8 "i", [4..9])])
      , (15, [(powBlockRef 9 "j", [0..4])])
      , (16, [(powBlockRef 9 "j", [5..7]), (powBlockRef 9 "k", [8..9])])
      , (17, [(powBlockRef 10 "l", [0..9])])
      , (19, [(powBlockRef 11 "m", [0..9]), (powBlockRef 12 "n", [0..9])])
      , (21, [(powBlockRef 13 "o", [0..9])])
      ]

    expectedCheckpoints = map (\(n, h) -> powBlockRef n h) $ [(3, "c"), (6, "f"), (9, "j"), (12, "n")]

    actualCheckpoints = map (\(Checkpoint b _) -> b) $ 
      runCheckpointing testDurationInSlots blockInterval requiredMajority federationSize powBlocks

  in 
    actualCheckpoints @?= expectedCheckpoints


powBlockRef :: Int -> String -> PoWBlockRef
powBlockRef n h = PoWBlockRef n (Hash h)

