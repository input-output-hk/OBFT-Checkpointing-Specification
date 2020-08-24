module LedgerUnitTests where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Domain
import Ledger


ledgerUnitTests = testGroup "Ledger Transitions unit tests"
  [ testGroup "NewBlock Transitions" 
    [ testCase "block interval not equal to k" blockIntervalNotEqualK
    , testCase "block interval equal to k" blockIntervalEqualK
    ]

  , testGroup "NewVote Transistions" 
    [ testCase "invalid signature" invalidSignature
    , testCase "no winner" noWinner
    , testCase "new checkpoint" newCheckpoint
    ]
  ]


blockIntervalNotEqualK :: Assertion
blockIntervalNotEqualK =
  let 
    pparams = PParams 3 3 (SKey "node00") []
    lastChkp = Checkpoint (PoWBlockRef 1 (Hash "cafebabe")) []
    initialState = LedgerState lastChkp []
    newBlock = PoWBlockRef 2 (Hash "deadbeef")
  in
    newBlockTransition pparams initialState newBlock @?= initialState

blockIntervalEqualK :: Assertion
blockIntervalEqualK =
  let 
    pparams = PParams 3 3 (SKey "node00") []
    lastChkp = Checkpoint (PoWBlockRef 1 (Hash "cafebabe")) []
    initialState = LedgerState lastChkp []
    newBlock = PoWBlockRef 4 (Hash "feedbabe")
    expectedState = LedgerState lastChkp [sign (_sKey pparams) newBlock] 
  in
    newBlockTransition pparams initialState newBlock @?= expectedState

keys = ["node00", "node01", "node02", "node03", "node04"]
fedPKeys :: [PKey]
fedPKeys = map PKey keys
fedSKeys :: [SKey]
fedSKeys = map SKey keys

invalidSignature :: Assertion
invalidSignature = 
  let 
    pparams = PParams 3 3 (fedSKeys !! 0) fedPKeys
    lastChkp = Checkpoint (PoWBlockRef 0 (Hash "cafebabe")) []
    initialState = LedgerState lastChkp []
    newVote = sign (SKey "bad") (PoWBlockRef 3 (Hash "deadbeef"))
  in
    newVoteTransition pparams initialState newVote @?= initialState

noWinner :: Assertion
noWinner = 
  let 
    pparams = PParams 3 3 (fedSKeys !! 0) fedPKeys
    lastChkp = Checkpoint (PoWBlockRef 0 (Hash "cafebabe")) []
    initialState = LedgerState lastChkp [
        sign (fedSKeys !! 0) (PoWBlockRef 3 (Hash "feedbabe"))
      , sign (fedSKeys !! 1) (PoWBlockRef 3 (Hash "feedbabe"))
      , sign (fedSKeys !! 2) (PoWBlockRef 3 (Hash "deadbeef"))
      ]
    newVote = sign (fedSKeys !! 3) (PoWBlockRef 3 (Hash "deadbeef"))
    expectedState = LedgerState lastChkp (newVote : currentVotes initialState)
  in
    newVoteTransition pparams initialState newVote @?= expectedState


newCheckpoint :: Assertion
newCheckpoint = 
  let 
    pparams = PParams 3 3 (fedSKeys !! 0) fedPKeys
    lastChkp = Checkpoint (PoWBlockRef 0 (Hash "cafebabe")) []
    initialState = LedgerState lastChkp [
        sign (fedSKeys !! 0) (PoWBlockRef 3 (Hash "feedbabe"))
      , sign (fedSKeys !! 1) (PoWBlockRef 3 (Hash "feedbabe"))
      , sign (fedSKeys !! 2) (PoWBlockRef 3 (Hash "deadbeef"))
      , sign (fedSKeys !! 3) (PoWBlockRef 3 (Hash "deadbeef"))
      ]
    newVote @ (Vote _ newSig) = sign (fedSKeys !! 4) (PoWBlockRef 3 (Hash "deadbeef"))
    msig = newSig : (map (\(Vote _ sig) -> sig) (drop 2 (currentVotes initialState)))
    newChkp = Checkpoint (PoWBlockRef 3 (Hash "deadbeef")) msig
    expectedState = LedgerState newChkp []
  in
    newVoteTransition pparams initialState newVote @?= expectedState