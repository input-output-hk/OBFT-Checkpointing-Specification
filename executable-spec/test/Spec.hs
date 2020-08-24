import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Ledger

import LedgerUnitTests
import FederationTests

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ledgerUnitTests, federationUnitTests]

