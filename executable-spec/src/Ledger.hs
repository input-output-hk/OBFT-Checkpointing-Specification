module Ledger where

import Domain

import qualified Data.Map as M
import Data.Ord (comparing)
import Data.List (maximumBy)
import Data.Tuple (swap)


data LedgerState = LedgerState
  { lastCheckpoint :: Checkpoint
  , currentVotes :: [Vote] } deriving (Eq, Show)

sign :: SKey -> PoWBlockRef -> Vote
sign (SKey key) blockRef = Vote blockRef (Sig key)

recoverPubKey :: Sig -> Maybe PKey
recoverPubKey (Sig key) = Just (PKey key)


newBlockTransition :: PParams -> LedgerState -> PoWBlockRef -> LedgerState
newBlockTransition (PParams k _ sKey _) ls blockRef 
  | number blockRef == lastCheckpointNumber + k = LedgerState (lastCheckpoint ls) $ sign sKey blockRef : currentVotes ls
  | otherwise = ls
  where
    lastCheckpointNumber = case ls of 
      LedgerState (Checkpoint b _) _ -> number b  

newVoteTransition :: PParams -> LedgerState -> Vote -> LedgerState
newVoteTransition (PParams _ m _ fedPKeys) ls vote@(Vote _ sig) =
  if not signatureValid then
    ls
  else 
    case winner of 
      Nothing -> LedgerState (lastCheckpoint ls) votes
      Just b  -> LedgerState (makeCheckpoint b) []
  where
    signatureValid = case recoverPubKey sig of
      Nothing   -> False
      Just pKey -> pKey `elem` fedPKeys
    votes = vote : currentVotes ls
    winner = findWinner votes m
    makeCheckpoint b = Checkpoint b $ map (\(Vote _ s) -> s) $ filter (\(Vote b' _) -> b == b') votes


findWinner :: [Vote] -> Int -> Maybe PoWBlockRef
findWinner [] _ = Nothing
findWinner votes m =
  if winnerCount < m then
    Nothing
  else
    Just winnerBlock
  where 
    f (Vote blockRef _) occurrences = case M.lookup blockRef occurrences of 
      Nothing -> M.insert blockRef 1 occurrences
      Just n  -> M.insert blockRef (n + 1) occurrences
    count vs = M.toList $ foldr f M.empty vs
    (winnerBlock, winnerCount) = maximumBy (comparing swap) $ count votes


