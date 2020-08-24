module Node where

import Domain
import Ledger
import Data.List


data Block = Block [Vote] deriving (Show)

data Node = Node
  { nodeId :: Int
  , blockchain :: [Block]
  , ledgerState :: LedgerState
  , checkpoints :: [Checkpoint]
  , votePool :: [Vote]
  , pparams :: PParams} deriving (Show)

onNewSlot :: Int -> [PoWBlockRef] -> [Vote] -> Maybe Block -> Node -> (Node, Maybe Block, [Vote])
onNewSlot slotNum powBlockRefs votesToPool maybeBlock node =
  let 
    newVotes = case maybeBlock of 
      Just (Block votes) -> votes
      Nothing -> []
    blockchain' = case maybeBlock of 
      Just block -> block : blockchain node
      Nothing -> blockchain node
    votePool' = votePool node \\ newVotes
    ledgerState' = foldl (newVoteTransition (pparams node)) (ledgerState node) newVotes
    ledgerState'' = foldl (newBlockTransition (pparams node)) ledgerState' powBlockRefs
    myVotes = currentVotes ledgerState'' \\ currentVotes ledgerState'
    fedSize = length $ _fedPKeys $ pparams node
    isSlotLeader = slotNum `mod` fedSize == nodeId node
    newBlock = if isSlotLeader then Just (Block votePool') else Nothing
    votePool'' = votesToPool ++ if isSlotLeader then [] else votePool'
    newCheckpoint = if lastCheckpoint ledgerState'' == lastCheckpoint (ledgerState node) then [] else [lastCheckpoint ledgerState'']
    checkpoints' = newCheckpoint ++ checkpoints node
    node' = Node (nodeId node) blockchain' ledgerState'' checkpoints' votePool'' (pparams node)
  in 
    (node', newBlock, myVotes)



