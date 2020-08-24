module Federation where

import Domain
import Node
import Ledger
import Text.Printf
import qualified Data.Map as M
import Data.List (sortOn, groupBy)
import Data.Maybe (fromMaybe)

runCheckpointing :: Int -> Int -> Int -> Int -> M.Map Int [(PoWBlockRef, [Int])] -> [Checkpoint]
runCheckpointing howLong blockInterval requiredMajority fedSize powBlocks =
  let
    keys = map (printf "node-%02d") [0..(fedSize - 1)] 
    pKeys = map PKey keys
    sKeys = map SKey keys
    genesisCheckpoint = Checkpoint (PoWBlockRef 0 (Hash "genesis")) []
    initialNodes = map (\i -> Node i [] (LedgerState genesisCheckpoint []) [] [] (PParams blockInterval requiredMajority (sKeys !! i) pKeys)) [0..(fedSize - 1)]

    go :: Int -> [Node] -> Maybe Block -> [Vote] -> [Node]
    go slotNum nodes lastBlock votesToPool = 
      if slotNum >= howLong then
        nodes
      else
        go (slotNum + 1) nodes' lastBlock' votesToPool'
      where
        orElse :: Maybe a -> Maybe a -> Maybe a
        orElse (Just a) _ = Just a
        orElse _ b        = b

        goNode :: ([Node], Maybe Block, [Vote]) -> Node -> ([Node], Maybe Block, [Vote])
        goNode (ns, mb, newVotes) node = 
          let
            powBlocksForSlot = fromMaybe [] $ M.lookup slotNum powBlocks
            powBlocksForNode = map fst $ filter (\b -> nodeId node `elem` snd b) powBlocksForSlot
            (n, mb', vs) = onNewSlot slotNum powBlocksForNode votesToPool lastBlock node
          in
            (n : ns, mb `orElse` mb', vs ++ newVotes)

        (nodes', lastBlock', votesToPool') = foldl goNode ([], Nothing, []) nodes        

    properCheckpoints nodes = 
      let 
        allCheckpoints = foldl (\chkps n -> checkpoints n ++ chkps) [] nodes
        grouped = groupBy (\(Checkpoint b1 _) (Checkpoint b2 _) -> b1 == b2) $ sortOn (\(Checkpoint b _) -> b) allCheckpoints
        properOnes = map head $ filter (\chkps -> length chkps >= requiredMajority) grouped
      in
        sortOn (\(Checkpoint (PoWBlockRef num _) _) -> num) properOnes
  in
    properCheckpoints $ go 0 initialNodes Nothing []

