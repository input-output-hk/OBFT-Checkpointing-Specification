module Domain where

newtype Hash = Hash String deriving (Eq, Show, Ord)

newtype Sig = Sig String deriving (Eq, Show, Ord)

newtype PKey = PKey String deriving (Eq, Show)

newtype SKey = SKey String deriving (Eq, Show)

type MSig = [Sig]

data PoWBlockRef = PoWBlockRef 
  { number :: Int
  , hash :: Hash }
  deriving (Eq, Show, Ord)

data Vote = Vote PoWBlockRef Sig deriving (Eq, Show)

data Checkpoint = Checkpoint PoWBlockRef MSig deriving (Eq, Show, Ord)

data PParams = PParams
  { _k :: Int
  , _m :: Int
  , _sKey :: SKey
  , _fedPKeys :: [PKey] } deriving (Show)