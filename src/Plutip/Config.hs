module Plutip.Config (
  PlutipConfig (..),
  WorkingDirectory (..),
  ExtraConfig (..),
  NominalDiffTimeMicro,
  EpochSize (..),
  stdTxSize,
) where

import Cardano.Ledger.Shelley.Genesis (NominalDiffTimeMicro)
import Cardano.Slotting.Slot (EpochSize (EpochSize, unEpochSize))
import Data.Default (Default, def)
import GHC.Generics (Generic)
import Plutip.Launch.Extra.Types (ExtraConfig (ExtraConfig, ecEpochSize, ecMaxTxSize, ecRaiseExUnitsToMax, ecSlotLength), stdTxSize)

-- | Configuration for the cluster working directory
-- This determines where the node database, chain-index database,
-- and bot-plutus-interface files will be stored for a running cluster
--
-- @since 0.2
data WorkingDirectory
  = -- | Stored in a temporary directory, deleted on cluster shutdown
    Temporary
  | -- | Stored in a set directory
    Fixed
      { path :: FilePath
      -- ^ Path to store cluster data, can be relative or absolute
      , shouldKeep :: Bool
      -- ^ Should the working data be kept on disk after cluster shutdown.
      --   Full directory will be deleted on shutdown if False
      }
  deriving stock (Generic, Show)

-- | Plutip configurable options
--
-- @since 0.2
data PlutipConfig = PlutipConfig
  { clusterDataDir :: Maybe FilePath
  -- ^ in case of `Nothing` cluster data from project `data-files` is used
  , clusterWorkingDir :: WorkingDirectory
  -- ^ cluster file location override, when provided, includes a `shouldKeep`
  , extraConfig :: ExtraConfig
  -- ^ Extra config to set (at the moment) slot length and epoch size
  --   for local network
  }
  deriving stock (Generic, Show)

instance Default PlutipConfig where
  def = PlutipConfig Nothing Temporary def
