{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Config.Partial
    ( PartialCardanoConfiguration (..)
    , PartialCore (..)
    , PartialNode (..)
    , PartialNTP (..)
    , PartialUpdate (..)
    , PartialLastKnownBlockVersion (..)
    , PartialDLG (..)
    -- * re-exports
    , NodeProtocol (..)
    , mkCardanoConfiguration
    ) where

import           Prelude (String)
import           Cardano.Prelude

import           Data.Monoid.Generic

import qualified Cardano.Chain.Update as Update
import qualified Ouroboros.Consensus.BlockchainTime as Consensus

import           Cardano.Config.Types
import           Cardano.Config.Topology
import           Cardano.Crypto (RequiresNetworkMagic)

-- | Partial @CardanoConfiguration@ configuration.
data PartialCardanoConfiguration = PartialCardanoConfiguration
    { pccLogPath             :: !(Last FilePath)
    , pccLogConfig           :: !(Last FilePath)
    , pccDBPath              :: !(Last FilePath)
    , pccSocketDir           :: !(Last FilePath)
    , pccApplicationLockFile :: !(Last FilePath)
    , pccTopologyInfo        :: !(Last TopologyInfo)
    , pccNodeAddress         :: !(Last NodeAddress)
    , pccProtocol            :: !(Last Protocol)
    , pccViewMode            :: !(Last ViewMode)
    , pccLogMetrics          :: !(Last Bool)
    , pccTraceOptions        :: !(Last TraceOptions)
    , pccCore                :: !PartialCore
    , pccNTP                 :: !PartialNTP
    , pccUpdate              :: !PartialUpdate
    , pccDLG                 :: !PartialDLG
    , pccNode                :: !PartialNode
    } deriving (Eq, Show, Generic)
    deriving Semigroup via GenericSemigroup PartialCardanoConfiguration
    deriving Monoid    via GenericMonoid PartialCardanoConfiguration

-- | Partial @Core@ configuration.
data PartialCore = PartialCore
    { pcoGenesisFile                :: !(Last FilePath)
    , pcoGenesisHash                :: !(Last Text)
    , pcoNodeId                     :: !(Last Int)
    -- ^ Core node ID, the number of the node.
    , pcoNumCoreNodes               :: !(Last Int)
    -- ^ The number of the core nodes.
    , pcoNodeProtocol               :: !(Last NodeProtocol)
    -- ^ The type of protocol run on the node.
    , pcoStaticKeySigningKeyFile    :: !(Last FilePath)
    , pcoStaticKeyDlgCertFile       :: !(Last FilePath)
    , pcoRequiresNetworkMagic       :: !(Last RequiresNetworkMagic)
    , pcoPBftSigThd                 :: !(Last Double)
    } deriving (Eq, Show, Generic)
    deriving Semigroup via GenericSemigroup PartialCore
    deriving Monoid    via GenericMonoid PartialCore

--- | Top-level Cardano SL node configuration
data PartialNode = PartialNode
    { pnoSlotLength                     :: !(Last Consensus.SlotLength)
    -- ^ Slot length time.
    , pnoNetworkConnectionTimeout       :: !(Last Int)
    -- ^ Network connection timeout in milliseconds.
    , pnoHandshakeTimeout               :: !(Last Int)
    -- ^ Protocol acknowledgement timeout in milliseconds.
    } deriving (Eq, Show, Generic)
    deriving Semigroup via GenericSemigroup PartialNode
    deriving Monoid    via GenericMonoid PartialNode

-- | Partial @NTP@ configuration.
data PartialNTP = PartialNTP
    { pntpResponseTimeout :: !(Last Int)
    -- ^ NTP response timeout.
    , pntpPollDelay       :: !(Last Int)
    -- ^ NTP poll delay.
    , pntpServers         :: !(Last [Text])
    -- ^ A list of NTP servers.
    } deriving (Eq, Show, Generic)
    deriving Semigroup via GenericSemigroup PartialNTP
    deriving Monoid    via GenericMonoid PartialNTP

-- | Partial @Update@ configuration.
data PartialUpdate = PartialUpdate
    { pupApplicationName       :: !(Last Update.ApplicationName)
    -- ^ Update application name.
    , pupApplicationVersion    :: !(Last Update.NumSoftwareVersion)
    -- ^ Update application version.
    , pupLastKnownBlockVersion :: !PartialLastKnownBlockVersion
    -- ^ Update last known block version.
    } deriving (Eq, Show, Generic)
    deriving Semigroup via GenericSemigroup PartialUpdate
    deriving Monoid    via GenericMonoid PartialUpdate

-- | Partial @LastKnownBlockVersion@ configuration.
data PartialLastKnownBlockVersion = PartialLastKnownBlockVersion
    { plkbvMajor :: !(Last Word16)
    -- ^ Last known block version major.
    , plkbvMinor :: !(Last Word16)
    -- ^ Last known block version minor.
    , plkbvAlt   :: !(Last Word8)
    -- ^ Last known block version alternative.
    } deriving (Eq, Show, Generic)
    deriving Semigroup via GenericSemigroup PartialLastKnownBlockVersion
    deriving Monoid    via GenericMonoid PartialLastKnownBlockVersion

-- | Partial @DLG@ configuration.
data PartialDLG = PartialDLG
    { pdlgCacheParam          :: !(Last Int)
      -- ^ This value parameterizes size of cache used in Delegation.
      -- Not bytes, but number of elements.
    , pdlgMessageCacheTimeout :: !(Last Int)
      -- ^ Interval we ignore cached messages for if it's sent again.
    } deriving (Eq, Show, Generic)
    deriving Semigroup via GenericSemigroup PartialDLG
    deriving Monoid    via GenericMonoid PartialDLG

-- | Return an error if the @Last@ option is incomplete.
mkComplete :: String -> Last a -> Either ConfigError a
mkComplete name (Last x) = maybe (Left $ PartialConfigValue name) Right x

-- This utilizes the mk* family of functions to make sure we have all
-- the required configuration values which then allows us to create a
-- 'CardanoConfiguration' value. This is called at the last stage of the
-- Partial Options Monoid approach.
-- https://medium.com/@jonathangfischoff/the-partial-options-monoid-pattern-31914a71fc67
mkCardanoConfiguration :: PartialCardanoConfiguration -> Either ConfigError CardanoConfiguration
mkCardanoConfiguration PartialCardanoConfiguration{..} = do

    ccLogPath                <- mkComplete "ccLogPath"    pccLogPath
    let ccLogConfig          = getLast pccLogConfig
    ccDBPath                 <- mkComplete "ccDBPath"     pccDBPath
    ccSocketDir              <- mkComplete "ccSocketPath" pccSocketDir
    ccApplicationLockFile    <- mkComplete "ccApplicationLockFile"
                                    pccApplicationLockFile

    ccTopologyInfo           <- mkComplete "ccTopologyInfo" pccTopologyInfo
    ccNodeAddress            <- mkComplete "ccNodeAddress" pccNodeAddress
    ccProtocol               <- mkComplete "ccProtocol" pccProtocol
    ccViewMode               <- mkComplete "ccViewMode" pccViewMode
    ccLogMetrics             <- mkComplete "ccLogMetrics" pccLogMetrics
    ccTraceOptions           <- mkComplete "ccTraceOptions" pccTraceOptions
    ccCore                   <- mkCore pccCore
    ccNTP                    <- mkNTP pccNTP
    ccUpdate                 <- mkUpdate pccUpdate
    ccDLG                    <- mkDLG pccDLG
    ccNode                   <- mkNode pccNode

    pure CardanoConfiguration{..}
  where
    -- | Finalize the @PartialCore@, convert to @Core@.
    mkCore :: PartialCore -> Either ConfigError Core
    mkCore PartialCore{..} = do

        coGenesisFile                   <- mkComplete "coGenesisFile"
                                            pcoGenesisFile

        coGenesisHash                   <- mkComplete "coGenesisHash"
                                            pcoGenesisHash

        let coNodeId                    = getLast pcoNodeId
        let coNumCoreNodes              = getLast pcoNumCoreNodes

        coNodeProtocol                  <- mkComplete "coNodeProtocol"
                                            pcoNodeProtocol

        let coStaticKeySigningKeyFile   = getLast pcoStaticKeySigningKeyFile
        let coStaticKeyDlgCertFile      = getLast pcoStaticKeyDlgCertFile

        coRequiresNetworkMagic          <- mkComplete "coRequiresNetworkMagic"
                                            pcoRequiresNetworkMagic

        let coPBftSigThd                = getLast pcoPBftSigThd

        pure Core{..}

    -- | Finalize the @PartialUpdate@, convert to @Update@.
    mkUpdate :: PartialUpdate -> Either ConfigError Update
    mkUpdate PartialUpdate{..} = do

        upApplicationName          <- mkComplete "upApplicationName"      pupApplicationName
        upApplicationVersion       <- mkComplete "upApplicationVersion"   pupApplicationVersion
        upLastKnownBlockVersion    <- mkLastKnownBlockVersion pupLastKnownBlockVersion

        pure Update{..}
      where
        mkLastKnownBlockVersion :: PartialLastKnownBlockVersion -> Either ConfigError LastKnownBlockVersion
        mkLastKnownBlockVersion PartialLastKnownBlockVersion{..} = do

            lkbvMajor  <- mkComplete "lkbvMajor"     plkbvMajor
            lkbvMinor  <- mkComplete "lkbvMinor"     plkbvMinor
            lkbvAlt    <- mkComplete "lkbvAlt"       plkbvAlt

            pure LastKnownBlockVersion{..}

    -- | Finalize the @PartialNTP@, convert to @NTP@.
    mkNTP :: PartialNTP -> Either ConfigError NTP
    mkNTP PartialNTP{..} = do

        ntpResponseTimeout  <- mkComplete "ntpResponseTimeout"    pntpResponseTimeout
        ntpPollDelay        <- mkComplete "ntpPollDelay"          pntpPollDelay
        ntpServers          <- mkComplete "ntpServers"            pntpServers

        pure NTP{..}

    -- | Finalize the @PartialNode@, convert to @Node@.
    mkNode :: PartialNode -> Either ConfigError Node
    mkNode PartialNode{..} = do

        noSlotLength                    <- mkComplete "noSlotLength"
                                            pnoSlotLength

        noNetworkConnectionTimeout      <- mkComplete "noNetworkConnectionTimeout"
                                            pnoNetworkConnectionTimeout

        noHandshakeTimeout              <- mkComplete "noHandshakeTimeout"
                                            pnoHandshakeTimeout

        pure Node{..}

    -- | Finalize the @PartialDLG@, convert to @DLG@.
    mkDLG :: PartialDLG -> Either ConfigError DLG
    mkDLG PartialDLG{..} = do

        dlgCacheParam           <- mkComplete "dlgCacheParam"             pdlgCacheParam
        dlgMessageCacheTimeout  <- mkComplete "dlgMessageCacheTimeout"    pdlgMessageCacheTimeout

        pure DLG{..}
