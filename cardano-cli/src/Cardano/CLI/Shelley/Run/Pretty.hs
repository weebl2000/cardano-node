{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | User-friendly pretty-printing for textual user interfaces (TUI)
module Cardano.CLI.Shelley.Run.Pretty (friendlyTxBodyBS) where

import           Cardano.Prelude hiding (undefined)
import           Prelude (error)

import           Cardano.Api as Api (AddressInEra (..),
                   AddressTypeInEra (ByronAddressInAnyEra, ShelleyAddressInEra), CardanoEra (..),
                   ShelleyBasedEra (ShelleyBasedEraAllegra, ShelleyBasedEraMary, ShelleyBasedEraShelley),
                   ShelleyEra, TxBody, serialiseToBech32)
import           Cardano.Api.Byron (TxBody (ByronTxBody))
import           Cardano.Api.Shelley (TxBody (ShelleyTxBody), fromShelleyAddr)
import           Cardano.CLI.Helpers (textShow)
import           Cardano.Ledger.Shelley as Ledger (ShelleyEra)
import           Cardano.Ledger.ShelleyMA (MaryOrAllegra (Allegra, Mary), ShelleyMAEra)
import qualified Cardano.Ledger.ShelleyMA.TxBody as ShelleyMA
import           Data.Aeson as JSON (Object, Value (..), object, toJSON, (.=))
import qualified Data.HashMap.Strict as HashMap
import           Data.Yaml.Pretty (defConfig, encodePretty, setConfCompare)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)
import           Shelley.Spec.Ledger.API (Addr (..), TxOut (TxOut))
import qualified Shelley.Spec.Ledger.API as Shelley
import qualified Shelley.Spec.Ledger.TxBody as Ledger

friendlyTxBodyBS :: CardanoEra era -> Api.TxBody era -> ByteString
friendlyTxBodyBS era txBody =
  encodePretty (setConfCompare compare defConfig) $ friendlyTxBody' era txBody

friendlyTxBody' :: CardanoEra era -> TxBody era -> Value
friendlyTxBody' era (ByronTxBody tx) = mergeEra era $ toJSON tx
friendlyTxBody' era (ShelleyTxBody sbe txbody _mAuxData) = do
  let hMap = case sbe of
            ShelleyBasedEraShelley ->
              Object $ HashMap.fromList
                [ "inputs" .= Ledger._inputs txbody
                , "outputs" .= fmap friendlyTxOutShelley (Ledger._outputs txbody)
                , "certificates" .= fmap textShow (Ledger._certs txbody)
                , "withdrawals" .= Shelley.unWdrl (Ledger._wdrls txbody)
                , "fee" .= Ledger._txfee txbody
                , "time to live" .= Ledger._ttl txbody
                , "update" .= fmap textShow Ledger._txUpdate txbody
                , "metadata hash" .= fmap textShow Ledger._mdHash txbody
                ]
            ShelleyBasedEraAllegra -> panic "TODO"
            ShelleyBasedEraMary -> panic "TODO"
  mergeEra era hMap



friendlyValidityInterval :: ShelleyMA.ValidityInterval -> Value
friendlyValidityInterval
  ShelleyMA.ValidityInterval{invalidBefore, invalidHereafter} =
    object
      [ "invalid before" .= invalidBefore
      , "invalid hereafter" .= invalidHereafter
      ]

friendlyTxOutShelley :: TxOut (Ledger.ShelleyEra StandardCrypto) -> Value
friendlyTxOutShelley (TxOut addr amount) =
  Object $ HashMap.insert "amount" (toJSON amount) $ friendlyAddress addr

friendlyAddress :: Addr StandardCrypto -> Object
friendlyAddress addr =
  HashMap.fromList $
    case addr of
      Addr net cred ref ->
        [ ( "address"
          , object
              [ "network" .= net
              , "credential" .= cred
              , "stake reference" .= textShow ref
              , "Bech32" .= addressBech32
              ]
          )
        ]
      AddrBootstrap _ ->
        [("bootstrap address", object ["Bech32" .= String addressBech32])]
  where
    addressBech32 =
      case fromShelleyAddr @Api.ShelleyEra addr of
        AddressInEra (ShelleyAddressInEra _) a -> serialiseToBech32 a
        AddressInEra ByronAddressInAnyEra _ -> error "expected Shelley address"

friendlyTxOutAllegra :: TxOut (ShelleyMAEra 'Allegra StandardCrypto) -> Value
friendlyTxOutAllegra = toJSON

friendlyTxOutMary :: TxOut (ShelleyMAEra 'Mary StandardCrypto) -> Value
friendlyTxOutMary = toJSON

mergeEra :: CardanoEra era -> JSON.Value -> JSON.Value
mergeEra era (JSON.Object obj) =
  case toJSON era of
    JSON.Object e -> JSON.Object $ e <> obj
    _ -> JSON.Object obj
mergeEra _   _  = JSON.Null
