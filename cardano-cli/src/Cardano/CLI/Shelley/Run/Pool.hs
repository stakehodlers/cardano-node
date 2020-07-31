{-# LANGUAGE NamedFieldPuns #-}

module Cardano.CLI.Shelley.Run.Pool
  ( ShelleyPoolCmdError
  , renderShelleyPoolCmdError
  , runPoolCmd
  ) where

import           Cardano.Prelude

import qualified Data.Text as Text

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither,
                     newExceptT)

import qualified Data.ByteString.Char8 as BS

import           Cardano.Api.TextView (TextViewDescription (..))
import           Cardano.Api.Typed

import qualified Shelley.Spec.Ledger.Slot as Shelley

import           Cardano.CLI.Shelley.Commands
import           Cardano.CLI.Types (VerificationKeyFile (..))


data ShelleyPoolCmdError
  = ReadFileError !(FileError TextEnvelopeError)
  | WriteFileError !(FileError ())
  | MetaDataValidationError !StakePoolMetadataValidationError
  deriving Show

renderShelleyPoolCmdError :: ShelleyPoolCmdError -> Text
renderShelleyPoolCmdError err =
  case err of
    ReadFileError fileErr -> Text.pack (displayError fileErr)
    WriteFileError fileErr -> Text.pack (displayError fileErr)
    MetaDataValidationError validationErr ->
      "Error validating stake pool metadata: " <> Text.pack (displayError validationErr)



runPoolCmd :: PoolCmd -> ExceptT ShelleyPoolCmdError IO ()
runPoolCmd (PoolRegistrationCert sPvkey vrfVkey pldg pCost pMrgn rwdVerFp ownerVerFps relays mbMetadata network outfp) =
  runStakePoolRegistrationCert sPvkey vrfVkey pldg pCost pMrgn rwdVerFp ownerVerFps relays mbMetadata network outfp
runPoolCmd (PoolRetirementCert sPvkeyFp retireEpoch outfp) =
  runStakePoolRetirementCert sPvkeyFp retireEpoch outfp
runPoolCmd (PoolGetId sPvkey) = runPoolId sPvkey
runPoolCmd (PoolMetaDataHash poolMdFile mOutFile) = runPoolMetaDataHash poolMdFile mOutFile


--
-- Stake pool command implementations
--

-- | Create a stake pool registration cert.
-- TODO: Metadata and more stake pool relay support to be
-- added in the future.
runStakePoolRegistrationCert
  :: VerificationKeyFile
  -- ^ Stake pool verification key.
  -> VerificationKeyFile
  -- ^ VRF Verification key.
  -> Lovelace
  -- ^ Pool pledge.
  -> Lovelace
  -- ^ Pool cost.
  -> Rational
  -- ^ Pool margin.
  -> VerificationKeyFile
  -- ^ Stake verification key for reward account.
  -> [VerificationKeyFile]
  -- ^ Pool owner stake verification key(s).
  -> [StakePoolRelay]
  -- ^ Stake pool relays.
  -> (Maybe StakePoolMetadataReference)
  -- ^ Stake pool metadata.
  -> NetworkId
  -> OutputFile
  -> ExceptT ShelleyPoolCmdError IO ()
runStakePoolRegistrationCert
  (VerificationKeyFile sPvkeyFp)
  (VerificationKeyFile vrfVkeyFp)
  pldg
  pCost
  pMrgn
  (VerificationKeyFile rwdVerFp)
  ownerVerFps
  relays
  mbMetadata
  network
  (OutputFile outfp) = do
    -- Pool verification key
    stakePoolVerKey <- firstExceptT ReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakePoolKey) sPvkeyFp
    let stakePoolId = verificationKeyHash stakePoolVerKey

    -- VRF verification key
    vrfVerKey <- firstExceptT ReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsVrfKey) vrfVkeyFp
    let vrfKeyHash = verificationKeyHash vrfVerKey

    -- Pool reward account
    stakeVerKey <- firstExceptT ReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakeKey) rwdVerFp
    let stakeCred = StakeCredentialByKey (verificationKeyHash stakeVerKey)
        rewardAccountAddr = makeStakeAddress network stakeCred

    -- Pool owner(s)
    sPoolOwnerVkeys <-
      mapM
        (\(VerificationKeyFile fp) ->
          firstExceptT ReadFileError
            . newExceptT
            $ readFileTextEnvelope (AsVerificationKey AsStakeKey) fp
        )
        ownerVerFps
    let stakePoolOwners = map verificationKeyHash sPoolOwnerVkeys

    let stakePoolParams =
          StakePoolParameters
            { stakePoolId = stakePoolId
            , stakePoolVRF = vrfKeyHash
            , stakePoolCost = pCost
            , stakePoolMargin = pMrgn
            , stakePoolRewardAccount = rewardAccountAddr
            , stakePoolPledge = pldg
            , stakePoolOwners = stakePoolOwners
            , stakePoolRelays = relays
            , stakePoolMetadata = mbMetadata
            }

    let registrationCert = makeStakePoolRegistrationCertificate stakePoolParams

    firstExceptT WriteFileError
      . newExceptT
      $ writeFileTextEnvelope outfp (Just registrationCertDesc) registrationCert
  where
    registrationCertDesc :: TextViewDescription
    registrationCertDesc = TextViewDescription "Stake Pool Registration Certificate"

runStakePoolRetirementCert
  :: VerificationKeyFile
  -> Shelley.EpochNo
  -> OutputFile
  -> ExceptT ShelleyPoolCmdError IO ()
runStakePoolRetirementCert (VerificationKeyFile sPvkeyFp) retireEpoch (OutputFile outfp) = do
    -- Pool verification key
    stakePoolVerKey <- firstExceptT ReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakePoolKey) sPvkeyFp

    let stakePoolId = verificationKeyHash stakePoolVerKey
        retireCert = makeStakePoolRetirementCertificate stakePoolId retireEpoch

    firstExceptT WriteFileError
      . newExceptT
      $ writeFileTextEnvelope outfp (Just retireCertDesc) retireCert
  where
    retireCertDesc :: TextViewDescription
    retireCertDesc = TextViewDescription "Stake Pool Retirement Certificate"

runPoolId :: VerificationKeyFile -> ExceptT ShelleyPoolCmdError IO ()
runPoolId (VerificationKeyFile vkeyPath) = do
    stakePoolVerKey <- firstExceptT ReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakePoolKey) vkeyPath
    liftIO $ BS.putStrLn $ serialiseToRawBytesHex (verificationKeyHash stakePoolVerKey)

runPoolMetaDataHash :: PoolMetaDataFile -> Maybe OutputFile -> ExceptT ShelleyPoolCmdError IO ()
runPoolMetaDataHash (PoolMetaDataFile poolMDPath) mOutFile = do
  metaDataBytes <- handleIOExceptT (ReadFileError . FileIOError poolMDPath) $
    BS.readFile poolMDPath
  (_metaData, metaDataHash) <-
      firstExceptT MetaDataValidationError
    . hoistEither
    $ validateAndHashStakePoolMetadata metaDataBytes
  case mOutFile of
    Nothing -> liftIO $ BS.putStrLn (serialiseToRawBytesHex metaDataHash)
    Just (OutputFile fpath) ->
      handleIOExceptT (WriteFileError . FileIOError fpath)
        $ BS.writeFile fpath (serialiseToRawBytesHex metaDataHash)
