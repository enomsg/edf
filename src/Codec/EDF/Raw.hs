-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.EDF.Raw
-- Copyright   :  (c) Anton Vorontsov <anton@enomsg.org> 2019
-- License     :  BSD 2-Clause (see the LICENSE file)
--
-- Maintainer  :  Anton Vorontsov <anton@enomsg.org>
-- Stability   :  experimental
-- Portability :  portable

{-# language ScopedTypeVariables #-}
{-# language RecordWildCards #-}

module Codec.EDF.Raw where
import Data.ByteString.Lazy as BL
import Data.ByteString.Internal as BI
import Data.Binary.Get
import Data.Text as T
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Monoid
import Control.Monad
import Control.Exception

data Header = Header {
      version           :: Text
    , patient           :: Text
    , numBytes          :: Integer
    , numRecords        :: Integer
    , durationSec       :: Integer
    , numSignals        :: Integer
    , labels            :: [Text]
    , transducerType    :: [Text]
    , dimensions        :: [Text]
    , physicalMin       :: [Integer]
    , physicalMax       :: [Integer]
    , digitalMin        :: [Integer]
    , digitalMax        :: [Integer]
    , prefiltering      :: [Text]
    , numSamples        :: [Integer]
    } deriving (Show)

getHeader :: Get (Header)
getHeader = do
    version         <- utf8              8
    patient         <- utf8             80
    recordingId     <- getByteString    80
    startDate       <- getByteString     8
    startTime       <- getByteString     8
    numBytes        <- int               8
    reserved1       <- getByteString    44
    numRecords      <- int               8
    durationSec     <- int               8
    numSignals      <- int               4

    labels          <- utf8s    numSignals  16
    transducerType  <- utf8s    numSignals  80
    dimensions      <- utf8s    numSignals   8
    physicalMin     <- ints     numSignals   8
    physicalMax     <- ints     numSignals   8
    digitalMin      <- ints     numSignals   8
    digitalMax      <- ints     numSignals   8
    prefiltering    <- utf8s    numSignals  80
    numSamples      <- ints     numSignals   8
    reserved2       <- skips    numSignals  32

    return $ Header {..}
  where
    utf8  width     = decodeUtf8 <$> getByteString width
    utf8s n width   = replicateM (fromInteger n) $ decodeUtf8 <$> getByteString width
    bs2i            :: BI.ByteString -> Integer
    bs2i            = read . T.unpack . decodeUtf8
    int width       = bs2i <$> getByteString width
    ints n width    = replicateM (fromInteger n) $ bs2i <$> getByteString width
    skip width      = getByteString width
    skips n width   = replicateM (fromInteger n) $ getByteString width

type Samples = [Integer]
type Signals = [Samples]
type Records = [Signals]

getSamples :: (Integral nSamples) => nSamples -> Get Samples
getSamples n = replicateM (fromIntegral n) (getInt16le >>= return . fromIntegral)

getRecords :: Get Records
getRecords = do
    h <- getHeader
    s <- replicateM (fromIntegral $ numRecords h) $ numSamples h `forM` \n -> getSamples n
    return s

getRecordsFromFile :: FilePath -> IO Records
getRecordsFromFile fn = do
    f <- BL.readFile fn
    return $ runGet getRecords f

getHeaderFromFile :: FilePath -> IO Header
getHeaderFromFile f = do
    f <- BL.readFile f
    return $ runGet getHeader f
