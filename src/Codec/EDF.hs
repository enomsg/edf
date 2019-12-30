-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.EDF
-- Copyright   :  (c) Anton Vorontsov <anton@enomsg.org> 2019
-- License     :  BSD 2-Clause (see the LICENSE file)
--
-- Maintainer  :  Anton Vorontsov <anton@enomsg.org>
-- Stability   :  experimental
-- Portability :  portable

module Codec.EDF where

import qualified Codec.EDF.Raw as Raw

import Data.Text (Text)
import Data.Monoid

type Label = Text
type Samples = [Integer]
type Signals = [(Label, Samples)]

getSignalsFromFile :: FilePath -> IO [(Label, Samples)]
getSignalsFromFile f = do
    h <- Raw.getHeaderFromFile f
    records <- Raw.getRecordsFromFile f
    let signals = foldr1 (zipWith (<>)) records
    return $ zip (Raw.labels h) signals
