#! /usr/bin/env nix-shell
#! nix-shell --pure -i runghc plot.nix

-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.EDF.Example.Plot.Main
-- Copyright   :  (c) Anton Vorontsov <anton@enomsg.org> 2019
-- License     :  BSD 2-Clause (see the LICENSE file)
--
-- Maintainer  :  Anton Vorontsov <anton@enomsg.org>
-- Stability   :  experimental
-- Portability :  portable

module Codec.EDF.Example.Plot.Main where

import Data.Text (unpack)
import Control.Monad (unless, forM)
import System.FilePath (takeFileName)
import System.Posix (fileExist)
import System.Process (callProcess)
import Graphics.Gnuplot.Simple (plotList, Attribute(Title))
import Codec.EDF (getSignalsFromFile)

download url = do
    e <- fileExist $ takeFileName url
    unless e $ callProcess "curl" ["-O", url]
    return $ takeFileName url

main :: IO ()
main = do
    f <- download "http://physionet.mit.edu/physiobank/database/eegmmidb/S001/S001R01.edf"
    s <- getSignalsFromFile f
    take 5 s `forM` \(l,s) -> plotList [Title $ unpack l] (zip [0 :: Integer,1..] s)
    getChar >> return ()
