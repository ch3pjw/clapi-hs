{-# LANGUAGE
    TemplateHaskell
#-}

module Clapi.TH (segq, ap, rp, btq) where

import Control.Monad ((>=>))
import Data.Char (ord)
import Data.Text (pack)
import Language.Haskell.TH.Lift (lift)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Clapi.Types.Base (mkTag)
import Clapi.Types.Path (absPathFromText, relPathFromText, mkSeg)

segq :: QuasiQuoter
segq = QuasiQuoter {
    quoteExp = mkSeg . pack >=> lift,
    quotePat = fail "Not supported",
    quoteDec = fail "Not supported",
    quoteType = fail "Not supported"}

-- | Path 'Abs quasiquoter
ap :: QuasiQuoter
ap = QuasiQuoter {
    quoteExp = absPathFromText . pack >=> lift,
    quotePat = fail "Not supported",
    quoteDec = fail "Not supported",
    quoteType = fail "Not supported"}

-- | Path 'Rel quasiquoter
rp :: QuasiQuoter
rp = QuasiQuoter {
    quoteExp = relPathFromText . pack >=> lift,
    quotePat = fail "Not supported",
    quoteDec = fail "Not supported",
    quoteType = fail "Not supported"}

btq :: QuasiQuoter
btq = QuasiQuoter {
    quoteExp = fromStr >=> lift,
    quotePat = fail "Not supported",
    quoteDec = fail "Not supported",
    quoteType = fail "Not supported"}
  where
    fromStr [c] = mkTag $ fromIntegral $ ord c
    fromStr _ = fail "Not one char"
