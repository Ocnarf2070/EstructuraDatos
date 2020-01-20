module Complex where

import Criterion (bench, nf)
import Criterion.Main (defaultMain)
import Data.List (subsequences)
import Control.DeepSeq (deepseq)

main = defaultMain (map createBenchmark [0, 2 .. 24])
    where
        createBenchmark n =
            let
                xs = replicate n 'x'
            in
                xs `deepseq` (bench (show n) $ nf subsequences xs)