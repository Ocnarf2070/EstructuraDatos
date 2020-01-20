import DataStructures.Graph.Graph
import DataStructures.Graph.GraphBFT

data PosBarca = I | D deriving (Eq, Ord, Show)
data MC = Es Int Int PosBarca deriving (Eq, Ord, Show)

embarque = [(m,c) | m <- [0..2], c <- [0..2], m + c > 0, m + c <= 2]

valida nm nc = nm >= 0 && nm <= 3 && nc >= 0 && nc <= 3
               && (nm == 0 || nm >= nc)

sucMC (Es m c I) = [Es nm nc D |
                     (em,ec) <- embarque,
                     let nm = m - em,
                     let nc = c - ec,
                     valida nm nc,
                     valida (3 - nm) (3 - nc)
                     ]

sucMC (Es m c D) = [Es nm nc I |
                      (em,ec) <- embarque,
                      let nm = m + em,
                      let nc = c + ec,
                      valida nm nc,
                      valida (3 - nm) (3 - nc)
                    ]

grafo = mkGraphSuc undefined sucMC

solucion = filter (\cs -> last cs  == Es 0 0 D) (bftPaths grafo (Es 3 3 I))