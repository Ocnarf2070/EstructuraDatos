module EightBoardGraph (
 nothing,
 eightPuzzleGraph)
 where
 
import DataStructures.Graph.Graph
import EightBoard

nothing :: Maybe a
nothing = Nothing

initial :: Board
initial = readBoard "2 4316758"


eightPuzzleGraph :: Graph Board
eightPuzzleGraph = mkGraphAdj undefined oneMove
