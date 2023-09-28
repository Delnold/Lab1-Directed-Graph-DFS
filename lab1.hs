-- Лаб.1 | Палієнко Дмитро | ТК-31 | Вар. 26
-- Умова: Орграф задано списком ребер. перевірити, чи є одна задана вершин досяжною з іншої.
{- Тестові дані: 
[(1,8), (3,4), (2,3), (4,7), (8,5), (5,3)] 1 4 | 1->8->5->3->4 | True
[(1,2), (2,3), (3,1), (3,5), (5,6), (6,7), (8,3)] 3 8 | 3->1->2 && 3->5->6->7 | False
[(1,2), (2,4), (4,3), (3,6)] 1 6 | 1->2->4->3->6 | True
[(1,2), (2,3), (3,4), (3,6), (6,7)] 4 7 | 4->Nothing False
-}
import Data.List
import System.IO

type Node = Int

type Edge = (Node, Node)

type Graph = [Edge]

neighborGraph :: Node -> Node -> Graph -> Graph
neighborGraph u v graph = [(x, y) | (x, y) <- graph, x /= u && y /= v]
 
isReachable :: Graph -> Node -> Node -> Bool
isReachable graph start target =
  (start == target) || any (\neighbor -> isReachable (neighborGraph start neighbor graph) neighbor target)
        [v | (u, v) <- graph, u == start]

main :: IO ()
main = do
  putStrLn "Input graph in the following format [(Int, Int)]"
  inputGraph <- getLine

  putStrLn "Input start vertex in the following format (Int)"
  inputStart <- getLine

  putStrLn "Input target vertex in the following format (Int)"
  inputTarget <- getLine
  
  let graph = read inputGraph :: Graph
  let start = read inputStart :: Node
  let target = read inputTarget :: Node

  if isReachable graph start target
      then putStrLn ("Vertex " ++ show target ++ " is reachable from the vertex " ++ show start)
      else putStrLn ("Vertex " ++ show target ++ " isn't reachable from the vertex " ++ show start)
