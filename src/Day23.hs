module Day23 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap)

solution = Solution "day25" "" run

run _ = (NoSolution, NoSolution)

{-
#############
#...........#
###C#A#B#D###
  #C#A#D#B#
  #########

10

#############
#AA.........#
###C# #B#D###
  #C# #D#B#
  #########

50

#############
#AA.........#
###C# # #D###
  #C#B#D#B#
  #########
  
2000

#############
#AA...... D.#
###C# # # ###
  #C#B#D#B#
  #########
  
70, 6000, 2000
  
#############
#AA......  .#
###C#B# #D###
  #C#B# #D#
  #########
  
700, 700, 3, 3


11536
-}