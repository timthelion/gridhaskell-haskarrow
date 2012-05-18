GPLV3.0 or later copyright brmlab.cz contact timothyhobbs@seznam.cz

Copyright 2012.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

>module CellMethods where

>import Cell
>import qualified Path
>import Super

>import Data.NumInstances
>import Data.Maybe

>emptyEnd :: Super.Point -> Cell
>emptyEnd point = (End (CellCommon (point,smallRectangle) []))

| This is the text that gets displayed on the screen.  It is not the haskell code to which these cells are precompiled.

>cellText :: Cell -> String
>cellText cell@Start{}        = code cell
>cellText cell@Action{}       = code cell
>cellText Lambda{}            = "λ"
>cellText Which{}             = "Which"
>cellText cell@Destination{}  = value cell
>cellText Jump{}              = "Jump"
>cellText Fork{}              = "Fork"
>cellText cell@NewEmptyMVar{} = "newEmptyMVar"
>cellText cell@PutMVar{}      = "putMVar"
>cellText cell@TakeMVar{}     = "takeMVar"
>cellText Return{}            = "Return"
>cellText End{}               = "End"
>cellText Exit{}              = "Exit"

We use this to build a list of cells for display on the screen.

>cellsNext :: Cell -> [Cell]
>cellsNext cell@Jump{}     = []
>cellsNext cell@Return{}   = []
>cellsNext cell@End{}      = []
>cellsNext cell@Exit{}     = []
>cellsNext cell@Fork{}     = newThreads cell
>cellsNext cell@Which{}    = map action (patterns cell)
>cellsNext cell@Lambda{}   = next cell : body cell : []
>cellsNext cell            = [next cell]

>cellComments :: Cell -> [Label]
>cellComments cell = comments (common cell)

>cellPoint :: Cell -> Super.Point
>cellPoint cell = fst (rectangle (common cell))

>cellPutCode :: Cell -> String -> Maybe Cell
>cellPutCode _ "Fork"        = Nothing
>cellPutCode _ "Jump"        = Nothing
>cellPutCode _ "End"         = Nothing
>cellPutCode _ "Which"       = Nothing
>cellPutCode _ "Destination" = Nothing

>cellPutCode cell@Start{}  code' = Just cell{code=code'}
>cellPutCode cell@Action{} code' = Just cell{code=code'}

>cellPutCode _ _ = Nothing

>decrimentPull :: Cell -> Cell
>decrimentPull cell =
> cell{pull = (pull cell) - 1}

>incrimentPull :: Cell -> Cell
>incrimentPull cell =
> cell{pull = (pull cell) + 1}

>cellPutPush :: Cell -> Bool -> Cell
>cellPutPush cell p =
> cell{push = p}


|Put the first cell into the seccond cell(tree), at the point of the first cell.  We return a tuple with our new tree of cells, plus the cells that used to come after the cell we just replaced.

>cellPutCell :: Cell -> Cell -> (Cell,Maybe Cell)

>cellPutCell whatToPut whereToPut@Lambda{}
> | cellPoint whatToPut == cellPoint whereToPut =
>   (whatToPut,Just whereToPut)
> | otherwise =
>   (cellPutCell',stray)

> where
>  cellPutCell' = whereToPut{next=fst treeTail,
>                            body=fst bodyPutCell}
>  stray =
>   case snd treeTail of
>    Just tail -> Just tail
>    Nothing -> snd bodyPutCell

>  treeTail = cellPutCell whatToPut (next whereToPut)

>  bodyPutCell = cellPutCell whatToPut
>                          $ body whereToPut

>cellPutCell whatToPut whereToPut@Which{} 
> | cellPoint whatToPut == cellPoint whereToPut =
>   (whatToPut,Just whereToPut)
> | otherwise =
>   (cellPutCell',stray)

> where
>  cellPutCell' =
>   whereToPut {patterns = insertNewActions treeTails 
>                                         $ patterns whereToPut}

>  stray =
>   case strays of
>    [] -> Nothing
>    x:xs -> Just x

>  strays = catMaybes $ map snd treeTails

>  treeTails = 
>   map (\pattern -> cellPutCell whatToPut (action pattern)) 
>       (patterns whereToPut)

>  insertNewActions = zipWith insertAction
 
>  insertAction treeTail pattern =
>   pattern{action = fst treeTail}


>cellPutCell whatToPut whereToPut@Fork{} 
> | cellPoint whatToPut == cellPoint whereToPut =
>   (whatToPut,Just whereToPut)
> | otherwise = 
>   (cellPutCell',stray)

>  where 
>   cellPutCell' =
>    whereToPut{newThreads =
>                map fst treeTail}

>   stray =
>    case (catMaybes $ map snd treeTail) of
>     [] -> Nothing
>     x:xs -> Just x

>   treeTail = map (cellPutCell whatToPut)
>                  (newThreads whereToPut)

>cellPutCell whatToPut whereToPut@Jump{} 
> | cellPoint whatToPut == cellPoint whereToPut =
>   (whatToPut,Just whereToPut)
> | otherwise =
>   (whereToPut,Nothing)

>cellPutCell whatToPut whereToPut@End{} 
> | cellPoint whatToPut == cellPoint whereToPut =
>   (whatToPut,Just whereToPut)
> | otherwise =
>   (whereToPut,Nothing)

>cellPutCell whatToPut whereToPut@Exit{} 
> | cellPoint whatToPut == cellPoint whereToPut =
>   (whatToPut,Just whereToPut)
> | otherwise = (whereToPut,Nothing)

>cellPutCell whatToPut whereToPut@Return{} 
> | cellPoint whatToPut == cellPoint whereToPut =
>   (whatToPut,Just whereToPut)
> | otherwise =
>   (whereToPut,Nothing)

>cellPutCell whatToPut whereToPut 
> | cellPoint whatToPut == cellPoint whereToPut = 
>   (whatToPut,Just whereToPut)
> | otherwise =
>   (cellPutCell', stray)
>   where
>    cellPutCell' = whereToPut{next=fst treeTail}
>    stray = snd treeTail 
>    treeTail = cellPutCell whatToPut (next whereToPut)

|Return two new Cell trees cut at the point.

>cellSplitAtPoint :: Cell -> Super.Point -> Maybe (Cell,Cell)
>cellSplitAtPoint cell point =
> let
>  (head, maybeTail) =
>   cellPutCell (End (CellCommon (point,smallRectangle) [])) cell 
> in
> case (head, maybeTail) of

>  (head ,Just tail) ->
>   Just (head, tail)

>  (head, Nothing) ->
>   Nothing


>deleteCellCell :: Super.Point -> Cell -> Maybe (Cell,[Cell])
>deleteCellCell point cell =
> case split of
>  Nothing   -> Nothing
>  otherwise -> Just (cell',strays)
> where
>  cell' = fst $ cellPutCell myTail head

>  myTail = cellPointsRelocation tail relocations

>  relocations = zip (cellPoints tail) 
>              $ map (difference +)
>                  $ cellPoints tail

>  difference = point - cellPoint tail

>  (tail,strays) = extractTail
>                $ cellsNext longTail
 
>  extractTail (tail:strays) = (tail,strays)
>  extractTail [] = (emptyEnd point,[])

>  Just (head,longTail) = split
>  split = cellSplitAtPoint cell point

| Returns a new cell with the list of point1s reloacted to their corresponding point2s in the (point1,point2) tuples.

>cellPointsRelocation :: Cell -> [(Super.Point,Super.Point)] -> Cell
>cellPointsRelocation originalCells@Start{} relocations  =
>  (cellPointsRelocation' originalCells relocations){
>  arguments = argumentsPointsRelocation (arguments originalCells) relocations,
>  next = cellPointsRelocation (next originalCells) relocations
>  }

>cellPointsRelocation originalCells@Action{} relocations  =
>  (cellPointsRelocation' originalCells relocations){
>  label = case (label originalCells) of
>            Just label -> Just $ labelRelocation label relocations
>            Nothing    -> Nothing,
>  path = case (path originalCells) of
>            Just path -> Just $ pathPointsRelocation path relocations
>            Nothing   -> Nothing,
>  next = cellPointsRelocation (next originalCells) relocations
>  }

>cellPointsRelocation originalCells@Lambda{} relocations  =
>  (cellPointsRelocation' originalCells relocations){
>  arguments = argumentsPointsRelocation (arguments originalCells) relocations,
>  arrow = rectangleRelocation (arrow originalCells) relocations,
>  body  = cellPointsRelocation (body originalCells) relocations,
>  next  = cellPointsRelocation (next originalCells) relocations
>  }


>cellPointsRelocation originalCells@Destination{} relocations  =
>  (cellPointsRelocation' originalCells relocations){
>  origin = relocatePoint (origin originalCells) relocations,
>  path = case (path originalCells) of
>            Just path -> Just $ pathPointsRelocation path relocations
>            Nothing   -> Nothing,
>  next = cellPointsRelocation (next originalCells) relocations
>  }

>cellPointsRelocation originalCells@Which{} relocations  =
>  (cellPointsRelocation' originalCells relocations){
>    patterns = map (\pattern -> patternPointsRelocation pattern relocations) (patterns originalCells)}

>cellPointsRelocation originalCells@Jump{} relocations  =
>  (cellPointsRelocation' originalCells relocations)

>cellPointsRelocation originalCells@Fork{} relocations  =
>  (cellPointsRelocation' originalCells relocations){
>  newThreads = map (\thread -> cellPointsRelocation thread relocations) (newThreads originalCells)
>  }

>cellPointsRelocation originalCells@NewEmptyMVar{} relocations  =
> mvarPointsRelocation originalCells relocations
>cellPointsRelocation originalCells@PutMVar{} relocations  =
> mvarPointsRelocation originalCells relocations
>cellPointsRelocation originalCells@TakeMVar{} relocations  =
> mvarPointsRelocation originalCells relocations

>cellPointsRelocation originalCells relocations =
> cellPointsRelocation' originalCells relocations

>cellPointsRelocation' :: Cell -> [(Super.Point,Super.Point)] -> Cell
>cellPointsRelocation' originalCells relocations  =
> originalCells{
>   common = commonRelocation (common originalCells) relocations}

>argumentsPointsRelocation :: [Label] ->  [(Super.Point,Super.Point)] -> [Label]
>argumentsPointsRelocation arguments relocations =
> map
>   (\label -> labelRelocation label relocations)
>   arguments

>pathPointsRelocation :: Path.Path -> [(Super.Point,Super.Point)] -> Path.Path
>pathPointsRelocation path@Path.SteppingStone{} relocations =
>  path{Path.point = relocatePoint (Path.point path) relocations,
>       Path.next  = pathPointsRelocation (Path.next path) relocations}
>pathPointsRelocation path@Path.PathDestination{} relocations =
>  path{Path.point = relocatePoint (Path.point path) relocations}

>patternPointsRelocation :: Pattern -> [(Super.Point,Super.Point)] -> Pattern
>patternPointsRelocation pattern relocations =
>  pattern{patternLabel = labelRelocation (patternLabel pattern) relocations,
>  action = cellPointsRelocation (action pattern) relocations}

>mvarPointsRelocation :: Cell -> [(Super.Point,Super.Point)] -> Cell
>mvarPointsRelocation originalCells relocations =
>  (cellPointsRelocation' originalCells relocations){
>   mvarLabel = labelRelocation (mvarLabel originalCells) relocations,
>   next = cellPointsRelocation (next originalCells) relocations
>   }

>labelRelocation :: Label -> [(Super.Point,Super.Point)] -> Label
>labelRelocation (rectangle,string) relocations = (rectangleRelocation rectangle relocations,string)

>commonRelocation :: CellCommon -> [(Super.Point,Super.Point)] -> CellCommon
>commonRelocation common relocations = common{rectangle = rectangleRelocation (rectangle common) relocations}

>rectangleRelocation :: Super.Rectangle ->  [(Super.Point,Super.Point)] -> Super.Rectangle
>rectangleRelocation (point,size) relocations = (relocatePoint point relocations,size)

>relocatePoint :: Super.Point -> [(Super.Point,Super.Point)] -> Super.Point
>relocatePoint point relocations =
>  case
>     catMaybes $
>      map
>        (\tup -> if (fst tup) == point
>                 then Just (snd tup)
>                 else Nothing)
>        relocations
>     of
>  (p:ps) -> p
>  []     -> point

>cellPoints :: Cell -> [Super.Point]
>cellPoints cell = cellPoint cell : (case cell of
>  Start{} ->
>   map (\label -> labelPoint label) (arguments cell)
>  Action{} -> (case (label cell) of
>               Just label -> [labelPoint label]
>               Nothing         -> []) ++
>              (case (path cell) of
>               Just path -> Path.pathPoints path
>               Nothing   -> [])
>  Lambda{} -> (map (\label -> labelPoint label) (arguments cell)) ++
>              [rectanglePoint $ arrow cell]
>  Destination{} -> case (path cell) of
>                    Just path -> Path.pathPoints path
>                    Nothing -> []
>  Which{} -> map (\pattern -> labelPoint (patternLabel pattern)) (patterns cell)
>  Jump{}  -> case (path cell) of
>              Just path -> Path.pathPoints path
>              Nothing -> []
>  NewEmptyMVar{} -> [labelPoint (mvarLabel cell)]
>  TakeMVar{}     -> [labelPoint  (mvarLabel cell)]
>  PutMVar{}      -> [labelPoint (mvarLabel cell)]
>  otherwise      -> []) ++
>  concatMap (cellPoints) (cellsNext cell)

>cellPointFilled :: Cell -> Super.Point -> Bool
>cellPointFilled cell pointToCheck =
> if pointToCheck == cellPoint cell
> then True
> else (case cell of
>  Start{} ->
>   any (\label -> labelPointFilled label pointToCheck) (arguments cell)
>  Action{} -> (case (label cell) of
>               Just label -> labelPointFilled label pointToCheck
>               Nothing         -> False) ||
>              (case (path cell) of
>               Just path -> Path.pathPointFilled path pointToCheck
>               Nothing   -> False)
>  Lambda{} -> (any (\label -> labelPointFilled label pointToCheck) (arguments cell)) ||
>              (rectanglePointFilled (arrow cell) pointToCheck)
>  Destination{} -> case (path cell) of
>                    Just path -> Path.pathPointFilled path pointToCheck
>                    Nothing -> False
>  Which{} -> or $ map (\pattern -> labelPointFilled (patternLabel pattern) pointToCheck) (patterns cell)
>  Jump{}  -> case (path cell) of
>              Just path -> Path.pathPointFilled path pointToCheck
>              Nothing -> False
>  NewEmptyMVar{} -> labelPointFilled (mvarLabel cell) pointToCheck
>  TakeMVar{}     -> labelPointFilled (mvarLabel cell) pointToCheck
>  PutMVar{}      -> labelPointFilled (mvarLabel cell) pointToCheck
>  otherwise      -> False) ||
>  (or $ map (\cell->cellPointFilled cell pointToCheck) (cellsNext cell))

| Is the point covered by the label?

>labelPointFilled :: Label -> Super.Point -> Bool
>labelPointFilled (rectangle,_) point = rectanglePointFilled rectangle point

| Is the point inside the Rectangle?

>rectanglePointFilled :: Super.Rectangle -> Super.Point -> Bool
>rectanglePointFilled ((rx,ry),(rw,rh)) (px,py) = (px >= rx && px < rx + rw) && (py >= ry && py < ry + rh)

>labelPoint :: Label -> Super.Point
>labelPoint (rectangle,_) = Super.rectanglePoint rectangle

>labelText :: Label -> String
>labelText (_,string) = string

>commonPoint :: CellCommon -> Super.Point
>commonPoint common = Super.rectanglePoint $ rectangle common

>patternPoint :: Pattern -> Super.Point
>patternPoint pattern = labelPoint (patternLabel pattern)
