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

>import Data.NumInstances()

>import Data.Maybe

>emptyEnd :: Super.Point -> Cell
>emptyEnd point = (End (CellCommon (point,small) []))

| This is the text that gets displayed on the screen.  It is not the haskell code to which these cells are precompiled.

>cellText :: Cell -> String
>cellText cell@Start{}        = code cell
>cellText cell@Action{}       = code cell
>cellText Lambda{}            = "λ"
>cellText Which{}             = "Which"
>cellText cell@Citation{}     = snd $ value cell
>cellText Jump{}              = "Jump"
>cellText Fork{}              = "Fork"
>cellText NewEmptyMVar{}      = "newEmptyMVar"
>cellText PutMVar{}           = "putMVar"
>cellText TakeMVar{}          = "takeMVar"
>cellText Return{}            = "Return"
>cellText End{}               = "End"
>cellText Exit{}              = "Exit"

We use this to build a list of cells for display on the screen.

>cellsNext :: Cell -> [Cell]
>cellsNext Jump{}          = []
>cellsNext Return{}        = []
>cellsNext End{}           = []
>cellsNext Exit{}          = []
>cellsNext cell@Fork{}     = newThreads cell
>cellsNext cell@Which{}    = map action (patterns cell)
>cellsNext cell@Lambda{}   = next cell : body cell : []
>cellsNext cell            = [next cell]

>cellComments :: Cell -> [Label]
>cellComments cell = comments (common cell)

>cellPoint :: Cell -> Super.Point
>cellPoint cell = fst (rectangle (common cell))

>cellPutCode :: Cell -> String -> (Point -> Point) -> Either Cell (Cell,[Cell])
>cellPutCode cell "Fork"  _     =
> Left (Fork (common cell) (cellsNext cell))
>cellPutCode cell "Jump"  _     =
> Right ((Jump (common cell) Nothing), cellsNext cell)
>cellPutCode cell "End"   _     =
> Right ((End (common cell)), cellsNext cell)
>cellPutCode cell "Which" _  =
> Right ((Which (common cell) []), cellsNext cell)
>cellPutCode cell "Cite"  near  =
> Right ((Citation (common cell) emptyLabel (head $ cellsNext cell)), tail $ cellsNext cell)
> where
>  emptyLabel =
>   (((near $ (cellPoint cell)+(1,0)),Super.small),"")

>cellPutCode cell@Start{}  code' _ = Left cell{code=code'}
>cellPutCode cell@Action{} code' _ = Left cell{code=code'}

>cellPutCode _ _ _ = error "Trying to put code into non code cell."

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
>   (cellPutCell',strays)

> where
>  cellPutCell' = whereToPut{next=fst continuation,
>                            body=fst bodyContinuation}
>  strays =
>   case snd continuation of
>    Just strays' -> Just strays'
>    Nothing -> snd bodyContinuation

>  continuation = cellPutCell whatToPut (next whereToPut)

>  bodyContinuation = cellPutCell whatToPut
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
>    x:_ -> Just x

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
>     x:_ -> Just x

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
>  (left, maybeRight) =
>   cellPutCell (End (CellCommon (point,small) [])) cell 
> in
> case maybeRight of

>  Just right ->
>   Just (left, right)

>  Nothing ->
>   Nothing


>deleteCellCell :: Super.Point -> Cell -> Maybe (Cell,[Cell])
>deleteCellCell point cell =
> case splitCell of
>  Nothing   -> Nothing
>  _         -> Just (cell',strays)
> where
>  cell' = fst $ cellPutCell myTail whereToPut

>  myTail = cellPointsRelocation myTail' relocations

>  relocations = zip (cellPoints myTail) 
>              $ map (difference +)
>                  $ cellPoints myTail

>  difference = point - cellPoint myTail

>  (myTail',strays) = extractTail
>                $ cellsNext longTail
 
>  extractTail (tailHead:strays') = (tailHead,strays')
>  extractTail [] = (emptyEnd point,[])

>  Just (whereToPut,longTail) = splitCell
>  splitCell = cellSplitAtPoint cell point

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
>            Just myLabel -> Just $ labelRelocation myLabel relocations
>            Nothing    -> Nothing,
>  next = cellPointsRelocation (next originalCells) relocations
>  }

>cellPointsRelocation originalCells@Lambda{} relocations  =
>  (cellPointsRelocation' originalCells relocations){
>  arguments = argumentsPointsRelocation (arguments originalCells) relocations,
>  arrow = rectangleRelocation (arrow originalCells) relocations,
>  body  = cellPointsRelocation (body originalCells) relocations,
>  next  = cellPointsRelocation (next originalCells) relocations
>  }


>cellPointsRelocation originalCells@Citation{} relocations  =
>  (cellPointsRelocation' originalCells relocations){
>  value = labelRelocation (value originalCells) relocations,
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
>argumentsPointsRelocation argumentsToRelocate relocations =
> map
>   (\thisLabel -> labelRelocation thisLabel relocations)
>   argumentsToRelocate

>pathPointsRelocation :: Path.Path -> [(Super.Point,Super.Point)] -> Path.Path
>pathPointsRelocation pathToRelocate@Path.SteppingStone{} relocations =
>  pathToRelocate{Path.point = relocatePoint (Path.point pathToRelocate) relocations,
>       Path.next  = pathPointsRelocation (Path.next pathToRelocate) relocations}
>pathPointsRelocation pathToRelocate@Path.PathDestination{} relocations =
>  pathToRelocate{Path.point = relocatePoint (Path.point pathToRelocate) relocations}

>patternPointsRelocation :: Pattern -> [(Super.Point,Super.Point)] -> Pattern
>patternPointsRelocation patternToRelocate relocations =
>  patternToRelocate{patternLabel = labelRelocation (patternLabel patternToRelocate) relocations,
>  action = cellPointsRelocation (action patternToRelocate) relocations}

>mvarPointsRelocation :: Cell -> [(Super.Point,Super.Point)] -> Cell
>mvarPointsRelocation originalCells relocations =
>  (cellPointsRelocation' originalCells relocations){
>   mvarLabel = labelRelocation (mvarLabel originalCells) relocations,
>   next = cellPointsRelocation (next originalCells) relocations
>   }

>labelRelocation :: Label -> [(Super.Point,Super.Point)] -> Label
>labelRelocation (rectangleToRelocate,string) relocations = (rectangleRelocation rectangleToRelocate relocations,string)

>commonRelocation :: CellCommon -> [(Super.Point,Super.Point)] -> CellCommon
>commonRelocation commonToRelocate relocations = commonToRelocate{rectangle = rectangleRelocation (rectangle commonToRelocate) relocations}

>rectangleRelocation :: Super.Rectangle ->  [(Super.Point,Super.Point)] -> Super.Rectangle
>rectangleRelocation (pointToRelocate,size) relocations = (relocatePoint pointToRelocate relocations,size)

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
>  (p:_) -> p
>  []     -> point

>cellPoints :: Cell -> [Super.Point]
>cellPoints cell = cellPoint cell : (case cell of
>  Start{} ->
>   map (\thisLabel -> labelPoint thisLabel) (arguments cell)
>  Action{} -> (case (label cell) of
>               Just myLabel -> [labelPoint myLabel]
>               Nothing         -> [])
>  Lambda{} -> (map (\thisLabel -> labelPoint thisLabel) (arguments cell)) ++
>              [rectanglePoint $ arrow cell]
>  Citation{} -> [labelPoint $ value cell]
>  Which{} -> map (\pattern -> labelPoint (patternLabel pattern)) (patterns cell)
>  Jump{}  -> case (path cell) of
>              Just myPath -> Path.pathPoints myPath
>              Nothing -> []
>  NewEmptyMVar{} -> [labelPoint (mvarLabel cell)]
>  TakeMVar{}     -> [labelPoint  (mvarLabel cell)]
>  PutMVar{}      -> [labelPoint (mvarLabel cell)]
>  _              -> []) ++
>  concatMap (cellPoints) (cellsNext cell)

>cellPointFilled :: Cell -> Super.Point -> Bool
>cellPointFilled cell pointToCheck = elem pointToCheck (cellPoints cell)

>labelPoint :: Label -> Super.Point
>labelPoint (myRectangle,_) = Super.rectanglePoint myRectangle

>labelText :: Label -> String
>labelText (_,string) = string

>commonPoint :: CellCommon -> Super.Point
>commonPoint thisCommon = Super.rectanglePoint $ rectangle thisCommon

>patternPoint :: Pattern -> Super.Point
>patternPoint pattern = labelPoint (patternLabel pattern)
