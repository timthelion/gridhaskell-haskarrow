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

>module DisplayCell where

>import Data.List
>import Data.Tuple

>import qualified Grid
>import qualified Cell
>import CellMethods
>import qualified Super
>import qualified Path

| A display cell is a cell which literally gets shown on the screen.  This includes not only the excecutable Cells but also the Comments and Paths which help the user understand how the grid's plumbing works.

>data DisplayCell = DisplayCellCode Cell.Cell
>                 | DisplayCellComment Cell.Label
>                 | DisplayCellArgument Cell.Label
>                 | DisplayCellStaticLabel Cell.Label
>                 | DisplayCellPattern Cell.Pattern
>                 | DisplayCellMVarLabel Cell.Cell
>                 | DisplayCellValueLabel Cell.Cell
>                 | DisplayCellPath Path.Path
>                 | DisplayCellBlank Super.Rectangle
>   deriving (Show)

   
>displayCellPoint :: DisplayCell -> Super.Point
>displayCellPoint (DisplayCellCode      cell)    = cellPoint    cell
>displayCellPoint (DisplayCellComment   comment) = labelPoint comment
>displayCellPoint (DisplayCellBlank     rectangle)   = Super.rectanglePoint rectangle
>displayCellPoint (DisplayCellPath      path)    = Path.point path
>displayCellPoint (DisplayCellArgument  argument) = labelPoint argument
>displayCellPoint (DisplayCellMVarLabel cell) = labelPoint $ Cell.mvarLabel cell
>displayCellPoint (DisplayCellValueLabel cell) = labelPoint $ Cell.value cell
>displayCellPoint (DisplayCellStaticLabel label) = labelPoint label
>displayCellPoint (DisplayCellPattern   pattern) =  patternPoint pattern

>displayCellText :: DisplayCell -> String
>displayCellText (DisplayCellCode      cell)    = cellText    cell
>displayCellText (DisplayCellComment   comment) = labelText comment
>displayCellText (DisplayCellBlank     _)       = ""
>displayCellText (DisplayCellPath      _)       = ""
>displayCellText (DisplayCellArgument  label)   = labelText label
>displayCellText (DisplayCellMVarLabel cell)   = labelText $ Cell.mvarLabel cell
>displayCellText (DisplayCellValueLabel cell)   = labelText $ Cell.value cell
>displayCellText (DisplayCellStaticLabel label)   = labelText label
>displayCellText (DisplayCellPattern   pattern) = snd $ Cell.patternLabel pattern


>displayCellBlank :: DisplayCell -> Bool
>displayCellBlank DisplayCellBlank{} = True
>displayCellBlank _                  = False

>displayCellPath :: DisplayCell -> Bool
>displayCellPath DisplayCellPath{} = True
>displayCellPath _                  = False


| A sorted list of all the non blank displayCells. We'll still mesh this out with blank cells to make a complete array.

>displayCellList :: Grid.Grid -> [DisplayCell]
>displayCellList grid = sortBy compareCells $
>       (displayCellsfromCells (Grid.gridCells grid)) ++ 
>       (concatMap displayCellsfromCells (Grid.gridLooseCells grid))
    
>compareCells :: DisplayCell -> DisplayCell -> Ordering
>compareCells c1 c2 = if (swap (displayCellPoint c1)) > (swap (displayCellPoint c2)) then GT else LT

>filledinDisplayCellList :: [DisplayCell] -> Super.Point -> Super.Point -> [DisplayCell]
>filledinDisplayCellList displayCells minPoint maxPoint = 
>    filledinDisplayCellList' displayCells minPoint minPoint maxPoint

>filledinDisplayCellList' :: [DisplayCell] -> Super.Point -> Super.Point -> Super.Point -> [DisplayCell]
>filledinDisplayCellList' (displayCell:displayCells) mypoint minPoint maxPoint =

If the next cell in the list is the same as the point we are filling

>    if (displayCellPoint displayCell) ==  mypoint

fill that point with the next cell in the list and append the rest of the display cells.

>    then displayCell : displayCellsTail displayCells

otherwise put a blank cell at mypoint and append the rest of the display cells.

>    else DisplayCellBlank (mypoint,Super.small) :  displayCellsTail (displayCell:displayCells)
> where displayCellsTail leftOverDisplayCells=

Fill out the rest of the DisplayCell list.  

"Is the next point past the extreme point?  Are we done yet?"

We have to swap the points when compairing because Y is more significant to us than X.  We want the list to be in order left to right top to bottom.

>         if (swap (nextPoint mypoint minPoint maxPoint))>(swap maxPoint)
>         then []
>         else (filledinDisplayCellList' leftOverDisplayCells  (nextPoint mypoint minPoint maxPoint) minPoint maxPoint)

In the case that the grid is really empty, don't display any cells.

>filledinDisplayCellList' [] (0,0) (0,0) (0,0) = []

Even after having exhausted our cells, we may need to add blank cells at the end, to fill out our buffer.  Take for example this situation:

x is filled by content.
o is blank.

xxx
xxx
xxo

The extreme point is (3,3) but there is no content to go there.

>filledinDisplayCellList' [] mypoint minPoint maxPoint =
>    if mypoint == maxPoint
>    then [(DisplayCellBlank (mypoint,Super.small))]
>    else (DisplayCellBlank (mypoint,Super.small)) : 
>    (filledinDisplayCellList' []  (nextPoint mypoint minPoint maxPoint) minPoint maxPoint)

| The 'nextPoint' as taken from left to right, top to bottom.

>nextPoint :: Super.Point -> Super.Point -> Super.Point -> Super.Point
>nextPoint (mypoint_x,mypoint_y) (minimum_x,_) (maximum_x,_) =
>    if mypoint_x == maximum_x
>    then (minimum_x,mypoint_y+1)
>    else (mypoint_x+1, mypoint_y)

| 'displayCellsfromCells' not only takes all the cells and turns them into DisplayCells, but also takes all their internal paths and does the same.

>displayCellsfromCells :: Cell.Cell -> [DisplayCell]
>displayCellsfromCells cell = (DisplayCellCode cell) : 
>   (map (\comment -> DisplayCellComment comment) (cellComments cell)) ++
>   (case cell of

DisplayCellArguments

>       Cell.Start{}  -> map (\argument -> DisplayCellArgument argument) (Cell.arguments cell)

DisplayCellPatterns

>       Cell.Which{} -> map (\pattern-> DisplayCellPattern pattern) (Cell.patterns cell)

>       Cell.Lambda{} -> DisplayCellStaticLabel ((Cell.arrow cell), "->") : map (\argument -> DisplayCellArgument argument) (Cell.arguments cell)

DisplayCellMVarLabels

>       Cell.NewEmptyMVar{} -> [displayCellMVarLabel cell]
>       Cell.PutMVar{}      -> [displayCellMVarLabel cell]
>       Cell.TakeMVar{}     -> [displayCellMVarLabel cell]

DisplayCellPaths

>       Cell.Action{}       -> case Cell.label cell of
>                                Just _ -> [DisplayCellMVarLabel cell]
>                                Nothing             -> []
>       Cell.Jump{}         -> (displayCellsfromPath (Cell.path cell))
>       Cell.Citation{}  -> [(DisplayCellValueLabel cell)]
>       _                   -> []
>       ) ++

cellNext gives us a list, because in the case of Fork or Which we will end up with more than one of them of them.  This map gives us a type [[DisplayCell]] so we need to concat it.

>          (concatMap displayCellsfromCells (cellsNext cell))

>displayCellMVarLabel :: Cell.Cell -> DisplayCell
>displayCellMVarLabel cell =
>   DisplayCellMVarLabel cell

>displayCellsfromPath :: (Maybe Path.Path) -> [DisplayCell]
>displayCellsfromPath (Just p)   = displayCellsfromPath' p
>displayCellsfromPath Nothing    = []


>displayCellsfromPath' :: Path.Path -> [DisplayCell]
>displayCellsfromPath' thisPath@Path.SteppingStone{} = 
>             DisplayCellPath thisPath : 
>             (displayCellsfromPath' (Path.next thisPath))
>displayCellsfromPath' Path.PathDestination{} =
>             []
