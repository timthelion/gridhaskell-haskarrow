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
>import qualified Super
>import qualified Path

| A display cell is a cell which literally gets shown on the screen.  This includes not only the excecutable Cells but also the Comments and Paths which help the user understand how the grid's plumbing works.

>data DisplayCell = DisplayCellCode Cell.Cell
>                 | DisplayCellComment Grid.Comment
>                 | DisplayCellArgument Super.Point String
>                 | DisplayCellPattern Cell.Pattern
>                 | DisplayCellMVarLabel Super.Point String
>                 | DisplayCellPath Path.Path
>                 | DisplayCellBlank Super.Point
>   deriving (Show)

   
>displayCellPoint :: DisplayCell -> Super.Point
>displayCellPoint (DisplayCellCode      cell)    = Cell.point    cell
>displayCellPoint (DisplayCellComment   comment) = Grid.commentPoint comment
>displayCellPoint (DisplayCellBlank     point)   = point
>displayCellPoint (DisplayCellPath      path)    = Path.point path
>displayCellPoint (DisplayCellArgument  point _) = point
>displayCellPoint (DisplayCellMVarLabel point _) = point
>displayCellPoint (DisplayCellPattern   pattern) = Cell.patternPoint pattern

>displayCellText :: DisplayCell -> String
>displayCellText (DisplayCellCode      cell)    = Cell.cellText    cell
>displayCellText (DisplayCellComment   comment) = Grid.commentText comment
>displayCellText (DisplayCellBlank     _)       = ""
>displayCellText (DisplayCellPath      _)       = ""
>displayCellText (DisplayCellArgument  _ t)     = t
>displayCellText (DisplayCellMVarLabel _ t)     = t
>displayCellText (DisplayCellPattern   pattern) = Cell.pattern pattern


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
>       (concatMap displayCellsfromCells (Grid.gridLooseCells grid)) ++
>       (displayCellsfromComments (Grid.gridComments grid))
    
>compareCells :: DisplayCell -> DisplayCell -> Ordering
>compareCells c1 c2 = if (swap (displayCellPoint c1)) > (swap (displayCellPoint c2)) then GT else LT

>filledinDisplayCellList :: [DisplayCell] -> Super.Point -> Super.Point -> [DisplayCell]
>filledinDisplayCellList displayCells minimum maximum = 
>    filledinDisplayCellList' displayCells minimum minimum maximum

>filledinDisplayCellList' :: [DisplayCell] -> Super.Point -> Super.Point -> Super.Point -> [DisplayCell]
>filledinDisplayCellList' (displayCell:displayCells) mypoint minimum maximum =

If the next cell in the list is the same as the point we are filling

>    if (displayCellPoint displayCell) ==  mypoint

fill that point with the next cell in the list and append the rest of the display cells.

>    then displayCell : displayCellsTail displayCells

otherwise put a blank cell at mypoint and append the rest of the display cells.

>    else DisplayCellBlank mypoint :  displayCellsTail (displayCell:displayCells)
> where displayCellsTail leftOverDisplayCells=

Fill out the rest of the DisplayCell list.  

"Is the next point past the extreme point?  Are we done yet?"

We have to swap the points when compairing because Y is more significant to us than X.  We want the list to be in order left to right top to bottom.

>         if (swap (nextPoint mypoint minimum maximum))>(swap maximum)
>         then []
>         else (filledinDisplayCellList' leftOverDisplayCells  (nextPoint mypoint minimum maximum) minimum maximum)

In the case that the grid is really empty, don't display any cells.

>filledinDisplayCellList' [] (0,0) (0,0) (0,0) = []

Even after having exhausted our cells, we may need to add blank cells at the end, to fill out our buffer.  Take for example this situation:

x is filled by content.
o is blank.

xxx
xxx
xxo

The extreme point is (3,3) but there is no content to go there.

>filledinDisplayCellList' [] mypoint minimum maximum =
>    if mypoint == maximum
>    then [(DisplayCellBlank mypoint)]
>    else (DisplayCellBlank mypoint) : 
>    (filledinDisplayCellList' []  (nextPoint mypoint minimum maximum) minimum maximum)

| The 'nextPoint' as taken from left to right, top to bottom.

>nextPoint :: Super.Point -> Super.Point -> Super.Point -> Super.Point
>nextPoint (mypoint_x,mypoint_y) (minimum_x,minimum_y) (maximum_x,maximum_y) =
>    if mypoint_x == maximum_x
>    then (minimum_x,mypoint_y+1)
>    else (mypoint_x+1, mypoint_y)

| 'displayCellsfromCells' not only takes all the cells and turns them into DisplayCells, but also takes all their internal paths and does the same.

>displayCellsfromCells :: Cell.Cell -> [DisplayCell]
>displayCellsfromCells cell = (DisplayCellCode cell) :
>   (case cell of

DisplayCellArguments

>       Cell.Start{}  -> map (\(argumentPoint,argumentName) -> DisplayCellArgument argumentPoint argumentName) (Cell.arguments cell)

DisplayCellPatterns

>       Cell.Which{} -> map (\pattern-> DisplayCellPattern pattern) (Cell.patterns cell)

>       Cell.Lambda{} -> DisplayCellArgument (Cell.arrow cell) "->" : map (\(argumentPoint,argumentName) -> DisplayCellArgument argumentPoint argumentName) (Cell.arguments cell)

DisplayCellMVarLabels

>       Cell.NewEmptyMVar{} -> [displayCellMVarLabel cell]
>       Cell.PutMVar{}      -> [displayCellMVarLabel cell]
>       Cell.TakeMVar{}     -> [displayCellMVarLabel cell]

DisplayCellPaths

>       Cell.Action{}       -> (displayCellsfromPath (Cell.path cell)) ++
>                              case Cell.label cell of
>                                Just (point, label) -> [DisplayCellMVarLabel point label]
>                                Nothing             -> []
>       Cell.Jump{}         -> (displayCellsfromPath (Cell.path cell))
>       Cell.Destination{}  -> (displayCellsfromPath (Cell.path cell))
>       _                   -> []
>       ) ++

cellNext gives us a list, because in the case of Fork or Which we will end up with more than one of them of them.  This map gives us a type [[DisplayCell]] so we need to concat it.

>          (concatMap displayCellsfromCells (Cell.cellNext cell))

>displayCellMVarLabel :: Cell.Cell -> DisplayCell
>displayCellMVarLabel cell =
>   DisplayCellMVarLabel (Cell.labelPoint cell) (Cell.mvar cell)

>displayCellsfromPath :: (Maybe Path.Path) -> [DisplayCell]
>displayCellsfromPath (Just p)   = displayCellsfromPath' p
>displayCellsfromPath Nothing    = []


>displayCellsfromPath' :: Path.Path -> [DisplayCell]
>displayCellsfromPath' p@Path.SteppingStone{} = 
>             DisplayCellPath p : 
>             (displayCellsfromPath' (Path.next p))
>displayCellsfromPath' p@Path.PathDestination{} =
>             []
    
>displayCellsfromComments :: [Grid.Comment] -> [DisplayCell]
>displayCellsfromComments comments = 
>     map (\comment -> (DisplayCellComment comment)) comments
