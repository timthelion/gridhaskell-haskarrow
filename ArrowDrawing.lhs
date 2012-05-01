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

>module ArrowDrawing where
>import Graphics.Rendering.Cairo
>import Graphics.UI.Gtk
>import Data.List

>import qualified Super
>import qualified DisplayCell
>import qualified Cell
>import qualified Path


Gather a list of arrows that look like [(Rectangle, Rectangle)].  Then draw the lines.

| 'drawArrows' or lines between cells. 

>drawArrows :: [(Rectangle,DisplayCell.DisplayCell)] -> Render ()
>drawArrows cells = do

We go through the list both forwards and backwards, completing arrows.

>      (arrows',unfinished_arrows) <- return (arrowsOf cells [])
>      arrows <- return (arrows' ++ 
>                        (fst (arrowsOf (reverse cells) unfinished_arrows)))
>      mapM drawArrow arrows
>      return ()

| The 'arrowsOf' or between a list of cells.

 The first argument is the list of the display cells, and their geometric locations on the screen.  The seccond argument is the list of unfinished arrows.  When we come across a Fork, or Which, or Code element we add a Rectangle and Point.  The Point is relative to the coordinate system of the Grid and not of the pixels on the screen :) .  We keep the tuple in the list of unfinished arrows untill we reach the Point of the other endo of the arrow.  Then we finish the arrow and add it to our output.

>arrowsOf :: [(Rectangle,DisplayCell.DisplayCell)] -> [(Rectangle,Super.Point)] -> ([(Rectangle,Rectangle)],[(Rectangle,Super.Point)])

Path Arrows

>arrowsOf ((r, (DisplayCell.DisplayCellPath path)):cells) unfinished_arrows = 
> arrowsOf' r (Path.point path) unfinished_arrows [(Path.point (Path.next path))] cells


The arrows going down to the branches of a switch.

>arrowsOf ((r, (DisplayCell.DisplayCellCode cell@Cell.Which{})):cells) unfinished_arrows = 
> arrowsOf' r (Cell.cellPoint cell) unfinished_arrows (map Cell.patternPoint (Cell.patterns cell)) cells

>arrowsOf ((r,(DisplayCell.DisplayCellPattern pattern)):cells) unfinished_arrows =
> arrowsOf' r (Cell.patternPoint pattern) unfinished_arrows [Cell.cellPoint (Cell.action pattern)] cells

The arrows comming from any other type of cell.
    
>arrowsOf ((r, (DisplayCell.DisplayCellCode cell)):cells) unfinished_arrows =
>  arrowsOf' r (Cell.cellPoint cell) unfinished_arrows ((map Cell.cellPoint (Cell.cellsNext cell))++path_points) cells
>   where path_points = 
>          if
>           case cell of

Types of cells which have paths:

>             Cell.Action{}      -> True
>             Cell.Destination{} -> True
>             Cell.Jump{}        -> True
>             _                  -> False
>          then maybe [] (\p -> [Path.point p]) (Cell.path cell)
>          else []

>arrowsOf (_:cells) unfinished_arrows =
>   arrowsOf cells unfinished_arrows

>arrowsOf [] unfinished_arrows = ([],unfinished_arrows)

>arrowsOf' :: Rectangle -> Super.Point -> [(Rectangle,Super.Point)] -> [Super.Point] -> [(Rectangle,DisplayCell.DisplayCell)] -> ([(Rectangle,Rectangle)],[(Rectangle,Super.Point)])
>arrowsOf' r p unfinished_arrows new_arrows cells =
>   (finished_arrows ++ finished_arrows',
>   unfinished_arrows')

>   where
>    (finished_arrows', unfinished_arrows') =
>      arrowsOf cells still_unfinished_arrows
 
>    (finished_arrows, still_unfinished_arrows) = 
>      finished_and_still_unfinished_arrows r p unfinished_arrows new_arrows


| 'finished_and_still_unfinished_arrows' given the Rectangle of the current Cell, the Point of the current cell, the list of previously unfinished arrows and the list of points which come next(this defines the newly unfinished arrows.)

Returns ([finished_arrows],[still_unfinished_arrows])

>finished_and_still_unfinished_arrows :: Rectangle -> Super.Point -> [(Rectangle,Super.Point)] -> [Super.Point] -> ([(Rectangle, Rectangle)] , [(Rectangle,Super.Point)])
>finished_and_still_unfinished_arrows r p unfinished_arrows new_arrow_destinations =
>   (zip (map fst matches) (repeat r),

Still unfinished arrows.

>   zip (repeat r) new_arrow_destinations ++
>   still_unmatched)
>   where (matches, still_unmatched) = partition (\unfinished_arrow -> snd unfinished_arrow == p) unfinished_arrows 


>drawArrow :: (Rectangle,Rectangle) -> Render ()
>drawArrow ((Rectangle x y w h), (Rectangle x1 y1 w1 h1)) = do
>    setSourceRGB 0 0 0
>    setLineWidth 2

>    moveTo (fromIntegral (x+(div w 2))) (fromIntegral (y+(div h 2)))
>    lineTo (fromIntegral (x1+(div w1 2))) (fromIntegral (y1+(div h1 2)))

>    stroke
