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

>module GridAction where

>import Grid
>import DisplayCell
>import CellMethods


>gridSetDisplayCellText :: DisplayCell -> String -> Grid -> Grid

gridSetDisplayCellText (DisplayCellComment (point,_)) text grid =
 gridPutComment point text grid

>gridSetDisplayCellText (DisplayCellCode cell) text grid =
> let cell'Maybe = (cellPutCode cell text (\point -> gridPointNear grid point)) in
> case cell'Maybe of
>   Left cell' -> gridPutCell cell' grid
>   Right (cell',strays)    -> 
>    updatedCell{gridLooseCells=(gridLooseCells updatedCell)++strays}
>    where updatedCell = (gridPutCell cell' grid)

>gridSetDisplayCellText dc _ _ = error $ "Don't know how to set the text of a " ++ (show dc)
