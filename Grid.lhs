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

>module Grid where
>import qualified Cell
>import Super

| Grid is our file type.  We read it and show it to load and unload the grid haskell files.

>data Grid = Grid{

| The 'message' is just to tell the user that this is not to be edited by hand after we save the whole thing to a file. 
 
>        message           :: String,
>        gridName          :: String,
>        gridLicence       :: String,
>        gridImports       :: [String],
>        gridCells         :: Cell.Cell,
>        gridLooseCells    :: [Cell.Cell]}
> deriving (Read, Show)
  

>emptyGrid :: Grid
>emptyGrid = Grid {
>       message = "This is a computer generated file.  Do not edit.",

>       gridName          = "",
>       gridLicence       = "Copyright <date> <maintainer>,<email>,<url>\
>\This program is free software: you can redistribute it and/or modify\
>\it under the terms of the GNU General Public License as published by\
>\the Free Software Foundation, either version 3 of the License, or\
>\(at your option) any later version.\
>\\
>\This program is distributed in the hope that it will be useful,\
>\but WITHOUT ANY WARRANTY; without even the implied warranty of\
>\MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\
>\GNU General Public License for more details.\
>\\
>\You should have received a copy of the GNU General Public License\
>\along with this program.  If not, see <http://www.gnu.org/licenses/>.\
>\-}",
>       gridImports       = [],
>       gridCells         = (Cell.Start (Cell.CellCommon ((0,0),(1,1)) []) "" "main" [] False $ Cell.End (Cell.CellCommon ((0,1),(1,1)) [])),
>       gridLooseCells    = []} 

Put the cell at the point specified into the grid.

>gridPutCell :: Cell.Cell -> Point -> Grid -> Maybe Grid
>gridPutCell cell point grid =
>  if pointFilledGrid grid point
>  then Nothing 
>  else Just grid{gridCells=fst (Cell.cellPutCell cell gridCells')}
>  where gridCells' = gridCells grid

>gridPutCellOverwrite :: Cell.Cell -> Point -> Grid -> Grid
>gridPutCellOverwrite cell point grid =
>  grid{gridCells=fst (Cell.cellPutCell cell gridCells')}
>   where gridCells' = gridCells grid

>gridPointsRelocation :: Grid -> [(Super.Point,Super.Point)] -> (Grid,Bool)
>gridPointsRelocation grid relocations =
> if not $ pointsFilledGrid grid $ map snd relocations
> then (grid{gridCells = Cell.cellPointsRelocation (gridCells grid) relocations,
>      gridLooseCells = map (\cell -> Cell.cellPointsRelocation cell relocations) (gridLooseCells grid)},True)
> else (grid,False)

>pointFilledGrid :: Grid -> Point -> Bool
>pointFilledGrid grid point =

Cells

>  (Cell.cellPointFilled (gridCells grid) point) ||

Or loose cells

>  (any (\cell->Cell.cellPointFilled cell point) (gridLooseCells grid))

>pointsFilledGrid :: Grid -> [Point] -> Bool
>pointsFilledGrid grid points = any (pointFilledGrid grid) points
