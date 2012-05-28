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

>import Data.List
>import Data.Ord

>import qualified Cell
>import CellMethods
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
>\",
>       gridImports       = ["Control.Concurrent"],
>       gridCells         = (Cell.Start (Cell.CellCommon ((0,0),(1,1)) []) "" "main" [] False $ Cell.End (Cell.CellCommon ((0,1),(1,1)) [])),
>       gridLooseCells    = []} 

Put the cell at the point specified into the grid.

>gridPutCell :: Cell.Cell -> Grid -> Grid
>gridPutCell cell grid =
>  grid{gridCells=fst (cellPutCell cell gridCells')}
>  where gridCells' = gridCells grid

>gridPointsRelocation :: Grid -> [(Super.Point,Super.Point)] -> (Grid,Bool)
>gridPointsRelocation grid relocations =
> if not $ pointsFilledGrid grid $ map snd relocations
> then (grid{gridCells = cellPointsRelocation (gridCells grid) relocations,
>      gridLooseCells = map (\cell -> cellPointsRelocation cell relocations) (gridLooseCells grid)},True)
> else (grid,False)

>pointFilledGrid :: Grid -> Point -> Bool
>pointFilledGrid grid point =
> elem point $ gridPoints grid

>gridPoints :: Grid -> [Point]
>gridPoints grid =
> cellPoints (gridCells grid) ++
> concatMap cellPoints (gridLooseCells grid)

| Returns the neares point to the given point which is not occupied by another cell in the grid.

>gridPointNear :: Grid -> Point -> Point
>gridPointNear grid point 
> | not $ elem point $ gridPoints grid = point
> | otherwise = gridPointNear grid (point + (1,0))

>pointsFilledGrid :: Grid -> [Point] -> Bool
>pointsFilledGrid grid points = any (pointFilledGrid grid) points

>isTopLevelStray :: Grid -> Cell.Cell -> Bool
>isTopLevelStray grid cell =
> let cell'sPoint = CellMethods.cellPoint cell in
>  any (\stray -> cell'sPoint == CellMethods.cellPoint stray) $ gridLooseCells grid

>gridConnectCells :: Grid -> Cell.Cell -> Cell.Cell -> Maybe Grid
>gridConnectCells grid cellToConnect whereToConnect =
> case gridWithRelocatedCellsMaybe of
>  Just gridWithRelocatedCells ->
>   Just $ gridWithRelocatedCells
>   {gridLooseCells=
>     snd $ partitionedLooseCells $ gridLooseCells gridWithRelocatedCells,
>    gridCells= CellMethods.cellPutCellDiscardingStrays (head $ fst $ partitionedLooseCells $ gridLooseCells gridWithRelocatedCells) $ gridCells gridWithRelocatedCells
>   }
>  Nothing -> Nothing
> where
>  partitionedLooseCells looseCells =
>   partition
>    (\cell -> pointWhereToConnect == CellMethods.cellPoint cell)
>    looseCells
>  pointToConnect =
>   CellMethods.cellPoint cellToConnect
>  pointWhereToConnect =
>   CellMethods.cellPoint whereToConnect
>  gridWithRelocatedCellsMaybe =
>   gridMoveCellsWithIgnoredPoints grid cellToConnect (pointWhereToConnect - pointToConnect) [pointWhereToConnect]

| Move the Cell in the Grid by the given difference.

>gridMoveCells :: Grid -> Cell.Cell -> Super.Point -> Maybe Grid
>gridMoveCells grid cell difference =
> gridMoveCellsWithIgnoredPoints grid cell difference []

>gridMoveCellsWithIgnoredPoints :: Grid -> Cell.Cell -> Super.Point -> [Super.Point] -> Maybe Grid
>gridMoveCellsWithIgnoredPoints grid cell difference ignoredPoints =
> if free
> then Just gridMoveCells'
> else Nothing
> where
>  free = 
>   any (\filledPoint -> not $ any (filledPoint ==) newPoints) filledPoints
>  newPoints = map (difference +) $ oldPoints
>  oldPoints = cellPoints cell
>  filledPoints = (gridPoints grid) \\ (oldPoints `union` ignoredPoints)
>  relocations = zip oldPoints newPoints
>  gridMoveCells' =
>   grid
>    {
>      gridCells=CellMethods.cellPointsRelocation (gridCells grid) relocations,
>      gridLooseCells=map (\cells ->CellMethods.cellPointsRelocation cells relocations) (gridLooseCells grid)
>    }

| Insert a blank action before the Cell specified by the Point.

>gridInsertBlankAction :: Grid -> Point -> (Grid,Maybe Cell.Cell)
>gridInsertBlankAction grid point =
> let 
>  Just (myHead, myTail) =
>   cellSplitAtPoint (gridCells grid) point

>  blankActionCell =
>    Cell.Action (Cell.CellCommon (point,small) []) "" False False 0 Nothing next

>  next = 
>    cellPointsRelocation myTail $ zip (CellMethods.cellPoints myTail) (map ((0,1)+) $ CellMethods.cellPoints myTail) in

>   (grid {gridCells =
>          fst $ cellPutCell blankActionCell myHead},
>          Just blankActionCell)

>deleteCellGrid :: Point -> Grid -> Grid
>deleteCellGrid point grid =
> grid{
>  gridCells = gridCells',
>  gridLooseCells = gridLooseCells'}
>  where
>   cellsPostDeleteMaybe = CellMethods.deleteCellCell point (gridCells grid)

>   cellsPostDelete =
>    case cellsPostDeleteMaybe of
>     Just cellsPostDelete' -> cellsPostDelete'
>     Nothing -> ((gridCells grid),[])

>   looseCellsPostDelete :: [(Cell.Cell,[Cell.Cell])]
>   looseCellsPostDelete =
>    map (\cell -> maybe
>                  (cell,[])
>                  id
>                $ CellMethods.deleteCellCell point cell) 
>        (gridLooseCells grid)
 
>   gridCells' = fst cellsPostDelete

>   gridLooseCells' =
>    fst (unzip looseCellsPostDelete) ++
>    (concat $ snd $ unzip looseCellsPostDelete) ++
>    (snd cellsPostDelete)

>gridAddThread :: Grid -> Cell.Cell -> Grid
>gridAddThread grid fork =
> gridPutCell
>  forkWithNewThread
>  grid
> where
>  forkWithNewThread =
>   fork{Cell.newThreads = threadsWithNewThread}
>  threadsWithNewThread =
>   Cell.End 
>    (Cell.CellCommon 
>     (gridPointNear
>       grid 
>       $ ((2,0) + (CellMethods.cellPoint $ last $ sortBy (comparing CellMethods.cellPoint) $ Cell.newThreads fork)),
>      Super.small)
>     []) :
>   Cell.newThreads fork

>gridApply :: Grid -> (Cell.Cell->Cell.Cell) -> Grid
>gridApply grid f =
> grid{
>  gridCells = f $ gridCells grid,
>  gridLooseCells = map f $ gridLooseCells grid}
