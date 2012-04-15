>module GridAction where

>import Grid
>import DisplayCell
>import Cell

>gridSetDisplayCellText :: DisplayCell -> String -> Grid -> Grid
>gridSetDisplayCellText (DisplayCellBlank point) text grid =
> gridPutComment point text grid

>gridSetDisplayCellText (DisplayCellComment (point,_)) text grid =
> gridPutComment point text grid

>gridSetDisplayCellText (DisplayCellComment (point,_)) text grid =
> gridPutComment point text grid

>gridSetDisplayCellText (DisplayCellCode cell) text grid =
> let cell'Maybe = (cellPutCode cell text) in
> case cell'Maybe of
>   Just cell' -> gridPutCell cell' grid
>   Nothing    -> grid
