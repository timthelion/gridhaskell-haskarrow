>module GridExtremes where

>import qualified DisplayCell
>import qualified Super

| The 'Int' returnd equals the most extreme value of the 'axis' which is present in the grid.  That is the distance farthest to the top, right, left, or bottom...

>extremePoint :: [DisplayCell.DisplayCell] -> (Super.Point -> Int) ->  ([Int] -> Int) -> Int

| The 'axis' is a function which given point (x,y) returns either x or y.
  The 'selector' is a function which selects an item from a list.  In this case we pass it either minimum or maximum, depending on which extreme we want.
  
>extremePoint displayCells axis selector = 
>    selector (map axis (map DisplayCell.displayCellPoint displayCells))

| 'maximumPoint' and 'minimumPoint' are just glosses for extremePoint.

>maximumPoint :: [DisplayCell.DisplayCell] -> (Super.Point -> Int) -> Int
>maximumPoint [] axis = 0
>maximumPoint displayCell axis = extremePoint displayCell axis maximum
>minimumPoint :: [DisplayCell.DisplayCell] -> (Super.Point -> Int) -> Int
>minimumPoint [] axis = 0
>minimumPoint displayCell axis = extremePoint displayCell axis minimum


Just more abstractions for minimumPoint and maximumPoint

>maximum_x  :: [DisplayCell.DisplayCell] -> Int
>maximum_x displayCell = maximumPoint displayCell (\(x,_) -> x)
>maximum_y :: [DisplayCell.DisplayCell] -> Int
>maximum_y displayCell = maximumPoint displayCell (\(_,y) -> y)
>minimum_x  :: [DisplayCell.DisplayCell] -> Int
>minimum_x displayCell = minimumPoint displayCell (\(x,_) -> x)
>minimum_y  :: [DisplayCell.DisplayCell] -> Int
>minimum_y displayCell = minimumPoint displayCell (\(_,y) -> y)

| The 'lengthBetween' two numbers is the true (length [a..b]).  length [a..b] however returns the wrong result in Haskell.  For [0..0] it returns 1 when we want 0.

>lengthBetween a b = if a==b then 0 else (b-a+1)
