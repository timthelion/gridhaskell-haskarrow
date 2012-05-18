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

>module GridDrawing (drawGrid) where

>import Graphics.UI.Gtk
>import Data.Tuple
>import Data.List
>import Data.Maybe
>import Data.NumInstances
>import Control.Concurrent
>import Control.Monad.IO.Class

>import Grid
>import GridExtremes
>import qualified Super
>import qualified DisplayCell
>import qualified Cell
>import qualified CellMethods
>import qualified Path
>import ThreadObject
>import ArrowDrawing
>import EditModes
>import GridEditorObjects
>import GridAction

| Draw the actual gtk table which shows our grid.

>drawGrid :: GridEditorObjects -> Grid -> ScrolledWindow -> Rectangle -> IO (Maybe Widget)
>drawGrid  editorObjects mygrid canvas oldRectangle = do{

This is a list of cells to be displayed.

>     displayCells <- return (DisplayCell.displayCellList mygrid);

We use these extremes to find how big a table to make.

>     grid_maximum_x <- return $ (maximum_x displayCells) + 1;
>     grid_maximum_y <- return $ (maximum_y displayCells) + 1;
>     grid_minimum_x <- return $ (minimum_x displayCells) - 1;
>     grid_minimum_y <- return $ (minimum_y displayCells) - 1;


Y before X because tableNew goes Rows Collumns order.

>     table <- tableNew (lengthBetween grid_minimum_y grid_maximum_y) (lengthBetween grid_minimum_x grid_maximum_x) True;
>     scrolledWindowAddWithViewport canvas table;

A "filled in" list of display cells, to make a complete grid, including blank cells.

>     displayCellsFilled <- return (DisplayCell.filledinDisplayCellList displayCells (grid_minimum_x,grid_minimum_y) (grid_maximum_x,grid_maximum_y));

Now we build gtk widgets for each displayCell.

>     cellForms <- sequence (map (cellForm editorObjects) displayCellsFilled);

We have to find the widget associated with the focused cell, such that upon redrawing everything, focus doesn't change.

>     focusedCellMaybe <- getObjectValue (focusedCellObject editorObjects);

>     focusedWidgetMaybe <- return (case focusedCellMaybe of
>       Just focusedCell ->
>        lookup (DisplayCell.displayCellPoint focusedCell) $ zip (map DisplayCell.displayCellPoint displayCellsFilled) (map snd cellForms)
>       Nothing ->
>        Nothing);

Now we make a list of all the points that appear in our table to actually be drawn on the screen.

>     places <- return (map 
>                 (\p -> Super.subtractPoint p 
>                       (grid_minimum_x,grid_minimum_y))
>          (map DisplayCell.displayCellPoint displayCellsFilled));

and then we zip those points with the widgets we want to attach to that table using the function attachCellForm to attach them.

>     sequence_ (zipWith (attachCellForm table) (map fst cellForms) places);

Now we add an event to draw the lines in our diagram.

>     onExpose table  $
>       (\_ -> do drawWin <- widgetGetDrawWindow table
>                 allocations <- (mapM widgetGetAllocation (map fst cellForms))
>                 renderWithDrawable drawWin (drawArrows (zip allocations displayCellsFilled))
>                 liftIO $ do {
>                  updateMulti
>                   (reFocusNeededObject editorObjects) $
>                  finallyUpdate
>                   (focusedRectangleObject editorObjects) $
>                     (\reFocus rect->
>                       if reFocus
>                       then (oldRectangle,False)
>                       else (rect,False));};
>                 return False);

>     return focusedWidgetMaybe;

     (case focusedWidgetMaybe of
       Just focusedWidget -> widgetGrabFocus focusedWidget;
       Nothing -> return ());

     widgetShowAll canvas;

>     }

>focusedPoint (Just dc) = (DisplayCell.displayCellPoint dc)
>focusedPoint Nothing = (0,0)

| 'cellForm' generates a gtk widget which represents the a cell in our grid.  The return type of this should be changed, so that we get something more usefull than a VBox.  This should return a data which includes all widgets who's contents might want to be updated or saved.

>cellForm :: GridEditorObjects -> DisplayCell.DisplayCell -> IO (Box,Widget)

>cellForm editorObjects dc = do
>       box <- hBoxNew False 0
>       box `on` keyPressEvent $ do
>        modifier <- eventModifier
>        key <- eventKeyName
>        case (modifier,key) of
>           ([],"Delete") -> do 
>               liftIO $
>                update (gridObject editorObjects)
>                       $ (\grid -> deleteCellGrid (DisplayCell.displayCellPoint dc) grid)
>               return True
>           otherwise     -> return False

>       widget <- cellFormFill (toBox box) False editorObjects dc
>       return ((toBox box), widget)

>cellFormFill :: Box -> Bool -> GridEditorObjects -> DisplayCell.DisplayCell -> IO Widget

>cellFormFill box edit editorObjects dc@(DisplayCell.DisplayCellCode cell@Cell.Action{}) = do
>        pure <- buttonNewWithLabel pureText
>        set pure [buttonRelief := ReliefHalf]
>        pure `on` buttonActivated $ do
>              liftIO $ do
>               update (gridObject editorObjects) 
>                      (\grid -> gridPutCell (cell{Cell.return = not $ Cell.return cell}) grid)

>        boxPackStart box pure PackNatural 0

>        bind <- buttonNewWithLabel bindText
>        set bind [buttonRelief := ReliefHalf]
>        boxPackStart box bind PackNatural 0

>        bind `on` keyPressEvent $ do
>            modifier <- eventModifier
>            key <- eventKeyName
>            case (modifier,key) of
>             ([Control],"Right") ->
>              liftIO $ do
>               update (gridObject editorObjects) 
>                      (\grid -> gridPutCell (CellMethods.decrimentPull cell) grid)
>               return True

>             ([Control],"Left") ->
>              liftIO $ do
>               update (gridObject editorObjects) 
>                      (\grid -> gridPutCell (CellMethods.incrimentPull cell) grid)
>               return True

>             ([Control],"Up") ->
>              liftIO $ do
>               update (gridObject editorObjects) 
>                      (\grid -> gridPutCell (CellMethods.cellPutPush cell True) grid)
>               return True

>             ([Control],"Down") ->
>              liftIO $ do
>               update (gridObject editorObjects) 
>                      (\grid -> gridPutCell (CellMethods.cellPutPush cell False) grid)
>               return True

>             othrewise ->
>              return False

>        widget <- cellFormFill' (DisplayCell.displayCellText dc) (toBox box) edit editorObjects dc

>        return widget

>           where
>            bindText :: String 
>            bindText =
>             case cell of

>              (Cell.Action _ _ _ True n _ _ _) -> (show n) ++ "^" 

>              (Cell.Action _ _ _ False n _ _ _) -> (show n) ++ ":" 

>            pureText :: String
>            pureText =
>             case cell of

Pure

>              (Cell.Action _ _ True _ _ _ _ _) -> "=" 

Not pure

>              (Cell.Action _ _ False _ _ _ _ _) -> "IO" 

>cellFormFill box edit editorObjects dc@(DisplayCell.DisplayCellCode cell@Cell.Lambda{}) = do
> now <- buttonNewWithLabel (nowText $ Cell.now cell)
> set now [buttonRelief := ReliefHalf]
> now `on` buttonActivated $ do
>  liftIO $ do
>   update (gridObject editorObjects) 
>          (\grid -> gridPutCell (cell {Cell.now = 
>                                       not $ Cell.now cell}) grid)

> bind <- buttonNewWithLabel bindText
> set bind [buttonRelief := ReliefHalf]
> boxPackStart box bind PackNatural 0

> bind `on` keyPressEvent $ do
>  modifier <- eventModifier
>  key <- eventKeyName
>  case (modifier,key) of
>   ([Control],"Right") ->
>    liftIO $ do
>     update (gridObject editorObjects) 
>            (\grid -> gridPutCell (CellMethods.decrimentPull cell) grid)
>     return True

>   ([Control],"Left") ->
>    liftIO $ do
>     update (gridObject editorObjects) 
>            (\grid -> gridPutCell (CellMethods.incrimentPull cell) grid)
>     return True

>   otherwise -> return False

> widget <- cellFormFill' (DisplayCell.displayCellText dc) (toBox box) edit editorObjects dc

> boxPackStart box now PackNatural 0

> return widget
>  where
>   nowText True  = "Now"
>   nowText False = "Later" 

>   bindText = show $ Cell.pull cell

>cellFormFill box edit editorObjects dc = do
> cellFormFill' (DisplayCell.displayCellText dc) box edit editorObjects dc

>cellFormFill' :: String -> Box -> Bool -> GridEditorObjects -> DisplayCell.DisplayCell -> IO Widget

>cellFormFill' text box edit editorObjects dc = do
>        if not edit
>        then do
>         enter <- buttonNewWithLabel text
>         if not $ (DisplayCell.displayCellBlank dc) || (DisplayCell.displayCellPath dc)
>         then set enter [buttonRelief := ReliefHalf]
>         else set enter [buttonRelief := ReliefNone]
>         boxPackStart box enter PackGrow 0

         enter `on` buttonPressEvent $ liftIO $ do{
             updateIO
               (editModeObject editorObjects)
                (editModeAction editorObjects dc box);
             return False;}
    
>         onClicked enter (do {updateIONoBlock (editModeObject editorObjects) (editModeAction editorObjects dc box)})

>         enter `on` focusOutEvent $ do 
>             { liftIO $ do
>            update (editModeObject editorObjects)
>                   (\mode -> case mode of
>                              ShowError e True -> ShowError e False
>                              ShowError _ False -> FreeMovement
>                              otherwise   -> mode);
>               return False};

>         enter `on` focusInEvent $ do 
>          { liftIO $ do {
>            update (focusedCellObject editorObjects)
>                   (\_->(Just dc));
>            updateIONoBlock (focusedRectangleObject editorObjects) (\_-> do
>                {rect <- postGUISync $ widgetGetAllocation box;
>                 return rect;});
>            return ();};
>            return False};
>         return (toWidget enter)
>        else do
>         entry <- entryNew
>         entrySetText entry text
>         boxPackStart box entry PackNatural 0
>         widgetGrabFocus entry;

>         entry `on` focusOutEvent $ do 
>             { liftIO $ do
>               updateIO (editModeObject editorObjects) 
>                        (editModeCancleCellEdit editorObjects dc box);
>               return False};

>         entry `on` keyPressEvent $ tryEvent $ do
>            key  <- eventKeyName

>            case key of
>             "Return" ->
>              liftIO $ do
>               text <- entryGetText entry
>               update (gridObject editorObjects) (GridAction.gridSetDisplayCellText dc text);

>             "Escape" -> 
>              liftIO $ do
>               updateIO (editModeObject editorObjects) 
>                        (editModeCancleCellEditEscape editorObjects dc box);

>         return (toWidget entry)

>editModeAction :: GridEditorObjects -> DisplayCell.DisplayCell -> Box -> EditMode -> IO EditMode

>editModeAction editorObjects dc vbox ShowError{} = return FreeMovement

If we are in FreeMovement mode we make a text entry and enter EditCell mode.

>editModeAction editorObjects dc vbox FreeMovement = do
>   postGUISync (do {

If we are on an End, Exit, or Return cell, we should move this cell and make a new one to edit.

>     let addCell common = do {
>      updateReturning
>       (gridObject editorObjects)
>         (\grid -> gridInsertBlankAction grid (CellMethods.commonPoint common))} in do

>   mDisplayCell <- (case dc of
>      DisplayCell.DisplayCellCode (Cell.Exit common) -> do 
>         mCell <- (addCell common)
>         return $ case mCell of {Nothing -> Nothing ; Just cell -> Just $ DisplayCell.DisplayCellCode cell} 
>      DisplayCell.DisplayCellCode (Cell.End common) -> do 
>         mCell <- (addCell common)
>         return $ case mCell of {Nothing -> Nothing ; Just cell -> Just $ DisplayCell.DisplayCellCode cell} 
>      DisplayCell.DisplayCellCode (Cell.Return common) -> do 
>         mCell <- (addCell common)
>         return $ case mCell of {Nothing -> Nothing ; Just cell -> Just $ DisplayCell.DisplayCellCode cell}
>      DisplayCell.DisplayCellBlank {} -> return Nothing
>      otherwise  -> return $ Just dc);

>   case mDisplayCell of
>    Nothing -> (return (ShowError "Cannot add a new Cell, there is something in the way." True))
>    Just dc -> do
>     containerForeach vbox (containerRemove vbox);

     print "filling with entry box";

>     cellFormFill vbox True editorObjects dc; 
>     widgetShowAll vbox;
>     return (EditCell dc);
>   })
  

>editModeAction editorObjects dc vbox (MoveCell cellWe'reMoving) = 
>   case cellWe'reMoving of
>     DisplayCell.DisplayCellBlank{} -> return (FreeMovement)
>     otherwise -> do
>              relocationSuccessfull <- updateReturning (gridObject editorObjects)
>                     (\grid -> gridPointsRelocation grid [(DisplayCell.displayCellPoint cellWe'reMoving,DisplayCell.displayCellPoint dc)])
>              if relocationSuccessfull
>              then return (FreeMovement)
>              else return (ShowError "Cannot move cell, there is something in the way." True)

>editModeAction editorObjects dc vbox (MoveCells cellWe'reMoving) = 
>   case cellWe'reMoving of
>     DisplayCell.DisplayCellCode cell -> do
>              relocationSuccessfull <- updateReturning (gridObject editorObjects)
>                     (\grid -> gridPointsRelocation grid $ zip oldPoints newPoints)
>              if relocationSuccessfull
>              then return (FreeMovement)
>              else return (ShowError "Cannot move cell, there is something in the way." True)
>      where 
>       oldPoints = CellMethods.cellPoints cell
>       newPoints = map (difference +) oldPoints 
>       difference = DisplayCell.displayCellPoint dc - (CellMethods.cellPoint cell)
>     otherwise -> return (FreeMovement)

>editModeCancleCellEdit :: GridEditorObjects -> DisplayCell.DisplayCell -> Box -> EditMode -> IO EditMode
>editModeCancleCellEdit editorObjects dc vbox EditCell{} = do
>   postGUIAsync (do {
>   containerForeach vbox (containerRemove vbox);
>   cellFormFill vbox False editorObjects dc; 
>   widgetShowAll vbox;

>   })
>   return FreeMovement

>editModeCancleCellEdit _ _ _ FreeMovement = do
>  return FreeMovement

>editModeCancleCellEdit _ _ _ mode = do
>  return $ error "Help! Expected to be in EditCell mode, but instead I got:" ++ (show mode)
>  return FreeMovement

>editModeCancleCellEditEscape :: GridEditorObjects -> DisplayCell.DisplayCell -> Box -> EditMode -> IO EditMode
>editModeCancleCellEditEscape editorObjects dc vbox EditCell{} = do
>   postGUIAsync (do {
>   containerForeach vbox (containerRemove vbox);
>   focusedWidget <- cellFormFill vbox False editorObjects dc;
>   widgetShowAll vbox;
>   widgetGrabFocus focusedWidget
>   })
>   return FreeMovement

>attachCellForm :: Table -> Box -> (Int,Int) -> IO ()
>attachCellForm table form (x,y) = 
>              tableAttachDefaults table form x (x+1) y (y+1)
