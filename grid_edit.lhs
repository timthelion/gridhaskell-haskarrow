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

The window drawing code was liberally stolen from various sources, such as http://muitovar.com/gtk2hs/index.html the code of which is copyright © 2007, 2008 Hans van Thiel and Alex Tarkovsky. I am currently awaiting permission to use this code.

>module Main where

Libraries external to application.

>import Graphics.UI.Gtk hiding (cellText)
>import System.Environment
>import Data.List
>import Data.Maybe
>import Control.Concurrent
>import System.Posix.Unistd
>import System.Exit
>import Control.Monad.IO.Class

Internal libraries.

Our top level data type/file type.

>import Grid

Grid haskell comands are defined by the type Cell in the module Cell.

These Cells are represented on the screen, but there are also other things that we display on the screen.  For example, comments.  So therefore we have a seccond type to encapsulate both.

>import DisplayCell

And then we need some code to actually draw these DisplayCells on the screen.

>import GridDrawing

And keep track of a few mutable objects, such as our grid...

>import GridEditorObjects

This type of mutable object, is defined by the module:

>import ThreadObject
>import StateRecords
>import EditModes

There are also some Super generic types that belong no where else.  Like type Point = (Int, Int)...

>import qualified Super

For reading and writing grid haskell files.

>import GridHaskellFile

>main :: IO ()
>main = do
>     (mygrid, filePath) <- loadGrid

     initGUI

>     unsafeInitGUIForThreadedRTS

>     gridEditWindowEvent <- newEmptyMVar

Special MVar used to quit.  Put a value here when you want to terminate.

>     exit                <- newEmptyMVar


>     gridObject'             <- threadObject
>     canvasObject'           <- threadObject 
>     focusedCellObject'      <- threadObject
>     focusedRectangleObject' <- threadObject
>     editModeObject'         <- threadObject
>     fileObject'             <- threadObject
>     filePathObject'         <- threadObject
>     reFocusNeededObject'    <- threadObject

>     gridRecords'            <- stateRecords gridObject' 
>     focusedCellRecords'     <- stateRecords focusedCellObject'
>     focusedRectangleRecords'<- stateRecords focusedRectangleObject'

>     editorObjects <- return (GridEditorObjects gridObject' canvasObject' editModeObject' focusedCellObject' focusedRectangleObject' reFocusNeededObject' fileObject' filePathObject' gridRecords' focusedCellRecords' focusedRectangleRecords')

>     myWidgets <- loadWidgets exit editorObjects
>     (window,_,canvas,scrwinContainer,_,cellInfo,modeInfo) <- return myWidgets

>     widgetShowAll window
>     forkOS mainGUI


Note.  It would seem that order is important here as the sync function for gridObject references scrwinObject.   Luckly it is not.  When syncGridwithScrwin calls updateIO scrwinObject, updateIO will wait till the scrwinObject is initialized before continuing.

>     objectInit (editModeObject editorObjects) FreeMovement noSyncOnGet (syncEditModeWithLabel modeInfo) False
>     objectInit (gridObject editorObjects) mygrid noSyncOnGet (syncGridwithCanvas editorObjects) False
>     objectInit (canvasObject editorObjects) canvas noSyncOnGet noSyncOnPut False
>     objectInit (focusedCellObject editorObjects) Nothing noSyncOnGet (syncFocusedCellWithLabel cellInfo) True
>     objectInit (focusedRectangleObject editorObjects) (Rectangle 0 0 0 0) noSyncOnGet (syncFocusedRectangleWithScrolledWindow editorObjects) True
>     objectInit (reFocusNeededObject editorObjects) False noSyncOnGet noSyncOnPut True
>     objectInit (fileObject editorObjects) Nothing noSyncOnGet (saveFile editorObjects) False
>     objectInit (filePathObject editorObjects) (Just filePath) noSyncOnGet noSyncOnPut False

>     signal <- takeMVar exit

We want to make sure that we have finished saving the file(if we are saving) before we exit.

>     freeObject (fileObject editorObjects)
>     exitWith signal

>syncGridwithCanvas :: GridEditorObjects -> Grid -> Maybe (RecorderSignal ()) -> IO ()
>syncGridwithCanvas editorObjects mygrid signal = do

This is as good a time as any to record the state of our grid.  We will keep at least 20 and up to 40 states which can be returned to with the undo(Ctrl-z) function.  

>   case signal of
>     Just (RecorderSignal False Nothing) ->
>       return ()
>     Nothing -> do
>       focusedCell <- getObjectValue (focusedCellObject editorObjects)
>       focusedRectangle <- getObjectValue (focusedRectangleObject editorObjects)
>       recordState 20 (gridRecords editorObjects) mygrid
>       recordState 20 (focusedCellRecords editorObjects) focusedCell
>       recordState 20 (focusedRectangleRecords editorObjects) focusedRectangle

We'll want to restore our scroll location(if possible) after we're done updating.

   print "Recorded states, now getting value of focusedRectangleObject"

>   oldRectangle <- getObjectValue (focusedRectangleObject editorObjects)

We update the canvas with a new one which displays the new state of the grid.

   print "entering update for canvasObject"

>   updateIO (canvasObject editorObjects)  (\canvas      -> do{

   print "entering postGUISync";

The function postGUIAsync is required to insure thread safety in GTK.  Without it, the sync function will succeed 30% of the time, and fail 70% of the time :D

>   postGUISync (do {

    print "Entered postGUISync on grid sync";

First we get rid of the old canvas

>    canvasContainer' <- (widgetGetParent canvas);

    print "Casting canvas's parent(VBox) to container.";

>    canvasContainer  <- return (castToContainer (fromJust canvasContainer'));

    print "destroying canvas!";

>    widgetDestroy canvas;

And we make a new one...

    print "Creating new canvas.";

>    canvas' <- scrolledWindowNew Nothing Nothing;

>    scrolledWindowSetPolicy canvas' PolicyNever PolicyNever;

    print "Drawing the grid.";

>    focusedWidgetMaybe <- drawGrid editorObjects mygrid canvas' oldRectangle;

    print "Adding canvas back into the container.";

>    containerAdd canvasContainer canvas';

    print "Canvas added.";

    print "updating reFocusNeededObject";

>    update (reFocusNeededObject editorObjects) (\_->True);


    print "Grabbing focus for focused cell.";

>    (case focusedWidgetMaybe of
>       Just focusedWidget -> do 
>        widgetGrabFocus focusedWidget;
>        return();

>       Nothing -> return ());

>    widgetShowAll canvas';

    update (focusedRectangleObject editorObjects)
              (\_->oldRectangle);

>    return canvas';
>   });})


>syncFocusedCellWithLabel :: Label -> Maybe 	DisplayCell.DisplayCell -> Maybe a -> IO()
>syncFocusedCellWithLabel cellInfo (Just dc) _ = postGUIAsync $ do
>     set cellInfo [ labelText := (show (displayCellPoint dc))]
>     return ()

>syncFocusedCellWithLabel _ Nothing _ = do
>     return ()

>syncEditModeWithLabel :: Label -> EditMode -> Maybe a -> IO()
>syncEditModeWithLabel modeInfo mode _ = postGUIAsync $ do
>     set modeInfo [ labelText := 
>       (case mode of
>         AddAction{}  -> "Add action mode"
>         AddPattern{} -> "Add pattern mode"
>         EditPath{}   -> "Path edit mode"
>         MoveCell{}   -> "Move cell mode | F6/Esc Exit Mode | Enter Place Cell"
>         EditCell{}   -> "Cell edit mode | Esc Exit Mode"
>         FreeMovement -> "Navigation mode | F6 MoveCell"
>         ShowError message _ -> message)]

>     return ()

>saveFile :: GridEditorObjects -> Maybe String -> Maybe () -> IO()
>saveFile editorObjects (Just contents) signal = do
> filePath <- updateIOReturning (filePathObject editorObjects) (\filePath -> do
>  if isNothing filePath
>  then postGUISync $ do
>     fileChooserDialog <- fileChooserDialogNew (Just "Save As...Dialog") Nothing
>                                     FileChooserActionSave
>                                     [("Cancel", ResponseCancel),
>                                      ("Save", ResponseAccept)]
 
>     fileChooserSetDoOverwriteConfirmation fileChooserDialog True
>     widgetShow fileChooserDialog
>     response <- dialogRun fileChooserDialog
>     value <- case response of
>          ResponseCancel -> return (Nothing,Nothing)
>          ResponseAccept -> do {
>                       newFilenameMaybe <- fileChooserGetFilename fileChooserDialog;
>                       return (case newFilenameMaybe of
>                                    Nothing -> (Nothing, Nothing)
>                                    Just path -> (Just path,Just path));}
>          ResponseDeleteEvent -> return (Nothing,Nothing)
>     widgetDestroy fileChooserDialog
>     return value
>  else return (filePath,filePath))

> case filePath of
>  Nothing -> return ()
>  Just filePath -> writeFile filePath contents

>saveFile _ Nothing signal = return ()

>syncFocusedRectangleWithScrolledWindow :: GridEditorObjects -> Rectangle -> Maybe () -> IO()
>syncFocusedRectangleWithScrolledWindow editorObjects rectangle@(Rectangle x y _ _) signal = do
>  updateIO (canvasObject editorObjects) (\scrolledWindow -> do {
>   postGUIAsync $ do {

WARNING! This is a hack! (to take into accound the size of the scroll bars.  )

>   scrolledWindowContents <- containerGetChildren scrolledWindow;
>   (Rectangle _ _ aW aH)  <- widgetGetAllocation (head scrolledWindowContents);

End of hack.

>   adjustmentX    <- scrolledWindowGetHAdjustment scrolledWindow;
>   upperX         <- adjustmentGetUpper adjustmentX;
>   lowerX         <- adjustmentGetLower adjustmentX;
>   adjustmentSetValue adjustmentX (scrollCord (fromIntegral x) upperX lowerX (fromIntegral aW));

>   adjustmentY    <- scrolledWindowGetVAdjustment scrolledWindow;
>   upperY         <- adjustmentGetUpper adjustmentY;
>   lowerY         <- adjustmentGetLower adjustmentY;
>   adjustmentSetValue adjustmentY (scrollCord (fromIntegral y) upperY lowerY (fromIntegral aH));
>};
>  return scrolledWindow;});

>scrollCord cord upper lower boxAllocation
> | cord + boxAllocation/2 > upper = upper - boxAllocation
> | cord - boxAllocation/2 < lower = lower
> | otherwise                      = cord - boxAllocation/2

>loadGrid :: IO (Grid,FilePath)
>loadGrid = do
>     args <- getArgs 
>     if null args
>     then return (emptyGrid,"")
>     else do {
>     gridString <- readFile (head args);
>     return (openGrid gridString::Grid,(head args))}

>createWindow :: IO Window
>createWindow = do
>    window <- windowNew
>    set window [ windowTitle := "Grid editor", 
>                  windowDefaultWidth := 300, windowDefaultHeight := 250]
>    return window

>loadWidgets :: MVar ExitCode -> GridEditorObjects -> IO (Window, VBox, ScrolledWindow, VBox, HBox, Label,Label) 
>loadWidgets exit editorObjects = do
>     window <- createWindow

>     myTopVBox <- vBoxNew False 0
>     containerAdd window myTopVBox

>     scrwinContainer <-vBoxNew False 0
>     boxPackStart myTopVBox scrwinContainer PackGrow 0
>     scrwin <- scrolledWindowNew Nothing Nothing
>     boxPackStart scrwinContainer scrwin PackGrow 0
     
>     sep2 <- hSeparatorNew
>     boxPackStart myTopVBox sep2 PackNatural 7
>     buttonBox <- hBoxNew False 0
>     boxPackStart myTopVBox buttonBox PackNatural 0

>     fileMenuAction <- actionNew "FMA" "File" Nothing Nothing

>     newAction <- actionNew "NEWA" "New"     (Just "Just a Stub") (Just stockNew)
>     openAction <- actionNew "OPNA" "Open"    (Just "Just a Stub") (Just stockOpen)
>     saveAction <- actionNew "SAVA" "Save"    (Just "Just a Stub") (Just stockSave)
>     saveAsAction <- actionNew "SVAA" "Save As" (Just "Just a Stub") (Just stockSaveAs)
>     exitAction <- actionNew "EXIA" "Exit"    (Just "Just a Stub") (Just stockQuit)

>     actionGroup <- actionGroupNew "AGR"
>     actionGroupAddAction actionGroup fileMenuAction
>     mapM_ (\ act -> actionGroupAddActionWithAccel actionGroup act Nothing)
>       [newAction,openAction,saveAction,saveAsAction]

>     actionGroupAddActionWithAccel actionGroup exitAction (Just "<Control>e")

>     ui <- uiManagerNew
>     uiManagerAddUiFromString ui "<ui>\
>\           <menubar>\
>\            <menu action=\"FMA\">\
>\              <menuitem action=\"NEWA\" />\
>\              <menuitem action=\"OPNA\" />\
>\              <menuitem action=\"SAVA\" />\
>\              <menuitem action=\"SVAA\" />\
>\              <separator />\
>\              <menuitem action=\"EXIA\" />\
>\            </menu>\
>\           </menubar>\
>\          </ui>"

>     uiManagerInsertActionGroup ui actionGroup 0

>     maybeMenubar <- uiManagerGetWidget ui "/ui/menubar"
>     let menubar = case maybeMenubar of
>                        (Just x) -> x
>                        Nothing -> error "Cannot get menubar from string." 
>     boxPackStart buttonBox menubar PackNatural 0

>     newAction `on` actionActivated $ do {
>       liftIO $ do {
>          continue <- saveOnPrompt window editorObjects "Save file before creating new one?";
>          if continue 
>          then do
>            update (filePathObject editorObjects) (\_->Nothing);
>            update (gridObject editorObjects) (\_-> emptyGrid);
>          else return ()}}

>     saveAsAction `on` actionActivated $ do {
>       liftIO $ do {update (filePathObject editorObjects) (\_->Nothing);
>          updateMulti
>            (gridObject editorObjects) $
>           finallyUpdate
>            (fileObject editorObjects) $
>            (\grid file -> ((Just $ saveGrid(grid)),grid))};}

>     saveAction `on` actionActivated $ do{
>         liftIO $ do {updateMulti
>            (gridObject editorObjects) $
>           finallyUpdate
>            (fileObject editorObjects) $
>            (\grid file -> ((Just $ saveGrid(grid)),grid))};}

>     exitAction `on` actionActivated $ do{
>         liftIO $ do {quit exit window editorObjects};}

>     cellInfo <- labelNew Nothing
>     boxPackStart buttonBox cellInfo PackNatural 0

>     modeInfo <- labelNew Nothing
>     boxPackStart buttonBox modeInfo PackNatural 0

>     window `on` keyPressEvent $ do
>            modifier <- eventModifier
>            key <- eventKeyName

<            liftIO (print key)

>            case (modifier,key) of
>             ([],"Escape") ->
>              liftIO $ do
>               updateReturning (editModeObject editorObjects) 
>                   (\mode->
>                     case mode of
>                       EditCell{} -> (mode,False)
>                       otherwise  -> (FreeMovement,True))
>             ([],"F6") ->
>              liftIO $ do
>               updateWith (focusedCellObject editorObjects) (editModeObject editorObjects)

No need to finish the pattern with a Nothing, tryEvent will catch the exception...

>                      (\(Just focusedCell) editMode -> 
>                        case editMode of
>                        MoveCell dc  -> FreeMovement
>                        otherwise -> MoveCell focusedCell);
>               return True;

>             ([Control],"z")  -> 
>              liftIO $ do
>                undoStateAction (focusedCellRecords editorObjects)
>                undoStateAction (focusedRectangleRecords editorObjects)
>                undoStateActionOfRecorder (gridRecords editorObjects)
>                return True
               
>             otherwise -> return False

>     window `on` objectDestroy $ do {
>      liftIO $ do {quit exit window editorObjects};
>     return ();}

>     return (window,myTopVBox, scrwin, scrwinContainer, buttonBox, cellInfo,modeInfo)


>quit :: MVar ExitCode -> Window -> GridEditorObjects -> IO ()
>quit exit window editorObjects = do
>      continueExit <- saveOnPrompt window editorObjects "Save this file before exiting?";
>      if continueExit
>      then putMVar exit ExitSuccess
>      else return ()

>saveOnPrompt :: Window -> GridEditorObjects -> String -> IO Bool
>saveOnPrompt window editorObjects prompt = do
>      saveOrNotDialog <- messageDialogNew (Just window) [] MessageQuestion ButtonsYesNo prompt;
>      saveOrNot <- dialogRun saveOrNotDialog; 
>      widgetDestroy saveOrNotDialog;
>      (case saveOrNot of
>        ResponseYes -> do {
>                       updateMulti
>                         (gridObject editorObjects) $
>                        finallyUpdate
>                         (fileObject editorObjects) $
>            (\grid file -> ((Just $ saveGrid(grid)),grid));
>                       return True;}
>        ResponseNo  -> return True
>        ResponseDeleteEvent -> return False 
>        ResponseNone -> return False
>        otherwise   -> error $ "Cannot process dialog responce:" ++ (show saveOrNot));
