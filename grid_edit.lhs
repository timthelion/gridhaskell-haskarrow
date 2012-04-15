>module Main where

Libraries external to application.

>import Graphics.UI.Gtk hiding (cellText)
>import System.Environment
>import Data.List
>import Data.Maybe
>import Control.Concurrent
>import System.Posix.Unistd
>import System.Exit
>import EditModes

Internal libraries.

Our top level data type/file type.

>import Grid

Grid haskell comands are defined by the type Cell in the module Cell.

These Cells are represented on the screen, but there are also other things that we display on the screen.  For example, comments.  So therefore we have a seccond type to encapsulate both.

>import DisplayCell

And then we need some code to actually draw these DisplayCells on the screen.

>import GridDrawing

Once we have a window, we need to set up some generic events for the whole window:

>import GridEditWindowEvents

And keep track of a few mutable objects, such as our grid...

>import GridEditorObjects

This type of mutable object, is defined by the module:

>import ThreadObject

There are also some Super generic types that belong no where else.  Like type Point = (Int, Int)...

>import qualified Super

>main :: IO ()
>main = do
>     (mygrid, filePath) <- loadGrid

>     initGUI

>     gridEditWindowEvent <- newEmptyMVar

Special MVar used to quit.  Put a value here when you want to terminate.

>     exit                <- newEmptyMVar


>     gridObject'          <- threadObject
>     canvasObject'        <- threadObject 
>     focusedCellObject'   <- threadObject
>     editModeObject'      <- threadObject
>     fileObject'          <- threadObject
>     filePathObject'      <- threadObject

>     editorObjects <- return (GridEditorObjects gridObject' canvasObject' editModeObject' focusedCellObject' fileObject' filePathObject')

>     myWidgets <- loadWidgets gridEditWindowEvent editorObjects
>     (window,_,canvas,scrwinContainer,_,cellInfo) <- return myWidgets

>     widgetShowAll window

Note.  It would seem that order is important here as the sync function for gridObject references scrwinObject.   Luckly it is not.  When syncGridwithScrwin calls updateIO scrwinObject, updateIO will wait till the scrwinObject is initialized before continuing.

>     objectInit (editModeObject editorObjects) FreeMovement noSyncOnGet noSyncOnPut
>     objectInit (gridObject editorObjects) mygrid noSyncOnGet (syncGridwithCanvas editorObjects)
>     objectInit (canvasObject editorObjects) canvas noSyncOnGet noSyncOnPut
>     objectInit (focusedCellObject editorObjects) Nothing noSyncOnGet (syncFocusedCellWithLabel cellInfo)
>     objectInit (fileObject editorObjects) Nothing noSyncOnGet (saveFile editorObjects)
>     objectInit (filePathObject editorObjects) filePath noSyncOnGet noSyncOnPut


>     forkIO $ handleGridEditWindowEvent exit Nothing gridEditWindowEvent

>     forkIO mainGUI

>     signal <- takeMVar exit
>     exitWith signal

>syncGridwithCanvas :: GridEditorObjects -> Grid -> IO ()
>syncGridwithCanvas editorObjects mygrid = do

We update the canvas with a new one which displays the new state of the grid.

>   updateIO (canvasObject editorObjects)  (\canvas      -> do{
>   canvasMVar <- newEmptyMVar;

The function postGUIAsync is required to insure thread safety in GTK.  Without it, the sync function will succeed 30% of the time, and fail 70% of the time :D

>   postGUIAsync (do {

First we get rid of the old canvas

>    canvasContainer' <- (widgetGetParent canvas);
>    canvasContainer  <- return (castToContainer (fromJust canvasContainer'));
>    containerRemove canvasContainer canvas;

And we make a new one...

>    canvas' <- scrolledWindowNew Nothing Nothing;

This is a bit tricky.  The drawGrid function will not actually run untill after this sync is finished.  Since it getObjectValue of the canvasObject and the gridObject in editorObjects, it will have to wait till these values are returned from this function...

>    forkIO $ do {drawGrid editorObjects mygrid;};

>    containerAdd canvasContainer canvas';

>    putMVar canvasMVar canvas';
>   });
>   canvas' <- takeMVar canvasMVar;
>   return canvas';})

>syncFocusedCellWithLabel :: Label -> Maybe 	DisplayCell.DisplayCell -> IO()
>syncFocusedCellWithLabel cellInfo (Just dc) = do
>     set cellInfo [ labelText := (show (displayCellPoint dc))]
>     return ()

>syncFocusedCellWithLabel _ Nothing = do
>     return ()

>saveFile :: GridEditorObjects -> Maybe String -> IO()
>saveFile editorObjects (Just contents) = do
> filePath <- getObjectValue (filePathObject editorObjects)
> writeFile filePath contents
>saveFile _ Nothing = return ()


>loadGrid :: IO (Grid,FilePath)
>loadGrid = do
>     args <- getArgs 
>     if null args
>     then return (emptyGrid,"")
>     else do {
>     gridString <- readFile (head args);
>     return (read gridString::Grid,(head args))}

>createWindow :: IO Window
>createWindow = do
>    window <- windowNew
>    set window [ windowTitle := "Grid editor", 
>                  windowDefaultWidth := 300, windowDefaultHeight := 250]
>    return window

>loadWidgets :: MVar GridEditWindowEvent -> GridEditorObjects -> IO (Window, VBox, ScrolledWindow, VBox, HBox, Label) 
>loadWidgets gridEditWindowEvent editorObjects = do
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

>     save <- buttonNewFromStock stockSave
>     boxPackStart buttonBox save PackNatural 0

>     cellInfo <- labelNew Nothing
>     boxPackStart buttonBox cellInfo PackNatural 0

>     quit <- buttonNewFromStock stockQuit
>     boxPackEnd buttonBox quit PackNatural 0

>     onClicked save (do {update2 (gridObject editorObjects) (fileObject editorObjects) (\grid file -> (grid, (Just $ show(grid))))})

>     onClicked quit (do {widgetDestroy window; putMVar gridEditWindowEvent GridEditWindowQuit})
>     onDestroy window (do {mainQuit; putMVar gridEditWindowEvent GridEditWindowQuit})
>     return (window,myTopVBox, scrwin, scrwinContainer, buttonBox, cellInfo)
