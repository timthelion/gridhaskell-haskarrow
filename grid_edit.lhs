>module Main where

Libraries external to application.

>import Graphics.UI.Gtk hiding (cellText)
>import System.Environment
>import Data.List
>import Control.Concurrent
>import System.Posix.Unistd
>import System.Exit

Internal libraries.

Our top level data type/file type.

>import Grid

Grid haskell comands are defined by the type Cell in the module Cell.

These Cells are represented on the screen, but there are also other things that we display on the screen.  For example, comments.  So therefore we have a seccond type to encapsulate both.

>import DisplayCell

And then we need some code to actually draw these DisplayCells on the screen.

>import GridDrawing

>import GridEditWindowEvents
>import ThreadObject

>import qualified Super

>main :: IO ()
>main = do
>     mygrid <- loadGrid

>     initGUI

>     gridEditWindowEvent <- newEmptyMVar

Special MVar used to quit.  Put a value here when you want to terminate.

>     exit                <- newEmptyMVar


>     gridObject          <- threadObject
>     scrwinObject        <- threadObject 
>     focusedCellObject   <- threadObject

>     myWidgets <- loadWidgets gridEditWindowEvent gridObject
>     (window,_,scrwin,scrwinContainer,_,cellInfo) <- return myWidgets

>     widgetShowAll window

>     objectInit gridObject mygrid (syncGridwithScrwin window scrwinContainer scrwinObject gridObject focusedCellObject)
>     objectInit scrwinObject scrwin (\_->return ())
>     objectInit focusedCellObject Nothing (syncFocusedCellWithLabel cellInfo)

>     forkIO $ handleGridEditWindowEvent exit Nothing gridEditWindowEvent

>     forkIO mainGUI

>     signal <- takeMVar exit
>     exitWith signal

>syncGridwithScrwin :: Window -> VBox -> ThreadObject ScrolledWindow -> ThreadObject Grid -> ThreadObject (Maybe DisplayCell.DisplayCell) -> Grid -> IO ()
>syncGridwithScrwin window scrwinContainer scrwinObject gridObject focusedCellObject mygrid = do
>   updateIO scrwinObject      (\scrwin      -> do{
>   scrwinMVar <- newEmptyMVar; 
>   postGUIAsync (do {
>    containerRemove scrwinContainer scrwin;
>    scrwin' <- scrolledWindowNew Nothing Nothing;
>    focusedWidget <- buildTable scrwin' gridObject focusedCellObject mygrid;
>    boxPackStart scrwinContainer scrwin' PackGrow 0;
>    widgetShowAll window;
>    widgetGrabFocus focusedWidget;
>    putMVar scrwinMVar scrwin';
>   });
>   scrwin' <- takeMVar scrwinMVar;
>   return scrwin';})

>syncFocusedCellWithLabel :: Label -> Maybe DisplayCell.DisplayCell -> IO()
>syncFocusedCellWithLabel cellInfo (Just dc) = do
>     set cellInfo [ labelText := (show (displayCellPoint dc))]
>     return ()

>syncFocusedCellWithLabel _ Nothing = do
>     return ()

>loadGrid :: IO Grid
>loadGrid = do
>     args <- getArgs 
>     (if null args then return emptyGrid else do {
>        gridString <- readFile (head args);
>        return (read gridString::Grid)})

>createWindow :: IO Window
>createWindow = do
>    window <- windowNew
>    set window [ windowTitle := "Grid editor", 
>                  windowDefaultWidth := 300, windowDefaultHeight := 250]
>    return window

>loadWidgets :: MVar GridEditWindowEvent -> ThreadObject Grid-> IO (Window, VBox, ScrolledWindow, VBox, HBox, Label) 
>loadWidgets gridEditWindowEvent gridObject = do
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

<     onClicked save (do {putMVar gridEditWindowEvent GridEditWindowSave})

>     onClicked quit (do {widgetDestroy window; putMVar gridEditWindowEvent GridEditWindowQuit})
>     onDestroy window (do {mainQuit; putMVar gridEditWindowEvent GridEditWindowQuit})
>     return (window,myTopVBox, scrwin, scrwinContainer, buttonBox, cellInfo)
