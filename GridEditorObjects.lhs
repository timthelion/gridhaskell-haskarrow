>module GridEditorObjects where

>import Graphics.UI.Gtk

>import ThreadObject
>import Grid
>import EditModes
>import qualified DisplayCell


>data GridEditorObjects = GridEditorObjects{

Our grid:

>  gridObject        :: ThreadObject Grid,

The grid is displayed in a scroll window.  There is also a canvas on which the grid is drawn.

>  canvasObject  :: ThreadObject ScrolledWindow,

Which mode is the program currently in?

>  editModeObject    :: ThreadObject EditMode,

Which display cell's widget is currently focused?

>  focusedCellObject :: ThreadObject (Maybe DisplayCell.DisplayCell),

>  focusedRectangleObject :: ThreadObject Rectangle,

>  reFocusNeededObject    :: ThreadObject Bool,

This is a special file saving object.  Put a string in it, and it will get saved to the file who's path is filePathObject.

> fileObject :: ThreadObject (Maybe String),

> filePathObject :: ThreadObject FilePath

>}
