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

>module GridEditorObjects where

>import Graphics.UI.Gtk

>import ThreadObject
>import StateRecords
>import Grid
>import EditModes
>import qualified DisplayCell


>data GridEditorObjects = GridEditorObjects{

Our grid:

>  gridObject        :: ThreadObject Grid (RecorderSignal ()),

The grid is displayed in a scroll window.  There is also a canvas on which the grid is drawn.

>  canvasObject  :: ThreadObject ScrolledWindow (),

Which mode is the program currently in?

>  editModeObject    :: ThreadObject EditMode (),

Which display cell's widget is currently focused?

>  focusedCellObject :: ThreadObject (Maybe DisplayCell.DisplayCell) (),

>  focusedRectangleObject :: ThreadObject Rectangle (),

>  reFocusNeededObject    :: ThreadObject Bool (),

This is a special file saving object.  Put a string in it, and it will get saved to the file who's path is filePathObject.

> fileObject :: ThreadObject (Maybe String) (),

> filePathObject :: ThreadObject (Maybe FilePath) (),

> gridRecords :: StateRecords Grid (RecorderSignal ()),

> focusedCellRecords :: StateRecords (Maybe DisplayCell.DisplayCell) (),

> focusedRectangleRecords :: StateRecords Rectangle ()
>}
