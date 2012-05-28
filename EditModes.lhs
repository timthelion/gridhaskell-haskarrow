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

>module EditModes where

>import Graphics.UI.Gtk
>import qualified DisplayCell
>import qualified Cell
>import qualified Path

>instance Show Widget where
>    show _ = " *GTK Widget* "

>data EditMode =
> AddFork Cell.Cell    | 
> AddPattern Cell.Cell |
> EditPath Path.Path   |
> Connect Cell.Cell    |
> MoveCell DisplayCell.DisplayCell  |
> MoveCells DisplayCell.DisplayCell |
> EditCell DisplayCell.DisplayCell Widget |
> FreeMovement |

The Bool should be set to True.  It is used internally for showing the error.  We want to showing the error through 1 change in cell focus, but not more.  So the value True tells us that we have yet to change the cell focus :)

> ShowError String Bool
> deriving (Show)
