>module EditModes where

>import qualified DisplayCell
>import qualified Cell
>import qualified Path

>data EditMode = AddAction Cell.Cell | AddPattern Cell.Cell | EditPath Path.Path | MoveCell DisplayCell.DisplayCell | EditCell DisplayCell.DisplayCell | FreeMovement
