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

>module Scope where

>import Data.Maybe

>import Super
>import CellMethods
>import qualified Cell

| Scopes are lists of the values and mvars being passed down from previous functions.  The current scope, contains not only it's values and mvars, but also all values and mvars contained by the higher scopes.

>data Scope = Scope {

> values       :: [String],
> patterns     :: [String],
> higherScope  :: Scope}
>		   | TopScope
>    deriving (Show, Read)

| An 'initialScope' to be used at the Start of each program.

>initialScope :: Cell.Cell -> Scope
>initialScope cell    = Scope {
>      values         = "exit":(cellValues cell),
>      patterns       = [],
>      higherScope    = TopScope}


| 'scopeAt' returns the socope at the 'Point' given the first 'Cell' in the grid.

>scopeAt :: Point -> Cell.Cell -> Scope
>scopeAt end start = case scopeAt' end start TopScope of
>                       Just s  -> s
>                       Nothing -> TopScope -- Should never happen.

>scopeAt' :: Point -> Cell.Cell -> Scope -> Maybe Scope

scopeAt reads down a tree of cells, building it's scope in the same way that scope is built in the actionCode function of PrecompileGrid.lhs  .  This is, if we are at the beginning, we have TopScope.  As we go down, action by action, the values in our scope increase.  Switches's and Forks make new scopes.  

>scopeAt' end cell@Cell.Start{} _ =
>	if end == (cellPoint cell) then Just (initialScope cell)
>   else scopeAt'' end cell (initialScope cell)

>scopeAt' end cell theseValues =
>	if end == (cellPoint cell) then Just theseValues
>   else scopeAt'' end cell theseValues

>scopeAt'' :: Point -> Cell.Cell -> Scope -> Maybe Scope
>scopeAt'' end cell theseValues =
>	if null nextScopes
>   then Nothing
>   else Just (head nextScopes)
>   where
>           nextScopes = catMaybes 
>               (map (\nextCell -> scopeAt' end nextCell theseValues) 
>                   (cellsNext cell))


>cellValues :: Cell.Cell -> [String]
>cellValues cell@Cell.Start{}  = map snd (Cell.arguments cell)

This is an awfully complicated way of saying "If there is a lable attached to this Cell this Cell's value should be preserved in scope, otherwise forget about it."

>cellValues cell@Cell.Action{} = 
>    case (Cell.label cell) of
>        Just (_,label) -> [label]
>        Nothing -> []

>cellValues _ = []

| Return a new Scope including the new MVars.

>valuesAddByName :: Scope -> [String] -> Scope
>valuesAddByName (Scope myValues myPatterns myHigherScope) valuesToAdd =
>    (Scope (myValues ++ valuesToAdd) myPatterns myHigherScope)
 
>valuesAddByName TopScope _ = error "Cannot add a value to the TopScope."

>valueAddByName :: Scope -> String -> Scope
>valueAddByName scope value =
>    valuesAddByName scope [value]

>addValueFromLabel :: Scope -> Maybe Cell.Label -> Scope
>addValueFromLabel scope (Just (_,label)) = scope{values=label:(values scope)}
>addValueFromLabel scope Nothing = scope

>addPattern :: Scope -> String -> Scope
>addPattern scope pattern = scope{patterns=pattern:(patterns scope)}
