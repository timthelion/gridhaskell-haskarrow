>module Scope where

>import Data.Maybe

>import Super
>import qualified Cell
>import Path

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

>scopeAt' end cell@Cell.Start{} values =
>	if end == (Cell.point cell) then Just (initialScope cell)
>   else scopeAt'' end cell (initialScope cell)

>scopeAt' end cell values =
>	if end == (Cell.point cell) then Just values
>   else scopeAt'' end cell values

>scopeAt'' :: Point -> Cell.Cell -> Scope -> Maybe Scope
>scopeAt'' end cell values =
>	if null nextScopes
>   then Nothing
>   else Just (head nextScopes)

|'nextScopes' will either be only one scope long, or it will contain multiple identical scopes.  This can happen in the following case:

ScopeAt (0,7) (Switch (0,2)...) values...

                                 (0,0) Start
                                 (0,1) getChar
                                 (0,2) Switch
                                 /          \
Patterns from switch ->   (-1,4)'y'       (1,4)'n'
                          (-1,5) ...      (1,5) ...
                                 \         /     
                                 (0,7) Join
                                 
After the Join, we go back to the higherScope which we had before the Switch.  But both Scopes, the one that we got going down the 'y' branch, and the one we got going down the 'n' branch are valid and identical.  

NOTE:  I no longer support Join.  Lambda makes it's existence irrelivant.

>   where
>           nextScopes = catMaybes 
>               (map (\cell -> scopeAt' end cell values) 
>                   (Cell.cellNext cell))


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
>valuesAddByName (Scope values patterns higherScope) valuesToAdd =
>    (Scope (values ++ valuesToAdd) patterns higherScope) 

>valueAddByName :: Scope -> String -> Scope
>valueAddByName scope value =
>    valuesAddByName scope [value]

>addValueFromLabel :: Scope -> Maybe (Super.Point, String) -> Scope
>addValueFromLabel scope (Just (_,label)) = scope{values=label:(values scope)}
>addValueFromLabel scope Nothing = scope

>addPattern :: Scope -> String -> Scope
>addPattern scope pattern = scope{patterns=pattern:(patterns scope)}
