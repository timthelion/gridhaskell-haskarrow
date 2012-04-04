>module Scope where

>import Data.Maybe

>import Super
>import qualified Cell
>import Path

| Scopes are lists of the values and mvars being passed down from previous functions.  The current scope, contains not only it's values and mvars, but also all values and mvars contained by the higher scopes.

>data Scope = Scope {

| 'values' is a list of Points which represent their origin.  This is based on our internal and arbitrary naming structure for values.  In our intermediate Haskell code, we name our values v0x0 where 0x0 is the point of the Cell from which the value originated.

> values      :: [Point],
> mvars       :: [String],
> higherScope :: Scope}
>		   | TopScope
>    deriving (Show, Read)

| An 'initialScope' to be used at the Start of each program.

>initialScope :: Cell.Cell -> Scope
>initialScope cell = Scope {values      = cellValues cell,
>      mvars       = ["exit"],
>      higherScope = TopScope}


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

>   where
>           nextScopes = catMaybes 
>               (map (\cell -> scopeAt' end cell values) 
>                   (Cell.cellNext cell))


>cellValues :: Cell.Cell -> [Point]
>cellValues cell@Cell.Start{}  = Cell.argumentsPoints (Cell.arguments cell)

This is an awfully complicated way of saying "If there is a path comming out of this Cell this Cell's value should be preserved in scope, otherwise forget about it."

>cellValues cell@Cell.Action{} = 
>    if isJust (Cell.path cell)
>    then []
>    else [Cell.point cell]

>cellValues _ = []

| Return a new Scope including the new MVars.

>mvarsAdd :: Scope -> [String] -> Scope
>mvarsAdd (Scope values oldMVars higherScope) mvars =
>    (Scope values (oldMVars ++ mvars) higherScope) 

>mvarAdd :: Scope -> String -> Scope
>mvarAdd scope mvar =
>    mvarsAdd scope [mvar]


| Return a new Scope including the new values.

>valuesAdd :: Scope -> [Point] -> Scope
>valuesAdd (Scope value_list mvar_list higher) values = 
>        (Scope (values++value_list) mvar_list higher)

| Return a new Scope with the point added to values feild depending on whether the Path given was real or a NoPath.

>addPath :: Scope -> (Maybe Path) -> Point -> Scope
>addPath values Nothing _ = values
>addPath (Scope values mvars higher) _ p =
>	(Scope (p:values) mvars higher)
