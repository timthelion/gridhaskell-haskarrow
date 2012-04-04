GPLv3 (c) Timothy Hobbs

>module GridPreCompile (GridPreCompile.code) where

>import Grid
>import qualified Scope
>import Super
>import qualified Cell
>import qualified Path

>code :: Grid -> String
>code grid =
>    (licenceCode        (gridLicence       grid )) ++
>    (importsCode        (gridImports       grid )) ++
>    (pureFunctionsCode  (gridPureFunctions grid )) ++
>    (cellsCode          (gridCells         grid ))

>licenceCode :: String -> String
>licenceCode licence = licence ++ "\n"

>importsCode :: [String] -> String
>importsCode imports = importsCode' imports

>importsCode' :: [String] -> String
>importsCode' (_import:imports) =
>    "import " ++ _import ++ "\n" ++ (importsCode' imports)
>importsCode' [] = ""

| This will have to change if we make Prototype into something other than a String, right now we just concatinate everything together...

>pureFunctionsCode :: [(Prototype, String)] -> String
>pureFunctionsCode ((prototype,code):functions) =
>       prototype ++ "\n" ++
>       code ++ "\n" ++
>       (pureFunctionsCode functions)
>pureFunctionsCode [] = ""

>cellsCode :: Cell.Cell -> String
>cellsCode grid = actionsCode grid grid Scope.TopScope 0

--------------------
Unique symbol names

| 'pointCode' returns strings that look like "0x0" "1x2" "xxy"...  If a value is negative, we use _ so as not to break Haskell syntax.

>pointCode :: String -> Point -> String
>pointCode name_type (x, y)
>	| x >= 0 && y <  0 =
>     name_type++(show x)++"x"++"_"++(show (abs y))++" "
>	| x <  0 && y >= 0 =
>     name_type++"_"++(show (abs x))++"x"++(show y)++" "
>	| x <  0 && y <  0 =
>     name_type++"_"++(show (abs x))++"x"++"_"++(show (abs y))++" "
>	| x >= 0 && y >= 0 =
>     name_type++(show x)++"x"++(show y)++" "

| functionCodes are the names of the Haskell functions to wich we precompile.  They look like "f0x0" "f1x3" "f3x75"  "f_3x2" If the function was displayed on the grid at point (-3,2).

>functionCode :: Point -> String 
>functionCode p = pointCode "f" p

>valueCode :: Point -> String
>valueCode p = pointCode "v" p

| 'valueCodes' evaluates to a string representing all values in Scope.  Aka "v0x0 v0x2 v0x3".  This includes the code for MVars as well.

>valueCodes :: Scope.Scope -> String

We write the values first.

>valueCodes (Scope.Scope (value:values) mvars higher) =
>    valueCode value ++
>    (valueCodes (Scope.Scope values mvars higher))

And then the MVars.

>valueCodes (Scope.Scope [] (mvar:mvars) higher) = 
>    " " ++ mvar ++ " " ++
>    (valueCodes (Scope.Scope [] mvars higher))

>valueCodes (Scope.Scope [] [] Scope.TopScope) = ""
>valueCodes (Scope.Scope [] [] higher) = valueCodes higher
>valueCodes Scope.TopScope = ""

| The precompiled stack looks like "p0 p1 p2..."

>stackCode :: Int -> String
>stackCode 0 = " "
>stackCode n = (stackCode (n-1)) ++ "p"++(show n)++ " "

--------------
Code snippets


The precompiled code has 3 parts.  The header, the body and the bind.
f1x1 v0x1  p1 p2  = return ((==) p1 p2 ) >>= \v1x1 ->f2x1 v0x1  v1x1 
\   header        /\  body              /\            bind         /

| The 'functionHeader' looks like "f7x1 v0x1  char  p1  =" first the values are listed, then the MVars, then the stack.

>functionHeaderHead :: Point -> Scope.Scope -> Int -> String
>functionHeaderHead p values stack =
>	(functionCode p) ++
>	(valueCodes values) ++
>	(stackCode stack)

>functionHeader :: Point -> Scope.Scope -> Int -> String
>functionHeader p values stack =
>	functionHeaderHead p values stack ++
>	" = " 

>bodyCode :: String -> Bool -> Int -> String
>bodyCode code preturn stack =
>	if preturn
>	then "return (" ++ code ++ 	(stackCode stack) ++ ")"
>	else code ++ (stackCode stack)

>bindCode :: Point -> Scope.Scope -> Int -> Cell.Cell -> String
>bindCode p values stack next =
>		" >>= \\" ++
>		(valueCode p) ++
>		"->" ++
>		(functionCode (Cell.point next)) ++
>		(valueCodes values) ++
>		(stackCode stack)

>functionNext :: Cell.Cell -> Cell.Cell -> Scope.Scope -> Int -> String
>functionNext next top values stack =
>		"\n" ++
>		(actionsCode next top values stack)

------------
Other Code Snippets

>mvarDeclarations :: [String] -> String
>mvarDeclarations mvars	= "do" ++ concatMap mvar_declaration mvars
>    where mvar_declaration mvar = " "++mvar++" <- newEmptyMVar ;"

This is the code for the arguments which are passed to our grid in the Start Cell.

>argumentCodes :: [(Point,String)] -> String
>argumentCodes arguments =
>    concat (map valueCode (Cell.argumentsPoints arguments))

| 'forksCode' is the segment of code which launches the child threads of a Fork.

>forksCode :: [Cell.Cell] -> Scope.Scope -> Int -> String 
>forksCode (child:children) values stack = 
>   "forkIO (" ++
>   (functionCode (Cell.point child)) ++
>   (valueCodes values) ++
>   (stackCode stack) ++ ");" ++
>   (forksCode children values stack)

>forksCode [] values stack = ""

----------------
actionsCode

>actionsCode :: Cell.Cell -> Cell.Cell -> Scope.Scope -> Int -> [Char]
>actionsCode start@(Cell.Start _ prototype name arguments next) top _ _ =
>    prototype ++ "\n" ++
>    name ++
>    (argumentCodes arguments) ++ " = " ++
>    "newEmptyMVar >>= " ++
>    (functionCode (Cell.point next)) ++
>    (functionNext next top (Scope.initialScope start) 0)

We have one syntax for when push the value but don't pull the stack.

>actionsCode (Cell.Action p code preturn True False path next) top values stack = 
>   functionHeader p values stack ++
>   (bodyCode code preturn 0) ++
>   bindCode p (Scope.addPath values path p) stack next ++
>   (valueCode p)  ++
>   (functionNext next top (Scope.addPath values path p) (stack + 1))

Another syntax for when we pull the stack but don't push a value.

>actionsCode (Cell.Action p code preturn False True path next) top values stack =
>   functionHeader p values stack  ++
>    (bodyCode code preturn stack) ++
>    bindCode p (Scope.addPath values path p) 0 next ++
>    functionNext next top (Scope.addPath values path p) 0

A third syntax for when we both push and pull.

>actionsCode (Cell.Action p code preturn True True path next) top values stack =
>	functionHeader p values stack ++
>	(bodyCode code preturn stack) ++
>	bindCode p (Scope.addPath values path p) 0 next ++
>	(valueCode p) ++
>	functionNext next top (Scope.addPath values path p) 1

And a forth syntax for when we simply preform an action without touching the stack.

>actionsCode (Cell.Action p code preturn False False path next) top values stack =
>   functionHeader p values stack ++
>   (bodyCode code preturn 0)     ++
>   bindCode p (Scope.addPath values path p) stack next ++
>   functionNext next top (Scope.addPath values path p) stack

>actionsCode (Cell.Switch point patterns) top values 1 =
>   concatMap patternCode patterns ++
>   (concatMap (\(Cell.Pattern _ _ next) -> functionNext next top values 0) patterns) 
>        where 


patternCode works by replacing the stack argument px with the pattern string.  This means that an item on the stack is consumed and applied to pattern matching.

>  patternCode :: Cell.Pattern -> String
>  patternCode (Cell.Pattern _ patternCode next) = 
>       (functionHeaderHead point values 0) ++
>       "(" ++ patternCode ++ ")" ++ " = " ++
>       (functionCode (Cell.point next)) ++
>		(valueCodes values) ++ "\n"


The call to higherScope relies on values not being TopScope.  But if we are joining, we are necesarilly not in the top scope.  We don't check for errors in the grid structure here.  grid_edit should never create incorrect grid code!

I'm not sure if we should carry the stack.  But it seems like it *might* be usefull.  I'll try to support it.

>actionsCode (Cell.Join point next) top values stack   =
>   actionsCode next top (Scope.higherScope values) stack

>actionsCode (Cell.Destination mypoint porigin path next) top values stack =
>   actionsCode (Cell.Action mypoint code True True False path next) top values stack
>   where code = valueCode porigin
	
>actionsCode (Cell.Jump p (Just path)) top values stack =
>	functionHeader p values stack ++
>	(functionCode (Path.destination path)) ++
>   (valueCodes (Scope.scopeAt (Path.destination path) top)) ++
>   (stackCode stack)

>actionsCode (Cell.Fork p myNewThreads) top values stack =
>   functionHeader p values stack ++

>   "do " ++ (forksCode (tail myNewThreads) values stack) ++

>   (functionCode (Cell.point (head myNewThreads))) ++ 
>   (valueCodes values) ++ 
>   (stackCode stack) ++
>   concatMap (\next->functionNext next top values stack) myNewThreads

The seccond value of NewEmptyMVar is the labelPoint.  The place where the lable is displayed has NO impact on the actual functioning of a grid.  It is esentially a comment.

>actionsCode (Cell.NewEmptyMVar p _ mvar next) top values stack =
>   functionHeader p values stack ++

>   "do " ++ mvar ++ " <- newEmptyMVar;" ++

>   (functionCode (Cell.point next)) ++ 
>   (valueCodes (Scope.mvarAdd values mvar)) ++ 
>   (stackCode stack) ++
>   functionNext next top (Scope.mvarAdd values mvar) stack

>actionsCode (Cell.PutMVar p _ mvar next) top values stack =
>  actionsCode (Cell.Action p code False False True Nothing next) top values stack
>    where code = "putMVar "++mvar

>actionsCode (Cell.TakeMVar p _ mvar next) top values stack =
>  actionsCode (Cell.Action p code False True False Nothing next) top values stack
>    where code = "takeMVar "++mvar

>actionsCode (Cell.End p) top values stack = 
>   functionHeader p values stack ++
>   "do signal <- takeMVar exit; putMVar exit signal; exitWith signal"

>actionsCode (Cell.Exit p signal) top values stack =
>   functionHeader p values stack ++
>   "putMVar exit " ++ signal ++ " >> exitWith " ++ signal
