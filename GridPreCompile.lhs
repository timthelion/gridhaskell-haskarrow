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

>module GridPreCompile (GridPreCompile.code) where

>import Grid
>import qualified Scope
>import Super
>import qualified Cell
>import qualified Path

>import Data.Maybe

>code :: Grid -> String
>code grid =
>    (licenceCode        (gridLicence       grid )) ++
>    (importsCode        (gridImports       grid )) ++
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

>valueCodesLHS :: Scope.Scope -> String

We write the values first.

>valueCodesLHS (Scope.Scope (value:values) patterns higher) =
>    (concatMap (\value-> " "++value++" ") (value:values)) ++
>    (valueCodesLHS (Scope.Scope [] patterns higher))

And then the patterns.

>valueCodesLHS (Scope.Scope [] (pattern:patterns) higher) = 
>    (concatMap (\pattern-> " "++pattern++" ") (pattern:patterns)) ++
>    (concatMap (\(_,n)-> " pattern"++(show n)++" ") $ zip (pattern:patterns) [1..]) ++
>    (valueCodesLHS (Scope.Scope [] [] higher))

>valueCodesLHS (Scope.Scope [] [] Scope.TopScope) = ""
>valueCodesLHS (Scope.Scope [] [] higher) = valueCodesLHS higher
>valueCodesLHS Scope.TopScope = ""

>valueCodesRHS :: Scope.Scope -> String

We write the values first.

>valueCodesRHS (Scope.Scope (value:values) patterns higher) =
>    (concatMap (\value-> " "++value++" ") (value:values)) ++
>    (valueCodesRHS (Scope.Scope [] patterns higher))

And then the patterns.

>valueCodesRHS (Scope.Scope [] (pattern:patterns) higher) = 
>    (concatMap (\(_,n)-> " pattern"++(show n)++" ") $ zip (pattern:patterns) [1..]) ++
>    (concatMap (\(_,n)-> " pattern"++(show n)++" ") $ zip (pattern:patterns) [1..]) ++
>    (valueCodesRHS (Scope.Scope [] [] higher))

>valueCodesRHS (Scope.Scope [] [] Scope.TopScope) = ""
>valueCodesRHS (Scope.Scope [] [] higher) = valueCodesRHS higher
>valueCodesRHS Scope.TopScope = ""

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
>	(valueCodesLHS values) ++
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

>bindCode :: Scope.Scope -> Bool -> Int -> Cell.Cell -> String
>bindCode values addValue stack next =
>		" >>= \\" ++
>		"value_" ++
>		"->" ++
>		(functionCode (Cell.point next)) ++
>		(valueCodesRHS values) ++
>       if addValue
>       then " value_ "
>       else ""
>		++ (stackCode stack)

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
>    concatMap snd arguments

| 'forksCode' is the segment of code which launches the child threads of a Fork.

>forksCode :: [Cell.Cell] -> Scope.Scope -> Int -> String 
>forksCode (child:children) values stack = 
>   "forkIO (" ++
>   (functionCode (Cell.point child)) ++
>   (valueCodesRHS values) ++
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

>actionsCode (Cell.Action p code preturn True False label path next) top values stack = 
>   functionHeader p values stack ++
>   (bodyCode code preturn 0) ++
>   bindCode (Scope.addValueFromLabel values label) (isJust label) stack next ++
>   "value_ " ++
>   (functionNext next top (Scope.addValueFromLabel values label) (stack + 1))

Another syntax for when we pull the stack but don't push a value.

>actionsCode (Cell.Action p code preturn False True label path next) top values stack =
>   functionHeader p values stack  ++
>    (bodyCode code preturn stack) ++
>    bindCode (Scope.addValueFromLabel values label) (isJust label) 0 next ++
>    functionNext next top (Scope.addValueFromLabel values label) 0

A third syntax for when we both push and pull.

>actionsCode (Cell.Action p code preturn True True label path next) top values stack =
>	functionHeader p values stack ++
>	(bodyCode code preturn stack) ++
>	bindCode (Scope.addValueFromLabel values label) (isJust label) 0 next ++
>   "value_ "++
>	functionNext next top (Scope.addValueFromLabel values label) 1

And a forth syntax for when we simply preform an action without touching the stack.

>actionsCode (Cell.Action p code preturn False False label path next) top values stack =
>   functionHeader p values stack ++
>   (bodyCode code preturn 0)     ++
>   bindCode (Scope.addValueFromLabel values label) (isJust label) stack next ++
>   functionNext next top (Scope.addValueFromLabel values label) stack

>actionsCode (Cell.Lambda point arguments arrow body next) top values stack =
>   functionHeader point values stack ++
>   "do lambda_ <- (" ++
>   (bodyCode lambdaCode False 0) ++
>   (valueCodesRHS values) ++ ");" ++
>   (functionCode (Cell.point next)) ++
>   (valueCodesRHS values) ++
>   (stackCode stack) ++
>   "lambda_\n" ++
>   (functionNext body top (Scope.valuesAddByName values (map snd arguments)) 0) ++
>   (functionNext next top values (stack + 1))
>     where
>       lambdaCode = (functionCode (Cell.point body))

>actionsCode (Cell.Which point patterns) top values 1 =
>   duplicationCode ++
>   concatMap patternCode patterns ++
>   (concatMap (\(Cell.Pattern _ patternCode next) -> functionNext next top (Scope.addPattern values ("("++patternCode++")")) 0) patterns) 
>        where

patternCode works by replacing the stack argument px with the pattern string.  This means that an item on the stack is consumed and applied to pattern matching.

>  duplicationCode :: String
>  duplicationCode =
>       (functionHeaderHead point values 0) ++ "pattern_ = " ++
>       "w"++(functionCode point) ++
>		(valueCodesRHS values) ++ "pattern_ pattern_ \n"

>  patternCode :: Cell.Pattern -> String
>  patternCode (Cell.Pattern _ patternCode next) =
>       "w"++(functionHeaderHead point (Scope.addPattern values ("("++patternCode++")")) 0) ++ "= " ++
>       (functionCode (Cell.point next)) ++
>		(valueCodesRHS (Scope.addPattern values ("("++patternCode++")"))) ++ "\n"

>actionsCode (Cell.Destination mypoint porigin value path next) top values stack =
>   actionsCode (Cell.Action mypoint value True True False (Just (mypoint, value)) path next) top values stack
	
>actionsCode (Cell.Jump p (Just path)) top values stack =
>	functionHeader p values stack ++
>	(functionCode (Path.destination path)) ++
>   (valueCodesRHS (Scope.scopeAt (Path.destination path) top)) ++
>   (stackCode stack)

>actionsCode (Cell.Fork p myNewThreads) top values stack =
>   functionHeader p values stack ++

>   "do " ++ (forksCode (myNewThreads) values stack) ++

>   "do signal <- takeMVar exit; putMVar exit signal; exitWith signal" ++
>   concatMap (\next->functionNext next top values stack) myNewThreads

The seccond value of NewEmptyMVar is the labelPoint.  The place where the lable is displayed has NO impact on the actual functioning of a grid.  It is esentially a comment.

>actionsCode (Cell.NewEmptyMVar p _ mvar next) top values stack =
>   functionHeader p values stack ++

>   "do " ++ mvar ++ " <- newEmptyMVar;" ++

>   (functionCode (Cell.point next)) ++ 
>   (valueCodesRHS (Scope.valueAddByName values mvar)) ++ 
>   (stackCode stack) ++
>   functionNext next top (Scope.valueAddByName values mvar) stack

>actionsCode (Cell.PutMVar p _ mvar next) top values stack =
>  actionsCode (Cell.Action p code False False True Nothing Nothing next) top values stack
>    where code = "putMVar "++mvar

>actionsCode (Cell.TakeMVar p _ mvar next) top values stack =
>  actionsCode (Cell.Action p code False True False Nothing Nothing next) top values stack
>    where code = "takeMVar "++mvar

>actionsCode (Cell.Return point) top values stack =
>  functionHeader point values stack ++
>  "return p" ++ (show stack)

>actionsCode (Cell.End p) top values stack =
>   functionHeader p values stack ++
>   "return ()"

>actionsCode (Cell.Exit p signal) top values stack =
>   functionHeader p values stack ++
>   "putMVar exit " ++ signal ++ " >> exitWith " ++ signal

>actionsCode cell top values stack = error $ "Incomplete pattern match processing cell:" ++ (show cell) ++ " with top cell of " ++ (show top) ++ "scope of" ++ (show values) ++ " and a stack of "++ (show stack)
