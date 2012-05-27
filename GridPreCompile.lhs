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
>import qualified CellMethods
>import qualified Path

>import Data.Maybe
>import Data.List
>import Data.Ord

>code :: Grid -> String
>code grid =
>    "{-This is a computer generated file.  Do not edit.  For the purposes of the GPL, this file counts as object code and NOT source code!  Releasing this file does not suffice to say you have released your source.  Editing this file is NOT the prefered method of editing grid haskell.\n" ++
>                        (message           grid )  ++ "\n" ++
>    (licenceCode        (gridLicence       grid )) ++ "-}\n" ++
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

>cellsCode :: Cell.Cell -> String
>cellsCode grid = actionsCode grid grid Scope.TopScope 0 False

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
>   | True = error "The impossible happened. OMG we're all going to die!"

| functionCodes are the names of the Haskell functions to wich we precompile.  They look like "f0x0" "f1x3" "f3x75"  "f_3x2" If the function was displayed on the grid at point (-3,2).

>functionCode :: Point -> String 
>functionCode p = pointCode "f" p

>valueCodesLHS :: Scope.Scope -> String

We write the values first.

>valueCodesLHS (Scope.Scope (myValue:myValues) patterns higher) =
>    (concatMap (\thisValue-> " "++thisValue++" ") (myValue:myValues)) ++
>    (valueCodesLHS (Scope.Scope [] patterns higher))

And then the patterns.

>valueCodesLHS (Scope.Scope [] (myPattern:myPatterns) higher) = 
>    (concatMap (\thisPattern-> " "++thisPattern++" ") (myPattern:myPatterns)) ++
>    (concatMap (\(_,n)-> " pattern"++(show n)++" ") $ zip (myPattern:myPatterns) ([1::Int ..])) ++
>    (valueCodesLHS (Scope.Scope [] [] higher))

>valueCodesLHS (Scope.Scope [] [] Scope.TopScope) = ""
>valueCodesLHS (Scope.Scope [] [] higher) = valueCodesLHS higher
>valueCodesLHS Scope.TopScope = ""

>valueCodesRHS :: Scope.Scope -> String

We write the values first.

>valueCodesRHS (Scope.Scope (myValue:myValues) patterns higher) =
>    (concatMap (\thisValue-> " "++thisValue++" ") (myValue:myValues)) ++
>    (valueCodesRHS (Scope.Scope [] patterns higher))

And then the patterns.

>valueCodesRHS (Scope.Scope [] (myPattern:myPatterns) higher) = 
>    (concatMap (\(_,n)-> " pattern"++(show n)++" ") $ zip (myPattern:myPatterns) [1 :: Int ..]) ++
>    (concatMap (\(_,n)-> " pattern"++(show n)++" ") $ zip (myPattern:myPatterns) [1 :: Int ..]) ++
>    (valueCodesRHS (Scope.Scope [] [] higher))

>valueCodesRHS (Scope.Scope [] [] Scope.TopScope) = ""
>valueCodesRHS (Scope.Scope [] [] higher) = valueCodesRHS higher
>valueCodesRHS Scope.TopScope = ""

| The precompiled stack looks like "p0 p1 p2..."

>stackCode :: Int -> String
>stackCode 0 = " "
>stackCode n
> | n < 0 = error "You've pulled more than you pushed, help!"
> | otherwise = (stackCode (n-1)) ++ (stackItemCode n)

>stackItemCode :: Int -> String
>stackItemCode n = "p"++(show n)++ " "

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
>bodyCode innerCode preturn stack =
>	if preturn
>	then "return (" ++ innerCode ++ 	(stackCode stack) ++ ")"
>	else innerCode ++ (stackCode stack)

>bindCode :: Scope.Scope -> Bool -> Bool -> [Int] -> Cell.Cell -> String
>bindCode values addValue push stack next =
>		" >>= \\" ++
>		"value_" ++
>		"->" ++
>		(functionCode (CellMethods.cellPoint next)) ++
>		(valueCodesRHS values) ++
>       (if addValue
>       then " value_ "
>       else "") ++
>       (if push
>       then " value_ "
>       else "") ++
>		(concatMap stackItemCode stack)


>functionNext :: Cell.Cell -> Cell.Cell -> Scope.Scope -> Int -> Bool -> String
>functionNext next top values stack pure =
>		"\n" ++
>		(actionsCode next top values stack pure)

------------
Other Code Snippets

This is the code for the arguments which are passed to our grid in the Start Cell.

>argumentCodes :: [(Rectangle,String)] -> String
>argumentCodes arguments =
>    concatMap snd arguments

| 'forksCode' is the segment of code which launches the child threads of a Fork.

>forksCode :: [Cell.Cell] -> Scope.Scope -> Int -> String 
>forksCode (child:children) values stack = 
>   "forkIO (" ++
>   (functionCode (CellMethods.cellPoint child)) ++
>   (valueCodesRHS values) ++
>   (stackCode stack) ++ ");" ++
>   (forksCode children values stack)

>forksCode [] _ _ = ""

----------------
actionsCode

>actionsCode :: Cell.Cell -> Cell.Cell -> Scope.Scope -> Int -> Bool -> String

>actionsCode start@(Cell.Start _ prototype name arguments pure next) top _ _ _ =
>    prototype ++ "\n" ++
>    name ++
>    (argumentCodes arguments) ++ " = " ++
>    "newEmptyMVar >>= " ++
>    (functionCode (CellMethods.cellPoint next)) ++
>    (functionNext next top (Scope.initialScope start) 0 pure)

----------------------------------------------------------------
Functions in the IO monad are handled differently from others.  The last argument, "pure", will be false.

>actionsCode (Cell.Action common actionCode preturn push pull label next) top values stack False =
>	functionHeader (CellMethods.commonPoint common) values stack ++
>	(bodyCode actionCode preturn pull) ++
>	bindCode (Scope.addValueFromLabel values label) (isJust label) push [pull+1..stack] next ++
>	functionNext next top (Scope.addValueFromLabel values label) (stack-pull+(if push then 1 else 0)) False

>actionsCode (Cell.Lambda common arguments _ _ pull pure body next) top values stack False =
>   functionHeader (CellMethods.commonPoint common) values stack ++
>   "do lambda_ <- (" ++
>   (bodyCode lambdaCode False pull) ++ ");" ++
>   (functionCode (CellMethods.cellPoint next)) ++
>   (valueCodesRHS values) ++
>   "lambda_\n" ++
>   (stackCode (stack-pull)) ++
>   (functionNext body top (Scope.valuesAddByName values (map snd arguments)) pull pure) ++
>   (functionNext next top values (stack - pull + 1) False)
>     where
>       lambdaCode = (functionCode (CellMethods.cellPoint body)) ++
>                    (valueCodesRHS values)

>actionsCode (Cell.Which common patterns) top values stack False =
> duplicationCode ++
> (concat $ patternCodes) ++
> (concat patternContinuations)

> where

>  patternCodes = map patternCode sortedPatterns
>  sortedPatterns = sortBy (comparing CellMethods.patternPoint) patterns

>  patternContinuations = 
>   map (\(Cell.Pattern patternLabel next) ->
>     functionNext next
>                  top
>                  (Scope.addPattern values ("("++(CellMethods.labelText patternLabel)++")"))
>                  (stack-1)
>                  False)
>              patterns

patternCode works by replacing the stack argument px with the pattern string.  This means that an item on the stack is consumed and applied to pattern matching.

>  duplicationCode :: String
>  duplicationCode =
>       (functionHeaderHead (CellMethods.commonPoint common) values 0) ++ "pattern_ = " ++
>       "w"++(functionCode (CellMethods.commonPoint common)) ++
>		(valueCodesRHS values) ++ "pattern_ pattern_ \n"

>  patternCode :: Cell.Pattern -> String
>  patternCode (Cell.Pattern patternLabel next) =
>       "w"++(functionHeaderHead (CellMethods.commonPoint common) (Scope.addPattern values ("("++(CellMethods.labelText patternLabel)++")")) 0) ++ "= " ++
>       (functionCode (CellMethods.cellPoint next)) ++
>		(valueCodesRHS (Scope.addPattern values ("("++(CellMethods.labelText patternLabel)++")"))) ++ "\n"

>actionsCode (Cell.Citation common value next) top values stack False =
>   actionsCode (Cell.Action common (CellMethods.labelText value) True True 0 Nothing next) top values stack False
	
>actionsCode (Cell.Jump common (Just path)) top values stack False =
>	functionHeader (CellMethods.commonPoint common) values stack ++
>	(functionCode (Path.destination path)) ++
>   (valueCodesRHS (Scope.scopeAt (Path.destination path) top)) ++
>   (stackCode stack)


Note, that the last argument here, the "pure" argument, MUST be false!  There is no such thing as forking outside of IO.

>actionsCode (Cell.Fork common myNewThreads) top values stack False =

>   functionHeader (CellMethods.commonPoint common) values stack ++

>   "do " ++ (forksCode (myNewThreads) values stack) ++

>   "do signal <- takeMVar exit; putMVar exit signal; exitWith signal" ++
>   (concat threadContinuations)
   
> where
>  threadContinuations = 
>   map
>    (\next->functionNext next 
>                         top
>                         values
>                         stack
>                         False)
>    myNewThreads

The seccond value of NewEmptyMVar is the labelPoint.  The place where the lable is displayed has NO impact on the actual functioning of a grid.  It is esentially a comment.

>actionsCode (Cell.NewEmptyMVar common mvarLabel next) top values stack False =
>   functionHeader (CellMethods.commonPoint common) values stack ++

>   "do " ++ (CellMethods.labelText mvarLabel) ++ " <- newEmptyMVar;" ++

>   (functionCode (CellMethods.cellPoint next)) ++ 
>   (valueCodesRHS (Scope.valueAddByName values (CellMethods.labelText mvarLabel))) ++ 
>   (stackCode stack) ++
>   functionNext next top (Scope.valueAddByName values (CellMethods.labelText mvarLabel)) stack False

>actionsCode (Cell.PutMVar common mvarLabel next) top values stack False = 

>  actionsCode (Cell.Action common actionCode False False 1 Nothing next) top values stack False

>    where actionCode = "putMVar "++(CellMethods.labelText mvarLabel)

>actionsCode (Cell.TakeMVar common mvarLabel next) top values stack False =
>  actionsCode (Cell.Action common actionCode False True 0 Nothing next) top values stack False
>    where actionCode = "takeMVar "++(CellMethods.labelText mvarLabel)

>actionsCode (Cell.Return common) _ values stack False =
>  functionHeader (CellMethods.commonPoint common) values stack ++
>  "return p" ++ (show stack)

>actionsCode (Cell.End common) _ values stack False =
>   functionHeader (CellMethods.commonPoint common) values stack ++
>   "return ()"

>actionsCode (Cell.Exit common) _ values stack False =
>   functionHeader (CellMethods.commonPoint common) values stack ++
>   "putMVar exit p1 >> exitWith p1"

>actionsCode cell top values stack pure = error $ "Incomplete pattern match processing cell:" ++ (show cell) ++ " with top cell of " ++ (show top) ++ "scope of" ++ (show values) ++ " and a stack of "++ (show stack) ++ " in code that was Pure/IO:" ++ (show pure)
