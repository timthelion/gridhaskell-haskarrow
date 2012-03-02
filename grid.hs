import System.Environment
name = "gridhaskell"
file_name = name++".hs"

licence_code = "--GPL\n"
imports_code = "\n"
pure_code = "\n"
main_code = "main = " ++ (function_code 0 0) ++ "\n"


type Push = Bool
type Pull = Bool
type Then = Cell
type Else = Cell
type Origin = Point
type Return = Bool
data Cell = Action Point String Return Push Pull ValuePath Cell
		  | Destination Origin Cell
		  | If Point Then Else
		  | Jump ActionPath
		  | End

cells = 
	(Action (0,0) "getChar" False True False None 
	(Action (1,0) "'n'" True True False None
	(Action (2,0) "(==)" True True True None
	(If (3,0)
	(Action (4,0) "'n'" True True False None 
	(Action (5,0) "putChar" False False True None End))
	(End)))))

point (Action (x,y) _ _ _ _ _ _) = (x, y)
point (If (x,y) _ _) = (x, y)


point_code name_type x y = name_type++(show x)++"x"++(show y)++" "
function_code x y = point_code "f" x y
value_code x y = point_code "v" x y
value_codes (value:values) = vc value ++ (value_codes values)
	where vc (x, y) = value_code x y
value_codes [] = ""
stack_code 0 = " "
stack_code n = (stack_code (n-1)) ++ "p"++(show n)++ " "

function_header x y values stack =
	(function_code x y) ++
	(value_codes values) ++
	(stack_code stack) ++
	" = " 

if_body End _ = "return ()"
if_body (Action (x, y) _ _ _ _ _ _) values =
	function_code x y ++
	(value_codes values)

body_code code preturn stack =
	if preturn
	then "return (" ++ code ++ 	(stack_code stack) ++ ")"
	else code ++ (stack_code stack)
	
bind x y values stack next = let fc (x, y) = function_code x y in
	if notEnd next
	then
		" >>= \\" ++
		(value_code x y) ++
		"->" ++
		(fc (point next)) ++
		(value_codes values) ++
		(stack_code stack) ++
		(value_code x y)
	else
		""
	where
		notEnd End = False
		notEnd _ = True
		
function_tail next values stack =
		"\n" ++
		(actions_code next values stack)

actions_code :: Cell -> [Point] -> Int -> [Char]

actions_code (Action (x, y) code preturn True False _ next) values stack = 
	function_header x y values stack ++
	(body_code code preturn 0) ++
	bind x y values stack next ++
	function_tail next values (stack + 1)
	
actions_code (Action (x, y) code preturn False True _ next) values stack =
	function_header x y values stack ++
	(body_code code preturn stack)++
	bind x y values 0 next ++
	function_tail next values 0

actions_code (Action (x, y) code preturn True True _ next) values stack =
	function_header x y values stack ++
	(body_code code preturn stack)++
	bind x y values 0 next ++
	function_tail next values 1

actions_code (If (x, y) a_then a_else) values 1 =
	function_header x y values 1 ++
	"if" ++
	(stack_code 1) ++
	" then " ++
	(if_body a_then values) ++
	" else " ++
	(if_body a_else values) ++
	function_tail a_then values 0 ++
	function_tail a_else values 0 
	
actions_code (End) _ _ = ""

destinations (ValueSteppingStone _ path) = destinations path
destinations (ValuePathFork _ path1 path2) = destinations path1 ++ destinations path2
destinations (ValuePathDestination point) = [point]
data ValuePath = ValueSteppingStone Point ValuePath
		  	   | ValuePathFork Point ValuePath ValuePath
		  	   | ValuePathDestination Point
		  	   | None

data ActionPath = ActionSteppingStone Point ActionPath
				| ActionPathDestination Point

type X = Int
type Y = Int
type Point = (X, Y)

main = do writeFile file_name code

code = licence_code++imports_code++pure_code++main_code++(actions_code cells [] 0)
