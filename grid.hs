import System.Environment
name = "gridhaskell"
file_name = name++".hs"

licence_code = "--GPL\n"
imports_code = "\n"
pure_code = "\n"
main_code = "main = " ++ (function_code 0 0) ++ "\n"

cells = 
	(Action (0,0) "getChar" False True False None 
	(Action (0,1) "'n'" True True False (SteppingStone (0,2)
										(SteppingStone (1,2)
										(SteppingStone (2,2)
										(SteppingStone (3,2)
										(PathDestination (3,1))))))
	(Action (1,1) "(==)" True True True None
	(If (2,1)
	(Destination (3,1) (0,1) None
	(Action (4,1) "putChar" False False True None End))
	(Jump (3,2) (SteppingStone (3,0)
				(SteppingStone (2,0)
				(SteppingStone (1,0)
				(PathDestination (0,0))))))))))

main = do writeFile file_name code

code = licence_code++imports_code++pure_code++main_code++(actions_code cells [] 0)

type Push = Bool
type Pull = Bool
type Then = Cell
type Else = Cell
type Origin = Point
type Return = Bool
data Cell = Action Point String Return Push Pull Path Cell
		  | Destination Point Origin Path Cell
		  | If Point Then Else
		  | Jump Point Path
		  | End
data Path = SteppingStone Point Path
		  	   | PathDestination Point
		  	   | None

type X = Int
type Y = Int
type Point = (X, Y)

point (Action (x,y) _ _ _ _ _ _) = (x, y)
point (If (x,y) _ _) = (x, y)

destination (SteppingStone _ path) = destination path
destination (PathDestination point) = point

values_of_scope (x_end,y_end) (Action (x_current, y_current) _ _ _ _ None next) values =
	if x_end == x_current && y_end == y_current then []
	else values_of_scope (x_end,y_end) next values
	
values_of_scope (x_end,y_end) (Action (x_current, y_current) _ _ _ _ _ next) values =
	if x_end == x_current && y_end == y_current then []
	else (x_current,y_current) : values_of_scope (x_end,y_end) next values

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
if_body (Destination mypoint _ _ _) values =
	if_body (Action mypoint undefined undefined undefined undefined undefined undefined) values
if_body (Jump mypoint _) values =
	if_body (Action mypoint undefined undefined undefined undefined undefined undefined) values
	

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

add_path values None _ _ =
	values
add_path values _ x y =
	(x,y):values

actions_code :: Cell -> [Point] -> Int -> [Char]

actions_code (Action (x, y) code preturn True False path next) values stack = 
	function_header x y values stack ++
	(body_code code preturn 0) ++
	bind x y (add_path values path x y) stack next ++
	function_tail next (add_path values path x y) (stack + 1)
	
actions_code (Action (x, y) code preturn False True path next) values stack =
	function_header x y values stack ++
	(body_code code preturn stack)++
	bind x y (add_path values path x y) 0 next ++
	function_tail next (add_path values path x y) 0

actions_code (Action (x, y) code preturn True True path next) values stack =
	function_header x y values stack ++
	(body_code code preturn stack)++
	bind x y (add_path values path x y) 0 next ++
	function_tail next (add_path values path x y) 1

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

actions_code (Destination mypoint (xo,yo) path next) values stack =
	actions_code (Action mypoint code False True False path next) values stack
	where code = "return "++(value_code xo yo)
	
actions_code (Jump (x,y) path) values stack =
	function_header x y values stack ++
	(function_code xd yd) ++
	(value_codes (values_of_scope (xd,yd) cells [])) ++
	(stack_code stack)
	where (xd,yd) = destination path

actions_code (End) _ _ = ""
