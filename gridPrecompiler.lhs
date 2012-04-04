This code generates more code which then compiles to a usefull executable.
The contents of this file are released under the GPLv3 licence by Timothy Hobbs,

>import System.Environment
>import System.Console.CmdArgs

>import Grid
>import GridPreCompile
>import GridExample
>import Super

>data GridPrecompiler = GridPrecompiler {
>   inputFile         :: FilePath,
>   outputFile        :: FilePath,
>   createExampleGrid :: Bool,
>   version           :: Bool   
>   } deriving (Data, Typeable)

>gridPrecompiler = GridPrecompiler{
>   inputFile  =
>    def &= typFile &= name "i" &= help "The grid file to be pre-compiled.",

>   outputFile =
>    def &= typFile &= name "o" &= help "The outputted .hs file.",

>   createExampleGrid =
>    def &= name "example" &= help "Output an example grid haskell grid into examplegrid.grid-haskell",

>   Main.version =
>    def &= name "v" &= help "Print out grid haskell's version."

>   } &= help "Precompile a grid haskell file to .hs"

>main :: IO()
>main = do 
>          args  <- cmdArgs gridPrecompiler

If two files are provided we precompile the one into the other.

>          if not ((null (inputFile args)) || (null (outputFile args)))
>          then preCompile (inputFile args) (outputFile args)
>          else(

If one file is given but not the other print an error...

>               if not ((null (inputFile args)) && (null (outputFile args)))
>               then print "Both an input AND output file must be specified to precompile." else return ())


>          if Main.version args then print versionString else return()

>          if createExampleGrid args then
>            writeFile "examplegrid.grid-haskell" (show grid)
>          else return()

>preCompile :: String -> String -> IO ()
>preCompile input output = do
>            gridString <- readFile input

<            print "Reading the grid."

>            mygrid     <- return (read gridString::Grid)

<            print "Precompiling to haskell."

>            writeFile output (code mygrid)


>versionString = "Version: " ++ (show Super.version)
