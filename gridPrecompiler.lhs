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

This code generates more code which then compiles to a usefull executable.

>import System.Environment
>import System.Console.CmdArgs.Implicit

>import GridHaskellFile
>import GridPrecompileToFile

>import GridExample
>import Super

>data GridPrecompilerOptions = GridPrecompilerOptions {
>   inputFile         :: FilePath,
>   outputFile        :: FilePath,
>   createExampleGrid :: Bool,
>   version           :: Bool   
>   } deriving (Data, Typeable)

>gridPrecompiler = GridPrecompilerOptions{
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
>          then preCompileToFile (inputFile args) (outputFile args)
>          else(

If one file is given but not the other print an error...

>               if not ((null (inputFile args)) && (null (outputFile args)))
>               then print "Both an input AND output file must be specified to precompile." else return ())


>          if Main.version args then print versionString else return()

>          if createExampleGrid args then
>            writeFile "examplegrid.grid-haskell" (saveGrid grid)
>          else return()

>versionString = "Version: " ++ (show Super.version)
