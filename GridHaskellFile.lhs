>module GridHaskellFile where
>import qualified Text.Show.Pretty as Pr
>import Grid

>openGrid :: String -> Grid
>openGrid text = read text::Grid

>saveGrid :: Grid -> String
>saveGrid grid = Pr.ppShow grid
