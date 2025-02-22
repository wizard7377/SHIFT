module Extra.Tree where 

import Data.Tree 

data Choice a = 
    AllOf 
    | AnyOf
    | Leaf a  