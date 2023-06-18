module Choice (Choice) where

import ElmChar (ElmChar)

data Choice a
  = Choice [(ElmChar -> a -> a, a)]
