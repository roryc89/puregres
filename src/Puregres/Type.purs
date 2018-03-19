module Puregres.Type where

import Prelude

data TABLE t next = TABLE t next

instance showTABLE :: (Show a, Show b) => Show (TABLE a b) where
  show (TABLE a b) = " " <> show a <> show b


-- end

data EndQuery = EndQuery

end :: EndQuery
end = EndQuery

instance showEndQuery :: Show EndQuery where
  show _ = ""
