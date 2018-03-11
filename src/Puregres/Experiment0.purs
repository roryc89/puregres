module Puregres.Experiment0 where

import Control.Apply (apply, lift2)
import Control.Monad.Free (resume)
import Data.Foreign (Foreign, F)
import Data.Foreign.Class (decode)
import Data.Foreign.Index ((!))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Maybe.First (First(..))
import Data.Monoid (class Monoid)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Prelude (class Functor, class Show, Unit, flip, map, show, unit, (#), ($), (<$>), (<*>), (<>), (>>=))
import Puregres.PuregresSqlValue (decode_)

data TABLE t next = TABLE t next

instance showTABLE :: (Show a, Show b) => Show (TABLE a b) where
  show (TABLE a b) = show a <> show b

data INNER_JOIN a = INNER_JOIN On a


instance showINNER_JOIN :: (Show a) => Show (INNER_JOIN a) where
  show (INNER_JOIN on a) = " INNER_JOIN" <> show on <> show a

data On = On String

instance showOn :: Show On where
  show (On s) = " On " <> s <> "\n"

on :: forall a b res. Column a res => Column b res => Show a => Show b => a -> b -> On
-- on :: forall a b res. Show a => Show b => a -> b -> On
on a b = On (show a <> "=" <> show b)

data LEFT_JOIN a = LEFT_JOIN a

instance showLEFT_JOIN :: (Show a) => Show (LEFT_JOIN a) where
  show (LEFT_JOIN a) = " LEFT_JOIN" <> show a

data Submissions = Submissions

instance showSubmissions :: Show Submissions where show _ = " Submissions"

data Users = Users

instance showUsers :: Show Users where show _ = " Users"

data Orders = Orders

instance showOrders :: Show Orders where show _ = " Orders"

data Items = Items

instance showItems :: Show Items where show _ = " Items"

data ORDER_ID = ORDER_ID

instance showORDER_ID :: Show ORDER_ID where show _ = " ORDER_ID"

instance columnORDER_ID :: Column ORDER_ID Int

instance colOfTABLE_ORDER_ID :: (ColOf ORDER_ID b Int) => ColOf ORDER_ID (TABLE a b) Int where
  from ORDER_ID (TABLE a b) = from ORDER_ID b

instance colOfOrdersORDER_ID :: ColOf ORDER_ID (TABLE Orders a) Int where
  from _ _ = \f -> f ! "order_id" >>= decode_

instance colOfINNER_JOIN_ORDER_ID :: (ColOf ORDER_ID a Int) => ColOf ORDER_ID (INNER_JOIN a) Int where
  from ORDER_ID (INNER_JOIN _ a) = from ORDER_ID a

data ORDER_ITEM_ID = ORDER_ITEM_ID

instance showORDER_ITEM_ID :: Show ORDER_ITEM_ID where show _ = " ORDER_ITEM_ID"

instance columnORDER_ITEM_ID :: Column ORDER_ITEM_ID Int

instance colOfTABLE_ORDER_ITEM_ID :: (ColOf ORDER_ITEM_ID b Int) => ColOf ORDER_ITEM_ID (TABLE a b) Int where
  from ORDER_ITEM_ID (TABLE a b) = from ORDER_ITEM_ID b

instance colOfOrdersORDER_ITEM_ID :: ColOf ORDER_ITEM_ID (TABLE Orders a) Int where
  from _ _ = \f -> f ! "item_id" >>= decode_

instance colOfINNER_JOIN_ORDER_ITEM_ID :: (ColOf ORDER_ITEM_ID a Int) => ColOf ORDER_ITEM_ID (INNER_JOIN a) Int where
  from ORDER_ITEM_ID (INNER_JOIN _ a) = from ORDER_ITEM_ID a

data ORDER_NAME = ORDER_NAME

instance showORDER_NAME :: Show ORDER_NAME where show _ = " ORDER_NAME"

instance columnORDER_NAME :: Column ORDER_NAME String

instance colOfTABLE_ORDER_NAME :: (ColOf ORDER_NAME b String) => ColOf ORDER_NAME (TABLE a b) String where
  from ORDER_NAME (TABLE a b) = from ORDER_NAME b

instance colOfOrdersORDER_NAME :: ColOf ORDER_NAME (TABLE Orders a) String where
  from _ _ = \f -> f ! "order_name" >>= decode_

instance colOfINNER_JOIN_ORDER_NAME :: (ColOf ORDER_NAME a String) => ColOf ORDER_NAME (INNER_JOIN a) String where
  from ORDER_NAME (INNER_JOIN _ a) = from ORDER_NAME a

data ITEM_ID = ITEM_ID

instance showITEM_ID :: Show ITEM_ID where show _ = " ITEM_ID"

instance columnITEM_ID :: Column ITEM_ID Int

instance colOfTABLE_ITEM_ID :: (ColOf ITEM_ID b Int) => ColOf ITEM_ID (TABLE a b) Int where
  from ITEM_ID (TABLE a b) = from ITEM_ID b

instance colOfItemsITEM_ID :: ColOf ITEM_ID (TABLE Items a) Int where
  from _ _ = \f -> f ! "item_id" >>= decode_

instance colOfINNER_JOIN_ITEM_ID :: (ColOf ITEM_ID a Int) => ColOf ITEM_ID (INNER_JOIN a) Int where
  from ITEM_ID (INNER_JOIN _ a) = from ITEM_ID a

class Column c res | c -> res

class (Column c res) <= ColOf c t res where
  from :: c -> t -> Foreign -> F res

data FROM tableExpr fn = FROM tableExpr fn

intoCol :: forall col table res1 res2. ColOf col table res1 =>
  FROM table (res1 -> res2)
  -> col
  -> FROM table (Foreign -> F res2)
intoCol (FROM tableExpr fn) col = FROM tableExpr (map (map fn) (from col tableExpr))
infixl 3 intoCol as >>>

anotherIntoCol :: forall col table res1 res2. ColOf col table res1 =>
  FROM table (Foreign -> F (res1 -> res2))
  -> col
  -> FROM table (Foreign -> F res2)
anotherIntoCol (FROM tableExpr fn) col = FROM tableExpr ((lift2 apply) fn (from col tableExpr))

infixl 2 anotherIntoCol as >>

-- ooo = on ORDER_ITEM_ID ITEM_ID
inner_join = flip INNER_JOIN

data SelectQuery a = SelectQuery String (Foreign -> F a)

select :: forall a b. Show a => FROM a (Foreign -> F b) -> SelectQuery b
select (FROM a b) = SelectQuery (show a) b

submissions = TABLE Submissions
items = TABLE Items
orders = TABLE Orders

from_ t = t unit
