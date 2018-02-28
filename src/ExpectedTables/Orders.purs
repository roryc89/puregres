module ExpectedTables.Orders where

import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Puregres.Type (Column, Table(Table), makeColumn)

orders :: Table
orders = Table "public.orders"

order_id :: Column Int
order_id = makeColumn orders "order_id"

user_id :: Column Int
user_id = makeColumn orders "user_id"

item_id :: Column Int
item_id = makeColumn orders "item_id"

order_notes :: Column (NullOrUndefined String)
order_notes = makeColumn orders "order_notes"
