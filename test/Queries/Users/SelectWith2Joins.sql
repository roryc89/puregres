SELECT users.user_id,
       email,
       registered,
       order_id
FROM items
INNER JOIN orders ON items.item_id = orders.item_id
INNER JOIN users ON orders.user_id = users.user_id
WHERE items.item_id = $1
