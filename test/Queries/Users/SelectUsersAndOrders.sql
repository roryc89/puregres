SELECT email,
       order_id,
       item_id
FROM users
INNER JOIN orders
  ON users.user_id = orders.user_id
WHERE users.user_id = $2
  AND item_id = $1
