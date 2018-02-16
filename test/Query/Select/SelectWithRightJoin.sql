SELECT email,
       order_id,
       item_id
FROM users
RIGHT JOIN orders
  ON users.user_id = orders.user_id
WHERE orders.item_id = $1
