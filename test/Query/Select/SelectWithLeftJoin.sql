SELECT email,
       order_id,
       item_id
FROM users
LEFT JOIN orders
  ON users.user_id = orders.user_id
WHERE users.user_id = $1
