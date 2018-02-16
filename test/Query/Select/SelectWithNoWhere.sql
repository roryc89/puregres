SELECT email,
       last_name,
       order_id,
       item_id
FROM users
INNER JOIN orders
  ON users.user_id = orders.user_id
