SELECT order_id,
       user_id
FROM orders
WHERE user_id =
    (SELECT user_id
     FROM users
     WHERE email = $1)
