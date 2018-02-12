SELECT user_id,
       users.email,
       public.users.last_name
FROM users
WHERE users.user_id = $1
  AND registered = TRUE
