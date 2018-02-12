CREATE TABLE users (
  user_id SERIAL PRIMARY KEY,
  last_name VARCHAR(60) DEFAULT NULL,
  email TEXT NOT NULL,
  registered BOOLEAN NOT NULL DEFAULT TRUE
);

CREATE TABLE orders (
  order_id SERIAL PRIMARY KEY,
  user_id INTEGER REFERENCES users(user_id),
  item TEXT NOT NULL
);
