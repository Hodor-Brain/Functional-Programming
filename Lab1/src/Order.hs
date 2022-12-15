{-# LANGUAGE OverloadedStrings #-}

module Order
  ( getOrder,
    createOrder,
    updateOrder,
    deleteOrder,
  )
where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

data Order = Order
  { id :: Int,
    customer :: String,
    total_price :: Int,
    status_id :: Int
  }
  deriving (Show)

instance FromRow Order where
  fromRow = Order <$> field <*> field <*> field <*> field

getOrder :: Connection -> Int -> IO [Order]
getOrder conn cid = query conn "SELECT * FROM orders WHERE order_id = ?" (Only cid)

createOrder :: Connection -> String -> Int -> Int -> IO [Order]
createOrder conn customer total_price status_id = do
  query conn "INSERT INTO orders (customer, total_price, status_id) VALUES (?, ?, ?) RETURNING *" (customer, total_price, status_id)

updateOrder :: Connection -> Int -> String -> Int -> Int -> IO [Order]
updateOrder conn cid customer total_price status_id = do
  query conn "UPDATE orders SET customer = ? total_price = ? status_id = ? WHERE order_id = ? RETURNING *" (customer, total_price, status_id, cid)

deleteOrder :: Connection -> Int -> IO Bool
deleteOrder conn cid = do
  n <- execute conn "DELETE FROM orders WHERE order_id = ?" (Only cid)
  return $ n > 0
