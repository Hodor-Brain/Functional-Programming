{-# LANGUAGE OverloadedStrings #-}

module OrderItem
  ( getOrderItem,
    createOrderItem,
    updateOrderItem,
    deleteOrderItem,
  )
where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

data OrderItem = OrderItem
  { id :: Int,
    meal :: String,
    quantity :: Int,
    order_id :: Int
  }
  deriving (Show)

instance FromRow OrderItem where
  fromRow = OrderItem <$> field <*> field <*> field <*> field

getOrderItem :: Connection -> Int -> IO [OrderItem]
getOrderItem conn cid = query conn "SELECT * FROM order_items WHERE order_item_id = ?" (Only cid)

createOrderItem :: Connection -> String -> Int -> Int -> IO [OrderItem]
createOrderItem conn meal quantity order_id = do
  query conn "INSERT INTO order_items (meal, quantity, order_id) VALUES (?, ?, ?) RETURNING *" (meal, quantity, order_id)

updateOrderItem :: Connection -> Int -> String -> String -> Int -> IO [OrderItem]
updateOrderItem conn cid meal quantity order_id = do
  query conn "UPDATE order_items SET meal = ?, quantity = ?, order_id = ? WHERE order_item_id = ? RETURNING *" (meal, quantity, order_id, cid)

deleteOrderItem :: Connection -> Int -> IO Bool
deleteOrderItem conn cid = do
  n <- execute conn "DELETE FROM order_items WHERE order_item_id = ?" (Only cid)
  return $ n > 0
