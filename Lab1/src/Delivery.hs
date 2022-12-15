{-# LANGUAGE OverloadedStrings #-}

module Delivery
  ( getDelivery,
    createDelivery,
    updateDelivery,
    deleteDelivery,
  )
where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

data Delivery = Delivery
  { id :: Int,
    order_id :: Int,
    address_id :: Int,
    delivery_price :: Int
  }
  deriving (Show)

instance FromRow Delivery where
  fromRow = Delivery <$> field <*> field <*> field <*> field

getDelivery :: Connection -> Int -> IO [Delivery]
getDelivery conn cid = query conn "SELECT * FROM deliveries WHERE delivery_id = ?" (Only cid)

createDelivery :: Connection -> Int -> Int -> Int -> IO [Delivery]
createDelivery conn order_id address_id delivery_price = do
  query conn "INSERT INTO deliveries (order_id, address_id, delivery_price) VALUES (?, ?, ?) RETURNING *" (order_id, address_id, delivery_price)

updateDelivery :: Connection -> Int -> Int -> Int -> Int -> IO [Delivery]
updateDelivery conn cid order_id address_id delivery_price = do
  query conn "UPDATE deliveries SET order_id = ?, address_id = ?, delivery_price = ? WHERE delivery_id = ? RETURNING *" (order_id, address_id, delivery_price, cid)

deleteDelivery :: Connection -> Int -> IO Bool
deleteDelivery conn cid = do
  n <- execute conn "DELETE FROM deliveries WHERE delivery_id = ?" (Only cid)
  return $ n > 0
