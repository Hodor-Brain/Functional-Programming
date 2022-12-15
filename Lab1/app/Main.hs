module Main (main) where

import Order
import Address
import Status
import OrderItem
import Delivery
import Database.PostgreSQL.Simple

localPG :: ConnectInfo
localPG = defaultConnectInfo
        { connectHost = "localhost"
        , connectDatabase = "Order"
        , connectUser = "postgres"
        , connectPassword = "14062002"
        }

main :: IO ()
main = do
  conn <- connect localPG
  putStrLn "Street of your address? "
  addressStreet <- getLine
  putStrLn "City of your address? "
  addressCity <- getLine
  newAddress <- createAddress conn addressStreet addressCity
  putStrLn $ "New Address: " ++ show newAddress
  firstAddress <- getAddress conn 1
  putStrLn $ "First Address: " ++ show firstAddress
  isDeleted <- deleteAddress conn 1
  putStrLn $ "First Address deleted: " ++ show isDeleted
  firstAddress <- getAddress conn 1
  putStrLn $ "First Address: " ++ show firstAddress
  secondAddress <- getAddress conn 2
  putStrLn $ "Second Address: " ++ show secondAddress
  updatedSecond <- updateAddress conn 2 "Another Street" "Another City"
  putStrLn $ "Updated Address: " ++ show updatedSecond
  secondAddress <- getAddress conn 2
  putStrLn $ "Second Address: " ++ show secondAddress
