{-# LANGUAGE OverloadedStrings #-}

module Status
  ( getStatus,
    createStatus,
    updateStatus,
    deleteStatus,
  )
where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

data Status = Status
  { id :: Int,
    status_name :: Int
  }
  deriving (Show)

instance FromRow Status where
  fromRow = Status <$> field <*> field

getStatus :: Connection -> Int -> IO [Status]
getStatus conn cid = query conn "SELECT * FROM statuses WHERE status_id = ?" (Only cid)

createStatus :: Connection -> Int -> IO [Status]
createStatus conn status_name = query conn "INSERT INTO statuses (status_name) VALUES (?) RETURNING *" (Only status_name)

updateStatus :: Connection -> Int -> Int -> IO [Status]
updateStatus conn cid status_name = do
  query conn "UPDATE statuses SET status_name = ? WHERE status_id = ? RETURNING *" (status_name, cid)

deleteStatus :: Connection -> Int -> IO Bool
deleteStatus conn cid = do
  n <- execute conn "DELETE FROM statuses WHERE status_id = ?" (Only cid)
  return $ n > 0
