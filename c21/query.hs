import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3)

query :: Int -> IO ()
query maxId = do
    conn <- connectSqlite3 "test1.db"
    r <- quickQuery' conn 
        "SELECT id, desc from test where id <= ? ORDER BY id, desc"
        [toSql maxId]

    -- convert each row to a string
    let stringRows = map convRow r

    mapM_ print stringRows

    disconnect conn

    where
        convRow :: [SqlValue] -> String
        convRow [sqlId, sqlDesc] = 
            show iniId ++ ":" ++ desc
            where iniId = (fromSql sqlId) :: Int 
                  desc = case fromSql sqlDesc of
                    Nothing -> "NULL"
                    Just x -> x
        convRow x = fail $ "Unexpected result: " ++ show x
                


