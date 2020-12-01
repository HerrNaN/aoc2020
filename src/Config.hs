module Config (readConfig, val) where 

newtype Config = Config{val :: String}
    deriving (Show, Read)

readConfig :: String -> Config
readConfig = read