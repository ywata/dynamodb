{-
get = do
  cfg <- Aws.baseConfiguration
  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.getItem (Just ["abcdefg"]) (Just True)
                                        ( Keys $ Map.fromList
                                         [("ForumName"::T.Text, ValueS "abcdef"), ("Index", ValueN 2)])  (Just True) "Txx"
  return rsp
 
put = do
  cfg <- Aws.baseConfiguration
  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.putItem
                               (Just . Expected . Map.fromList $ [])
                               (Item . Map.fromList $ [("ForumName", ValueS "abcdefg"), ("Index", ValueN 1)])
--                               (Item Map.empty)
                               (Just TOTAL)
                               (Just NONE_)
                               (Just ALL_OLD)
                               "Txx"
  return rsp
{-scan = do
  cfg <- Aws.baseConfiguration
  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.scan
                               ""
  return rsp
-}

{-  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.listTables -}
{-  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.describeTable ("NewTable")
-}

  return rsp

main = cre "a123"


-}
