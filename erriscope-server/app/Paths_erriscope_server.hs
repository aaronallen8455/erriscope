module Paths_erriscope_server where

getDataFileName :: FilePath -> IO FilePath
getDataFileName = pure . ("erriscope-server/" <>)
