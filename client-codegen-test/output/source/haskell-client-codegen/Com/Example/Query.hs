{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Query (

) where


module Com.Example.Query where

import qualified Data.Text as T

class ToQuery a where
    toQuery :: a -> T.Text

instance ToQuery T.Text where
    toQuery = id

instance ToQuery [T.Text] where
    toQuery xs = T.intercalate (T.pack ",") xs

