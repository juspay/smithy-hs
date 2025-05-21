{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Model.SomeUnion (
    SomeUnion
) where
import qualified Com.Example.Model.CoffeeItem
import qualified Com.Example.Model.CoffeeType
import qualified Data.Aeson
import qualified Data.Text

-- Union implementation for SomeUnion
data SomeUnion =
    Label1 (Data.Text.Text)
    | Label2 (Com.Example.Model.CoffeeType.CoffeeType)
    | Label3 (Com.Example.Model.CoffeeItem.CoffeeItem)

instance Data.Aeson.ToJSON SomeUnion where
    toJSON (Label1 a) = Data.Aeson.object [ "stringType" Data.Aeson..= a ]
    toJSON (Label2 a) = Data.Aeson.object [ "Label2" Data.Aeson..= a ]
    toJSON (Label3 a) = Data.Aeson.object [ "Label3" Data.Aeson..= a ]


