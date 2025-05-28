{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Model.SomeUnion (
    SomeUnion
) where
import qualified Com.Example.Model.CoffeeItem
import qualified Com.Example.Model.CoffeeType
import qualified Control.Applicative
import qualified Data.Aeson
import qualified Data.Functor
import qualified Data.Text

-- Union implementation for SomeUnion
data SomeUnion =
    Label1 (Data.Text.Text)
    | 
    Label2 (Com.Example.Model.CoffeeType.CoffeeType)
    | 
    Label3 (Com.Example.Model.CoffeeItem.CoffeeItem)

instance Data.Aeson.ToJSON SomeUnion where
    toJSON (Label1 a) = Data.Aeson.object [ "stringType" Data.Aeson..= a ]
    toJSON (Label2 a) = Data.Aeson.object [ "label2" Data.Aeson..= a ]
    toJSON (Label3 a) = Data.Aeson.object [ "label3" Data.Aeson..= a ]

instance Data.Aeson.FromJSON SomeUnion where
    parseJSON = Data.Aeson.withObject "SomeUnion" $ \v ->
        (Label1 Data.Functor.<$> v Data.Aeson..: "stringType") Control.Applicative.<|>
        (Label2 Data.Functor.<$> v Data.Aeson..: "label2") Control.Applicative.<|>
        (Label3 Data.Functor.<$> v Data.Aeson..: "label3") Control.Applicative.<|>
        fail "Could not parse SomeUnion. Expected an object with one of keys: label1, label2, label3."
    


