module Com.Example.Model.InternalServerError (
    InternalServerError
) where

import qualified Data.Maybe
import qualified Data.Text

data InternalServerError = InternalServerError {
    message :: Data.Maybe.Maybe Data.Text.Text
}

