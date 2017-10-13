module DataMining.DataSource.Types where

import Control.Exception
import Data.Typeable
import Internal.Types

data DataSourceError backendError = BackendError backendError
                                  | GenericError String
                                  deriving (Eq, Show, Typeable)

instance (Exception b) => Exception (DataSourceError b)
