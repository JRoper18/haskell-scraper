{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use join" #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}


module Lib where


-- deriving instance Generic ( LHsExpr GhcTc )

-- instance ToJSON ( LHsExpr GhcTc ) where
--     -- No need to provide a toJSON implementation.

--     -- For efficiency, we write a simple toEncoding implementation, as
--     -- the default version uses toJSON.

-- instance FromJSON ( LHsExpr GhcTc )
