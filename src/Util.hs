{-|
Module      : $Header$
Description : Utils for the lesson builder.
Copyright   : (c) Justus Adam 2017.
License     : MIT
Maintainer  : dev@justus.science
Stability   : experimental
Portability : portable
-}
module Util where

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left err) = Left $ f err
mapLeft _ (Right v)  = Right v
