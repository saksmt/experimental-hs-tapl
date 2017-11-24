{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Util.Trace.Dumpable where

class Dumpable v where
    dumpToStrings :: v -> [String]

instance {-# OVERLAPPABLE #-} Show a => Dumpable a where
    dumpToStrings = lines . show

