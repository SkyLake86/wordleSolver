{-# LANGUAGE FlexibleInstances #-}

module M where

instance {-# OVERLAPPING #-} Show String where
    show x = ['"'] ++ x ++ ['"']