{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Lang where

instance {-# OVERLAPPING #-} Show String where
    show x =  x 
prin x = show x
