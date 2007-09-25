module Helpers (
    on
) where

-- in GHC 6.8, but not 6.6 apparently
on g f x y = f x `g` f y
