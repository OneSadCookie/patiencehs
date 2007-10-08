module Common.UIPatience (
    UIPatience (cardLocations),
    FillDirection (FillsRight, FillsDown, PilesUp),
    CellLayout (CellIgnored, Cell),
    GridPatience (grid),
    defaultCardLocations,
) where

import Common.Card
import Common.Patience
import Common.Pile

class Patience p => UIPatience p where
    cardLocations :: p -> Double -> Double -> [(FacingCard, Double, Double)]

data FillDirection = FillsRight | FillsDown | PilesUp

data CellLayout = CellIgnored | Cell PileName FillDirection

class (PilePatience p, UIPatience p) => GridPatience p where
    grid :: p -> [[CellLayout]]

defaultCardLocations :: GridPatience p =>
    p -> Double -> Double -> [(FacingCard, Double, Double)]
defaultCardLocations p width height =
    concatMap (uncurry cardLocationsForPile) (piles p) where
        cardLocationsForPile name pile =
            map (\(c, i) -> (c, i, i)) (zip pile [0..])
