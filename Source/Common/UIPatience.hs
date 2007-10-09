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

rightFillSpacing = 27.0

horizontalGridSpacing = 150.0
verticalGridSpacing = 210.0

horizontalOffset = 80.0
verticalOffset = 110.0

cardLocationsForCell p y (x, (CellIgnored)) = []
cardLocationsForCell p y (x, (Cell name FillsRight)) =
    let pile = pileNamed p name
        revPile = reverse pile
    in map offsetRight (zip revPile [0..]) where
        offsetRight (c, i) = (c, horizontalOffset + x * horizontalGridSpacing + i * rightFillSpacing, y)
cardLocationsForCell p y (x, (Cell name PilesUp)) =
    let pile = pileNamed p name
        revPile = reverse pile
    in map offsetUp (zip revPile [0..]) where
        offsetUp (c, i) = (c, i + horizontalOffset + x * horizontalGridSpacing, i + y)
cardLocationsForCell p y _ = error "Unimplemented"

defaultCardLocations :: GridPatience p =>
    p -> Double -> Double -> [(FacingCard, Double, Double)]
defaultCardLocations p width height =
    let numberedRows = zip [0..] (grid p)
    in concatMap (cardLocationsForRow p) numberedRows where
        cardLocationsForRow p (y, row) =
            let numberedCells = zip [0..] row
            in concatMap (cardLocationsForCell p (verticalOffset + y * verticalGridSpacing)) numberedCells
