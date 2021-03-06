#import <Cocoa/Cocoa.h>

#import "CardPlacement.h"
#import "PatienceHS.h"

HsStablePtr gState = NULL;

void PatienceStart(HsStablePtr state)
{
    gState = state;
    char const *argv = "PatienceHS";
    NSApplicationMain(1, &argv);
}

void PlaceCard(
    void   *userData,
    int     faceUp,
    int     suit,
    int     rank,
    double  x,
    double  y)
{
    [(id)userData placeCardSuit:(Suit)suit
                           rank:(Rank)rank
                         faceUp:(BOOL)faceUp
                             at:NSMakePoint((CGFloat)x, (CGFloat)y)];
}
