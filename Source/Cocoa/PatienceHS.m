#import <Cocoa/Cocoa.h>

#import "CardPlacement.h"
#import "PatienceHS.h"

void *gState = NULL;

void PatienceStart(void *state)
{
    gState = state;
    NSApplicationMain(0, NULL);
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
