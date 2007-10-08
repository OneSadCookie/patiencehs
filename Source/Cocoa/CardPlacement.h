#import "MacOSX.h"

#import "Rank.h"
#import "Suit.h"

@interface NSObject (CardPlacement)

- (void)placeCardSuit:(Suit)suit
                 rank:(Rank)rank
               faceUp:(BOOL)faceUp
                   at:(NSPoint)where;

@end
