#import "MacOSX.h"
#import "Rank.h"
#import "Suit.h"

@interface CardRenderer : NSObject
{
    CGSize  cardSize;
    CGFloat cardCornerRadius;
    CGFloat cardLegendTextSize;
    CGFloat cardCourtFaceSize;
    CGFloat cardAceFaceSize;
    CGFloat cardPipFaceSize;
}

- (id)init;

- (void)drawCardSuit:(Suit)suit
                rank:(Rank)rank
              faceUp:(BOOL)faceUp
                  at:(NSPoint)where;

- (void)drawSpaceWithEmblem:(NSString *)emblem
                         at:(NSPoint)where;

@end
