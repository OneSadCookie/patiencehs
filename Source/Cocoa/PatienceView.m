#import "PatienceView.h"
#import "Rank.h"
#import "Suit.h"

@implementation PatienceView

- (id)initWithFrame:(NSRect)frameRect
{
    self = [super initWithFrame:frameRect];
    if (self == nil)
    {
        return nil;
    }
    
    cardSize = CGSizeMake(140.0, 200.0);
    cardCornerRadius = 10.0;
    
    return self;
}

- (void)drawCardFrameInContext:(CGContextRef)context center:(CGPoint)center
{
    CGFloat r = cardCornerRadius;
    
    CGFloat halfWidth = 0.5 * cardSize.width - r;
    CGFloat halfHeight = 0.5 * cardSize.height - r;
    
    CGFloat x0 = center.x - halfWidth;
    CGFloat x1 = center.x + halfWidth;
    CGFloat y0 = center.y - halfHeight;
    CGFloat y1 = center.y + halfHeight;
    
    CGFloat a0 = 0.0;
    CGFloat a1 = 0.5 * M_PI;
    CGFloat a2 = 2.0 * a1;
    CGFloat a3 = 3.0 * a1;
    
    CGContextSetRGBFillColor(context, 1.0, 1.0, 1.0, 1.0);
    CGContextSetRGBStrokeColor(context, 0.0, 0.0, 0.0, 1.0);
    
    CGContextBeginPath(context);
    CGContextMoveToPoint(context, x0, y0 - r);
    CGContextAddArc(context, x1, y0, r, a3, a0, 0);
    CGContextAddArc(context, x1, y1, r, a0, a1, 0);
    CGContextAddArc(context, x0, y1, r, a1, a2, 0);
    CGContextAddArc(context, x0, y0, r, a2, a3, 0);
    CGContextDrawPath(context, kCGPathFillStroke);
}

- (NSString *)legendToStringSuit:(Suit)suit rank:(Rank)rank
{
    return [NSString stringWithFormat:@"%C%C",
        RankToChar(rank), SuitToChar(suit)];
}

- (void)drawCardLegendInContext:(CGContextRef)context
                         center:(CGPoint)center
                           suit:(Suit)suit
                           rank:(Rank)rank
{
    NSColor *color = nil;
    switch (ColorOfSuit(suit))
    {
    case Red:
        color = [NSColor redColor];
        break;
    case Black:
        color = [NSColor blackColor];
        break;
    }
    
    NSPoint where = NSMakePoint(
        center.x - (0.5 * cardSize.width - 0.5 * cardCornerRadius),
        center.y - (0.5 * cardSize.height - 0.5 * cardCornerRadius));
    
    [[self legendToStringSuit:suit rank:rank]
           drawAtPoint:where
        withAttributes:[NSDictionary dictionaryWithObjectsAndKeys:
            color, NSForegroundColorAttributeName,
            nil]];
}

- (void)drawRect:(NSRect)clip
{
    CGContextRef context = [[NSGraphicsContext currentContext] graphicsPort];
    
    CGPoint cardCenter = CGPointMake(
        10.0 + 0.5 * cardSize.width,
        10.0 + 0.5 * cardSize.height);
    
    [self drawCardFrameInContext:context
                          center:cardCenter];
    [self drawCardLegendInContext:context
                           center:cardCenter
                             suit:Hearts
                             rank:King];
}

@end
