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
    cardLegendTextSize = 18.0;
    
    return self;
}

- (void)drawCardFrameInContext:(CGContextRef)context
{
    CGFloat r = cardCornerRadius;
    
    CGFloat halfWidth = 0.5 * cardSize.width - r;
    CGFloat halfHeight = 0.5 * cardSize.height - r;
    
    CGFloat x0 = -halfWidth;
    CGFloat x1 =  halfWidth;
    CGFloat y0 = -halfHeight;
    CGFloat y1 =  halfHeight;
    
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
    return [NSString stringWithFormat:@"%C\n%C",
        RankToChar(rank), SuitToChar(suit)];
}

- (NSColor *)colorOfSuit:(Suit)suit
{
    switch (ColorOfSuit(suit))
    {
    case Red:
        return [NSColor redColor];
    case Black:
        return [NSColor blackColor];
    default:
        assert(0 && "Unhandled color in switch");
    }
}

- (void)drawCardLegendSuit:(Suit)suit
                      rank:(Rank)rank
{
    NSColor *color = [self colorOfSuit:suit];
    NSMutableParagraphStyle *style =
        [[[NSParagraphStyle defaultParagraphStyle] mutableCopy] autorelease];
    [style setAlignment:NSCenterTextAlignment];
    NSFont *font = [NSFont fontWithName:@"Lucida Grande"
                                   size:cardLegendTextSize];
    NSDictionary *attributes = [NSDictionary dictionaryWithObjectsAndKeys:
        color, NSForegroundColorAttributeName,
        style, NSParagraphStyleAttributeName,
        font,  NSFontAttributeName,
        nil];
    NSAttributedString *string = [[NSAttributedString alloc]
        initWithString:[self legendToStringSuit:suit rank:rank]
            attributes:attributes];
    NSSize size = [string size];
    
    NSAffineTransform *translation = [NSAffineTransform transform];
    [translation translateXBy:-0.5 * (cardSize.width - cardCornerRadius)
                          yBy: 0.5 * (cardSize.height - cardCornerRadius) -
                                   size.height];
    NSAffineTransform *rotation = [NSAffineTransform transform];
    [rotation rotateByRadians:M_PI];
    
    [NSGraphicsContext saveGraphicsState];
    [translation concat];
    [string drawInRect:NSMakeRect(
        0.0, 0.0, size.width, size.height)];
    [NSGraphicsContext restoreGraphicsState];
    
    [NSGraphicsContext saveGraphicsState];
    [rotation concat];
    [translation concat];
    [string drawInRect:NSMakeRect(
        0.0, 0.0, size.width, size.height)];
    [NSGraphicsContext restoreGraphicsState];
}

- (void)drawRect:(NSRect)clip
{
    CGContextRef context = [[NSGraphicsContext currentContext] graphicsPort];
    
    NSAffineTransform *transform = [NSAffineTransform transform];
    [transform translateXBy:10.0 + 0.5 * cardSize.width
                        yBy:10.0 + 0.5 * cardSize.height];
    [transform concat];
    
    [self drawCardFrameInContext:context];
    [self drawCardLegendSuit:Hearts
                        rank:King];
}

@end
