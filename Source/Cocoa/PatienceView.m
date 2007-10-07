#import "PatienceView.h"
#import "Rank.h"
#import "Suit.h"

#define SIXTH (1.0 / 6.0)
#define EIGHTEENTH (1.0 / 18.0)

static NSPoint PipPositions[9][10/*should be variable*/] =
{
    { { 0.5, 1 * SIXTH }, { 0.5, 5 * SIXTH } },
    { { 0.5, 1 * SIXTH }, { 0.5, 3 * SIXTH }, { 0.5, 5 * SIXTH } },
    { { 0.333, 1 * SIXTH }, { 0.333, 5 * SIXTH },
      { 0.667, 1 * SIXTH }, { 0.667, 5 * SIXTH } },
    { { 0.333, 1 * SIXTH }, { 0.333, 5 * SIXTH }, { 0.5, 3 * SIXTH },
      { 0.667, 1 * SIXTH }, { 0.667, 5 * SIXTH } },
    { { 0.333, 1 * SIXTH }, { 0.333, 3 * SIXTH }, { 0.333, 5 * SIXTH },
      { 0.667, 1 * SIXTH }, { 0.667, 3 * SIXTH }, { 0.667, 5 * SIXTH } },
    { { 0.333, 1 * SIXTH }, { 0.333, 3 * SIXTH }, { 0.333, 5 * SIXTH },
      { 0.5, 4 * SIXTH },
      { 0.667, 1 * SIXTH }, { 0.667, 3 * SIXTH }, { 0.667, 5 * SIXTH } },
    { { 0.333, 1 * SIXTH }, { 0.333, 3 * SIXTH }, { 0.333, 5 * SIXTH },
      { 0.5, 2 * SIXTH }, { 0.5, 4 * SIXTH },
      { 0.667, 1 * SIXTH }, { 0.667, 3 * SIXTH }, { 0.667, 5 * SIXTH } },
    { { 0.333, 1 * SIXTH }, { 0.333, 7 * EIGHTEENTH },
      { 0.333, 11 * EIGHTEENTH }, { 0.333, 5 * SIXTH },
      { 0.5, 3 * SIXTH },
      { 0.667, 1 * SIXTH }, { 0.667, 7 * EIGHTEENTH },
      { 0.667, 11 * EIGHTEENTH }, { 0.667, 5 * SIXTH } },
    { { 0.333, 1 * SIXTH }, { 0.333, 7 * EIGHTEENTH },
      { 0.333, 11 * EIGHTEENTH }, { 0.333, 5 * SIXTH },
      { 0.5, 5 * EIGHTEENTH }, { 0.5, 13 * EIGHTEENTH },
      { 0.667, 1 * SIXTH }, { 0.667, 7 * EIGHTEENTH },
      { 0.667, 11 * EIGHTEENTH }, { 0.667, 5 * SIXTH } },    
};

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
    cardCourtFaceSize = 48.0;
    cardAceFaceSize = 72.0;
    cardPipFaceSize = 36.0;
    
    return self;
}

- (void)drawCardFrame
{
    CGContextRef context = [[NSGraphicsContext currentContext] graphicsPort];
    
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
    return [NSString stringWithFormat:@"%@\n%C",
        RankToString(rank), SuitToChar(suit)];
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

- (NSAttributedString *)string:(NSString *)text
                        ofSize:(CGFloat)size
                 coloredBySuit:(Suit)suit
{
    NSColor *color = [self colorOfSuit:suit];
    NSMutableParagraphStyle *style =
        [[[NSParagraphStyle defaultParagraphStyle] mutableCopy] autorelease];
    [style setAlignment:NSCenterTextAlignment];
    NSFont *font = [NSFont fontWithName:@"Lucida Grande"
                                   size:size];
    NSDictionary *attributes = [NSDictionary dictionaryWithObjectsAndKeys:
        color, NSForegroundColorAttributeName,
        style, NSParagraphStyleAttributeName,
        font,  NSFontAttributeName,
        nil];
    return [[NSAttributedString alloc] initWithString:text
                                           attributes:attributes];
}

- (void)drawCardLegendSuit:(Suit)suit
                      rank:(Rank)rank
{
    NSAttributedString *string = [self
                string:[self legendToStringSuit:suit rank:rank]
                ofSize:cardLegendTextSize
         coloredBySuit:suit];
    NSSize size = [string size];
    
    NSAffineTransform *translation = [NSAffineTransform transform];
    [translation translateXBy:-0.5 * (cardSize.width - cardCornerRadius)
                          yBy: 0.5 * (cardSize.height - cardCornerRadius)];
    NSAffineTransform *rotation = [NSAffineTransform transform];
    [rotation rotateByRadians:M_PI];
    
    [NSGraphicsContext saveGraphicsState];
    [translation concat];
    [string drawInRect:NSMakeRect(
        0.0, -size.height, size.width, size.height)];
    [NSGraphicsContext restoreGraphicsState];
    
    [NSGraphicsContext saveGraphicsState];
    [rotation concat];
    [translation concat];
    [string drawInRect:NSMakeRect(
        0.0, -size.height, size.width, size.height)];
    [NSGraphicsContext restoreGraphicsState];
}

- (NSString *)aceFaceToStringSuit:(Suit)suit
{
    return [NSString stringWithFormat:@"%C", SuitToChar(suit)];
}

- (void)drawAceFaceSuit:(Suit)suit
{
    NSAttributedString *string = [self
                string:[self aceFaceToStringSuit:suit]
                ofSize:cardAceFaceSize
         coloredBySuit:suit];
    NSSize size = [string size];
    
    [string drawInRect:NSMakeRect(
        -0.5 * size.width, -0.5 * size.height, size.width, size.height)];
}

- (NSString *)courtFaceToStringSuit:(Suit)suit rank:(Rank)rank
{
    return [NSString stringWithFormat:@"%@%C",
        RankToString(rank), SuitToChar(suit)];
}

- (void)drawCourtFaceSuit:(Suit)suit rank:(Rank)rank
{
    NSAttributedString *string = [self
                string:[self courtFaceToStringSuit:suit rank:rank]
                ofSize:cardCourtFaceSize
         coloredBySuit:suit];
    NSSize size = [string size];
    
    NSAffineTransform *rotation = [NSAffineTransform transform];
    [rotation rotateByRadians:M_PI];
    
    [NSGraphicsContext saveGraphicsState];
    [string drawInRect:NSMakeRect(
        -0.5 * size.width, 0.0, size.width, size.height)];
    [NSGraphicsContext restoreGraphicsState];
    
    [NSGraphicsContext saveGraphicsState];
    [rotation concat];
    [string drawInRect:NSMakeRect(
        -0.5 * size.width, 0.0, size.width, size.height)];
    [NSGraphicsContext restoreGraphicsState];
}

- (void)drawPipFaceSuit:(Suit)suit rank:(Rank)rank
{
    NSAttributedString *string = [self
                string:[NSString stringWithFormat:@"%C", SuitToChar(suit)]
                ofSize:cardPipFaceSize
         coloredBySuit:suit];
    NSSize size = [string size];
    
    NSAffineTransform *rotation = [NSAffineTransform transform];
    [rotation rotateByRadians:M_PI];
    
    unsigned i;
    for (i = 0; i <= rank; ++i)
    {
        NSPoint pipPoint = PipPositions[rank - 1][i];
        
        NSAffineTransform *translation = [NSAffineTransform transform];
        [translation translateXBy:(pipPoint.x - 0.5) * cardSize.width
                              yBy:(pipPoint.y - 0.5) * cardSize.height];
                
        [NSGraphicsContext saveGraphicsState];
        [translation concat];
        if (pipPoint.y < 0.499)
        {
            [rotation concat];
        }
        [string drawInRect:NSMakeRect(
            -0.5 * size.width, -0.5 * size.height, size.width, size.height)];
        [NSGraphicsContext restoreGraphicsState];
    }
}

- (void)drawCardFaceSuit:(Suit)suit rank:(Rank)rank
{
    switch (rank)
    {
    case Ace:
        [self drawAceFaceSuit:suit];
        break;
    case Jack:
    case Queen:
    case King:
        [self drawCourtFaceSuit:suit rank:rank];
        break;
    default:
        [self drawPipFaceSuit:suit rank:rank];
        break;
    }
}

- (void)drawCardSuit:(Suit)suit rank:(Rank)rank at:(NSPoint)where
{
    NSAffineTransform *transform = [NSAffineTransform transform];
    [transform translateXBy:where.x yBy:where.y];
    
    [NSGraphicsContext saveGraphicsState];
    [transform concat];
    [self drawCardFrame];
    [self drawCardLegendSuit:suit rank:rank];
    [self drawCardFaceSuit:suit rank:rank];
    [NSGraphicsContext restoreGraphicsState];
}

- (void)drawCardBack
{
    CGContextRef context = [[NSGraphicsContext currentContext] graphicsPort];
    
    CGFloat r = cardCornerRadius;
    
    CGFloat halfWidth = 0.5 * cardSize.width - r;
    CGFloat halfHeight = 0.5 * cardSize.height - r;
    
    CGFloat x0 = -halfWidth;
    CGFloat x1 =  halfWidth;
    CGFloat y0 = -halfHeight;
    CGFloat y1 =  halfHeight;
    
    CGContextSetRGBFillColor(context, 0.5, 0.5, 0.5, 1.0);
    CGContextSetRGBStrokeColor(context, 0.0, 0.0, 0.0, 1.0);
    
    CGContextBeginPath(context);
    CGContextMoveToPoint(context, x0, y0);
    CGContextAddLineToPoint(context, x1, y0);
    CGContextAddLineToPoint(context, x1, y1);
    CGContextAddLineToPoint(context, x0, y1);
    CGContextAddLineToPoint(context, x0, y0);
    CGContextDrawPath(context, kCGPathFillStroke);
}

- (void)drawCardBackAt:(NSPoint)where
{
    NSAffineTransform *transform = [NSAffineTransform transform];
    [transform translateXBy:where.x yBy:where.y];
    
    [NSGraphicsContext saveGraphicsState];
    [transform concat];
    [self drawCardFrame];
    [self drawCardBack];
    [NSGraphicsContext restoreGraphicsState];
}

- (void)drawRect:(NSRect)clip
{
    [[NSColor colorWithDeviceRed:0.0 green:0.25 blue:0.0 alpha:1.0] set];
    NSRectFill(clip);
    
    CGFloat y = 10.5 + 0.5 * cardSize.height;
    
    [self drawCardSuit:Hearts rank:King at:NSMakePoint(
        10.5 + 0.5 * cardSize.width, y)];
    [self drawCardSuit:Spades rank:Ace at:NSMakePoint(
        20.5 + 1.5 * cardSize.width, y)];
    [self drawCardSuit:Clubs rank:Seven at:NSMakePoint(
        30.5 + 2.5 * cardSize.width, y)];
    [self drawCardSuit:Diamonds rank:Ten at:NSMakePoint(
        40.5 + 3.5 * cardSize.width, y)];
    [self drawCardBackAt:NSMakePoint(
        50.5 + 4.5 * cardSize.width, y)];
}

@end
