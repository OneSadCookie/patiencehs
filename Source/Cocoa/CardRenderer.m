#import "CardRenderer.h"

#define SIXTH (1.0 / 6.0)
#define EIGHTEENTH (1.0 / 18.0)

static NSPoint PipPositions[9][10/*should be variable*/] =
{
    { { 0.500, 1 * SIXTH }, { 0.500, 5 * SIXTH } },
    { { 0.500, 1 * SIXTH }, { 0.500, 3 * SIXTH }, { 0.500, 5 * SIXTH } },
    { { 0.333, 1 * SIXTH }, { 0.333, 5 * SIXTH },
      { 0.667, 1 * SIXTH }, { 0.667, 5 * SIXTH } },
    { { 0.333, 1 * SIXTH }, { 0.333, 5 * SIXTH }, { 0.500, 3 * SIXTH },
      { 0.667, 1 * SIXTH }, { 0.667, 5 * SIXTH } },
    { { 0.333, 1 * SIXTH }, { 0.333, 3 * SIXTH }, { 0.333, 5 * SIXTH },
      { 0.667, 1 * SIXTH }, { 0.667, 3 * SIXTH }, { 0.667, 5 * SIXTH } },
    { { 0.333, 1 * SIXTH }, { 0.333, 3 * SIXTH }, { 0.333, 5 * SIXTH },
      { 0.500, 4 * SIXTH },
      { 0.667, 1 * SIXTH }, { 0.667, 3 * SIXTH }, { 0.667, 5 * SIXTH } },
    { { 0.333, 1 * SIXTH }, { 0.333, 3 * SIXTH }, { 0.333, 5 * SIXTH },
      { 0.500, 2 * SIXTH }, { 0.500, 4 * SIXTH },
      { 0.667, 1 * SIXTH }, { 0.667, 3 * SIXTH }, { 0.667, 5 * SIXTH } },
    { { 0.333, 1 * SIXTH }, { 0.333, 7 * EIGHTEENTH },
      { 0.333, 11 * EIGHTEENTH }, { 0.333, 5 * SIXTH },
      { 0.500, 3 * SIXTH },
      { 0.667, 1 * SIXTH }, { 0.667, 7 * EIGHTEENTH },
      { 0.667, 11 * EIGHTEENTH }, { 0.667, 5 * SIXTH } },
    { { 0.333, 1 * SIXTH }, { 0.333, 7 * EIGHTEENTH },
      { 0.333, 11 * EIGHTEENTH }, { 0.333, 5 * SIXTH },
      { 0.500, 5 * EIGHTEENTH }, { 0.500, 13 * EIGHTEENTH },
      { 0.667, 1 * SIXTH }, { 0.667, 7 * EIGHTEENTH },
      { 0.667, 11 * EIGHTEENTH }, { 0.667, 5 * SIXTH } },
};

static CGFloat Adjust(CGFloat in)
{
    return 0.5 + floor(in);
}

@implementation CardRenderer

- (id)init
{
    self = [super init];
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

- (void)drawCardOutline
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
    
    CGContextBeginPath(context);
    CGContextMoveToPoint(context, x0, y0 - r);
    CGContextAddArc(context, x1, y0, r, a3, a0, 0);
    CGContextAddArc(context, x1, y1, r, a0, a1, 0);
    CGContextAddArc(context, x0, y1, r, a1, a2, 0);
    CGContextAddArc(context, x0, y0, r, a2, a3, 0);
    CGContextDrawPath(context, kCGPathFillStroke);
}

- (void)drawCardFrame
{
    CGContextRef context = [[NSGraphicsContext currentContext] graphicsPort];
    
    CGContextSetRGBFillColor(context, 1.0, 1.0, 1.0, 1.0);
    CGContextSetRGBStrokeColor(context, 0.0, 0.0, 0.0, 1.0);
    
    [self drawCardOutline];
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
                         color:(NSColor *)color
{
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
    return [[[NSAttributedString alloc]
        initWithString:text
            attributes:attributes] autorelease];
}

- (NSAttributedString *)string:(NSString *)text
                        ofSize:(CGFloat)size
                 coloredBySuit:(Suit)suit
{
    NSColor *color = [self colorOfSuit:suit];
    return [self string:text ofSize:size color:color];
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

- (void)drawCardSuit:(Suit)suit rank:(Rank)rank
{
    [self drawCardFrame];
    [self drawCardLegendSuit:suit rank:rank];
    [self drawCardFaceSuit:suit rank:rank];
}

- (void)drawCardBack
{
    [self drawCardFrame];
    
    CGContextRef context = [[NSGraphicsContext currentContext] graphicsPort];
    
    CGFloat r = cardCornerRadius;
    
    CGFloat halfWidth = 0.5 * cardSize.width - r;
    CGFloat halfHeight = 0.5 * cardSize.height - r;
    
    CGFloat x0 = -halfWidth;
    CGFloat x1 =  halfWidth;
    CGFloat y0 = -halfHeight;
    CGFloat y1 =  halfHeight;
    
    CGContextSetRGBFillColor(context, 0.0, 0.0, 0.25, 1.0);
    CGContextSetRGBStrokeColor(context, 0.0, 0.0, 0.0, 1.0);
    
    CGContextBeginPath(context);
    CGContextMoveToPoint(context, x0, y0);
    CGContextAddLineToPoint(context, x1, y0);
    CGContextAddLineToPoint(context, x1, y1);
    CGContextAddLineToPoint(context, x0, y1);
    CGContextAddLineToPoint(context, x0, y0);
    CGContextDrawPath(context, kCGPathFillStroke);
}

- (void)translateTo:(NSPoint)where for:(unsigned)seed
{
    NSAffineTransform *translate = [NSAffineTransform transform];
    [translate translateXBy:Adjust(where.x) yBy:Adjust(where.y)];
    
    CGFloat maxRot = 0.0125;
    NSAffineTransform *rotate = [NSAffineTransform transform];
    [rotate rotateByRadians:maxRot * sin(where.x + where.y + seed)];
    
    [NSGraphicsContext saveGraphicsState];
    [translate concat];
    [rotate concat];
}

- (void)drawCardSuit:(Suit)suit
                rank:(Rank)rank
              faceUp:(BOOL)faceUp
                  at:(NSPoint)where
{
    [self translateTo:where for:suit ^ rank ^ faceUp];
    
    if (faceUp)
    {
        [self drawCardSuit:suit rank:rank];
    }
    else
    {
        [self drawCardBack];
    }
    
    [NSGraphicsContext restoreGraphicsState];
}

- (void)drawSpaceWithEmblem:(NSString *)emblem
                         at:(NSPoint)where
{
    [self translateTo:where for:0];
    
    CGContextRef context = [[NSGraphicsContext currentContext] graphicsPort];
    
    // CGFloat lengths[] = { 10.0, 10.0 };
    // CGContextSetLineDash(context, 0.0, lengths, sizeof(lengths) / sizeof(CGFloat));
    CGContextSetRGBFillColor(context, 1.0, 1.0, 1.0, 0.5);
    CGContextSetRGBStrokeColor(context, 0.0, 0.0, 0.0, 0.0);
    
    [self drawCardOutline];
    
    NSAttributedString *string = [self
                string:emblem
                ofSize:cardAceFaceSize
                 color:[NSColor whiteColor]];
    NSSize size = [string size];
    
    [string drawInRect:NSMakeRect(
        -0.5 * size.width, -0.5 * size.height, size.width, size.height)];
    
    [NSGraphicsContext restoreGraphicsState];
}

@end
