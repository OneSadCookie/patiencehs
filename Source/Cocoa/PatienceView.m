#import "CardRenderer.h"
#import "HSCocoaAPI.h"
#import "PatienceView.h"

@implementation PatienceView

- (id)initWithFrame:(NSRect)frameRect
{
    self = [super initWithFrame:frameRect];
    if (self == nil)
    {
        return nil;
    }
    
    renderer = [[CardRenderer alloc] init];
    
    [NSTimer scheduledTimerWithTimeInterval:0.001
                                     target:self
                                   selector:@selector(step)
                                   userInfo:nil
                                    repeats:YES];
    
    return self;
}

- (void)dealloc
{
    [renderer release];
    [super dealloc];
}

- (void)step
{
    gState = stepState(gState);
}

- (void)drawRect:(NSRect)clip
{
    [[NSColor colorWithDeviceRed:0.0 green:0.25 blue:0.0 alpha:1.0] set];
    NSRectFill(clip);
    
    NSSize cardSize = NSMakeSize(140.0, 200.0);
    NSPoint p = NSMakePoint(
        10.0 + 0.5 * cardSize.width,
        10.0 + 0.5 * cardSize.height);
    
    [renderer drawCardSuit:Diamonds rank:King faceUp:YES at:p];
    p.x += 10.0 + cardSize.width;
    [renderer drawCardSuit:Spades rank:Ace faceUp:YES at:p];
    p.x += 10.0 + cardSize.width;
    [renderer drawCardSuit:Clubs rank:Seven faceUp:YES at:p];
    p.x += 10.0 + cardSize.width;
    [renderer drawCardSuit:Hearts rank:Ten faceUp:YES at:p];
    p.x += 10.0 + cardSize.width;
    [renderer drawCardSuit:Diamonds rank:Two faceUp:NO at:p];
    p.x += 10.0 + cardSize.width;
    [renderer drawSpaceWithEmblem:[NSString stringWithFormat:@"%C", 0x21A9] at:p];
}

@end
