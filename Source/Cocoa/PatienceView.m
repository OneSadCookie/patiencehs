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
    [self setNeedsDisplay:YES];
}

- (void)placeCardSuit:(Suit)suit
                 rank:(Rank)rank
               faceUp:(BOOL)faceUp
                   at:(NSPoint)where
{
    [renderer drawCardSuit:suit
                      rank:rank
                    faceUp:faceUp
                        at:where];
}

- (void)drawRect:(NSRect)clip
{
    [[NSColor colorWithDeviceRed:0.0 green:0.25 blue:0.0 alpha:1.0] set];
    NSRectFill(clip);
    
    NSSize size = [self bounds].size;
    placeCards(gState, self, size.width, size.height);
}

@end
