#import "CardRenderer.h"
#import "HSCocoaAPI.h"
#import "PatienceView.h"

#if MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_5
#define NSWindowBackingLocationVideoMemory 1

@interface NSWindow (LeopardExtras)

// if we're not on Leopard, we're 32-bit...
- (void)setPreferredBackingLocation:(unsigned)backingLocation;

@end
#endif

@implementation PatienceView

- (id)initWithFrame:(NSRect)frameRect
{
    self = [super initWithFrame:frameRect];
    if (self == nil)
    {
        return nil;
    }
    
    renderer = [[CardRenderer alloc] init];
    
    /*
    // QuartzGL is a massive performance penalty with my current
    // drawing code
    if ([[self window] respondsToSelector:
            @selector(setPreferredBackingLocation:)])
    {
        // Enable QuartzGL in Leopard
        [[self window] setPreferredBackingLocation:
            NSWindowBackingLocationVideoMemory];
    }
    */
    
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
