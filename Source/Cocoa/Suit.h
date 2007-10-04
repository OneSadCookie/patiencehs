#import <Cocoa/Cocoa.h>

typedef enum Suit
{
    Hearts,
    Clubs,
    Diamonds,
    Spades,
}
Suit;

static inline unichar SuitToChar(Suit s)
{
    if (s == Hearts)       return 0x2665;
    if (s == Clubs)        return 0x2663;
    if (s == Diamonds)     return 0x2666;
    /* if (s == Spades) */ return 0x2660;
}
