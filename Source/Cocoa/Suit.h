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
    switch (s)
    {
        case Hearts:   return 0x2665;
        case Clubs:    return 0x2663;
        case Diamonds: return 0x2666;
        case Spades:   return 0x2660;
        default:
            assert(0 && "Unhandled suit in switch");
    }
}

typedef enum Color
{
    Red,
    Black,
}
Color;

static inline Color ColorOfSuit(Suit s)
{
    switch (s)
    {
    case Hearts:
    case Diamonds:
        return Red;
    case Clubs:
    case Spades:
        return Black;
    default:
        assert(0 && "Unhandled rank in switch");
    }
}
