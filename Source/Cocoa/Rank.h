#import <Cocoa/Cocoa.h>

typedef enum Rank
{
    Ace,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Jack,
    Queen,
    King,
}
Rank;

static inline NSString *RankToString(Rank r)
{
    switch (r)
    {
        case Ace:   return @"A";
        case Two:   return @"2";
        case Three: return @"3";
        case Four:  return @"4";
        case Five:  return @"5";
        case Six:   return @"6";
        case Seven: return @"7";
        case Eight: return @"8";
        case Nine:  return @"9";
        case Ten:   return @"10";
        case Jack:  return @"J";
        case Queen: return @"Q";
        case King:  return @"K";
        default:
            assert(0 && "Unhandled rank in switch");
    }
}
