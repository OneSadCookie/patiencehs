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

static inline unichar RankToChar(Rank r)
{
    switch (r)
    {
        case Ace:   return 'A';
        case Ten:   return 'X';
        case Jack:  return 'J';
        case Queen: return 'Q';
        case King:  return 'K';
        default:    return r + '1';
    }
}
