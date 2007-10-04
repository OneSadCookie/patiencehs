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
    if (r == Ace)   return 'A';
    if (r == Ten)   return 'X';
    if (r == Jack)  return 'J';
    if (r == Queen) return 'Q';
    if (r == King)  return 'K';
    return r + '1';
}
