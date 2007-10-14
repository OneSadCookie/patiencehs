#ifndef PatienceHS_h
#define PatienceHS_h

#import "Haskell.h"

extern void PatienceStart(HsStablePtr state);

extern void PlaceCard(
    void   *userData,
    int     faceUp,
    int     suit,
    int     rank,
    double  x,
    double  y);

#endif
