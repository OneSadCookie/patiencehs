#import <Cocoa/Cocoa.h>

#import "PatienceHS.h"

HsPtr gState = NULL;

void PatienceStart(HsPtr state)
{
    gState = state;
    NSApplicationMain(0, NULL);
}
