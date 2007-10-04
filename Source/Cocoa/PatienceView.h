#import <ApplicationServices/ApplicationServices.h>
#import <Cocoa/Cocoa.h>

#if !defined(CGFLOAT_DEFINED)
// 10.4 SDK
typedef float CGFloat;
#endif

@interface PatienceView : NSView
{
    CGSize cardSize;
    CGFloat cardCornerRadius;
}

@end
