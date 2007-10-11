import glob

Import('env')

env.Program(
    target = 'patience-app',
    source = glob.glob('Source/Cocoa/*.hs') +
             glob.glob('Source/Cocoa/*.m') +
             glob.glob('Source/Common/*.hs') +
             glob.glob('Source/Games/*.hs'))
