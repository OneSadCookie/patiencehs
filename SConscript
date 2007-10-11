import glob

Import('env')

exe = env.Program(
    target = 'Patience',
    source = glob.glob('Source/Cocoa/*.hs') +
             glob.glob('Source/Cocoa/*.m') +
             glob.glob('Source/Common/*.hs') +
             glob.glob('Source/Games/*.hs'))

Return('exe')
