import glob, os

env = Environment(
    tools = [ 'default', 'ghc' ],
    toolpath = [ 'SConsHs' ],
    ENV = os.environ,
    CPPPATH = [
        '$GHCINCDIR',
        'Source/Cocoa' ],
    FRAMEWORKS = [ 'Cocoa' ],
    HSPATH = [ 'Source' ],
    LINK = '$GHC')
# why doesn't this work?!
#env.BuildDir('build', 'Source')

env.Program(
    target = 'patience-app',
    source = glob.glob('Source/Cocoa/*.hs') +
             glob.glob('Source/Cocoa/*.m') +
             glob.glob('Source/Common/*.hs') +
             glob.glob('Source/Games/*.hs'))
