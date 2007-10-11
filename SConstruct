import os

SConscriptChdir(0)

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

SConscript(
    'SConscript',
    exports = [ 'env' ],
    build_dir = 'build/i386',
    duplicate = 0)
