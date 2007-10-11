import os

SConscriptChdir(0)

archflags = [
    '-arch', 'i386',
    '-isysroot', '/Developer/SDKs/MacOSX10.4u.sdk',
    '-mmacosx-version-min=10.4']

env = Environment(
    tools = [ 'default', 'ghc' ],
    toolpath = [ 'SConsHs' ],
    ENV = os.environ,
    CPPPATH = [ '$GHCINCDIR', 'Source/Cocoa' ],
    FRAMEWORKS = [ 'Cocoa' ],
    HSPATH = [ 'Source' ],
    LINK = '$_GHCCOM',
    HSCFLAGS = archflags,
    HSAFLAGS = archflags,
    HSLFLAGS = archflags)

SConscript(
    'SConscript',
    exports = [ 'env' ],
    build_dir = 'build/i386')
