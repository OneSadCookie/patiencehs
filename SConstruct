import os

SConscriptChdir(0)

def arch_env(env, arch, host, sdk, min):
    arch_env = env.Clone(
        ARCH=arch,
        SDK=sdk,
        MACOSX_VERSION_MIN=min,
        GHC=os.path.join(os.getcwd(), 'ghc-6.6.1/bin/' + host, 'ghc'))
    return arch_env

archflags = [
    '-arch', '${ARCH}',
    '-isysroot', '/Developer/SDKs/MacOSX${SDK}.sdk',
    '-mmacosx-version-min=${MACOSX_VERSION_MIN}']

global_env = Environment(
    tools = [ 'default', 'ghc' ],
    toolpath = [ 'SConsHs' ],
    CPPPATH = [ '$GHCINCDIR', 'Source/Cocoa' ],
    FRAMEWORKS = [ 'Cocoa' ],
    HSPATH = [ 'Source' ],
    LINK = '$_GHCCOM',
    HSCFLAGS = archflags,
    HSAFLAGS = archflags,
    HSLFLAGS = archflags,
    CCFLAGS = archflags + [
        '-Wall',
        '-Wextra',
        '-Wno-unused-parameter',
        '-Wnewline-eof',
        '-Werror',
        '-g',
        '-O2' ])

def arch_build(arch, host, sdk, min):
    env = arch_env(global_env, arch, host, sdk, min)
    SConscript(
        'SConscript',
        exports = [ 'env' ],
        build_dir = 'build/' + arch)

arch_build('i386', 'i386-apple-darwin', '10.4u', '10.4')
arch_build('ppc', 'powerpc-apple-darwin', '10.4u', '10.4')
