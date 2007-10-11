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
    LINK = '$_GHCCOM -optl-dead_strip -optl-headerpad_max_install_names',
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
    return SConscript(
        'SConscript',
        exports = [ 'env' ],
        build_dir = 'build/' + arch)

i386_exe = arch_build('i386', 'i386-apple-darwin', '10.4u', '10.4')
ppc_exe = arch_build('ppc', 'powerpc-apple-darwin', '10.4u', '10.4')

global_env.Command(
    target = 'Patience.app/Contents/MacOS/Patience',
    source = [ i386_exe, ppc_exe ],
    action = 'lipo $SOURCES -create -output $TARGET')

global_env.Install(
    dir = 'Patience.app/Contents/Resources/English.lproj/MainMenu.nib',
    source = 'Resources/English.lproj/MainMenu.nib/keyedobjects.nib')

global_env.Install(
    dir = 'Patience.app/Contents',
    source = 'Resources/Info.plist')

global_env.Command(
    target = 'Patience.app/Contents/PkgInfo',
    source = [],
    action = "echo 'APPL????' > $TARGET")

global_env.Command(
    target = Dir('Patience.app/Contents/Frameworks/GMP.framework'),
    source = 'GMP-framework.zip',
    action = 'mkdir -p ${TARGET.dir} && cd ${TARGET.dir} && unzip ${SOURCE.abspath}')
