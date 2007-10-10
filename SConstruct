import glob, os, re, stat
from os.path import join, exists

import SCons.Defaults
import SCons.Node.FS

env = Environment(
    ENV = os.environ,
    CPPPATH = [
        '/Volumes/Files/Unix/lib/ghc-6.6.1/include',
        'Source/Cocoa' ],
    FRAMEWORKS = [ 'Cocoa' ],
    HSPATH = [ 'Source' ],
    LINK = 'ghc')
# why doesn't this work?!
#env.BuildDir('build', 'Source')

def is_system_import(hi_file):
    return exists(join('/Volumes/Files/Unix/lib/ghc-6.6.1/imports/', hi_file))

def scan_hs(node, env, path):
    text = node.get_contents()
    regex = re.compile(r'^\s*import\s+([\w.]+)\s*$', re.M)
    list = []
    for match in regex.finditer(text):
        base = re.sub('\.', '/', match.group(1))
        hi_file = base + '.hi'
        node = SCons.Node.FS.find_file(hi_file, path)
        if node:
            list.append(node)
    #print map(str, list)
    return list

HsScanner = Scanner(
    name     = 'haskell',
    function = scan_hs,
    skeys    = ['.hs'],
    path_function = FindPathDirs('HSPATH'))
SourceFileScanner.add_scanner('.hs', HsScanner)

HsAction = Action('ghc $_HSINCFLAGS -c -o $TARGET $SOURCES')
env.Replace(_HSINCFLAGS =
    '$( ${_concat("-i", HSPATH, "", __env__, RDirs, TARGET, SOURCE)} $)')

def scan_ffi(node):
    text = node.get_contents()
    regex = re.compile(r'^\s*foreign\s+(import|export)\s*ccall', re.M)
    if regex.search(text):
        base = os.path.splitext(str(node))[0]
        return [base + '_stub.h', base + '_stub.c']
    else:
        return []

def emit_hs(target, source, env):
    newtarget = []
    for t in target:
        newtarget.append(t)
        newtarget.append(os.path.splitext(str(t))[0] + '.hi')
    for s in source:
        newtarget += scan_ffi(s)
    return (newtarget, source)

static_obj, shared_obj = SCons.Tool.createObjBuilders(env)
static_obj.add_action('.hs', HsAction)
static_obj.add_emitter('.hs', emit_hs)

env.Program(
    target = 'patience-app',
    source = glob.glob('Source/Cocoa/*.hs') +
             glob.glob('Source/Cocoa/*.m') +
             glob.glob('Source/Common/*.hs') +
             glob.glob('Source/Games/*.hs'))
