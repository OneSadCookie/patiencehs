import os, re
from os.path import join, exists

import SCons.Defaults
import SCons.Node.FS
from SCons.Action import Action
from SCons.Scanner import Scanner, FindPathDirs
from SCons.Tool import SourceFileScanner

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

# -ohi ${TARGET.base}.hi
HsAction = Action('$_GHCCOM -c -o $TARGET $SOURCES')

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

def ghcdir(target, source, env, for_signature):
    return os.popen(env['GHC'] + " --print-libdir").read().rstrip()

def generate(env, **kw):
    hs_suffix = '.hs'
    
    static_obj, shared_obj = SCons.Tool.createObjBuilders(env)
    static_obj.add_action(hs_suffix, HsAction)
    static_obj.add_emitter(hs_suffix, emit_hs)
    
    SourceFileScanner.add_scanner(hs_suffix, HsScanner)
    
    env['GHC'] = 'ghc'
    env['GHCDIR'] = ghcdir
    env['GHCIMPORTDIR'] = '$GHCDIR/imports'
    env['GHCINCDIR'] = '$GHCDIR/include'
    env['HSSUFFIX'] = hs_suffix
    #env['HSPATH'] = []
    #env['HSFLAGS'] = []
    #env['HSCFLAGS'] = []
    #env['HSAFLAGS'] = []
    #env['HSLFLAGS'] = []
    
    env['_HSINCFLAGS'] = '$( ${_concat("-i", HSPATH, "", __env__, RDirs, TARGET, SOURCE)} $)'
    env['_HSFLAGS'] = '${_concat("", HSFLAGS, "", __env__)}'
    env['_HSCFLAGS'] = '${_concat("-optc", HSCFLAGS, "", __env__)}'
    env['_HSAFLAGS'] = '${_concat("-opta", HSAFLAGS, "", __env__)}'
    env['_HSLFLAGS'] = '${_concat("-optl", HSLFLAGS, "", __env__)}'
    env['_GHCCOM'] = '$GHC $_HSFLAGS $_HSINCFLAGS $_HSCFLAGS $_HSAFLAGS $_HSLFLAGS'

def exists(env):
    return 1
