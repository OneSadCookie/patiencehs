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

HsAction = Action('$GHC $_HSINCFLAGS -c -o $TARGET $SOURCES')

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

def generate(env, **kw):
    hs_suffix = '.hs'
    
    static_obj, shared_obj = SCons.Tool.createObjBuilders(env)
    static_obj.add_action(hs_suffix, HsAction)
    static_obj.add_emitter(hs_suffix, emit_hs)
    
    SourceFileScanner.add_scanner(hs_suffix, HsScanner)
    
    env['_HSINCFLAGS'] = '$( ${_concat("-i", HSPATH, "", __env__, RDirs, TARGET, SOURCE)} $)'
    env['GHC'] = 'ghc'
    env['GHCDIR'] = '/Volumes/Files/Unix/lib/ghc-6.6.1'
    env['GHCIMPORTDIR'] = '$GHCDIR/imports'
    env['GHCINCDIR'] = '$GHCDIR/include'
    env['HSSUFFIX'] = hs_suffix

def exists(env):
    return 1
