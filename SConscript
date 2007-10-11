import glob

Import('env')

unedited = env.Program(
    target = 'patience-unedited',
    source = glob.glob('Source/Cocoa/*.hs') +
             glob.glob('Source/Cocoa/*.m') +
             glob.glob('Source/Common/*.hs') +
             glob.glob('Source/Games/*.hs'))

exe = env.Command(
    target = 'Patience',
    source = unedited,
    action = 'cp $SOURCE $TARGET && install_name_tool -change GMP.framework/Versions/A/GMP @executable_path/../Frameworks/GMP.framework/Versions/A/GMP $TARGET')

Return('exe')
