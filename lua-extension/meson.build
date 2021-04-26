project('glirc-lua', 'c',
  license: 'ISC',
  version: '2.29',
  meson_version: '>=0.37.0',
  default_options: 'c_std=c11')

add_global_arguments('-Werror=vla', language: 'c')

foreach name : ['lua', 'lua5.4', 'lua-5.4', 'lua54']
    luaDep = dependency(name, version: ['>=5.4', '<5.5'], required: false)
    if luaDep.found()
        break
    endif
endforeach
if not luaDep.found()
    error('Lua could not be found!')
endif

incdir = include_directories('../include', 'include')
sources = ['glirc-lua.c', 'glirc-marshal.c', 'glirc-lib.c', 'glirc-thread.c', 'glirc-warn.c']

if build_machine.system() == 'darwin'
  suffix = 'bundle'
else
  suffix = 'so'
endif

shared_module('glirc-lua', sources,
  dependencies : luaDep,
  include_directories: incdir,
  name_prefix: '',
  name_suffix: suffix)
