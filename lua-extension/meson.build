project('glirc-lua', 'c',
  license: 'ISC',
  version: '2.29',
  meson_version: '>=0.37.0',
  default_options: 'c_std=c11')

luadep = dependency('lua', version: ['>=5.3', '<5.4'])
incdir = include_directories('../include', 'include')
sources = ['glirc-lua.c', 'glirc-marshal.c', 'glirc-lib.c']

if build_machine.system() == 'darwin'
  suffix = 'bundle'
else
  suffix = 'so'
endif

shared_module('glirc-lua', sources,
  dependencies : luadep,
  include_directories: incdir,
  name_prefix: '',
  name_suffix: suffix)
