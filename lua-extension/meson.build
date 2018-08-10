project('glirc-lua', 'c')
luadep = dependency('lua-5.3')
incdir = include_directories('../include', 'include')
sources = ['glirc-lua.c', 'glirc-marshal.c', 'glirc-lib.c']
shared_module('glirc-lua', sources,
  dependencies : luadep,
  include_directories: incdir)
