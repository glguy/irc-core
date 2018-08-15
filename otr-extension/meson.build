project('glirc-otr', 'cpp',
  license: 'ISC',
  default_options: 'cpp_std=c++14')

otrdep = dependency('libotr')
incdir = include_directories('../include')
sources = ['glirc-otr.cpp', 'OTR.cpp']

if build_machine.system() == 'darwin'
  suffix = 'bundle'
else
  suffix = 'so'
endif

shared_module('glirc-otr', sources,
  dependencies : otrdep,
  include_directories: incdir,
  name_prefix: '',
  name_suffix: suffix)
