project('glirc-otr', 'cpp',
  license: 'ISC',
  version: '2.29',
  meson_version: '>=0.37.0',
  default_options: 'cpp_std=c++14')

otrdep = dependency('libotr', version: ['>=4.1', '<4.2'])
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
