#!/usr/bin/env python
"""
"""
#from distutils.core import setup
import os, sys, re
from distutils.core import setup
import warnings
import versioneer


with open('README.md') as file:
    long_description = file.read()


# Actually important part
setup(name='nml2f90',
      version=versioneer.get_version(),
      cmdclass=versioneer.get_cmdclass(),
      author='Mahe Perrette',
      author_email='mahe.perrette@pik-potsdam.de',
      description='Generate fortran source code from a namelist',
      keywords=('fortran','template','namelist'),
      # basic stuff here
      py_modules = ['nml2f90'],
      packages = ['nml2f90'],
      package_data = {'nml2f90':['templates/*.f90', 'libraries/*f90']},
      scripts = ['scripts/nml2f90', 'scripts/f2nml'],
      long_description=long_description,
      url='https://github.com/perrette/nml-to-f90',
      license = "MIT",
      )


