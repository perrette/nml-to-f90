#!/usr/bin/env python
"""
"""
#from distutils.core import setup
import os, sys
from distutils.core import setup
import warnings

with open('README.md') as file:
    long_description = file.read()

#
#
#
setup(name='namelist',
      version="1.0",
      author='Mahe Perrette',
      author_email='mahe.perrette@pik-potsdam.de',
      description='Generate fortran source code from a namelist',
      keywords=('fortran','generic','namelist'),
      # basic stuff here
      py_modules = ['namelist'],
      scripts = ['nml2f90.py'],
      long_description=long_description,
      url='https://github.com/perrette/nml-to-f90',
      license = "MIT",
      )
