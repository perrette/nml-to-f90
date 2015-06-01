#!/usr/bin/env python
"""
"""
#from distutils.core import setup
import os, sys, re
from distutils.core import setup
import warnings

with open('README.md') as file:
    long_description = file.read()

#
# Track version after pandas' setup.py
#
MAJOR = 0
MINOR = 0
MICRO = 0
ISRELEASED = False
VERSION = '%d.%d.%d' % (MAJOR, MINOR, MICRO)
QUALIFIER = ''

FULLVERSION = VERSION
write_version = True

if not ISRELEASED:
    import subprocess
    FULLVERSION += '.dev'

    pipe = None
    for cmd in ['git','git.cmd']:
        try:
            pipe = subprocess.Popen([cmd, "describe", "--always", "--match", "v[0-9]*"],
                                stdout=subprocess.PIPE)
            (so,serr) = pipe.communicate()
            if pipe.returncode == 0:
                break
        except:
            pass

    if pipe is None or pipe.returncode != 0:
        # no git, or not in git dir
        if os.path.exists('nml2f90/version.py'):
            warnings.warn("WARNING: Couldn't get git revision, using existing nml2f90/version.py")
            write_version = False
        else:
            warnings.warn("WARNING: Couldn't get git revision, using generic version string")
    else:
      # have git, in git dir, but may have used a shallow clone (travis does this)
      rev = so.strip()
      # makes distutils blow up on Python 2.7
      if sys.version_info[0] >= 3:
          rev = rev.decode('ascii')

      if not rev.startswith('v') and re.match("[a-zA-Z0-9]{7,9}",rev):
          # partial clone, manually construct version string
          # this is the format before we started using git-describe
          # to get an ordering on dev version strings.
          rev ="v%s.dev-%s" % (VERSION, rev)

      # Strip leading v from tags format "vx.y.z" to get th version string
      FULLVERSION = rev.lstrip('v')

else:
    FULLVERSION += QUALIFIER

def write_version_py(filename=None):
    cnt = """\
version = '%s'
short_version = '%s'
"""
    if not filename:
        #filename = os.path.join(
        #    os.path.dirname(__file__), 'dimarray', 'version.py')
        filename = os.path.join('nml2f90', 'version.py')

    with open(filename, 'w') as a:
        a.write(cnt % (FULLVERSION, VERSION))

# Write version.py to dimarray
if write_version:
    write_version_py()

#
# Actually important part
#
setup(name='nml2f90',
      version=FULLVERSION,
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


