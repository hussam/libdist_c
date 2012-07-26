#!/usr/bin/env python

from distutils.core import setup

setup(name='libdist',
      version='0.1',
      description='Python Interface to LibDist',
      author='Peter Halliday',
      author_email='phalliday@excelsiorsystems.net',
      packages=['libdist'],
      package_dir={'': 'src'}
     )