#!/usr/bin/env python
# -*- coding: iso-8859-15 -*-

from setuptools import setup, find_packages

setup(name='mykeylight',
      version='1.0',
      # Modules to import from other scripts:
      packages=find_packages(),
      # Executables
      scripts=["mykeylight.py"],
     )

