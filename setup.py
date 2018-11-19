#!/usr/bin/env python
from setuptools import find_packages, setup

setup(
    name="hypoKanren",
    version="0.0.1",
    install_requires=[
        'hy>=0.15.0+48.gc5abc85',
        'pyrsistent',
        'multipledispatch',
    ],
    packages=find_packages(exclude=['tests']),
    package_data={
        'hypoKanren': ['*.hy'],
    },
    tests_require=[
        'pytest'
    ],
    author="Brandon T. Willard",
    author_email="brandonwillard+hypoKanren@gmail.com",
    long_description="""A microKanren implementation in Hy.""",
    license="LGPL-3",
    url="https://github.com/brandonwillard/hypoKanren",
    platforms=['any'],
    python_requires='>=3.6',
    dependency_links=[
        # XXX: Install with `--process-dependency-links`!
        'git+https://github.com/hylang/hy@c5abc85a2d78b318f47c34536a99f11b72c68f2b#egg=hy-0.15.0+48.gc5abc85',
    ],
    classifiers=[
        "Development Status :: 4 - Beta",
        "Intended Audience :: Developers",
        "License :: DFSG approved",
        "License :: OSI Approved :: GNU Lesser General Public License v3 or later (LGPLv3+)",
        "Operating System :: OS Independent",
        "Programming Language :: Lisp",
        "Programming Language :: Python :: 3.6",
        "Topic :: Software Development :: Libraries",
    ]
)
