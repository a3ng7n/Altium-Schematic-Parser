import os
from setuptools import setup, find_packages

with open("README.md", "r") as fh:
    long_description = fh.read()
    
def read(fname):
    return open(os.path.join(os.path.dirname(__file__), fname)).read()

setup(
    name='Altium-Schematic-Parser',
    version='0.0.0',
    packages=find_packages(exclude=['tests']),
    url='https://github.com/a3ng7n/Altium-Schematic-Parser',
    license='see LICENSE',
    author='Aaron Goldstein',
    author_email='aaronmgoldstein@gmail.com',
    description='Converts Altium .SchDoc files into json',
    long_description=long_description,
    long_description_content_type="text/markdown",
    install_requires=read('requirements.txt').splitlines()
)
