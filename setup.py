from setuptools import setup

with open("README.md", "r") as fh:
    long_description = fh.read()
    
setup(
    name='Altium-Schematic-Parser',
    version='0.0.0',
    packages=[''],
    url='https://github.com/a3ng7n/Altium-Schematic-Parser',
    license='see LICENSE',
    author='Aaron Goldstein',
    author_email='aaronmgoldstein@gmail.com',
    description='Converts Altium .SchDoc files into json',
    long_description=long_description,
    long_description_content_type="text/markdown"
)
