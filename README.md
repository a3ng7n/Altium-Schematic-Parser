# Altium-Schematic-Parser
Converts Altium .SchDoc files into json
## Prerequisites
* python 3
* olefile
## Install
    git clone git@github.com:a3ng7n/Altium-Schematic-Parser.git
    cd Altium-Schematic-Parser
    pip install -e .
## Usage
    python parse.py -i "path/to/altiumschematic.schdoc" -o "path/to/jsonfile.json"