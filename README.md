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
# Notes
## schdoc file format
Record ids:
* 1:    a part
* 2:    a pin on a part
* 6:    a "drawing" I think... "Xn"/"Yn" are values of where a line should be drawn
* 41:   text associated with a part - lots of different types indicated by "NAME"
    * PinUniqueId:  I suspect a unique id for the associated pin
    * Fitted:       ...wat?
    * Comment:      self explanatory
* 45:   appears to be a reference to which "model" a particular part can be represented by. Since this is just a
possible model, the one actually selected for a given part will have the "ISCURRENT" flag set to "T"