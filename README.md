# Altium-Schematic-Parser
Converts Altium .SchDoc files into json
## Prerequisites
* python 3 (3.7 to 3.11; currently doesn't support 3.12+)
* olefile
## Install
    pip install altium-schematic-parser
## Usage
Full description

    usage: parse [-h] [-o OUTPUT] [-f {all-list,all-hierarchy,parts-list,net-list}] input

Ex: to export all contents of altiumschematic.schdoc to jsonfile.json within path/to/:

    parse "path/to/altiumschematic.schdoc" -o "path/to/jsonfile.json"
**NOTE**:
the default output formatting behavior (-f/--format) if not supplied is to show all contents
retaining all hierarchy information

## Details
```
$ parse -h
usage: parse [-h] [-o OUTPUT] [-f {all-list,all-hierarchy,parts-list,net-list}] input

Converts Altium .SchDoc files into json.

positional arguments:
  input                 path/to/altiumschematic.schdoc file to parse

options:
  -h, --help            show this help message and exit
  -o OUTPUT, --output OUTPUT
                        path/to/jsonfile.json file to output json to, otherwise prints to terminal
  -f {all-list,all-hierarchy,parts-list,net-list}, --format {all-list,all-hierarchy,parts-list,net-list}
                        all-list: All records in a flattened list
                        all-hierarchy: All records in an owner/child "hierarchy"
                        parts-list: A listing of parts and their designators
                        net-list: A listing of nets between parts pins, referred to by their designators
```

# Notes
## schdoc file format
Record ids:
* 1:    a part, type identified by either "LIBREFERENCE" or "DESIGNITEMID"
* 2:    a pin on a part, with types indicated by "ELECTRICAL"
    * 4:            "Passive"
    * 7:            "Power"
* 4:    a "Annotation", which appears to just be a text box for referential purposes
* 6:    a "drawing" I think... "Xn"/"Yn" are values of where a line should be drawn
* 17:   a "Power Port", used commonly as GND or VCC, identified by "TEXT", "LOCATION.X", "LOCATION.Y",
and a symbol denoted by "STYLE"
* 25:   a "Net Label", which is similar to a "Power Port" in giving net designation to a wire,
but doesn't come with a symbol (aka STYLE)
* 27:   a "Wire", aka connecting line used to determine net associations
* 34:   a designator?
* 41:   text associated with an "OWNERPARTID" - lots of different types indicated by "NAME"
    * PinUniqueId:  I suspect a unique id for the associated pin
    * Fitted:       ...wat?
    * Comment:      self explanatory
* 44:   a container of "models" aka record 45's - see below
* 45:   appears to be a reference to which "model" a particular part can be represented by. Since this is just a
possible model, the one actually selected for a given part will have the "ISCURRENT" flag set to "T"

## Net Association
Altium seems to have a very very very bizarre way of designating or determining the designation of what is a net,
and what's connected to that net.
