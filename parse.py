import argparse
import olefile
import re
import json
import copy

def parse(input, output, json_format, **kwargs):
    fullPath = input
    
    blah = olefile.OleFileIO(fullPath)
    stream = blah.openstream('FileHeader')
    
    # split binary stream into lines using a repeated 5 byte signature
    pattern = re.compile(b'.{3}\x00\x00\|')
    lines = pattern.split(stream.read()[5:-1]) # lopping off first 4 bytes, and last byte, since they don't seem to matter?
    
    schematic = {}
    
    datums = []
    
    # loop through every "line" and parse each into a dictionary
    for line in lines:
        datum = {}
        pairs = line.split(b"|")
        
        for pair in pairs:
            data = pair.split(b"=")
            
            datum[data[0].decode()] = data[1].decode()
        
        datums.append(datum)
    
    # separate out the header dictionary from the "records" dictionaries
    schematic["header"] = [x for x in datums if 'HEADER' in x.keys()]
    schematic["records"] = [x for x in datums if 'RECORD' in x.keys()]
    
    # prep a scratchpad copy of records to build hierarchy from
    records_copy = copy.deepcopy(schematic["records"])
    schematic["hierarchy"] = []
    
    # loop through all "records" and organize them into owner/children
    for i, current in enumerate(records_copy):
        current['index'] = i
        s = current.get("OWNERINDEX")
        if (s is None):
            schematic["hierarchy"].append(current)
        else:
            ownerIndex = int(s)
            
            owner = records_copy[ownerIndex]
            if (owner.get("children") == None):
                owner["children"] = []
            
            owner["children"].append(current)
    
    if json_format == 'hierarchy':
        schematic["records"] = schematic["hierarchy"]
    
    schematic.pop("hierarchy", None)
    
    if output:
        json_file = open(output, 'w')
        json.dump(schematic, json_file)
    else:
        print(schematic)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Converts Altium .SchDoc files into json.')
    parser.add_argument('--input', '-i', dest='input',
                        help='path/to/altiumschematic.schdoc file to parse')
    parser.add_argument('--output', '-o', dest='output',
                        help='path/to/jsonfile.json file to output json to, otherwise prints to terminal')
    parser.add_argument('json_format', default='hierarchy', nargs='?', choices=['flat', 'hierarchy'],
                        help='Organize records into owner/child "hierarchy" or leave as a "flat" list.')
    
    args = parser.parse_args()
    parse(**vars(args))