import argparse, textwrap
import olefile
import re
import json
import copy

def parse(input, format, **kwargs):
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
    
    hierarchy_schematic = determine_hierarchy(schematic)
    if format == 'all-hierarchy':
        schematic = hierarchy_schematic
    elif format == 'parts-list':
        schematic = determine_parts_list(hierarchy_schematic)
    elif format == 'net-list':
        schematic = determine_net_list(hierarchy_schematic)
    
    return schematic

def determine_hierarchy(schematic):
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
    
    schematic["records"] = schematic["hierarchy"]
    schematic.pop("hierarchy", None)
    return schematic

def determine_parts_list(schematic):
    parts_list = {
        "records": [ record for record in schematic["records"] if record["RECORD"] is "1" ]
    }
    return parts_list

def determine_net_list(schematic):
    print('NOT IMPLEMENTED')
    return schematic

def main(args):
    schematic = parse(**vars(args))
    
    if args.output:
        json_file = open(args.output, 'w')
        json.dump(schematic, json_file, indent=4)
    else:
        print(schematic)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Converts Altium .SchDoc files into json.', formatter_class=argparse.RawTextHelpFormatter)
    parser.add_argument('input',
                        help='path/to/altiumschematic.schdoc file to parse')
    parser.add_argument('-o', '--output', dest='output',
                        help='path/to/jsonfile.json file to output json to, otherwise prints to terminal')
    parser.add_argument('-f', '--format', dest='format', default='all-hierarchy',
                        choices=['all-list', 'all-hierarchy', 'parts-list', 'net-list'],
                        help=textwrap.dedent('''\
                        all-list: All records in a flattened list
                        all-hierarchy: All records in an owner/child "hierarchy"
                        parts-list: A listing of parts and their designators
                        net-list: A listing of nets between parts pins, referred to by their designators'''))
    
    args = parser.parse_args()
    main(args)