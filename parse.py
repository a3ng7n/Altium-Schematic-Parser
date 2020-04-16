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
    wires = [ record for record in schematic["records"] if record["RECORD"] == "27" ]
    contacts = [ record for record in schematic["records"] if record["RECORD"] == "2" ]
    
    wire_groups = []
    
    p = re.compile('^(?P<prefix>X)(?P<index>\d+)$')
    for wire in wires:
        coord_name_matches = [x for x in [p.match(key) for key in wire.keys()] if x]
        wire['coords'] = [ ( int(wire['X' + match.group('index')]) , int(wire['Y' + match.group('index')]) )
                           for match in coord_name_matches ]

    group_counter = 0
    for wire_a in wires:
        other_wires = [wire for wire in wires if wire["index"] != wire_a["index"]]
        wire_a_vertex_pairs = [(wire_a['coords'][i], wire_a['coords'][i + 1]) for i in
                               range(len(wire_a['coords']) - 1)]
        
        for wire_b in other_wires:
            wire_b_vertex_pairs = [(wire_b['coords'][i], wire_b['coords'][i + 1]) for i in
                                   range(len(wire_b['coords']) - 1)]
            
            for vertex_a_pair in wire_a_vertex_pairs:
                for vertex_b_pair in wire_b_vertex_pairs:
                    if ( (( vertex_a_pair[0][0] <= vertex_b_pair[0][0] <= vertex_a_pair[1][0] )
                                and ( vertex_a_pair[0][1] <= vertex_b_pair[0][1] <= vertex_a_pair[1][1] ))
                            or (( vertex_a_pair[0][0] >= vertex_b_pair[0][0] >= vertex_a_pair[1][0] )
                                and ( vertex_a_pair[0][1] >= vertex_b_pair[0][1] >= vertex_a_pair[1][1] ))
                            or (( vertex_a_pair[0][0] <= vertex_b_pair[1][0] <= vertex_a_pair[1][0] )
                                and ( vertex_a_pair[0][1] <= vertex_b_pair[1][1] <= vertex_a_pair[1][1] ))
                            or (( vertex_a_pair[0][0] >= vertex_b_pair[1][0] >= vertex_a_pair[1][0] )
                                and ( vertex_a_pair[0][1] >= vertex_b_pair[1][1] >= vertex_a_pair[1][1] )) )\
                        or ( (( vertex_b_pair[0][0] <= vertex_a_pair[0][0] <= vertex_b_pair[1][0] )
                                and ( vertex_b_pair[0][1] <= vertex_a_pair[0][1] <= vertex_b_pair[1][1] ))
                            or (( vertex_b_pair[0][0] >= vertex_a_pair[0][0] >= vertex_b_pair[1][0] )
                                and ( vertex_b_pair[0][1] >= vertex_a_pair[0][1] >= vertex_b_pair[1][1] ))
                            or (( vertex_b_pair[0][0] <= vertex_a_pair[1][0] <= vertex_b_pair[1][0] )
                                and ( vertex_b_pair[0][1] <= vertex_a_pair[1][1] <= vertex_b_pair[1][1] ))
                            or (( vertex_b_pair[0][0] >= vertex_a_pair[1][0] >= vertex_b_pair[1][0] )
                                and ( vertex_b_pair[0][1] >= vertex_a_pair[1][1] >= vertex_b_pair[1][1] )) ):
                        print("line {0} intersects with line {1}".format(vertex_a_pair, vertex_b_pair))
                        
                        
                        
                        if ( wire_a.get('group', None) != None ) and ( wire_b.get('group', None) == None ):
                            wire_b['group'] = wire_a['group']
                        elif (wire_a.get('group', None) == None) and (wire_b.get('group', None) != None):
                            wire_a['group'] = wire_b['group']
                        else:
                            wire_a['group'] = group_counter
                            wire_b['group'] = group_counter
                            group_counter += 1
                        
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