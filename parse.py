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
    pins = [ record for record in schematic["records"] if record["RECORD"] == 2 ]
    
    p = re.compile('^(?P<prefix>X)(?P<index>\d+)$')
    for wire in wires:
        coord_name_matches = [x for x in [p.match(key) for key in wire.keys()] if x]
        wire['coords'] = [ ( int(wire['X' + match.group('index')]) , int(wire['Y' + match.group('index')]) )
                           for match in coord_name_matches ]
    
    nets = []
    for wire in wires:
        if wire["index"] not in [id for net in nets for id in net]:
            nets.append(find_connected_wires(wire, [], schematic))
    
    visited, found = find_record(schematic, key="RECORD", value="2")
    
    schematic["nets"] = nets
    
    return schematic

def find_record(schematic, key, value, record=None, visited=None, found=None):
    print("finding records where: {0} = {1}".format(key, value))
    
    if visited == None:
        visited = []
    if found == None:
        found = []
    if record == None:
        for record in schematic['records']:
            visited, found = find_record(schematic, key, value, record=record, visited=visited, found=found)
    else:
        if record['index'] not in [r['index'] for r in visited]:
            visited.append(record)
            
            if key in record.keys():
                if record[key] == value:
                    found.append(record)
        
        if "children" in record.keys():
            for child_record in record["children"]:
                visited, found = find_record(schematic, key, value, record=child_record, visited=visited, found=found)
    
    return visited, found
    
def find_connected_wires(wire, visited, schematic):
    neighbors = find_neighbors(wire, schematic)
    print('entering: {0}'.format(wire['index']))
    
    if wire['index'] not in [w['index'] for w in visited]:
        print('adding: {0} to {1}'.format(wire['index'], [w['index'] for w in visited]))
        visited.append(wire)
        
        for neighbor in neighbors:
            print('trying: {0} of {1}'.format(neighbor['index'], [x['index'] for x in neighbors]))
            visited = find_connected_wires(neighbor, visited, schematic)
            print('visited = {0}'.format([w['index'] for w in visited]))
    else:
        print('skipping: {0} already in list {1}'.format(wire['index'], [w['index'] for w in visited]))
    
    print('returning: {0}'.format(wire['index']))
    return visited

def find_neighbors(wire, schematic):
    all_wires = [record for record in schematic["records"] if record["RECORD"] == "27"]
    other_wires = [record for record in all_wires if record != wire]
    
    neighbors = []
    for other_wire in other_wires:
        if is_connected(wire, other_wire):
            neighbors.append(other_wire)
    
    return neighbors

def is_connected(wire_a, wire_b):
    a_line_segments = [(wire_a['coords'][i], wire_a['coords'][i + 1]) for i in
                      range(len(wire_a['coords']) - 1)]
    b_line_segments = [(wire_b['coords'][i], wire_b['coords'][i + 1]) for i in
                      range(len(wire_b['coords']) - 1)]
    
    # check if any vertices in wire_a lie on wire_b
    for vertex in [vx for line in a_line_segments for vx in line]:
        for b_line in b_line_segments:
            b_xs = sorted(list(zip(*b_line))[0])
            b_ys = sorted(list(zip(*b_line))[1])
            
            if ((min(b_xs) <= vertex[0] <= max(b_xs))
                    and (min(b_ys) <= vertex[1] <= max(b_ys))):
                return True
    
    # check if any vertices in wire_b lie on wire_a
    for vertex in [vx for line in b_line_segments for vx in line]:
        for a_line in a_line_segments:
            a_xs = sorted(list(zip(*a_line))[0])
            a_ys = sorted(list(zip(*a_line))[1])
        
            if ((min(a_xs) <= vertex[0] <= max(a_xs))
                    and (min(a_ys) <= vertex[1] <= max(a_ys))):
                return True
    
    return False

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