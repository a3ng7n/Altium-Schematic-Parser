import argparse, textwrap
import olefile
import re
import json
import copy
import math
import logging
logging.basicConfig()
lg = logging.getLogger(__name__)

def parse(input, format, **kwargs):
    fullPath = input

    blah = olefile.OleFileIO(fullPath)
    stream = blah.openstream('FileHeader')

    # split binary stream into lines using a repeated 5 byte signature
    pattern = re.compile(b'.{3}\x00\x00\\|')
    lines = pattern.split(stream.read()[5:-1]) # lopping off first 4 bytes, and last byte, since they don't seem to matter?

    schematic = {}

    datums = []

    # loop through every "line" and parse each into a dictionary
    for line in lines:
        datum = {}
        pairs = line.split(b"|")

        for pair in pairs:
            data = pair.split(b"=")
            # only add to dict if we have both a key and a value
            if len(data) >= 2:
                datum[data[0].decode()] = data[1].decode('utf-8', 'ignore')

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
    """Convert a dict containing a flat list of records
    into a dict of records in a hierarchy

    :param schematic: dict with 'header' and 'records' populated
    :return: the input dict with 'records' assembled into parent/child hierarchy
    """

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
        "records": [ record for record in schematic["records"] if record["RECORD"] == "1" ]
    }
    return parts_list

def determine_net_list(schematic):
    _, wires = find_record(schematic, key="RECORD", value="27")
    _, pins = find_record(schematic, key="RECORD", value="2")
    _, labels = find_record(schematic, key="RECORD", value="25")
    _, power_ports = find_record(schematic, key="RECORD", value="17")
    devices = wires + pins + labels + power_ports

    p = re.compile('^(?P<prefix>X)(?P<index>\\d+)$')
    for device in devices:
        # if a Pin, do some fancy geometry math
        if device["RECORD"] == "2":
            rotation = (int(device.get("PINCONGLOMERATE", 0)) & 0x03) * 90
            pin_length = int(device.get('PINLENGTH', 0))
            loc_x = int(device.get('LOCATION.X', 0))
            loc_y = int(device.get('LOCATION.Y', 0))
            device['coords'] = [[
                int(loc_x + math.cos(rotation / 180 * math.pi) * pin_length),
                int(loc_y + math.sin(rotation / 180 * math.pi) * pin_length)
            ]]
        # if a Wire, follow inconsistent location key names (X1 vs LOCATION.X, etc..)
        elif device["RECORD"] == "27":
            coord_name_matches = [x for x in [p.match(key) for key in device.keys()] if x]
            device['coords'] = [ ( int(device['X' + match.group('index')]) , int(device['Y' + match.group('index')]) )
                               for match in coord_name_matches ]
        # everything else, just convert the location values to ints
        else:
            device['coords'] = [(int(device.get('LOCATION.X', 0)), int(device.get('LOCATION.Y', 0)))]

    nets = []
    for device in devices:
        if device["index"] not in [d['index'] for net in nets for d in net['devices']]:
            net = {'name': None,
                   'devices': find_connected_wires(device, devices, [], schematic)}
            nets.append(net)

    for net in nets:
        net['devices'].sort(key=lambda k: k['index'])
        if not net['name']:
            net['name'] = next(iter(d.get('TEXT') for d in net['devices'] if ((d['RECORD'] == '17') or (d['RECORD'] == '25')) and d.get('TEXT')), None)

        if not net['name']:
            naming_pin = next(iter(d for d in net['devices'] if d['RECORD'] == '2'), None)
            owner_index = naming_pin.get('OWNERINDEX') if naming_pin else None
            parent = next(iter(find_record(schematic, key="index", value=int(owner_index))[1]), None) if owner_index else None
            net['name'] = next(iter('Net' + r.get('TEXT', '') for r in parent.get('children', []) if (r['RECORD'] == '34') and r.get('TEXT')), None) if parent else None

    schematic["nets"] = nets

    return schematic

def find_record(schematic, key, value, record=None, visited=None, found=None):
    lg.debug("finding records where: {0} = {1}".format(key, value))

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

def find_connected_wires(wire, devices, visited, schematic):
    neighbors = find_neighbors(wire, devices, schematic)
    lg.debug('entering: {0}'.format(wire['index']))

    if wire['index'] not in [w['index'] for w in visited]:
        lg.debug('adding: {0} to {1}'.format(wire['index'], [w['index'] for w in visited]))
        visited.append(wire)

        for neighbor in neighbors:
            lg.debug('trying: {0} of {1}'.format(neighbor['index'], [x['index'] for x in neighbors]))
            visited = find_connected_wires(neighbor, devices, visited, schematic)
            lg.debug('visited = {0}'.format([w['index'] for w in visited]))
    else:
        lg.debug('skipping: {0} already in list {1}'.format(wire['index'], [w['index'] for w in visited]))

    lg.debug('returning: {0}'.format(wire['index']))
    return visited

def find_neighbors(wire, devices, schematic):
    all_wires = devices
    other_wires = [record for record in all_wires if record != wire]

    neighbors = []
    for other_wire in other_wires:
        if is_connected(wire, other_wire):
            neighbors.append(other_wire)

    return neighbors

def is_connected(wire_a, wire_b):

    if wire_a["RECORD"] == "27":
        a_line_segments = [(wire_a['coords'][i], wire_a['coords'][i + 1]) for i in
                      range(len(wire_a['coords']) - 1)]
    else:
        a_line_segments = [(wire_a['coords'][0], wire_a['coords'][0])]

    if wire_b["RECORD"] == "27":
        b_line_segments = [(wire_b['coords'][i], wire_b['coords'][i + 1]) for i in
                      range(len(wire_b['coords']) - 1)]
    else:
        b_line_segments = [(wire_b['coords'][0], wire_b['coords'][0])]

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

    # check if both items are Power Ports with the same TEXT value
    if ( wire_a["RECORD"] == "17" ) and ( wire_b["RECORD"] == "17" ) and ( wire_a["TEXT"] == wire_b["TEXT"] ):
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