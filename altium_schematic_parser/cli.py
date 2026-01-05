import argparse
import json
import textwrap

from altium_schematic_parser import parse


def main():
    parser = argparse.ArgumentParser(
        description="Converts Altium .SchDoc files into json.",
        formatter_class=argparse.RawTextHelpFormatter,
    )
    _ = parser.add_argument("input", help="path/to/altiumschematic.schdoc file to parse")
    _ = parser.add_argument(
        "-o",
        "--output",
        dest="output",
        help="path/to/jsonfile.json file to output json to, otherwise prints to terminal",
    )
    _ = parser.add_argument(
        "-f",
        "--format",
        dest="format",
        default="all-hierarchy",
        choices=["all-list", "all-hierarchy", "parts-list", "net-list"],
        help=textwrap.dedent("""\
                        all-list: All records in a flattened list
                        all-hierarchy: All records in an owner/child "hierarchy"
                        parts-list: A listing of parts and their designators
                        net-list: A listing of nets between parts pins, referred to by their designators"""),
    )

    args = parser.parse_args()

    schematic = parse.parse(**vars(args))

    if args.output:
        json_file = open(args.output, "w")
        json.dump(schematic, json_file, indent=4)
    else:
        print(schematic)
