import argparse
import olefile
import os
import re

# class ParametricFile:
#     records = []
#     #public String signature
#
#     def getJSON(self):
#         return JSONObject(records.get(0));
#
#     def load(self, inputStream):
#         records = []
#         while (inputStream.available() >= 4):
#             record = self.readRecord(inputStream)
#             if (record != b''):
#                 records.append(record)
#
#         signature = records.pop(0)["HEADER"]
#         self.buildObjectHierarchy()
#         return len(records) > 0
#
#     def buildObjectHierarchy(self):
#         for i, current in enumerate(self.records):
#             s = current["OWNERINDEX"]
#             if (s == None | | len(s) == 0):
#                 s = "0"
#             ownerIndex = int(s)
#
#             self.records.remove(i)
#
#             Map < String, Object > owner = records.get(ownerIndex);
#             List < Object > children = (List < Object >)
#             owner.get("children");
#             if (children == null):
#                 children = new
#                 ArrayList < Object > ();
#                 owner.put("children", children);
#
#             children.add(current);
#
# def readRecord(inputStream):
#     line = readLine(inputStream)
#
#     if (line == null): return null
#
#     result = []
#
#     pairs = line.split("|")
#     for (String pair: pairs) {
#         if (pair.trim().isEmpty())
#             continue;
#
#     data = pair.split("=")
#     if (data.length == 2) {
#     result.put(data[0], data[1])
    #
#
#
#     return result
#
#
    # def readLine(inputStream):
    #     length = inputStream.readInt()
    #     if (length == -1):
    #         return None
    #
    #     buffer = new
    #     byte[length]
    #     inputStream.read(buffer, 0, length)
    #     if (buffer[0] == 0):
    #         return None
    #
    #     return new
    #     String(buffer).split("\u0000")[0]
    #
    #     BLOCKSIZE = 4096
    #     result = []
    #     current = ''
    #     for block in iter(lambda: fp.read(BLOCKSIZE), ''):
    #         current += block
    #         while 1:
    #             markerpos = current.find(marker)
    #             if markerpos == -1:
    #                 break
    #             result.append(current[:markerpos])
    #             current = current[markerpos + len(marker):]
    #     result.append(current)
    #     return result


def parse(input, output, **kwargs):
    fullPath = input
    print(fullPath)
    
    blah = olefile.OleFileIO(fullPath)
    stream = blah.openstream('FileHeader')

    pattern = re.compile(b'.{3}\x00\x00\|')
    lines = pattern.split(stream.read()[5:-1])
    
    records = []
    for line in lines:
        record = {}
        pairs = line.split(b"|")
        
        for pair in pairs:
            data = pair.split(b"=")
            
            if len(data) == 2:
                record[data[0].decode()] = data[1].decode()
        
        records.append(record)
    
    print(records)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Converts Altium .SchDoc files into json.')
    parser.add_argument('--input', '-i', dest='input',
                        help='schdoc file to parse')
    parser.add_argument('--output', '-o', dest='output',
                        help='file to output json to, otherwise prints to terminal')
    
    args = parser.parse_args()
    parse(**vars(args))