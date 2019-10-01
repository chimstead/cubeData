# deckFileMaker.py

import re
import os
# Final project


# global variables
filelabels = 'ls.txt'
datadir = 'dckfiles/'
output = 'deckfile.csv'
def main():
    print("in main")
    filedata = readlabelfile(filelabels)
    makedatafile(filedata, output)
    os.system('./make_all_data.R')

def makedatafile(data, out):
    k = 0
    with open(out, 'w') as outfd:
        outfd.write('deckID, location, number, name, deckName\n')
        for i in range(len(data)):
            dckfile = datadir + data[i]
            print(dckfile)
            with open(dckfile, encoding="ascii", errors="surrogateescape") as fp:
                    for line in fp:
                        print(type(line))
                        writeline(line, k, outfd, data, i)
                    k += 1

def writeline(line, k, fd, data, i):
    line1 = line.replace("\n", ' ')
    line2 = line1.split(' ')
    if len(line2) >2:
        if 'SB:' not in line2:
            line2.insert(0, "MB:")
        fd.write(str(k) + ',')
        q = 0
        while q < len(line2):
            if ('LAYOUT' in line2[q]) or ('[' in line2[q]):
                del line2[q]
            else:
                q +=1

        q = 0
        str1 = ''

        str1 = '"'
        while q < len(line2):
            if q > 1:
                str1 += (line2[q] + ' ')
                del line2[q]
            else:
                q +=1
        str1 = str1[:-1]
        if len(line2) != 1:
            str1 += '"'
        line2.append(str1)
        q = 0
        while q < len(line2):
            lineout = (line2[q] + ",")
            regexp = re.compile('[^A-Za-z1234567890"\.\s\,\-]')
            lineout = regexp.sub('', lineout)
            fd.write(lineout)
            q +=1
        fd.write(" " + data[i] + "\n")

def readlabelfile(labelfile):
    """reads label file and returns list of tuples (label, fname)"""
    data = []
    print('in readlabelfile')
    with open(labelfile) as fp:
        for line in fp:
            line = line.replace("\n", '')
            data.append(line)
    return data


main()
