# deckFileMaker.py

import re
import os

# global variables
filelabels = 'ls.txt'
datadir = 'dckfiles/'
output = 'deckfile.csv'


def main():
    os.chdir('/Users/conorhimstead/Desktop/xcd')
    filedata = readlabelfile(filelabels)
    makedatafile(filedata, output)

# make the output .csv
def makedatafile(data, out):
    k = 0
    dates = []
    with open(out, 'w') as outfd:
        outfd.write('deckID, location, number, name, deckName, date\n')
        # iterate through the list of file names
        for i in range(len(data)):
            dckfile = datadir + data[i]
            # open one file and iterate through lines, pass those lines to .csv line writer
            with open(dckfile, encoding="ascii", errors="surrogateescape") as fp:
                for line in fp:
                    writeline(line, k, outfd, data, i, dates)
                k += 1

# write one line of the .csv
def writeline(line, k, fd, data, i, dates):
    line1 = line.replace("\n", '')
    line2 = line1.split(' ')
    if len(dates) < i + 1:
        dates.append("")

    # the date is the last line with no spaces (that doesn't start with "NAME")
    if len(line2) == 1 and not line2[0].startswith("NAME"):
        dates[i] = line2[0]

    # this is true if the line contains a card
    if len(line2) > 1 and (re.search("^[0-9]+$", line2[0]) or re.search("SB:", line2[0])):
        if 'SB:' not in line2:
            line2.insert(0, "MB:")
        fd.write(str(k) + ',')
        q = 0

        # needed for corner cases
        while q < len(line2):
            if ('LAYOUT' in line2[q]) or ('[' in line2[q]):
                del line2[q]
            else:
                q += 1

        q = 0

        # put card name into str1
        str1 = '"'
        while q < len(line2):
            if q > 1:
                str1 += (line2[q] + ' ')
                del line2[q]
            else:
                q += 1
        str1 = str1[:-1]
        if len(line2) != 1:
            str1 += '"'
        line2.append(str1)
        q = 0

        # write location, number, and card name
        while q < len(line2):
            lineout = (line2[q] + ",")
            regexp = re.compile('[^A-Za-z1234567890"\s,.\'\-]')
            lineout = regexp.sub('', lineout)
            fd.write(lineout)
            q += 1

        # write deck name and date
        fd.write(" " + data[i] + ", " + dates[i] + "\n")

# create a list of file names
def readlabelfile(labelfile):
    """reads label file and returns list of tuples (label, fname)"""
    data = []
    with open(labelfile) as fp:
        for line in fp:
            line = line.replace("\n", '')
            data.append(line)
    return data


main()
