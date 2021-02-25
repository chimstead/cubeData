import os
import re


# unzip
# rename unzipped folder chickenDinner

def main():
    # move around, delete, and rename folders
    os.chdir('/Users/conorhimstead/Desktop/xcd/cubeapp_data')
    os.system('mv "chickenDinner/Powered or Unpowered Decklists" .')
    os.rename("Powered or Unpowered Decklists", "pnp")
    os.system('rm -R chickenDinner')

    # write dates on the top line of files
    fileList = getListOfFiles("pnp")
    for fpath in fileList:
        if re.search("pnp/(.*)/(.*)/(.*)/(.*)", fpath):
            line_prepender(fpath, fpath.split("/")[3])
        if re.search("pnp/(.*)/(.*)/(.*)", fpath):
            line_prepender(fpath, fpath.split("/")[2])
        if re.search("pnp/(.*)/(.*)", fpath):
            line_prepender(fpath, fpath.split("/")[1])

    # flatten deck file directory and remove folders
    os.system('mv pnp/*/*/*/* pnp')
    os.system('mv pnp/*/* pnp')
    os.system('mv pnp/*/* pnp')
    os.system('rm -rf pnp/*/')
    os.system('rm -rf pnp/*.rtf')
    os.system('rm pnp/Chicken_*')

    # create ls.txt and move to parent folder
    os.system('ls pnp > ls.txt')
    os.system('mv pnp dckFiles')
    os.system('rm ../ls.txt')
    os.system('rm -R ../dckFiles')
    os.system('mv ls.txt ..')
    os.system('mv dckFiles ..')
    os.chdir('/Users/conorhimstead/Desktop/xcd')
    #os.system('Python3 deckFileMaker.py')


def getListOfFiles(dirName):
    # create a list of file and sub directories
    # names in the given directory
    listOfFile = os.listdir(dirName)
    allFiles = list()
    # Iterate over all the entries
    for entry in listOfFile:
        # Create full path
        fullPath = os.path.join(dirName, entry)
        # If entry is a directory then get the list of files in this directory
        if os.path.isdir(fullPath):
            allFiles = allFiles + getListOfFiles(fullPath)
        else:
            allFiles.append(fullPath)

    return allFiles


def line_prepender(filename, line):
    # code to write the date lines at the beginning of deck files
    with open(filename, 'r+', encoding="ISO-8859-1") as f:
        content = f.read()
        f.truncate(0)
        f.seek(0, 0)
        f.write(line.rstrip('\r\n') + '\n' + content)


main()
