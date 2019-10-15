import os
#unzip
#rename unzipped folder chickenDinner

def main():
    os.system('mv "chickenDinner/Chicken Dinner /Powered or Unpowered Decklists" .')
    os.rename("Powered or Unpowered Decklists", "pnp")
    os.system('rm pnp/Notes.txt')
    os.system('rm -R chickenDinner')
    os.system('mv pnp/*/*/*/* pnp')
    os.system('mv pnp/*/* pnp')
    os.system('rm -rf pnp/*/')
    os.system('rm pnp/Chicken_*')
    os.system('ls pnp > ls.txt')
    os.system('mv pnp dckFiles')
    os.system('rm ../ls.txt')
    os.system('rm -R ../dckFiles')
    os.system('mv ls.txt ..')
    os.system('mv dckFiles ..')
    os.chdir('/Users/conorhimstead/Desktop/math216/finalProject/data')
    os.system('Python3 deckFileMaker.py')

main()
