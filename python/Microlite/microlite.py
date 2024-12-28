import sys,json
from random import randrange

#-------------------------dice
def die(faces):
    return randrange(faces) + 1

#-------------------------test_dice
def test_dice(therange, faces):
    cumul = 0
    for i in range(therange):
        cumul += die(faces)
    print("Average of " + str(therange)
          + " throws of dice with " + str(faces)
          + ": " + str(cumul/therange))

#=============================Character
def name2filename(name):
    return "C_" + name.replace(' ','_') + ".json"

class Character:
    def __init__(self,name="",STR=0,DEX=0,MIND=0):
        self.name = name
        self.STR = STR
        self.DEX = DEX
        self.MIND = MIND
    def save(self):
        with open(name2filename(self.name),"w") as f: 
            json.dump({
                "name": self.name,
                "STR": self.STR,
                "DEX": self.DEX,
                "MIND": self.MIND
            },f)
    def load(self,filename):
        with open(filename,"r") as f: 
            data = json.load(f)
            self.name = data["name"]
            self.STR = data["STR"]
            self.DEX = data["DEX"]
            self.MIND = data["MIND"]
    def print(self):
        print(self.name)
        print("FOR:  " + str(self.STR))
        print("DEX:  " + str(self.DEX))
        print("MIND: " + str(self.MIND))

def test_character():
    john = Character("John Smart",13, 13, 18)
    john.save()
    john.print()
    john2 = Character()
    john2.load("C_John_Smart.json")
    john2.print()





#---------------------------------------Usage
def usage():
    print("Nothing now")
    sys.exit()
    

#======================================
if __name__ == "__main__":
    if len(sys.argv) != 1:# toujours vrai :)
        print(sys.argv)
        usage()
    test_dice(1000,6)
    test_dice(2000, 20)
    test_character()

    
