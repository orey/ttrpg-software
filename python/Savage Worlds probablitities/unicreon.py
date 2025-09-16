FACES = [4, 6, 8, 10, 12]

DIFF = {
    "facile" : 3,
    "moyen": 4,
    "difficile": 5,
    "très difficile": 6,
    "oulala": 7,
    "improbable": 8,
    "carrément fou":9,
    "impossible": 10
}

#----------------------------------------- Dé
class De():
    def __init__(self, faces):
        self.faces = faces

    def combis(self):
        return [i for i in range(1,self.faces+1)]

    def proba_at_least(self, num):
        if num > self.faces:
            return 0
        elif num == self.faces:
            return 1/self.faces
        else:
            return (self.faces - num + 1) / self.faces
    
    def probas(self, verbose = False):
        for e in DIFF:
            if verbose:
                print(f"Difficulté {e} ({DIFF[e]}) : "
                      + "{:.0%}".format(self.proba_at_least(DIFF[e])))
        return [self.proba_at_least(DIFF[e]) for e in DIFF]

class Duo():
    def __init__(self, faces1, faces2):
        self.faces1 = faces1
        self.faces2 = faces2

    def proba_at_least(self, num):
        count = 0
        for i in range(1,self.faces1+1):
            for j in range(1, self.faces2+1):
                if i>= num or j>= num:
                    count += 1
        return count / (self.faces1 * self.faces2)

    def probas(self):
        return [self.proba_at_least(DIFF[e]) for e in DIFF]



#----------------------------------------- Pretty print
def pprint(label, tab, percentage=True):
    print("{:7}".format(label), end = " | ")
    for e in tab:
        if percentage:
            print("{:3.0%}".format(e), end = " | ")
        else:
            print("{:3}".format(e), end = " | ")
    print("")


#============================================= Main
if __name__ == "__main__":
    d4 = De(4)
    d6 = De(6)
    d8 = De(8)
    d10 = De(10)
    d12 = De(12)
    
    #print(d6.combis())

    pprint("Dif", DIFF.values(), False)
    pprint("d4",d4.probas())
    pprint("d6",d6.probas())
    pprint("d8",d8.probas())
    pprint("d10",d10.probas())
    pprint("d12",d12.probas())

    print("-"*10)
    
    pprint("Dif", DIFF.values(), False)
    pprint("d4/d4",Duo(4,4).probas())
    pprint("d6/d4",Duo(6,4).probas())
    pprint("d8/d4",Duo(8,4).probas())
    pprint("d10/d4",Duo(10,4).probas())
    pprint("d12/d4",Duo(12,4).probas())
                  
    print("-"*10)
    
    pprint("Dif", DIFF.values(), False)
    pprint("d4/d6",Duo(4,6).probas())
    pprint("d6/d6",Duo(6,6).probas())
    pprint("d8/d6",Duo(8,6).probas())
    pprint("d10/d6",Duo(10,6).probas())
    pprint("d12/d6",Duo(12,6).probas())

    print("-"*10)
    
    pprint("Dif", DIFF.values(), False)
    pprint("d4/d8",Duo(4,8).probas())
    pprint("d6/d8",Duo(6,8).probas())
    pprint("d8/d8",Duo(8,8).probas())
    pprint("d10/d8",Duo(10,8).probas())
    pprint("d12/d8",Duo(12,8).probas())

    print("-"*10)
    
    pprint("Dif", DIFF.values(), False)
    pprint("d4/d10",Duo(4,10).probas())
    pprint("d6/d10",Duo(6,10).probas())
    pprint("d8/d10",Duo(8,10).probas())
    pprint("d10/d10",Duo(10,10).probas())
    pprint("d12/d10",Duo(12,10).probas())

    print("-"*10)
    
    pprint("Dif", DIFF.values(), False)
    pprint("d4/d12",Duo(4,12).probas())
    pprint("d6/d12",Duo(6,12).probas())
    pprint("d8/d12",Duo(8,12).probas())
    pprint("d10/d12",Duo(10,12).probas())
    pprint("d12/d12",Duo(12,12).probas())
