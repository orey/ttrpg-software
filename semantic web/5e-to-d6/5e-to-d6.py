#-----------------------------------------------------
# footprint RDF database
# O. Rey
# April 28 2024
#-----------------------------------------------------
# dependencies: rdflib
#-----------------------------------------------------

import hashlib, json
from rdflib import Graph, Literal, RDF, URIRef
import urllib.parse
#import os

# Constants
CARAC = "characteristics"

5E = { CARAC : {
    "STR" : "strength",
    "DEX" : "dexterity",
    "CON" : "constitution",
    "INT" : "intelligence",
    "WIS" : "wisdom",
    "CHA" : "charisma"
}}




LIST_OF_ENCRYPTED_FILES="encrypted-file-list.db"
EXTENSION="-encrypted.zip"
TRASH = "_TRASH"
NHI_URI = "https://www.nhindustries.com/ec/1.0/"

EXPORT_DB_TTL = "encrypted-files.ttl"

#++++++++++++++++++++++++++++++++++++++term is cleaning up the bad chars to create terms
def term(s):
    temp = str(s).strip().replace(" ","_").replace('"','_')
    return URIRef(NHI_URI + temp)

#++++++++++++++++++++++++++++++++++++++grammar
# super type
EC = term("EXPORT_CONTROL")

#types
LICENSE = term("LICENSE")
FOOTPRINT = term("FOOTPRINT")
FILE = term("FILE")

#predicates
CONFIRMED = term("CONFIRMED") # filename CONFIRMED ITAR - Note: should be CONFIRMED licence
ITAR = term("ITAR")
NOT_ITAR = term("NOT_ITAR") # filename CONFIRMED NOT_ITAR

SUSPECTED = term("SUSPECTED") # filename SUSPECTED licence

ID = term("ID") # footprint ID filename

PATH = term("PATH") # file PATH Literal(...)
REALNAME = term("REALNAME") # file REALNAME Literal()

#------------------------------------------------initgraph
def initGraph(g):
    g.add((LICENSE, RDF.type, EC))
    g.add((FOOTPRINT, RDF.type, EC))
    g.add((FILE, RDF.type, EC))
    g.add((PATH, RDF.type, EC))
    g.add((ITAR, RDF.type, EC))
    g.add((NOT_ITAR, RDF.type, EC))
    g.add((SUSPECTED, RDF.type, EC))
    g.add((ID, RDF.type, EC))


#--------------------------------ensure folders
def ensureFolder(dire):
    if not os.path.isdir(dire): 
        os.makedirs(dire)

        
#--------------------------------ensure folders
def ensureFile(f):
    if os.path.isfile(f):
        return True
    else:
        return False


#--------------------------------parseLine
def parseLine(db, lina):
    line = lina.strip()
    tokens = line.split('\\')
    #warning the name of the file can be altered but we will save it in Literals
    #same for fullpath
    filename = tokens[-1]
    t_filename = term(filename.replace(EXTENSION, '')) 
    g.add((t_filename, RDF.type, FILE))
    g.add((t_filename, REALNAME, Literal(filename)))
    g.add((t_filename, PATH, Literal(line)))
    if filename.endswith(EXTENSION):
        if len(tokens) == 5:
            # We have an encrypted file, get the footprint
            t_footprint = term(tokens[-3] + tokens[-2])
            t_licence = term(tokens[-4])
            #types
            g.add((t_licence, RDF.type, LICENSE))
            g.add((t_footprint, RDF.type, FOOTPRINT))
            #infos
            g.add((t_filename, SUSPECTED, t_licence))
            g.add((t_footprint, ID, t_filename))
            return True
        elif len(tokens) == 4:
            print("Strange folder => " + line)
            return False
        elif len(tokens) == 3:
            if tokens[-2] == TRASH:
                #infos
                g.add((t_filename, CONFIRMED, NOT_ITAR))
                print("This item has been declassified => " + line)
                return True
            else:
                print("Strange folder => " + line)
                return False
        else:
            print("Strange place for an encrypted file => " + line)
            return False
    else:
        print("Strange: not an encrypted file => " + line)
        return False


#--------------------------------SHA1
def SHA1(msg: str) -> str:
    return hashlib.sha1(msg.encode()).hexdigest()

def countFootprints(g):
    qurey1 ="""
PREFIX ns1: <https://www.nhindustries.com/ec/1.0/> .

SELECT (COUNT(*) AS ?n)
WHERE {
?a rdf:type FOOTPRINT .
?a ns1:ID ?s .
}"""
    query2 = "SELECT (COUNT(*) AS ?n) WHERE {?a  <https://www.nhindustries.com/ec/1.0/ID> ?s .}"
    response2 = g.query(query2)
    for r in response2:
        print("Unique footprints: " + r["n"])

    query3 = "SELECT (COUNT(*) AS ?n) WHERE {?a  rdf:type <https://www.nhindustries.com/ec/1.0/FILE> .}"
    response3 = g.query(query3)
    for r in response3:
        print("Unique files: " + r["n"])
        

    query4b = """
    SELECT ?a (COUNT(?b) AS ?n)
    WHERE {
         ?a  <https://www.nhindustries.com/ec/1.0/PATH> ?b .
    } GROUP BY ?a ORDER BY ASC(?n)
"""
    response4b = g.query(query4b)
    for r in response4b:
        print(r["n"] + " instances of: " + r["a"])


#================================main
if __name__ == "__main__":
    #Graph
    g = Graph()
    initGraph(g)
    with open("encrypted-file-list.db", "r", encoding="iso-8859-1") as f:
        count = 0
        for line in f:
            data = parseLine(g,line)
            count += 1
        print(str(count) + " lines treated")
    g.serialize(destination=EXPORT_DB_TTL)
    countFootprints(g)
    print("Graph serialized to " + EXPORT_DB_TTL)
    





        
