import sqlite3
conn = sqlite3.connect(r'C:\Users\tsgil\OneDrive\Documents\VGS - R\arizona-basketball-talent\data\recruiting.db')
cur = conn.cursor()
for n in ['Bootle', 'Chapa', 'Hartzog', 'Tagoa']:
    print(f'--- {n} ---')
    for r in cur.execute(f"SELECT Name, Year, Type, Position, Height, Weight, Ranking FROM recruit_class_football WHERE Name LIKE '%{n}%'"):
        print(' ', r)
    for r in cur.execute(f"SELECT Name, Position, Weight, Class FROM roster_football WHERE Name LIKE '%{n}%' AND School='arizona'"):
        print('  ON ROSTER:', r)
