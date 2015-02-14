import json

CONGRESS_NUM = "114"
with open("data/senate.json") as f:
    senate = json.load(f)

votes = [v for v in senate if v["congress"] == CONGRESS_NUM]

senators = {}
bills = []
senator_names = []
for v in votes:
    number = v["number"]
    for k, v in v["data"].items():
        if k not in senators:
            senators[k] = {}
        senators[k][number] = v
        bills.append(number)
        senator_names.append(k)

bills = sorted(list(set(bills)))
senator_names = sorted(list(set(senator_names)))

vote_matrix = [["Name", "Party", "State"] + bills]
for s in senator_names:
    data = s.replace(", ", "")
    name, info = data.split(" ")
    info = info.replace("(", "")
    info = info.replace(")", "")
    party, state = info.split("-")
    row = [name, party, state]
    for b in bills:
        vote = senators[s][b]
        code = "2"
        if vote == "Yea":
            code = "1"
        elif vote == "Nay":
            code = "0"
        row.append(code)
    vote_matrix.append(row)

rows = [",".join(v) for v in vote_matrix]
write_data = "\n".join(rows)
with open("data/{0}_data.csv".format(CONGRESS_NUM), "w+") as f:
    f.write(write_data)

