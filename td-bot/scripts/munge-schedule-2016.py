###############################################################
## The input to this script was extracted using TableTools2
## from http://www.fftoday.com/nfl/schedule.php
## 
## The output is stored at data/fixtures/nfl-schedule-2016.tsv
###############################################################

import re
import datetime
from dateutil.tz import gettz

EASTERN_TZ = gettz('US/Eastern')

def parse_time(s):
    t = datetime.datetime.strptime(s, "%Y %b %d %I:%M %p")
    return t.replace(tzinfo=EASTERN_TZ)

def print_row(fields):
    print "\t".join(fields)

with open('2016.tsv', 'r') as f:
    lines = f.read().split("\n")

teams = { "New England Patriots" : "NE",
          "Pittsburgh Steelers" : "PIT",
          "Chicago Bears" : "CHI",
          "Green Bay Packers" : "GB",
          "Houston Texans" : "HOU",
          "Kansas City Chiefs" : "KC",
          "New York Jets" : "NYJ",
          "Cleveland Browns" : "CLE",
          "Buffalo Bills" : "BUF",
          "Indianapolis Colts" : "IND",
          "Washington Redskins" : "WAS",
          "Miami Dolphins" : "MIA",
          "Jacksonville Jaguars" : "JAC",
          "Carolina Panthers" : "CAR",
          "Los Angeles Rams" : "LA",
          "Philadelphia Eagles" : "PHI",
          "Seattle Seahawks" : "SEA",
          "Arizona Cardinals" : "ARI",
          "New Orleans Saints" : "NO",
          "San Diego Chargers" : "SD",
          "Detroit Lions" : "DET",
          "Tampa Bay Buccaneers" : "TB",
          "Tennessee Titans" : "TEN",
          "Oakland Raiders" : "OAK",
          "Cincinnati Bengals" : "CIN",
          "Denver Broncos" : "DEN",
          "Baltimore Ravens" : "BAL",
          "Dallas Cowboys" : "DAL",
          "New York Giants" : "NYG",
          "San Francisco 49ers" : "SF",
          "Atlanta Falcons" : "ATL",
          "Minnesota Vikings" : "MIN"}


cur_week = None
month = None
day = None
utc_offset = None
rows = []
for line in lines:
    if "Home Team" in line:
        next
    elif "Week" in line:
        week_match = re.search("^Week (\d+)", line)
        cur_week = week_match.group(1)
    else:
        date_match = re.search(r"^(Mon|Tue|Wed|Thu|Fri|Sat|Sun) (\w+) (\d+)", line)
        if date_match:
            dow, month, day = date_match.groups()
            year = 2017 if month in ["Jan", "Feb"] else 2016

        (_, time_of_day, home, away) = line.split("\t")
        date_fields = [year, month, day, time_of_day]
        dt = parse_time(" ".join([str(x) for x in date_fields]))
        rows.append([
            cur_week,
            teams.get(home),
            teams.get(away),
            dt.isoformat()])

print_row(['week', 'home', 'away', 'start-time'])
for row in rows:
    print_row(row)
