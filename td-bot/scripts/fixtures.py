import csv
import re
import sys
from datetime import timedelta
from dateutil import parser

rows = []
year = sys.argv[1]

with open("../data/nfl-%s-schedule.tsv" % year, 'r') as f:
    reader = csv.reader(f, delimiter='\t')
    for row in reader:
        rows.append(row)

teams = { "Patriots" : "NE",
          "New England" : "NE",
          "Steelers" : "PIT",
          "Pittsburgh" : "PIT",
          "Bears" : "CHI",
          "Chicago" : "CHI",
          "Packers" : "GB",
          "Green Bay" : "GB",
          "Texans" : "HOU",
          "Houston" : "HOU",
          "Chiefs" : "KC",
          "Kansas City" : "KC",
          "Jets" : "NYJ",
          "NY Jets" : "NYJ",
          "Browns" : "CLE",
          "Cleveland" : "CLE",
          "Bills" : "BUF",
          "Buffalo" : "BUF",
          "Colts" : "IND",
          "Indianapolis" : "IND",
          "Redskins" : "WAS",
          "Washington" : "WAS",
          "Dolphins" : "MIA",
          "Miami" : "MIA",
          "Jaguars" : "JAC",
          "Jacksonville" : "JAC",
          "Panthers" : "CAR",
          "Carolina" : "CAR",
          "Rams" : "STL",
          "St. Louis" : "STL",
          "Eagles" : "PHI",
          "Philadelphia" : "PHI",
          "Seahawks" : "SEA",
          "Seattle" : "SEA",
          "Cardinals" : "ARI",
          "Arizona" : "ARI",
          "Saints" : "NO",
          "New Orleans" : "NO",
          "Chargers" : "SD",
          "San Diego" : "SD",
          "Lions" : "DET",
          "Detroit" : "DET",
          "Buccaneers" : "TB",
          "Tampa Bay" : "TB",
          "Titans" : "TEN",
          "Tennessee" : "TEN",
          "Raiders" : "OAK",
          "Oakland" : "OAK",
          "Bengals" : "CIN",
          "Cincinnati" : "CIN",
          "Broncos" : "DEN",
          "Denver" : "DEN",
          "Ravens" : "BAL",
          "Baltimore" : "BAL",
          "Cowboys" : "DAL",
          "Dallas" : "DAL",
          "Giants" : "NYG",
          "NY Giants" : "NYG",
          "49ers" : "SF",
          "San Francisco" : "SF",
          "Falcons" : "ATL",
          "Atlanta" : "ATL",
          "Vikings" : "MIN",
          "Minnesota" : "MIN"}
          
headers = ["week", "home", "away", "start-time"]

def extract_home(row):
    return teams[row[1][1:]] # second column, remove the leading '@'

def extract_away(row):
    return teams[row[0]]

def extract_week(row):
    return int(re.search('\d+', row[2]).group(0))

def extract_start_time(row):
    DATE_COL = 4
    TIME_COL = 5
    # dateutil parser can't handle the time zone, so this takes some hackery
    # DST starts on November 1 in 2015, November 2 in 2014
    utc_offset = -4 if re.search('September|October', row[DATE_COL]) else -5 
    date_str = (row[DATE_COL] + (' %s ' % year) + row[TIME_COL]).strip()
    print date_str
    utc_datetime = parser.parse(date_str) + timedelta(hours=-utc_offset)
    return utc_datetime.isoformat() + 'Z'

result = []
for row in rows[1:]:
    new_row = [extract_week(row), extract_home(row), extract_away(row), extract_start_time(row)]
    result.append(new_row)

with open("/tmp/nfl-schedule-%s.tsv" % year, "wb") as f:
    writer = csv.writer(f, delimiter='\t')
    writer.writerow(headers)
    for row in result:
        writer.writerow(row)

for row in result:
    print row
