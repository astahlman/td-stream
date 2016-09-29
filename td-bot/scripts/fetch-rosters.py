import urllib2, re, csv

def openPage(url):
    "Read a page and clean common HTML encodings"
    content = urllib2.urlopen(url).read()
    return content.replace("&amp;", "&")
    
def getTeamList():
    """
    Returns a list of tuples like:
    (teamId, teamName)
    """
    teamRegEx = re.compile('/players/search\?category=team&filter=([0-9]+)&playerType=current">([^<]+)')
    return teamRegEx.findall(openPage("http://www.nfl.com/players/search?category=team&playerType=current"))

def extractPlayers(html):
    playerRegEx = re.compile('/player/[^\/]+/[0-9]+/profile">([^,]+), ([^<]+)')
    return playerRegEx.findall(html)

def getPlayers(teamId):
    """
    Return a list of tuples representing all the players on the given team, like:
    (playerId, firstName, lastName)
    """
    
    teamPageHtml = openPage('http://www.nfl.com/players/search?category=team&filter=%s&playerType=current' % teamId)
    players = extractPlayers(teamPageHtml)
    
    nextPageRegEx = re.compile('href="([^"]+)">next</a>')
    nextPageUrl = nextPageRegEx.findall(teamPageHtml)
    i = 0
    while len(nextPageUrl) > 0:
        i = i + 1
        print "page %d" % i
        teamPageHtml = openPage('http://www.nfl.com' + nextPageUrl[0])
        players.extend(extractPlayers(teamPageHtml))
        nextPageUrl = nextPageRegEx.findall(teamPageHtml)
    
    return players

def downloadRosters():
    """
    Return all active NFL players via a list of tuples, like:
    (teamName, playerLastName, playerFirstName)
    """
    records = []
    for (teamId, teamName) in getTeamList():
        players = [(teamName,) + player for player in getPlayers(teamId)]
        records.extend(players)
    return records

# Output the current roster to a file named rosters.csv
columnHeaders = ("team_name", "last_name", "first_name")
with open('2016-rosters.csv', 'w') as outfile:
    writer = csv.writer(outfile, delimiter=',')
    writer.writerow(columnHeaders)
    for player in downloadRosters():
        writer.writerow(player)

