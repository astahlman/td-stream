import nflgame
import csv

YEAR=2015
WEEKS=range(1,4)

weeks = {}
for i in WEEKS:
    weeks[i] = nflgame.games(YEAR, week=i)

touchdowns = {}
for week_num, week in weeks.iteritems():
    plays = nflgame.combine_plays(week)
    tds = filter(lambda play: play.touchdown, plays)
    def to_record(td):
        record = {}
        record['team'] = td.team
        record['players'] = "+".join([player.name for player in td.players])
        record['description'] = td.desc
        record['reversed'] = "REVERSED" in td.desc
        record['nullified'] = "NULLIFIED" in td.desc
        return record
    touchdowns[week_num] = [to_record(td) for td in tds]


with open('touchdowns.csv', 'wb') as csvfile:
    td_writer = csv.writer(csvfile, delimiter='\t')
    cols = ['team', 'players', 'description', 'reversed', 'nullified']
    td_writer.writerow(cols + ['week'])
    for week, tds in touchdowns.iteritems():
        for td in tds:
            row = [td[col] for col in cols] + [week]
            td_writer.writerow(row)
