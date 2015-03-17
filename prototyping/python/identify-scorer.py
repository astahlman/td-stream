import nltk
import operator
import itertools
import re
import time
import sys
from collections import Counter
from os import system

nltk.data.path.append('nltk/')

STOP_WORDS = set(["td", "touchdown", "rt","baby"]).union(nltk.corpus.stopwords.words('english'))
PLAYERS = set([line.strip() for line in open('../../data/players.txt')])
TEAMS = set([line.strip().split()[-1] for line in open('../../data/teams.txt')])

NUM_BIGRAMS = 15

def normalize(word):
    return re.sub("['-.]", "", word.lower())

def bigrams(tweet):
    words = nltk.word_tokenize(tweet)
    words = [normalize(word) for word in words]
    words = [word for word in words if re.match('^[\w-]+$', word) is not None]
    words = [word for word in words if word not in STOP_WORDS]
    words = [x for x in words if x]
    nextword = iter(words)
    next(nextword, None)
    bigram = itertools.izip(words,nextword)
    return list(bigram)

def find_player(bigrams):
    candidates = set([p1 + " " + p2 for ((p1, p2), c) in bigrams])
    matches = candidates.intersection(PLAYERS)
    if len(matches) > 1:
        raise "More than one potential match: %s" % list(matches)
    elif not matches:
        return None
    else:
        return matches.pop()

def find_team(words):
    matches = set(words).intersection(TEAMS)
    if len(matches) > 1:
        raise "More than one potential match: %s" % list(matches)
    elif not matches:
        return None
    else:
        return matches.pop()

def generate_alert(tweets):
    all_bis = [bigrams(t) for t in tweets]
    all_bis = [x for x in all_bis if x]
    all_bis = [x for sublist in all_bis for x in sublist]
    sorted_bis = Counter(all_bis).most_common(10)
    top_words = [word[0] for word in Counter([x for tup in all_bis for x in tup]).most_common(10)]
    team = find_team(top_words)

    scorer = find_player(sorted_bis[:NUM_BIGRAMS]) or "Someone"
    td_alert = "%s scored a touchdown for the %s! Hooray!" % (scorer, team)
    return td_alert

if __name__ == "__main__":
    f = sys.argv[1] if len(sys.argv) > 1 else "../../data/demarco-td.txt"
    start = time.time()
    tweets = [line.strip() for line in open(f)]
    print "Read tweets in: %f seconds" % (time.time() - start)
    alert = generate_alert(tweets)
    print "Finished in: %f seconds" % (time.time() - start)
    print alert
    #system("say " + alert)

