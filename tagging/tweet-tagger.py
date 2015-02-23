import nltk
import operator
import itertools
import re
from collections import Counter

nltk.data.path.append('/Users/astahlman/Documents/Programming/ML/td-stream/nltk')

tweets = [line.strip() for line in open('pos-data.txt')]

STOP_WORDS = set(["td", "touchdown", "rt", "baby"]).union(nltk.corpus.stopwords.words('english'))
PLAYERS = set([line.strip() for line in open('players.txt')])
NUM_BIGRAMS = 25

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
    candidates = set([p1 + " " + p2 for (c, (p1, p2)) in bigrams])
    matches = candidates.intersection(PLAYERS)
    if len(matches) > 1:
        raise "More than one potential match: %s" % list(matches)
    elif not matches:
        return None
    else:
        return matches.pop()

all_bis = [bigrams(t) for t in tweets]
all_bis = [x for x in all_bis if x]
all_bis = [x for sublist in all_bis for x in sublist]
sorted_bis = sorted(((c, b) for b,c in Counter(all_bis).iteritems()), reverse=True)

scorer = find_player(sorted_bis[:NUM_BIGRAMS]) or "Someone"
print scorer + " scored a touchdown! Hooray!"
