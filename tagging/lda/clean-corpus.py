import nltk
import re

STOP_WORDS = nltk.corpus.stopwords.words('english')
OUT_FILE = open("corpus.txt", "w")
IN_FILE = "pos-data.txt"

tweets = [line.strip() for line in open(IN_FILE)]

for tweet in tweets:
    words = [word.replace('"', '').lower() for word in tweet.split()]
    words = [word.lower() for word in words if re.match('^[\w-]+$', word) is not None]
    words = [word for word in words if word not in STOP_WORDS]
    if words:
        OUT_FILE.write(" ".join(words) + "\n") 

OUT_FILE.close()



