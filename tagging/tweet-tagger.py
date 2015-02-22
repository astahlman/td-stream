import nltk
from collections import Counter
import itertools

nltk.data.path.append('/Users/astahlman/Documents/Programming/ML/td-stream/nltk')

tweets = [line.strip() for line in open('pos-data.txt')]
# tweets = ["Touchdown Murray!",
# "TD",
# "Touchdown DeMarco Murray!!!!!",
# "TD #SprayTan #DALvsPHI",
# "TD Murray!!!!!!",
# "TOUCHDOWN",
# "Touchdown DALLAS!",
# "Touchdown !!",
# "Touchdown!",
# "We dem boyzz!!!! #TouchDown #EatADickPhilly",
# "TOUCHDOWN!!!",
# "TD hoe",
# "RT @FishPondTAMU: Touchdown Cowboys!",
# "Cowboys touchdown",
# "TOUCHDOWN",
# "TOUCHDOWN @dallascowboys",
# "Touchdown!",
# "Touchdown.",
# "Touchdown \ud83d\udcaf\ud83d\udc4d",
# "TOUCHDOWN!!!!",
# "Ya touchdown",
# "Touchdown bitch !",
# "\ud83d\ude4c touchdown",
# "TD!!!",
# "Touchdown #CowboysNation",
# "TOUCHDOWN",
# "Touchdown",
# "TOUCHDOWN!!!!",
# "TD #SprayTan \u2b50\ufe0f\ud83d\udc99\ud83c\udfc8 !!!!!!",
# "Touchdown",
# "TOUCHDOWN BABY",
# "Touchdown #SprayTan",
# "touchdown! @DeMarcoMurray #DALvsPHI",
# "TOUCHDOWN HOES",
# "COWBOYS TOUCHDOWN \ud83d\ude4c\ud83d\ude4c\ud83d\ude4c",
# "TD\ud83d\udc99",
# "Touchdown babyyyyyyyy",
# "TD DeMarco!",
# "TOUCHDOWN BABYYYY \ud83d\udc99",
# "Touchdown!",
# "TOUCHDOWN DEMARCO MURRAY!! #DALvsPHI"]


all_tokens = [nltk.word_tokenize(tweet) for tweet in tweets]
all_tags = [nltk.pos_tag(tokens) for tokens in all_tokens]
flat_tags = [tag for tag_list in all_tags for tag in tag_list]
proper_nouns = [tag[0].lower() for tag in flat_tags if (tag[1] == 'NNP' and tag[0].lower() not in ["touchdown", "td"])]
ordered = collections.OrderedDict(sorted(Counter(proper_nouns).items(), key=lambda kv: kv[1], reverse=True))
