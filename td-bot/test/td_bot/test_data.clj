(ns td-bot.test-data)

;; Ground truth from Cowboys vs. Eagles - tweets.2014-12-15.01.json
(def dal-phi-touchdowns
  (map (partial zipmap [:t :player :team])
       [[1418607396778 "demarco murray" "dallas cowboys"]
        [1418608620457 "dez bryant" "dallas cowboys"]
        [1418609918817 "dez bryant" "dallas cowboys"]
        [1418610531857 "chris polk" "philadelphia eagles"]
        [1418614014826 "chris polk" "philadelphia eagles"]
        [1418614584563 "darren sproles" "philadelphia eagles"]
        [1418615124627 "demarco murray" "dallas cowboys"]
        [1418615763277 "dez bryant" "dallas cowboys"]]))

;; Ground truth from our entire test data set
(def ground-truth
  [{:file "data/raw/tweets.2014-12-15.01.json",
    :team "dallas cowboys",
    :player "demarco murray",
    :t 1418607396778}
   {:file "data/raw/tweets.2014-12-15.01.json",
    :team "dallas cowboys",
    :player "dez bryant",
    :t 1418608620457}
   {:file "data/raw/tweets.2014-12-15.01.json",
    :team "dallas cowboys",
    :player "dez bryant",
    :t 1418609918817}
   {:file "data/raw/tweets.2014-12-15.01.json",
    :team "philadelphia eagles",
    :player "chris polk",
    :t 1418610531857}
   {:file "data/raw/tweets.2014-12-15.01.json",
    :team "philadelphia eagles",
    :player "chris polk",
    :t 1418614014826}
   {:file "data/raw/tweets.2014-12-15.01.json",
    :team "philadelphia eagles",
    :player "darren sproles",
    :t 1418614584563}
   {:file "data/raw/tweets.2014-12-15.01.json",
    :team "dallas cowboys",
    :player "demarco murray",
    :t 1418615124627}
   {:file "data/raw/tweets.2014-12-15.01.json",
    :team "dallas cowboys",
    :player "dez bryant",
    :t 1418615763277}
   {:file "data/raw/tweets.2015-01-03.21.json",
    :team "carolina panthers",
    :player "jonathan stewart",
    :t 1420322307784}
   {:file "data/raw/tweets.2015-01-03.21.json",
    :team "arizona cardinals",
    :player "darren fells",
    :t 1420323311546}
   {:file "data/raw/tweets.2015-01-03.21.json",
    :team "arizona cardinals",
    :player "marion grice",
    :t 1420325444064}
   {:file "data/raw/tweets.2015-01-03.21.json",
    :team "carolina panthers",
    :player "fozzy whittaker",
    :t 1420328443799}
   {:file "data/raw/tweets.2015-01-04.00.json",
    :team "baltimore ravens",
    :player "bernard pierce",
    :t 1420336225464}
   {:file "data/raw/tweets.2015-01-04.00.json",
    :team "baltimore ravens",
    :player "torrey smith",
    :t 1420341051666}
   {:file "data/raw/tweets.2015-01-04.00.json",
    :team "pittsburgh steelers",
    :player "antonio brown",
    :t 1420342472537}
   {:file "data/raw/tweets.2015-01-04.00.json",
    :team "pittsburgh steelers",
    :player "martavis bryant",
    :t 1420342780503}
   {:file "data/raw/tweets.2015-01-04.00.json",
    :team "baltimore ravens",
    :player "crockett gillmore",
    :t 1420343794738}
   {:file "data/raw/tweets.2015-01-10.21.json",
    :team "baltimore ravens",
    :player "kamar aiken",
    :t 1420925929770}
   {:file "data/raw/tweets.2015-01-10.21.json",
    :team "carolina panthers",
    :player "steve smith",
    :t 1420927164259}
   {:file "data/raw/tweets.2015-01-10.21.json",
    :team "new england patriots",
    :player "tom brady",
    :t 1420927826867}
   {:file "data/raw/tweets.2015-01-10.21.json",
    :team "new england patriots",
    :player "danny amendola",
    :t 1420930017796}
   {:file "data/raw/tweets.2015-01-10.21.json",
    :team "baltimore ravens",
    :player "owen daniels",
    :t 1420931329205}
   {:file "data/raw/tweets.2015-01-10.21.json",
    :team "baltimore ravens",
    :player "justin forsett",
    :t 1420933189578}
   {:file "data/raw/tweets.2015-01-10.21.json",
    :team "new england patriots",
    :player "rob gronkowski",
    :t 1420933853390}
   {:file "data/raw/tweets.2015-01-10.21.json",
    :team "new england patriots",
    :player "danny amendola",
    :t 1420934675490}
   {:file "data/raw/tweets.2015-01-11.00.json",
    :team "seattle seahawks",
    :player "doug baldwin",
    :t 1420941439960}
   {:file "data/raw/tweets.2015-01-11.00.json",
    :team "carolina panthers",
    :player "kelvin benjamin",
    :t 1420942356604}
   {:file "data/raw/tweets.2015-01-11.00.json",
    :team "seattle seahawks",
    :player "jermaine kearse",
    :t 1420942868721}
   {:file "data/raw/tweets.2015-01-11.00.json",
    :team "seattle seahawks",
    :player "luke willson",
    :t 1420948391051}
   {:file "data/raw/tweets.2015-01-11.00.json",
    :team "seattle seahawks",
    :player "kam chancellor",
    :t 1420948923760}
   {:file "data/raw/tweets.2015-01-11.00.json",
    :team "carolina panthers",
    :player "kelvin benjamin",
    :t 1420949535778}
   {:file "data/raw/tweets.2015-01-11.18.json",
    :team "green bay packers",
    :player "andrew quarless",
    :t 1421000075195}
   {:file "data/raw/tweets.2015-01-11.18.json",
    :team "dallas cowboys",
    :player "tyler clutts",
    :t 1421001163846}
   {:file "data/raw/tweets.2015-01-11.18.json",
    :team "dallas cowboys",
    :player "terrance williams",
    :t 1421002527647}
   {:file "data/raw/tweets.2015-01-18.20.json",
    :team "green bay packers",
    :player "randall cobb",
    :t 1421614079309}
   {:file "data/raw/tweets.2015-01-18.20.json",
    :team "seattle seahawks",
    :player "garry gilliam",
    :t 1421618825996}
   {:file "data/raw/tweets.2015-01-18.20.json",
    :team "seattle seahawks",
    :player "marshawn lynch",
    :t 1421621824353}
   {:file "data/raw/tweets.2015-01-18.20.json",
    :team "seattle seahawks",
    :player "russell wilson",
    :t 1421622090788}
   {:file "data/raw/tweets.2015-01-18.20.json",
    :team "seattle seahawks",
    :player "marshawn lynch",
    :t 1421622554679}
   {:file "data/raw/tweets.2015-01-18.20.json",
    :team "seattle seahawks",
    :player "jermaine kearse",
    :t 1421623760853}
   {:file "data/raw/tweets.2015-01-18.20.json",
    :team "new england patriots",
    :player "legarrette blount",
    :t 1421625677420}
   {:file "data/raw/tweets.2015-01-18.20.json",
    :team "new england patriots",
    :player "james develin",
    :t 1421627195617}
   {:file "data/raw/tweets.2015-01-18.20.json",
    :team "indianapolis colts",
    :player "zurlon tipton",
    :t 1421629101030}
   {:file "data/raw/tweets.2015-01-18.20.json",
    :team "new england patriots",
    :player "nate solder",
    :t 1421632134037}
   {:file "data/raw/tweets.2015-01-18.20.json",
    :team "new england patriots",
    :player "rob gronkowski",
    :t 1421633258404}
   {:file "data/raw/tweets.2015-01-18.20.json",
    :team "new england patriots",
    :player "legarrette blount",
    :t 1421633815193}
   {:file "data/raw/tweets.2015-01-19.02.json",
    :team "new england patriots",
    :player "legarrette blount",
    :t 1421634938146}])
