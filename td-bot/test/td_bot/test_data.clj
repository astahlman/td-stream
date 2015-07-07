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

;; TODO: Need teams here, too...
;; Ground truth from our entire test data set
(def ground-truth
  [{:t 1418607396778 :player "demarco murray" :file "data/raw/tweets.2014-12-15.01.json"},
   {:t 1418608620457 :player "dez bryant" :file "data/raw/tweets.2014-12-15.01.json"},
   {:t 1418609918817 :player "dez bryant" :file "data/raw/tweets.2014-12-15.01.json"},
   {:t 1418610531857 :player "chris polk" :file "data/raw/tweets.2014-12-15.01.json"},
   {:t 1418614014826 :player "chris polk" :file "data/raw/tweets.2014-12-15.01.json"},
   {:t 1418614584563 :player "darren sproles" :file "data/raw/tweets.2014-12-15.01.json"},
   {:t 1418615124627 :player "demarco murray" :file "data/raw/tweets.2014-12-15.01.json"},
   {:t 1418615763277 :player "dez bryant" :file "data/raw/tweets.2014-12-15.01.json"},
   {:t 1420322307784 :player "jonathan stewart" :file "data/raw/tweets.2015-01-03.21.json"},
   {:t 1420323311546 :player "darren fells" :file "data/raw/tweets.2015-01-03.21.json"},
   {:t 1420325444064 :player "marion grice" :file "data/raw/tweets.2015-01-03.21.json"},
   {:t 1420328443799 :player "fozzy whittaker" :file "data/raw/tweets.2015-01-03.21.json"},
   {:t 1420336225464 :player "bernard pierce" :file "data/raw/tweets.2015-01-04.00.json"},
   {:t 1420341051666 :player "torrey smith" :file "data/raw/tweets.2015-01-04.00.json"},
   {:t 1420342472537 :player "antonio brown" :file "data/raw/tweets.2015-01-04.00.json"},
   {:t 1420342780503 :player "martavis bryant" :file "data/raw/tweets.2015-01-04.00.json"},
   {:t 1420343794738 :player "crockett gillmore" :file "data/raw/tweets.2015-01-04.00.json"},
   {:t 1420925929770 :player "kamar aiken" :file "data/raw/tweets.2015-01-10.21.json"},
   {:t 1420927164259 :player "steve smith" :file "data/raw/tweets.2015-01-10.21.json"},
   {:t 1420927826867 :player "tom brady" :file "data/raw/tweets.2015-01-10.21.json"},
   {:t 1420930017796 :player "danny amendola" :file "data/raw/tweets.2015-01-10.21.json"},
   {:t 1420931329205 :player "owen daniels" :file "data/raw/tweets.2015-01-10.21.json"},
   {:t 1420933189578 :player "justin forsett" :file "data/raw/tweets.2015-01-10.21.json"},
   {:t 1420933853390 :player "rob gronkowski" :file "data/raw/tweets.2015-01-10.21.json"},
   {:t 1420934675490 :player "danny amendola" :file "data/raw/tweets.2015-01-10.21.json"},
   {:t 1420941439960 :player "doug baldwin" :file "data/raw/tweets.2015-01-11.00.json"},
   {:t 1420942356604 :player "kelvin benjamin" :file "data/raw/tweets.2015-01-11.00.json"},
   {:t 1420942868721 :player "jermaine kearse" :file "data/raw/tweets.2015-01-11.00.json"},
   {:t 1420948391051 :player "luke willson" :file "data/raw/tweets.2015-01-11.00.json"},
   {:t 1420948923760 :player "kam chancellor" :file "data/raw/tweets.2015-01-11.00.json"},
   {:t 1420949535778 :player "kelvin benjamin" :file "data/raw/tweets.2015-01-11.00.json"},
   {:t 1421000075195 :player "andrew quarless" :file "data/raw/tweets.2015-01-11.18.json"},
   {:t 1421001163846 :player "tyler clutts" :file "data/raw/tweets.2015-01-11.18.json"},
   {:t 1421002527647 :player "terrance williams" :file "data/raw/tweets.2015-01-11.18.json"},
   {:t 1421614079309 :player "randall cobb" :file "data/raw/tweets.2015-01-18.20.json"},
   {:t 1421618825996 :player "garry gilliam" :file "data/raw/tweets.2015-01-18.20.json"},
   {:t 1421621824353 :player "marshawn lynch" :file "data/raw/tweets.2015-01-18.20.json"},
   {:t 1421622090788 :player "russell wilson" :file "data/raw/tweets.2015-01-18.20.json"},
   {:t 1421622554679 :player "marshawn lynch" :file "data/raw/tweets.2015-01-18.20.json"},
   {:t 1421623760853 :player "jermaine kearse" :file "data/raw/tweets.2015-01-18.20.json"},
   {:t 1421625677420 :player "legarrette blount" :file "data/raw/tweets.2015-01-18.20.json"},
   {:t 1421627195617 :player "james develin" :file "data/raw/tweets.2015-01-18.20.json"},
   {:t 1421629101030 :player "zurlon tipton" :file "data/raw/tweets.2015-01-18.20.json"},
   {:t 1421632134037 :player "nate solder" :file "data/raw/tweets.2015-01-18.20.json"},
   {:t 1421633258404 :player "rob gronkowski" :file "data/raw/tweets.2015-01-18.20.json"},
   {:t 1421633815193 :player "legarrette blount" :file "data/raw/tweets.2015-01-18.20.json"},
   {:t 1421634938146 :player "legarrette blount" :file "data/raw/tweets.2015-01-19.02.json"}])

