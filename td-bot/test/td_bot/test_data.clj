(ns td-bot.test-data)

;; Ground truth from sample data - tweets.2014-12-15.01.json
(def touchdowns
  (map (partial zipmap [:t :player])
       [[1418607396778 "demarco murray"]
        [1418608620457 "dez bryant"]
        [1418609918817 "dez bryant"]
        [1418610531857 "chris polk"]
        [1418614014826 "chris polk"]
        [1418614584563 "darren sproles"]
        [1418615124627 "demarco murray"]
        [1418615763277 "dez bryant"]]))
