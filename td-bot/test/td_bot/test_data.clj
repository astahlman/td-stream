(ns td-bot.test-data)

;; Ground truth from sample data - tweets.2014-12-15.01.json
(def touchdowns
  (map (partial zipmap [:t :player :team])
       [[1418607396778 "demarco murray" "dallas cowboys"]
        [1418608620457 "dez bryant" "dallas cowboys"]
        [1418609918817 "dez bryant" "dallas cowboys"]
        [1418610531857 "chris polk" "philadelphia eagles"]
        [1418614014826 "chris polk" "philadelphia eagles"]
        [1418614584563 "darren sproles" "philadelphia eagles"]
        [1418615124627 "demarco murray" "dallas cowboys"]
        [1418615763277 "dez bryant" "dallas cowboys"]]))
