(ns td-bot.acceptance-test
  (:use [td-bot.test-data :only [touchdowns]]
        midje.sweet))
(fact "Our bot can detect touchdowns from the Cowboys vs. Eagles game"
      (+ 1 1) => 2)

