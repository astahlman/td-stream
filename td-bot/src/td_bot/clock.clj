(ns td-bot.clock)

(defprotocol Clock
  (tick [this])
  (now [this]))

(deftype SystemClock []
  Clock
  (tick [this] this)
  (now [this] (System/currentTimeMillis)))

(deftype TestClock [t increment]
  Clock
  (tick [this] (TestClock. (+ t increment) increment))
  (now [this] t))
