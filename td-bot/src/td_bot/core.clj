(ns td-bot.core
  (:require [td-bot.bot :as bot]
            [clojure.tools.logging :as log])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (comment (alter-var-root #'*read-eval* (constantly false)))
  (log/info "Starting touchdown bot")
  (bot/main-loop))
