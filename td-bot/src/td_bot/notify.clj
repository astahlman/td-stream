(ns td-bot.notify
  (:require [amazonica.aws.sns :refer [publish list-topics]]
            [clojure.tools.logging :as log]
            [clojure.data.json :as json]))

(def ^:const creds {:endpoint "us-west-2"})
(def ^:const topic-arn "arn:aws:sns:us-west-2:704877052711:prod-all-tds")

(declare sns-publish)

(defn sns-td-hook [{:keys [team happened-at] :as td-alert}]
  (try
    (sns-publish (json/write-str td-alert))
    (catch Exception e
      (log/error e))))

(defn- sns-publish [msg]
  (publish creds
           {:topic-arn topic-arn
            :subject "touchdown-detection"
            :message msg}))


