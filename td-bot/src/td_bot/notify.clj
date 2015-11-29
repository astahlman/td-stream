(ns td-bot.notify
  (:require [amazonica.aws.sns :refer [publish list-topics]]
            [clojure.data.json :as json]))

(def ^:const creds {:profile "td-bot-dev"
                    :endpoint "us-west-2"})

(def ^:const topic-arn "arn:aws:sns:us-west-2:704877052711:prod-all-tds")

(declare sns-publish)

(defn sns-td-hook [{:keys [team happened-at] :as td-alert}]
  (sns-publish (json/write-str td-alert)))

(defn- sns-publish [msg]
  (publish creds
           {:topic-arn topic-arn
            :subject "touchdown-detection"
            :message msg}))


