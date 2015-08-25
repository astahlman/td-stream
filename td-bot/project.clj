(defproject td-bot "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.clojure/data.json "0.2.6"]
                 [incanter "1.9.0"]
                 [clj-tokenizer "0.1.0"]
                 [org.clojure/data.csv "0.1.2"]
                 [twitter-streaming-client/twitter-streaming-client "0.3.2" :exclusions [ch.qos.logback/logback-classic
                                                                                         org.slf4j/slf4j-api
                                                                                         org.slf4j/jcl-over-slf4j
                                                                                         org.slf4j/log4j-over-slf4j
                                                                                         org.slf4j/jul-to-slf4j]]
                 ;; metrics
                 [metrics-clojure "2.5.1"]
                 ;; Logging dependencies
                 [org.clojure/tools.logging "0.3.1"]
                 [org.slf4j/slf4j-log4j12 "1.7.1"]
                 [log4j/log4j "1.2.17" :exclusions [javax.mail/mail
                                                    javax.jms/jms
                                                    com.sun.jmdk/jmxtools
                                                    com.sun.jmx/jmxri]]]
  :main td-bot.core)


