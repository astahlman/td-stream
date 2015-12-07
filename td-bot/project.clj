(defproject td-bot "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/data.json "0.2.6"]
                 [clj-tokenizer "0.1.0"]
                 [org.clojure/data.csv "0.1.3"]
                 [twitter-streaming-client/twitter-streaming-client "0.3.2" :exclusions [ch.qos.logback/logback-classic
                                                                                         org.slf4j/slf4j-api
                                                                                         org.slf4j/jcl-over-slf4j
                                                                                         org.slf4j/log4j-over-slf4j
                                                                                         org.slf4j/jul-to-slf4j]]
                 [clj-time "0.11.0"]
                 [amazonica "0.3.39"]
                 ;; metrics
                 [metrics-clojure "2.5.1" :exclusions [org.slf4j/slf4j-api]]
                 ;; Logging dependencies
                 [org.clojure/tools.logging "0.3.1"]
                 [org.slf4j/slf4j-log4j12 "1.7.1"]
                 [log4j/log4j "1.2.17" :exclusions [javax.mail/mail
                                                    javax.jms/jms
                                                    com.sun.jmdk/jmxtools
                                                    com.sun.jmx/jmxri]]]

  :java-agents [[com.newrelic.agent.java/newrelic-agent "3.20.0"]]
  :test-selectors {:default (complement :integration)
                   :integration :integration
                   :all (constantly true)}
  :profiles {:dev {:jvm-opts ["-Dlog4j.configuration=log4j-dev.properties"]
                   :dependencies [[incanter "1.9.0" :exclusions [commons-codec org.clojure/tools.macro]]]}}
  :test-paths ["src" "test"]
  :pedantic? :warn
  :bootclasspath true
  :repl-options {:timeout 90000}
  :main td-bot.core)


