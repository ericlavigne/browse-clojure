(ns browse-clojure.core
  (:require [net.cgrand.enlive-html :as enlive-html]
            [clj-http.client :as http]
            [clojure.contrib.json :as json]))

(def fetching-throttle (agent (org.joda.time.DateTime.)))

(defn fetch 
  "Performs HTTP GET on specified URL, guaranteeing less than one GET per second."
  [address]
  (let [result (promise)
        fetcher (fn [previous-time]
                  (let [now (org.joda.time.DateTime.)
                        next-allowed (.plusSeconds previous-time 1)
                        sleep-millis (if (.isAfter now next-allowed)
                                       0 (- (.getMillis next-allowed)
                                            (.getMillis now)))]
                    (when (< 0 sleep-millis)
                      (Thread/sleep sleep-millis))
                    (deliver result (:body (http/get address)))
                    (org.joda.time.DateTime.)))]
    (send-off fetching-throttle fetcher)
    @result))

(defn fetch-clojars-poms []
  (let [pom-list-text (:body (http/get "http://clojars.org/repo/all-poms.txt"))
        pom-relative-addresses (remove #(empty? %)
                                       (.split pom-list-text "\n"))
        pom-absolute-addresses (map #(str "http://clojars.org/repo"
                                          (.substring % 1 (.length %)))
                                    pom-relative-addresses)]
    pom-absolute-addresses))

(defn fetch-json [address]
  (json/read-json (fetch address)))

(defn github-api [& args]
  (fetch-json (str "http://github.com/api/v2/json/"
                   (apply str (interpose "/" args)))))

(defn user-projects [user]
  (:repositories (github-api "repos" "show" user)))

(defn project-contributors [user project]
  (map (fn [contributor] [(:login contributor) (:contributions contributor)])
       (:contributors (github-api "repos" "show" user project "contributors"))))

(defn project-network [user project]
  (:network (github-api "repos" "show" user project "network")))

(defn project-languages [user project]
  (:languages (github-api "repos" "show" user project "languages")))

(defn project-includes-clojure [user project]
  (:Clojure (project-languages user project)))
