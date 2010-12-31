(ns browse-clojure.core
  (:require [net.cgrand.enlive-html :as enlive-html]
            [clj-http.client :as http]))

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

(defn fetch-clojars-repo-names []
  (let [html-tree (enlive-html/html-resource 
                   (java.net.URL. "http://clojars.org/repo/"))
        link-texts (map enlive-html/text
                        (enlive-html/select html-tree [:td :a]))
        repo-texts (drop 3 
                         (take-while #(not (= % "all-jars.clj"))
                                     link-texts))]
    repo-texts))

(defn fetch-clojars-poms []
  (let [pom-list-text (:body (http/get "http://clojars.org/repo/all-poms.txt"))
        pom-relative-addresses (remove #(empty? %)
                                       (.split pom-list-text "\n"))
        pom-absolute-addresses (map #(str "http://clojars.org/repo"
                                          (.substring % 1 (.length %)))
                                    pom-relative-addresses)]
    pom-absolute-addresses))

