(ns browse-clojure.test-core
  (:use [browse-clojure.core] :reload)
  (:use [clojure.test]))

(deftest test-fetch
  (let [start (org.joda.time.DateTime.)
        urls (repeat 5 "http://hello.ericlavigne.net")
        pages (map fetch urls)
        cnt (count pages)
        end (org.joda.time.DateTime.)]
    (is (= 5 cnt) "Fetching five pages.")
    (is (apply = pages) "Those pages are identical.")
    (is (.contains (first pages) "Hello world!") "Each page says 'Hello world!'")
    (is (.isAfter end (.plusSeconds start 4)) "Throttle allows one fetch per second.")))

(deftest test-fetch-clojars-poms
  (is (not-empty (fetch-clojars-poms)) 
      "Clojars lists at least one project."))
