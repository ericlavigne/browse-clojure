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

(deftest test-github-api
  (is (some #(= "browse-clojure"
                (:name %))
            (user-projects "ericlavigne"))
      "browse-clojure is one of the projects in my GitHub account.")
  (is (some #(= "ericlavigne"
                (first %))
            (project-contributors "ericlavigne" "browse-clojure"))
      "I am one of the contributors to the browse-clojure project.")
  (is (project-includes-clojure "ericlavigne" "browse-clojure")
      "The browse-clojure project contains Clojure code."))

(deftest test-project-files
  (let [bc-proj-files (project-files "ericlavigne" "browse-clojure")]
    (is (= 1 (count bc-proj-files))
        "This project has one project file: project.clj")
    (let [proj-form (read-string (first bc-proj-files))]
      (is (= 'defproject (first proj-form))
          "The project file contains a defproject form.")
      (is (= 'browse-clojure (second proj-form))
          "The project file contains the project name."))))
