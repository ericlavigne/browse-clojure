(ns browse-clojure.core
  (:require [net.cgrand.enlive-html :as enlive-html]))

(defn fetch-clojars-repo-names []
  (let [html-tree (enlive-html/html-resource 
		   (java.net.URL. "http://clojars.org/repo/"))
	link-texts (map enlive-html/text
			(enlive-html/select html-tree [:td :a]))
	repo-texts (drop 3 
			 (take-while #(not (= % "all-jars.clj"))
				     link-texts))]
    repo-texts))

