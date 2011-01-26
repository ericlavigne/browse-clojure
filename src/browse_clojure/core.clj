(ns browse-clojure.core
  (:require [net.cgrand.enlive-html :as enlive-html]
            [clj-http.client :as http]
            [clojure.contrib.json :as json]
            clojure.set)
  (:use [clojure.contrib.seq :only (positions)]))

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

(defn github-raw [& args]
  (fetch (str "http://github.com/api/v2/json/"
              (apply str (interpose "/" args)))))

(defn github-api [& args]
  (json/read-json (apply github-raw args)))

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

(defn project-file-listing [user project]
  (:blobs (github-api "blob/all" user project "master")))

(defn github-file [user project sha]
  (github-raw "blob/show" user project sha))

(defn project-files [user project]
  (let [file-listing (project-file-listing user project)
        lein-filenames (filter #(.contains (name %) "project.clj")
                               (keys file-listing))
        lein-files (map #(github-file user project (% file-listing))
                        lein-filenames)]
    lein-files))

(def initial-network
     {:programmers #{"clojure"}
      :programmers-new-gen #{"clojure"}
      :programmers-by-gen [#{"clojure"}]
      :projects {}})
                                        ; {project-name {:owner
					;                :total-contributions
					;                :contributions
					;                :name
					;                :all-owners}

(defn merge-projects [proj1 proj2]
  (let [main-proj (if (< (:total-contributions proj1)
                         (:total-contributions proj2))
                    proj2
                    proj1)]
    {:owner (:owner main-proj)
     :total-contributions (:total-contributions main-proj)
     :contributions (:contributions main-proj)
     :name (:name main-proj)
     :all-owners (clojure.set/union (:all-owners proj1)
                                    (:all-owners proj2))}))

(defn advance-generation [old-generation]
  (let [new-gen-projects (distinct (mapcat (fn [user]
                                             (map (fn [proj]
                                                    {:name (:name proj)
                                                     :owner (:owner proj)})
                                                  (user-projects user)))
                                           (:programmers-new-gen old-generation)))
        discovered-projects (remove (fn [proj] (and (contains? (:projects old-generation)
                                                               (:name proj))
                                                    (contains? (:all-owners ((:projects old-generation)
                                                                               (:name proj)))
                                                               (:owner proj))))
                                    new-gen-projects)
        discovered-clojure-projects (filter (fn [proj] (project-includes-clojure (:owner proj) (:name proj)))
                                            discovered-projects)
        projects-with-contributions (map (fn [proj]
                                           (let [owner (:owner proj)
                                                 proj-name (:name proj)
                                                 contributions (project-contributors owner proj-name)
                                                 contributors (map first contributions)
                                                 contribution-amounts (map second contributions)]
                                             {:owner owner
                                              :name proj-name
                                              :all-owners #{owner}
                                              :total-contributions (reduce + 0 contribution-amounts)
                                              :contributions (zipmap contributors
                                                                     contribution-amounts)}))
                                         discovered-clojure-projects)
        projects-with-enough-contributors (filter (fn [proj] (< 1 (count (:contributions proj))))
                                                  projects-with-contributions)
        next-gen-programmers (clojure.set/difference (set (mapcat (fn [proj] (keys (:contributions proj)))
                                                                  projects-with-enough-contributors))
                                                     (:programmers old-generation))
        new-project-map (reduce (fn [proj-map new-proj]
                                  (merge-with merge-projects
                                              proj-map
                                              {(:name new-proj)
                                               new-proj}))
                                (:projects old-generation)
                                projects-with-enough-contributors)]
    {:programmers (clojure.set/union (:programmers old-generation)
                                     next-gen-programmers)
     :programmers-new-gen next-gen-programmers
     :programmers-by-gen (conj (:programmers-by-gen old-generation)
                               next-gen-programmers)
     :projects new-project-map}))

(def company-affiliations
     {"Relevance" #{}
      "Sonian" #{}})

(defn snowball-generation [programmer generations]
  (first (positions #(% programmer) generations)))

(defn contribution-levels [contributions]
  (zipmap (map first (reverse (sort-by second contributions)))
          (concat [5 4 3 2] (repeat (count contributions) 1))))

(defn convert-to-vna [network]
  (let [sb (StringBuilder.)]
    (.append sb "*Node data\r\n")
    (.append sb "ID, role, snowball_generation\r\n")
    (doseq [programmer (:programmers network)]
      (.append sb (str " " programmer " programmer "
                       (snowball-generation programmer
                                            (:programmers-by-gen network))
                       "\r\n")))
    (doseq [project (vals (:projects network))]
      (.append sb (str " _" (:name project) " project "
                       (snowball-generation (:owner project)
                                            (:programmers-by-gen network))
                       "\r\n")))
    (.append sb "*Tie data\r\n")
    (.append sb "from to contribution\r\n")
    (doseq [project (vals (:projects network))]
      (let [levels (contribution-levels (:contributions project))]
        (doseq [programmer (keys levels)]
          (.append sb (str programmer " _" (:name project) " "
                           (levels programmer) "\r\n")))))
    (.toString sb)))
