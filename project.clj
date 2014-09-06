(defproject random-access-map "1.0.0"
  :description "Retrieve and cut elements from a set based on an index."
  :url "http://djhaskin987.blogspot.com/2013/07/the-rankedset-optimizations-best-friend.html"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.match "0.2.1"]]
  :profiles {:default {:plugins [[lein-cloverage "1.0.2"]]}}
  :aot :all)
