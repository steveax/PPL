(defproject die "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/math.combinatorics "0.1.3"]
                 [incanter "1.2.3-SNAPSHOT"]]
  :main ^:skip-aot die.core

  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
