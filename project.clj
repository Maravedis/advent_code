(defproject advent_of_code "1.0.0"
  :description "Advent of code solves"
  :dependencies [[org.clojure/clojure "1.12.0"]
                 [org.clojure/core.match "1.1.0"]
                 [org.clojure/math.combinatorics "0.3.0"]
                 [com.rpl/specter "1.1.4"]
                 [criterium "0.4.6"]
                 [org.clojure/core.rrb-vector "0.2.0"]
                 [org.clojure/math.numeric-tower "0.1.0"]
                 [clj-http "3.13.0"]
                 [clojure.java-time "1.4.3"]
                 [cheshire "5.13.0"]
                 [org.clj-commons/digest "1.4.100"]
                 [tesser.core "1.0.6"]
                 [org.clojure/data.priority-map "1.2.0"]
                 [net.mikera/core.matrix "0.63.0"]
                 [net.mikera/vectorz-clj "0.48.0"]
                 [com.microsoft/z3 "4.15.4"]
                 [org.jgrapht/jgrapht-core "1.5.2"]
                 [dom-top "1.0.9"]]
  :resource-paths ["resources" "inputs"]
  :jvm-opts ["-Xss1G" "-Djdk.attach.allowAttachSelf"]
  :profiles {:dev {:source-paths   ["test" "dev"]
                   :resource-paths ["test-inputs"]}})

; To install z3:
; clone https://github.com/Z3Prover/z3
; run: `python scripts/mk_make.py --java`, then `cd build; make`, then `sudo make install`
; then install into local maven:
; mvn install:install-file -Dfile=/usr/lib/com.microsoft.z3.jar -DgroupId=com.microsoft -DartifactId=z3 -Dversion=4.15.4 -Dpackaging=jar -DgeneratePom=true
