(defproject neat "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [rhizome "0.2.0"]
                 [aysylu/loom "0.4.2"]
                 [incanter "1.5.4"]
                 [com.taoensso/nippy "2.5.2"]
                 [seesaw "1.4.4"]]
  :aot [loom.graph]
  :jvm-opts ^:replace []
  :java-source-paths ["java/src"] 
  :main neat.evolution2)
