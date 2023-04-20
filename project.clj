(defproject dactyl-keyboard "0.1.0-SNAPSHOT"
  :description "A parametrized, split-hand, concave, columnar, erogonomic keyboard"
  :url "http://example.com/FIXME"
  :main dactyl-keyboard.dactyl
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-auto "0.1.3"]
            [lein-exec "0.3.7"]
            [lein-git-deps "0.0.2-SNAPSHOT"]
            [lein-kibit "0.1.8"]]
  :aliases {"generate" ["exec" "-p" "src/dactyl_keyboard/dactyl.clj"]
            "generate-low" ["exec" "-p" "src/dactyl_keyboard/low/dactyl_low.clj"]
            :profiles {:dev {:dependencies [[org.clojure/test.check "1.1.1"]]}}} 
  :dependencies [[org.clojure/clojure "1.11.0"]
                 ;[unicode-math "0.2.0"]
                 ;[uncomplicate/neanderthal "0.44.0"]
                 ;[scad-clj "0.5.4-SNAPSHOT"] 
                 ;[org.bytedeco/mkl-platform-redist "2022.0-1.5.7"]
                 [net.mikera/core.matrix "0.63.0"]
                 [net.mikera/vectorz-clj "0.48.0"]
                 [org.clojure/spec.alpha "0.3.218"]
                 [org.clojure/test.check "1.1.1"]
                 [expresso "0.2.4"]
                 ;[org.clojars.janherich/chisel "0.1.0-SNAPSHOT"]
                 ;[sicmutils "0.22.0"]
                 ]
  :git-dependencies [["https://github.com/farrellm/scad-clj.git"]]
  )


