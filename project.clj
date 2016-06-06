(defproject potapenko/micro-rn "1.0.4"
  :description "Components for reagent/react-native projects - reagent wrappers, styles, utilites, coucbase-lite REST api helpers"
  :url "http://github.com/potapenko/micro-rn"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :min-lein-version "2.6.1"

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.8.51"]
                 [org.clojure/core.async "0.2.374"
                  :exclusions [org.clojure/tools.reader]]
                 [reagent "0.6.0-alpha"]]

  :plugins [[lein-figwheel "0.5.3-2"]
            [lein-cljsbuild "1.1.3" :exclusions [[org.clojure/clojure]]]]

  :source-paths ["src"]

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]

  :cljsbuild {:builds
              [{:id "dev"
                :source-paths ["src"]

                ;; If no code is to be run, set :figwheel true for continued automagical reloading
                :figwheel {:on-jsload "micro-rn.core/on-js-reload"}

                :compiler {:main micro-rn.core
                           :asset-path "js/compiled/out"
                           :output-to "resources/public/js/compiled/micro_rn.js"
                           :output-dir "resources/public/js/compiled/out"
                           :source-map-timestamp true}}

               ]}

  :figwheel {}

  :profiles {:dev {:dependencies [[figwheel-sidecar "0.5.3-2"]
                                  [com.cemerick/piggieback "0.2.1"]]
                   ;; need to add dev source path here to get user.clj loaded
                   :source-paths ["src" "dev"]
                   ;; for CIDER
                   ;; :plugins [[cider/cider-nrepl "0.12.0"]]
                   :repl-options {; for nREPL dev you really need to limit output
                                  :init (set! *print-length* 50)
                                  :nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}}

)
