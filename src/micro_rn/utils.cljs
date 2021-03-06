(ns micro-rn.utils
 (:require
   [clojure.string :as string]
   [cljs.core.async :as async :refer [<! >! put! chan timeout]]
   [clojure.walk :as walk]
   [goog.crypt.base64 :as base-64]
   )
 (:require-macros
   [cljs.core.async.macros :refer [go go-loop]]
   )
 )

(enable-console-print!)

(def react-ref (atom nil))

(defn get-react []
  (when-not @react-ref
    (reset! react-ref (js/require "react-native")))
  @react-ref
  )

(defn dash-to-camel
  [s]
  (map string/capitalize (string/split #"\-" s)))

(defn prepare-db-value
  [v]
  (str "\"" v "\"")
  )

(defn cemelify-keys
  [ob]
  (let [f (fn [[k v]] (if (keyword? k) [(dash-to-camel k) v] [k v]))]
    (walk/postwalk (fn [x] (if (map? x) (into {} (map f x)) x)) ob)))

(defn keywordize
  [ob]
  (if (nil? ob)
    ob
    (walk/keywordize-keys (js->clj ob)))
  )

(def prepare-to-clj keywordize)

(defn prepare-to-js
  [m]
  (clj->js (cemelify-keys m))
  )

(defn catch-err [fn]
  (try
    (fn)
    (catch js/Error e (js/console.error " -> " e))))

(defn- create-list-view-ds
  ([] (create-list-view-ds (fn[r1 r2] (not= r1 r2))))
  ([row-compare-fn]
   (let[DataSource (-> (get-react) .-ListView .-DataSource)]
     (DataSource. (clj->js {:rowHasChanged row-compare-fn})))
   )
  )

(defn- convert-to-array [vec]
  (let [arr #js[]]
    (doseq [x vec] (.push arr x))
    arr)
  )

(declare empty-ds)

(defn create-list-model
  ([model row-compare-fn]
   (if model
     (.cloneWithRows (create-list-view-ds row-compare-fn) (convert-to-array model))
     empty-ds))
  ([model]
   (if model
     (.cloneWithRows (create-list-view-ds) (convert-to-array model))
     empty-ds))
  )

(def empty-ds (create-list-model []))

(defn re-quote [s]
  (let [special (set ".?*+^$[]\\(){}|")
        escfn #(if (special %) (str \\ %) %)]
    (apply str (map escfn s))))

(defn re-prepare [value]
  (str ".*"
       (-> value
           re-quote
           string/lower-case
           string/trim) ".*")
  )

(def lazy-id (atom 0))

(defn- clear-lazy []
  (js/clearInterval @lazy-id))

(defn lazy-call
  ([cb] (lazy-call cb 400))
  ([cb idle]
   (clear-lazy)
   (reset! lazy-id
           (js/setInterval
            (fn[]
              (clear-lazy)
              (cb)) idle))))

;; old conv 000

(def do-later-queue (atom []))
(def do-later-interval (atom -1))

(defn- do-next []
  (if-not (empty? @do-later-queue)
    (do
      (let [[[cb t] & tail] @do-later-queue]
        (println "do-next: " (fn? cb) (number? t) (count tail))
        (reset! do-later-queue tail)
        (reset! do-later-interval (js/setInterval do-next t))
        (catch-err cb)
        ))
    (reset! do-later-interval -1)
    )
  )

(defn do-later
  ([cb] (do-later cb 1))
  ([cb t]
   (reset! do-later-queue (concat @do-later-queue [[cb t]]))
   (when (neg? @do-later-interval) (do-next))
   )
  )

;; -----

(defn conv
  ([cb] (conv cb 0))
  ([cb t])
  )

(defn camel-to-dash
  [s]
  (subs
   (apply str
     (map #(if (re-find #"A-Z" %)
             (str "-" (clojure.string/lower-case %))
             %)
          s)) 1))

(defn db-key
  [k]
  ;(if (clojure.string/empty? k)
  ;{}
  {:key (prepare-db-value k)}
  ;)
  )

(defn- two-letters [ds]
  (.. (str "00" ds) (substr -2)))

(defn format-date [date]
  (let[date (if (or (string? date) (number? date)) (js/Date. date) date)]
    (str (.getUTCFullYear date) "-"
         (two-letters (inc (.getUTCMonth date))) "-"
         (two-letters (.getUTCDate date))))
  )

(defn format-time
  [date]
  (let[date (if (string? date) (js/Date. date) date)]
    (str (.getUTCFullYear date) "-"
         (two-letters (inc (.getUTCMonth date))) "-"
         (two-letters (.getUTCDate date)) " "
         (two-letters (.getHours date)) ":"
         (two-letters (.getMinutes date))
         ))
  )

(defn format-money [n]
  (let [n (str n)]
    (if (-> n count (> 3))
      (let [[pre pro] (split-at (- (count n) 3) n)]
        (str (string/join "" pre) "," (string/join "" pro)))
      n))
  ;(rest (f/currency-format n))
  )

(defn get-cache-stamp
  ([] (get-cache-stamp (* 10 60 1000)))
  ([lifetime]
   (-> (js/Date.) (.getTime) (/ lifetime) (js/Math.round)))
  )

(defn await
  [promise]
  (let [port (chan)]
    (-> promise
        (.then (fn [res] (put! port [nil res]))
               (fn [err] (put! port [err nil]))
               ))

    port
    )
  )

(defn fetch
  ([url] (fetch "GET" url))
  ([method url] (fetch method url nil))
  ([method url body]
   (let [credintals (when (re-find #".+:.+@" url) (-> url (string/split "//") (nth 1) (string/split "@") (nth 0)))
         auth-header (when credintals (str "Basic " (base-64/encodeString credintals)))
         headers (merge (if body
                           {"Content-Type" "application/json"}
                           {"Accept" "application/json"})
                         (when credintals {"Authorization" auth-header}))]
     (fetch method url body headers)
     )
   )
  ([method url body headers]
   (let [port (chan)
         settings (merge {:method method
                          :headers headers}
                         (when body {:body (-> body clj->js js/JSON.stringify)}))]
     (go
      (-> (js/fetch url (clj->js settings))
          (.then (fn [res]
                   (.json res)))
          (.then (fn [res]
                   (put! port (let [res (keywordize res)
                                    error-message (:error res)]
                                (if error-message [res nil] [nil res])))))
          (.catch (fn[res] (put! port [(keywordize res) nil]))))
      )
     port
     ))
  )
