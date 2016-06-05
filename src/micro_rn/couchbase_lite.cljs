(ns micro-rn.couchbase-lite
  (:refer-clojure :exclude [get post put delete replicate])
  (:require
    [reagent.core :as r :refer [atom]]
    [clojure.string :as str]
    [clojure.walk :as walk]
    [goog.crypt.base64 :as base-64]
    [micro-rn.utils :as utils]
    [cljs.core.async :as async :refer [<! >! put! chan timeout]]
    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]
    )
  )

(declare document get <? aggregate)

(enable-console-print!)

(defn print-err [cb]
  (try
    (cb)
    (catch js/Error e (js/console.error "create-db error:" e))))

(set! js/React (js/require "react-native"))

(def cbl-light (.-ReactCBLite (js/require "react-native-couchbase-lite")))

(defprotocol IResultBuilder
  (as-one-doc [this])
  (as-list [this])
  (as-docs [this])
  (as-data-source [this])
  (then [this cb])
  (get-atom [this])
  (set-filter [this fn])
  (watch [this [atoms]])
  (success [this])
  (result [this])
  (json [this])
  (refresh [this])
  (aggregate [this aggregate-info])
  (<? [this])
  )

(defprotocol IDataConverter
  (convert [this data])
  )

(def messages-queue (atom (chan)))
(def waiting-queue (atom (chan)))

(go-loop[]
  (let[cb (<! @messages-queue)]
    (cb)
    (<! @waiting-queue)
    (recur)))

(defn do-later [c & a]
  (js/setTimeout #(apply c a) 1))

(defn- add-keys [docs]
  (map (fn[d]
         (let[result (atom d)]
           (aset result "key" (:_id d))
           result)
         ) docs)
  )

(defn mark-last-document [docs]
  (let[first-docs (drop-last docs)
       last-doc (last docs)]
    (if-not (nil? last-doc)
      (concat first-docs [(assoc last-doc :last true)])
      docs)
    )
  )

(defn- get-docs-from-view
  ([data] (get-docs-from-view data #(true)))
  ([data data-filter]
   (if-not data
     []
     (let[docs (mark-last-document (map #(or (:doc %) %) (or (:rows data) [])))]
       (add-keys (filter data-filter docs)))
     )))

(defn- create-list-model [data data-filter]
  (utils/create-list-model (get-docs-from-view data data-filter) (fn[r1 r2] (not= (.-key r1) (.-key r2)))))

(defn add-default-param [params k v]
  (if (contains? params k) params (into {k v} params)))

(defn make-request
  ([method url] (make-request nil method url nil))
  ([db method url] (make-request db method url nil))
  ([db method url body]
   (let [credintals (-> url (str/split "//") (nth 1) (str/split "@") (nth 0))
         auth-header (str "Basic " (base-64/encodeString credintals))
         aggregate-info (atom nil)
         settings (if body
                    {:method method
                     :headers {"Accept" "application/json"
                               "Content-Type" "application/json"
                               "Authorization" auth-header}
                     :body (-> body clj->js js/JSON.stringify)}
                    {:method method
                     :headers {"Authorization" auth-header}})
         ]

     (let[json-doc (atom nil)
          result-atom (atom nil)
          then-fn (atom (fn[json-doc res]))
          data-converter (atom (reify IDataConverter (convert [this data] data)))
          fetch-chan (chan)
          data-filter (atom (fn[d]true))
          success-state (atom nil)
          result-state (atom nil)
          send-data (fn[]
                      (put! @messages-queue
                            (fn[]
                              (println ">>> make request:" method url body)
                              ;(js/console.log "settings:" (clj->js settings))
                              (def t (js/Date.now))
                              (-> (js/fetch url (clj->js settings))
                                  (.then (fn[res]
                                           (.json res)))
                                  (.then (fn[res]
                                           (js/console.log "  [] " url)
                                           (js/console.log "  [db delay]: " (- (js/Date.now) t) res)

                                           (reset! json-doc (walk/keywordize-keys (js->clj res)))
                                           (reset! success-state (and
                                                                  (nil? (:error @json-doc))
                                                                  (let[status (:status @json-doc)]
                                                                    (and
                                                                     (not= status 404)
                                                                     (not= status 403)
                                                                     (not= status 401)
                                                                     ))
                                                                  ))
                                           (reset! result-state res)

                                           ;(js/console.log ">>> success:" @success-state (clj->js @json-doc))

                                           (go
                                            (when (and @success-state @aggregate-info)
                                              (doseq [[k v] @aggregate-info]
                                                (assert (keyword? k))
                                                (let [aggregate-doc
                                                      (fn[doc]
                                                        (let[port (chan)
                                                             id (k doc)]
                                                          (assert (string? id) (str "Invalid aggreggation param - id is not string: " doc k v))
                                                          (go
                                                           (>! port
                                                               (assoc doc k
                                                                      (<! (-> db
                                                                              (document id)
                                                                              (get)
                                                                              (aggregate v) <?))))
                                                           )
                                                          port
                                                          )

                                                        )]
                                                  (reset! json-doc
                                                          (if (:rows @json-doc)
                                                            (assoc @json-doc :rows
                                                                   (loop[[d & t] (:rows @json-doc) result []]
                                                                     (if d
                                                                       (recur t (concat [{:doc (<! (aggregate-doc (:doc d)))}] result))
                                                                       result)
                                                                     )
                                                                   )
                                                            (<! (aggregate-doc @json-doc))
                                                            )))
                                                )
                                              )

                                            (if @success-state
                                              (do
                                                (reset! result-atom (-> @data-converter (convert  @json-doc)))
                                                (>! fetch-chan @result-atom))
                                              (do
                                                (js/console.warn "Error: " method url (clj->js @json-doc))
                                                (>! fetch-chan (-> @data-converter (convert  false)))
                                                ))

                                            (@then-fn @json-doc res)

                                            )
                                           (put! @waiting-queue "ping")
                                           ))
                                  (.catch (fn[res] (js/console.error "Couchbase Error:" res) (put! fetch-chan false) (put! @waiting-queue "ping"))))
                              )))
          ]

       (send-data)

       (reify IResultBuilder
         (watch [this atoms] (doseq [x atoms]
                               (add-watch x :any send-data))
                this)
         (refresh [this] (send-data))
         (as-one-doc
          [this]
          (reset! data-converter
                  (reify IDataConverter
                    (convert [this data]
                             (let[res (get-docs-from-view data @data-filter)]
                               (if (empty? res) false @(-> res (nth 0)))))))
          (reset! result-atom {})
          this)
         (as-list
          [this]
          (reset! data-converter (reify IDataConverter (convert [this data] (map (fn[e] @e) (get-docs-from-view data @data-filter)))))
          (reset! result-atom (convert @data-converter []))
          this)
         (set-filter [this df] (reset! data-filter df) this)
         (as-docs
          [this]
          (reset! data-converter (reify IDataConverter (convert [this data] (get-docs-from-view data @data-filter))))
          (reset! result-atom (convert @data-converter []))
          this)
         (aggregate
          [this info]
          (when-not (nil? info)
            (when (or (map? info) (string? info) (keyword? info))
              (reset! aggregate-info
                      (if (map? info)
                        info (assoc {} (keyword (name info)) nil)))))
          this)
         (as-data-source
          [this]
          (reset! data-converter (reify IDataConverter (convert [this data] (create-list-model data @data-filter))))
          (reset! result-atom (convert @data-converter []))
          this)
         (then [this cb] (reset! then-fn cb) this)
         (success [this] @success-state)
         (result [this] @result-state)
         (json [this] @json-doc)
         (get-atom [this] result-atom)
         (<? [this] fetch-chan)
         )
       )
     )
   )
  )

(defn escape-query-params [k v]
  #_(js/JSON.stringify v)
  (if (or
       (= k :key)
       (= k :keys)
       (= k :startKey)
       (= k :endKey)
       )
    (cond
      (= (boolean v) v) (str v)
      (and (string? v) (not= (first v) "\"")) (str "\"" v "\"")
      (number? v) (str "\"" v "\"")
      (vector? v) (js/JSON.stringify (clj->js v))
      :else (str v))
    (str v))
  )

(defn build-url
  [& parts]
  (let[url (str/join "/"
                     (map (fn[part]
                            (if (string? part)
                              part
                              (str "?" (str/join "&"
                                                 (for [[k v] part]
                                                   (str (name k) "=" (escape-query-params k v))
                                                   ))))) parts))]
    ;(println ">" url)
    url
    )
  )

;; --------  protocols

(defprotocol IRest
  (post [this] [this req-type] [this req-type params] [this req-type params data])
  (get [this] [this req-type] [this req-type params] [this req-type params data])
  (delete [this] [this req-type] [this req-type params] [this req-type params data])
  (put [this] [this req-type] [this req-type params] [this req-type params data])
  )

(defprotocol IServer
  "Server resources enable you to interact with a server that hosts Couchbase Lite databases."
  (init [this] "Init Server")
  (active-stasks [this] "This request retrieves a list of all tasks running on the server.")
  (all-dbs [this] "This request retrives a list of databases on the server.")
  (replicate [this params] "This request starts or cancels a database replication operation.")
  (session [this] "This request retrieves session information.")
  (uuids [this] "This request retrieves a list of the database identifiers.")
  (authentication [this] "Authentication Constructor")
  (database [this database-id])
  )

(defprotocol IDatabase
  "Database resources provide an interface to an entire database."
  (new-document [this document] "This request created new document.")
  (get-all-docs [this params] "type = :GET This request returns a built-in view of all documents in the database.")
  (post-all-docs [this params] "type = :POST This request retrieves specified documents from the database.")
  (bulk-docs [this params] "This request enables you to add, update, or delete multiple documents to a database in a single request.")
  (changes [this params] "This request retrieves a sorted list of changes made to documents in the database, in time order of application, can be obtained from the database’s _changes resource. ")
  (compact [this] "This request compacts the database.")
  (purge [this params] "This request permanently removes references to specified deleted documents from the database.")
  (temp-view [this params] "Executes a temporary view function for all documents and returns the result.")
  (document [this id] "Document Constructor")
  (local-document [this id] "Local Document Constructor")
  (design-document [this id] "Design Document Constructor")
  )

(defprotocol IHasAttachment
  (attachment [this attachment-id] "Attachment Constructor")
  )

(defprotocol IHasView
  (view [this view-name] "View Constructor")
  )

(defprotocol IAuthentication
  "Authentication resources register user authentication information that you receive when users log in to your app via Facebook Login or Mozilla Persona"
  (facebook-token [this options] "Registers a Facebook user access token.")
  (persona_assertion [this options] "Registers a Mozilla Persona assertion.")
  )

;; --------  implementation

(declare Server Database Authentication Document LocalDocument DesignDocument Attachment View)

(defn get-server-url
  [login password port]
  (str "http://" login ":" password "@localhost:" port))

(deftype Server [port login password]

  IRest

  (get [this] (get this (make-request "GET" (build-url (get-server-url login password port)))))

  IServer

  (init [this]
        (println "init coucbase server:" port login password)
        (-> cbl-light (.init port login password (fn[] "[server loaded]"))))
  (active-stasks [this] (make-request "GET" (build-url (get-server-url login password port) "_active_tasks")))
  (all-dbs [this] (make-request "GET" (build-url (get-server-url login password port) "_all_dbs")))
  (replicate [this body] (make-request nil "POST" (build-url (get-server-url login password port) "_replicate") body))
  (session [this] (make-request "GET" (build-url (get-server-url login password port) "_session")))
  (uuids [this] (make-request "GET" (build-url (get-server-url login password port) "_uuids")))
  (authentication [this] (Authentication. (get-server-url login password port)))
  (database [this database-id] (Database. (build-url (get-server-url login password port) database-id)))
  )

(deftype Database [database-url]

  IRest

  (get [this] (make-request this "GET" (build-url database-url)))
  (put [this] (make-request this "PUT" (build-url database-url)))
  (post [this document] (make-request this "POST" (build-url database-url) document))
  (delete [this] (make-request this "DELETE" (build-url database-url)))

  IDatabase

  (get-all-docs [this params] (make-request this "GET" (build-url database-url "_all_docs" params)))
  (post-all-docs [this params] (make-request this "POST" (build-url database-url "_all_docs") params))
  (bulk-docs [this params] (make-request this "POST" (build-url database-url "_bulk_docs") params))
  (changes [this params] (get this (make-request this "GET" (build-url database-url "_changes" params))))
  (compact [this] (make-request this "POST" (build-url database-url "_compact")))
  (purge [this params] (make-request this "POST" (build-url database-url "_purge") params))
  (temp-view [this params] (make-request this "POST" (build-url database-url "_temp_view" params)))
  (document [this id] (Document. this database-url (name id)))
  (local-document [this id] (LocalDocument. this (build-url database-url "_local" (name id))))
  (design-document [this id] (DesignDocument. this (build-url database-url "_design" (name id)))))

(deftype Document [db database-url document-id]

  IRest

  (get [this] (make-request db "GET" (build-url database-url document-id)))
  (post [this document] (make-request db "POST" (build-url database-url) document))
  (post [this params document] (make-request db "POST" (build-url database-url params) document))
  (put [this document] (make-request db "PUT" (build-url database-url document-id) document))
  (put [this params document] (make-request db "PUT" (build-url database-url document-id params) document))
  (delete [this] (make-request db "DELETE" (build-url database-url document-id)))
  (delete [this params] (make-request db "DELETE" (build-url database-url document-id params)))

  IHasAttachment

  (attachment [this attachment-id] (Attachment. db (build-url database-url document-id attachment-id))))

(deftype Attachment [db attachment-url]

  IRest

  (put [this] (make-request db "PUT" (build-url attachment-url)))
  (get [this] (make-request db "GET" (build-url attachment-url)))
  (delete [this] (make-request db "DELETE" (build-url attachment-url))))

(deftype LocalDocument [db local-document-url]

  IRest

  (put [this] (make-request db "PUT" (build-url local-document-url)))
  (get [this] (make-request db "GET" (build-url local-document-url)))
  (delete [this] (make-request db "DELETE" (build-url local-document-url))))

(deftype DesignDocument [db design-document-url]

  IRest

  (put [this params] (make-request db "PUT" (build-url design-document-url) params))
  (get [this] (make-request db "GET" (build-url design-document-url)))
  (delete [this] (make-request db "DELETE" (build-url design-document-url)))

  IHasAttachment

  (attachment [this attachment-id] (Attachment. db (build-url design-document-url attachment-id)))

  IHasView

  (view [this view-name] (View. db (build-url design-document-url "_view" (name view-name)))))

(deftype View [db view-url]
  IRest

  (get [this] (get this {}))
  (get [this params] (make-request db "GET" (build-url view-url (add-default-param params :include_docs true))))
  (post [this params] (make-request db "POST" (build-url view-url) params))
  )

(deftype Authentication [server-url]

  IAuthentication

  (facebook-token [this options] (make-request nil "POST" (build-url server-url "_facebook_token") options))
  (persona_assertion [this options] (make-request nil "POST" (build-url server-url "_persona_assertion") options)))
