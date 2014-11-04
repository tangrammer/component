(ns com.stuartsierra.component-test
  (:require [clojure.test :refer (deftest is are)]
            [clojure.set :refer (map-invert)]
            [com.stuartsierra.component :as component]
            [bidi.bidi :refer :all]))

(def ^:dynamic *log* nil)

(defn- log [& args]
  (when (thread-bound? #'*log*)
    (set! *log* (conj *log* args))))

(defn- ordering
  "Given an ordered collection of messages, returns a map from the
  head of each message to its index position in the collection."
  [log]
  (into {} (map-indexed (fn [i [message & _]] [message i]) log)))

(defn before?
  "In the collection of messages, does the message beginning with
  symbol a come before the message begging with symbol b?"
  [log sym-a sym-b]
  (let [order (ordering log)]
    (< (get order sym-a) (get order sym-b))))

(defn started? [component]
  (true? (::started? component)))

(defn stopped? [component]
  (false? (::started? component)))

(defrecord ComponentA [state]
  component/Lifecycle
  (start [this]
    (log 'ComponentA.start this)
    (assoc this ::started? true))
  (stop [this]
    (log 'ComponentA.stop this)
    (assoc this ::started? false)))

(defn component-a []
  (->ComponentA (rand-int Integer/MAX_VALUE)))


(defrecord ComponentB [state a]
  component/Lifecycle
  (start [this]
    (log 'ComponentB.start this)
    (assert (started? a))
    (assoc this ::started? true))
  (stop [this]
    (log 'ComponentB.stop this)
    (assert (started? a))
    (assoc this ::started? false))
  component/Listen
  (listen [this]
    " cooo listen-impl"))

(defn component-b []
  (component/using
    (map->ComponentB {:state (rand-int Integer/MAX_VALUE)})
    [:a]))

(defrecord ComponentC [state a b]
  component/Lifecycle
  (start [this]
    (log 'ComponentC.start this)
    (assert (started? a))
;    (assert (started? b))
    (assoc this ::started? true))
  (stop [this]
    (log 'ComponentC.stop this)
    (assert (started? a))
 ;   (assert (started? b))
    (assoc this ::started? false)))

(defn component-c []
  (component/using
    (map->ComponentC {:state (rand-int Integer/MAX_VALUE)})
    [:a :b]))

(defprotocol Whoami
  (who-am-i [_]))


(defrecord ComponentD [state my-c b]
  component/Lifecycle
  (start [this]
    (log 'ComponentD.start this)
 ;   (assert (started? b))
    (assert (started? my-c))
    (assoc this ::started? true))
  (stop [this]
    (log 'ComponentD.stop this)
  ;  (assert (started? b))
    (assert (started? my-c))
    (assoc this ::started? false))
  Whoami
  (who-am-i [this]
    (component/start b)
    )
  )

(defn component-d []
  (map->ComponentD {:state (rand-int Integer/MAX_VALUE)}))

(defrecord ComponentE [state]
  Whoami
  (who-am-i [this]
    (str "working injection " (:state this))
    )

  component/Lifecycle
  (start [this]
    (println "staritng EEEE")
    (log 'ComponentE.start this)
    (assoc this ::started? true))
  (stop [this]
    (log 'ComponentE.stop this)
    (assoc this ::started? false)
  )
  )

(defn component-e []
  (map->ComponentE {:state (rand-int Integer/MAX_VALUE)}))


(def routes-welcome ["" {"com.stuartsierra.component.Listen"
                         {
                          "/listen/this" (fn [& more] (println "logging Listen" more))}
                         "com.stuartsierra.component.Lifecycle"
                         {"" (fn [& more]
                               (println "you're in component/lifecycle :-)" (:function-name (last more)) (first more)))
                          "/start/this" (fn [& more]
                               (println "you're in component/lifecycle :-)" (:function-name (last more))))}}])


(match-route routes-welcome "com.stuartsierra.component.Lifecycle/start/this")
(def s (component/start (vary-meta (apply component/system-map
                                    :a (component-a)
                                    :b (component/wrapped (component-b) :e )
                                    ;;:b  (component-b)
                                    :c (component-c)
                                    :d (component/using (component-d)
                                                        {:b :b
                                                         :my-c :c})
                                    :e (component/using (component-e) [:f])
                                    :f (component-e)
                                    ) assoc-in [:aop-routes ] routes-welcome)))
