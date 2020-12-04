(ns purge-from-clojure.core
  (:require [clojure.tools.reader :as r]))

(defprotocol IntoCssClasses
  (->classes [x]))

(extend-protocol IntoCssClasses
  #?(:cljs string
     :default String)
    (->classes [x] (re-seq #"\S+" x))
  #?(:cljs Keyword
     :default clojure.lang.Keyword)
    (->classes [x] [(name x)])
  #?(:cljs Symbol
     :default clojure.lang.Symbol)
    (->classes [x] [(name x)]))

(def ^:private append!
  #?(:cljs (fn ([] #js []) ([acc] acc) ([acc x] (doto acc (.push x))))
     :default conj))

(defn purge
  [content]
  (transduce (comp ;->
               (filter #(satisfies? IntoCssClasses %))
               (mapcat ->classes)
               (distinct))
             append!
             (tree-seq coll? seq (r/read-string content))))
