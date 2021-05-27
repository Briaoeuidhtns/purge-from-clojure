(ns purge-from-clojure.core
  (:require
   [clojure.tools.reader :as r]
   [clojure.string :as str]
   [lambdaisland.regal :as regal]))

(defprotocol IntoCssClasses
  (->classes [x]))

;!zprint {:format :skip}
(let [sep              [:+ [:alt \. :whitespace]]
      no-esc           [:* [:not \[ \]]]
      outside-brackets [:lookahead [:cat no-esc
                                    [:* [:cat \[ no-esc \] no-esc]]
                                    no-esc :end]]

      form             [:cat sep outside-brackets]]
  (def split-classes (regal/regex form)))

(extend-protocol IntoCssClasses
  #?(:cljs string
     :default String)
    (->classes [x] (str/split x split-classes))
  #?(:cljs Keyword
     :default clojure.lang.Keyword)
    (->classes [x] (->classes (name x)))
  #?(:cljs Symbol
     :default clojure.lang.Symbol)
    (->classes [x] (->classes (name x))))

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
