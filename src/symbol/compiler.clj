(ns symbol.compiler
  (:refer-clojure :exclude [load-file])
  (:use [clojure.walk :only [macroexpand-all]])
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.pprint :as pprint]
            [clojure.zip :as zip]))

(defn load-file [f]
  "load of the forms of the given resource and return them as a vector"
  (let [res (or (io/resource f) (io/as-url (io/as-file f)))]
    (assert res (str "Can't find " f " in classpath"))
    (with-open [r (io/reader res)]
      (let [pbr (clojure.lang.LineNumberingPushbackReader. r)
            eof (java.lang.Object.)]
        (loop [forms []
               r (read pbr false eof false)]
            (if-not (identical? eof r)
              (do
                (recur
                  (conj forms r)
                  (read pbr false eof false)))
              forms))))))

(defn is-form? [& s]
  (let [symbols (set s)]
    (fn [f]
      (and (seq? f) (symbols (first f))))))

(def is-macro? (is-form? 'defmacro))

(defn morph-form [tree pred f]
  (loop [loc (zip/seq-zip tree)]
    (if (zip/end? loc)
      (zip/root loc)
      (recur
        (zip/next
          (if (pred (zip/node loc))
            (zip/replace loc (f (zip/node loc)))
            loc))))))

; TODO improve
(defn expand-macros [namespace macros forms]
  (let [macro-names (map second macros)]     
    (create-ns namespace)
    (binding [*ns* (the-ns namespace)]
      (refer 'clojure.core :exclude (concat macro-names ['defn]))
      (if (not= namespace 'symbol.core)
        (require 'symbol.core))      
      (use 'clojure.contrib.macro-utils)
      (doseq [m macros]
        (eval m)))

    (let [expanded (for [form forms] 
            (morph-form form
                        (apply is-form? macro-names)
                        (fn [f]
                          (binding [*ns* (the-ns namespace)]
                            (macroexpand-all f)))))]
      ;(remove-ns namespace)
      expanded)))

; TODO improve
(defn get-contents
  ([file]
    (get-contents {} file))
  ([parent file]
    (let [forms (load-file file)
          macros (filter is-macro? forms)
          namespace (->> forms (filter (is-form? 'ns)) first second)
          other (remove is-macro? forms)]
      {:ns       namespace
       :forms    forms
       :macros   macros        
       :expanded (expand-macros namespace (concat (:macros parent) macros) other)})))

(defn compile-files [& files]
  (let [core (get-contents "symbol/core.clj")]
    (doseq [file files]
      (let [{:keys [ns forms macros expanded]} (get-contents core file)]
        (doseq [form expanded]
          (println (meta form))
          (pprint/pprint form)
          (println))))))

       ; TODO remove namespaces after run 
