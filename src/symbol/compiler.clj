(ns symbol.compiler
  (:refer-clojure :exclude [load-file])
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.pprint :as pprint]
            [clojure.walk :as walk]
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

(defn to-fn
  [[_ name args & body :as macro]]
  [name (eval `(fn ~args ~@body))])

(defn expand-form
  [macros form]
  (if-let [f (macros (first form))]
    (let [ex (f form)]
      (cond (identical? ex form) form
            (seq? ex) (expand-form macros ex)
            :else form))))

(defn expand-all
  [macros form]
  (walk/prewalk 
    (fn [x] (if (seq? x) (expand-form macros x) x)) 
    form))

(defn expand-forms
  [ns macros forms]
  (reduce
    (fn [acc form]
      (let [ex (expand-all (merge macros (:macros acc)) form)]
        (if (is-macro? ex) 
          (update-in acc [:macros] assoc (to-fn ex))
          (update-in acc [:forms] conj ex))))                 
    {:ns ns :macros {} :forms []}
    forms))

(def core-forms
  (expand-forms 'symbol.core {} (load-file "symbol/core.clj")))

; TODO take map of namespace content mappings as argument
; TODO use also macros from imported namespaces
(defn get-contents
  [file]
  (let [forms (load-file file)
        namespace (->> forms (filter (is-form? 'ns)) first second)]
    (expand-forms namespace (:macros core-forms) forms)))

; FIXME
(comment (defn compile-files [& files]
  (let [core (get-contents "symbol/core.clj")]
    (doseq [file files]
      (let [{:keys [ns forms macros expanded]} (get-contents core file)]
        (doseq [form expanded]
          (println (meta form))
          (pprint/pprint form)
          (println)))))))