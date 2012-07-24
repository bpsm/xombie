(ns xombie.create
  "Convenient programmatic creation of XOM documents in memory."
  (:require [xombie.core :as xom])
  (:import [nu.xom Element Comment ProcessingInstruction Attribute]
           [clojure.lang IPersistentMap]))

(defn document
  ([root doc-type]
     (doto (xom/new-document root)
       (xom/set-doc-type! doc-type)))
  ([root]
     (xom/new-document root)))


(defn ^:private call-with-tag [tag f]
  {:pre [(or (string? tag)
             (keyword? tag)
             (symbol? tag)
             (and (sequential? tag)
                  (or (string? (first tag))
                      (keyword? (first tag))
                      (symbol? (first tag)))
                  (string? (second tag))))]}
  (if (sequential? tag)
    (f (name (first tag)) (second tag))
    (f (name tag))))

(defmulti ^:private add-to-element
  (fn [el x] (class x)))

(defmethod add-to-element Element [el x]
  (xom/append-child! el x))

(defmethod add-to-element Comment [el x]
  (xom/append-child! el x))

(defmethod add-to-element ProcessingInstruction [el x]
  (xom/append-child! el x))

(defmethod add-to-element Attribute [el x]
  (xom/add-attribute! el x))

(defmethod add-to-element String [el x]
  (xom/append-child! el (xom/new-text x)))

(defn ^:private new-attr
  ([value name] (xom/new-attribute name value))
  ([value name uri] (xom/new-attribute name uri value)))

(defn ^:private attr-map-into-element
  [el attr-map]
  (doseq [[tag value] attr-map]
    (xom/add-attribute! el (call-with-tag tag (partial new-attr value)))))

(defn ^:private ns-map-into-element
  [el ns-map]
  (doseq [[prefix uri] ns-map :let [prefix (name prefix)]]
    (xom/add-namespace-declaration! el prefix uri)))

(defmethod add-to-element IPersistentMap [el x]
  (if (:ns (meta x))
    (ns-map-into-element el x)
    (attr-map-into-element el x)))

(defmethod add-to-element nil [el x]
  el)

(defmethod add-to-element clojure.lang.Sequential [el xs]
  (doseq [x xs]
    (add-to-element el x)))

(defn el [tag & more]
  (doto (call-with-tag tag xom/new-element)
    (add-to-element more)))
