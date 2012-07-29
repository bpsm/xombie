(ns xombie.pxml
  "Presistent XML. Experiments with representing XML using Clojure's
persistent data structures."
  (:require [xombie.core :as xom])
  (:import [nu.xom Text Element Comment ProcessingInstruction Attribute
            Document]
           [clojure.lang IPersistentMap]))

;;; Inspired by the style of of 'Fuller XML support':
;;; <http://dev.clojure.org/display/DXML/Fuller+XML+support>

(defmulti xom->pxml class)

(defmethod xom->pxml Text
  [node]
  (xom/value node))


(defn make-doc-type-map [doctype]
  {:public-id (xom/public-id doctype)
   :system-id (xom/system-id doctype)
   :root-element-name (xom/root-element-name doctype)
   :internal-dtd-subset (xom/internal-dtd-subset doctype)})

(defmethod xom->pxml Document
  [doc]
  (vary-meta (xom->pxml (xom/root-element doc))
             assoc :doc-type (make-doc-type-map (xom/doc-type doc))))

(defmethod xom->pxml Comment
  [node]
  {:comment (xom/value node)})

(defmethod xom->pxml ProcessingInstruction
  [node]
  {:processing-instruction (xom/value node)})

(defn make-element-content-seq [el]
  (map xom->pxml (xom/xom-seq el)))

(defn make-element-attr-map [el ns-map]
  (let [n (xom/attribute-count el)
        as (map (partial xom/attribute-at el) (range n))]
    (into {}
            (for [a as :let [nsuri (xom/namespace-uri a)
                             name (keyword (xom/local-name a))]]
              {(if (empty? nsuri)
                 name
                 (with-meta
                   [name nsuri]
                   {:ns ns-map
                    :prefix (xom/namespace-prefix a)}))
               (xom/value a)}))))

(defn make-element-tag [el]
  (keyword (xom/local-name el)))

(defn make-element-ns-map [el]
  (let [n (xom/namespace-declaration-count el)
        ps (map (partial xom/namespace-prefix-at el) (range n))
        us (map (partial xom/namespace-uri-for-prefix el) ps)]
    (into {} (map #(do {%1 %2}) ps us))))

(defn make-element-meta-map [el ns-map]
  {:ns ns-map
   :prefix (xom/namespace-prefix el)})

(defn make-element-map [el ns-map]
  {:tag (make-element-tag el)
   :uri (xom/namespace-uri el)
   :attrs (make-element-attr-map el ns-map)
   :content (make-element-content-seq el)})

(defmethod xom->pxml Element
  [el]
  (let [ns-map (make-element-ns-map el)]
    (with-meta
      (make-element-map el ns-map)
      (make-element-meta-map el ns-map))))

(defn pxml-kind [x]
  (cond (string? x)                           :text
        (contains? (meta x) :doc-type)        :document
        (contains? x :tag)                    :element
        (contains? x :comment)                :comment
        (contains? x :processing-instruction) :processing-instruction))

(defmulti pxml->xom pxml-kind)

(defmethod pxml->xom :text
  [x]
  (xom/new-text x))

(defmethod pxml->xom :comment
  [x]
  (xom/new-comment (:comment x)))

(defmethod pxml->xom :processing-instruction
  [x]
  (xom/new-processing-instruction (:processing-instruction x)))

(defn pxml-doc-type->xom [x]
  (doto (xom/new-doc-type (:root-element-name x)
                          (:system-id x)
                          (:public-id x))
    (xom/set-internal-dtd-subset! (:internal-dtd-subset x))))

(defmethod pxml->xom :document
  [x]
  (doto (xom/new-document (pxml->xom (vary-meta x dissoc :doc-type)))
    (xom/set-doc-type! (-> x meta :doc-type pxml-doc-type->xom))))

(defmethod pxml->xom :element
  [x]
  (let [{:keys [tag uri attrs content]} x
        el (xom/new-element (name tag) uri)]
    (doseq [[k v] attrs]
      (let [attr (if (keyword? k)
                   (xom/new-attribute (name k) v)
                   (let [[local-name uri] k
                         {ns-map :ns, prefix :prefix} (meta k)
                         qname (str prefix ":" (name local-name))]
                     (xom/new-attribute qname uri v)))]
        (xom/add-attribute! el attr)))
    (doseq [child content]
      (xom/append-child! el (pxml->xom child)))
    el))

