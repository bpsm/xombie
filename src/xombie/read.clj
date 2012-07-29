(ns xombie.read
  "Convenient parsing of XML to XOM."
  (:require [clojure.java.io :as io]
            [xombie.core :as xom])
  (:import [java.io Reader StringReader]))

(defn ^:private xml-buildable [xml]
  (cond (and (string? xml) (re-find #"^\<" xml))  (StringReader. xml)
        (instance? Reader xml)                    xml
        :else                                     (io/input-stream xml)))

(defn ^:private make-builder [node-factory validate xml-reader]
  (let [node-factory (or node-factory (xom/new-node-factory))
        validate     (boolean validate)]
    (if xml-reader
      (xom/new-builder xml-reader validate node-factory)
      (xom/new-builder validate node-factory))))

(defn parse
  [xml & {:keys [base-uri xml-reader node-factory validate builder]}]
  (let [builder (or builder (make-builder node-factory validate xml-reader))]
    (with-open [in (xml-buildable xml)]
      (if base-uri
        (xom/build builder in base-uri)
        (xom/build builder in)))))

