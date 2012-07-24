(ns xombie.write
  "Convenient serialization of XOM as XML"
  (:require [clojure.java.io :as io]
            [xombie.core :as xom])
  (:import [java.io ByteArrayOutputStream ByteArrayInputStream StringWriter]))

(defn serialize
  [doc out & {:keys [encoding
                     indent
                     line-separator
                     max-line-length
                     preserve-base-uri
                     normalize-unicode]
              :or {encoding          "UTF-8"
                   indent            2
                   line-separator    "\n"
                   max-line-length   78
                   preserve-base-uri false
                   normalize-unicode false}}]
  (with-open [out (io/output-stream out)]
    (let [ser (if encoding
                (xom/new-serializer out encoding)
                (xom/new-serializer out))]
      (when indent (xom/set-indent! ser indent))
      (when line-separator (xom/set-line-separator! ser line-separator))
      (when max-line-length (xom/set-max-length! ser max-line-length))
      (xom/set-preserve-base-uri! ser (boolean preserve-base-uri))
      (xom/set-unicode-normalization-form-c! ser (boolean normalize-unicode))
      (xom/write ser doc)
      (xom/flush-serializer ser))))

(defn serialize->string
  [doc & opts]
  (let [{:keys [encoding] :or {encoding "UTF-8"}} opts
        bos (ByteArrayOutputStream.)
        sw (StringWriter.)]
    (apply serialize doc bos opts)
    (io/copy (.toByteArray bos) sw :encoding encoding)
    (.toString sw)))
