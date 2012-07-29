(ns xombie.core
  "Functions wrapping the methods of the primary XOM classes (nu.xom.*).

Functions, unlike methods are first-class. This also gives us a central
place to do all the type hinting."
  (:require [clojure.java.io :as io])
  (:import [nu.xom Attribute Attribute$Type Builder Comment
            DocType Document Element Elements Node NodeFactory
            Nodes ParentNode ProcessingInstruction Serializer
            Text XPathContext]
           [org.xml.sax XMLReader]
           [java.io File InputStream OutputStream Reader StringReader
            ByteArrayOutputStream StringWriter]
           [clojure.lang IPersistentMap]))

;;; ------------------------------------------------------------------------
;;; Protocols
;;; ------------------------------------------------------------------------

(defprotocol ImmutableIndexedList
  (size [self])
  (get-at [self index]))

(defn xom-seq [xom-list]
  (map (partial get-at xom-list) (range (size xom-list))))

(defprotocol Named
  (local-name [self])
  (qualified-name [self])
  (namespace-uri [self])
  (namespace-prefix [self]))

(defprotocol CanSetLocalName
  (set-local-name! [self name]))

(defprotocol CanSetValue
  (set-value! [self ^String content]))


;;; ------------------------------------------------------------------------
;;; NodeFactory
;;; ------------------------------------------------------------------------

(defn node-factory? [nf]
  (instance? NodeFactory nf))

(defn new-node-factory []
  (NodeFactory.))


;;; ------------------------------------------------------------------------
;;; Builder
;;; ------------------------------------------------------------------------

(defn builder? [b]
  (instance? Builder b))

(defn new-builder
  ([^XMLReader parser validate? ^NodeFactory factory]
     (Builder. parser validate? factory))
  ([validate? ^NodeFactory factory]
     (Builder. (boolean validate?) factory))
  ([validate?]
     (Builder. (boolean validate?)))
  ([]
     (Builder.)))

(defmulti build (fn [b in & [base-uri]] (class in)))

(defmethod build File [^Builder b ^File in]
  (.build b in))

(defmethod build InputStream
  ([^Builder b ^InputStream in]
     (.build b in))
  ([^Builder b ^InputStream in ^String base-uri]
     (.build b in base-uri)))

(defmethod build Reader
  ([^Builder b ^Reader in]
     (.build b in))
  ([^Builder b ^Reader in ^String base-uri]
     (.build b in base-uri)))

(defmethod build String
  ([^Builder b ^String in]
     (.build b in nil))
  ([^Builder b ^String in ^String base-uri]
     (.build b in base-uri)))

(defn build-from-system-id [^Builder b ^String sysid]
  (.build b sysid))


;;; ------------------------------------------------------------------------
;;; Node
;;; ------------------------------------------------------------------------

(defn node? [node] (instance? Node node))
(defn copy [^Node node] (.copy node))
(defn detach! [^Node node] (.detach node))
(defn base-uri [^Node node] (.getBaseURI node))
(defn child [^Node node pos] (.getChild node pos))
(defn child-count [^Node node] (.getChildCount node))
(defn document ^Document [^Node node] (.getDocument node))
(defn value [^Node node] (.getValue node))
(defn to-xml [^Node node] (.toXML node))
(defn parent [^Node node] (.getParent node))
(defn query
  ([^Node node ^String xpath]
     (.query node xpath))
  ([^Node node ^String xpath ^XPathContext namespaces]
     (.query node xpath namespaces)))

(extend-type Node
  ImmutableIndexedList
  (size [self] (.getChildCount self))
  (get-at [self index] (.getChild self index)))


;;; ------------------------------------------------------------------------
;;; Nodes
;;; ------------------------------------------------------------------------

(defn nodes? [nodes]
  (instance? Nodes nodes))

(defn new-nodes
  ([] (Nodes.))
  ([^Node node] (Nodes. node)))

(defn append! [^Nodes nodes ^Node node]
  (doto nodes (.append node)))
(defn contains-node? [^Nodes nodes ^Node node]
  (.contains nodes node))
(defn insert! [^Nodes nodes ^Node node index]
  (doto nodes (.insert node index)))
(defn remove! [^Nodes nodes index]
  (.remove nodes index))

(extend-type Nodes
  ImmutableIndexedList
  (size [self] (.size self))
  (get-at [self index] (.get self index)))


;;; ------------------------------------------------------------------------
;;; ParentNode
;;; ------------------------------------------------------------------------

(defn parent-node? [parent]
  (instance? ParentNode parent))

(defn append-child! [^ParentNode parent ^Node child]
  (doto parent (.appendChild child)))
(defn index-of [^ParentNode parent ^Node child]
  (.indexOf parent child))
(defn insert-child! [^ParentNode parent ^Node child index]
  (doto parent (.insertChild child index)))
(defn remove-child-at! [^ParentNode parent index]
  (doto parent (.removeChild (int index))))
(defn remove-child! [^ParentNode parent ^Node child]
  (doto parent (.removeChild child)))
(defn replace-child! [^ParentNode parent ^Node old-child ^Node new-child]
  (doto parent (.replaceChild old-child new-child)))
(defn set-base-uri! [^ParentNode parent ^String uri]
  (doto parent (.setBaseURI parent uri)))


;;; ------------------------------------------------------------------------
;;; Document
;;; ------------------------------------------------------------------------

(defn document? [doc]
  (instance? Document doc))

(defn new-document [^Element root]
  (Document. root))

(defn doc-type [^Document doc]
  (.getDocType doc))
(defn root-element [^Document doc]
  (.getRootElement doc))
(defn set-doc-type! [^Document doc ^DocType type]
  (doto doc (.setDocType type)))
(defn set-root-element! [^Document doc ^Element root]
  (doto doc (.setRootElement root)))


;;; ------------------------------------------------------------------------
;;; DocType
;;; ------------------------------------------------------------------------

(defn doc-type? [dtyp]
  (instance? DocType dtyp))

(defn new-doc-type
  ([^String root-element-name]
     (DocType. root-element-name))
  ([^String root-element-name ^String system-id]
     (DocType. root-element-name system-id))
  ([^String root-element-name ^String system-id ^String public-id]
     (DocType. root-element-name public-id system-id)))

(defn internal-dtd-subset [^DocType dtyp]
  (.getInternalDTDSubset dtyp))
(defn public-id [^DocType dtyp]
  (.getPublicID dtyp))
(defn root-element-name [^DocType dtyp]
  (.getRootElementName dtyp))
(defn system-id [^DocType dtyp]
  (.getSystemID dtyp))
(defn set-internal-dtd-subset! [^DocType dtyp ^String subset]
  (doto dtyp (.setInternalDTDSubset subset)))
(defn set-public-id! [^DocType dtyp ^String id]
  (doto dtyp (.setPublicID id)))
(defn set-root-element-name! [^DocType dtyp ^String name]
  (doto dtyp (.setRootElementName name)))
(defn set-system-id! [^DocType dtyp ^String id]
  (doto dtyp (.setSystemID id)))


;;; ------------------------------------------------------------------------
;;; Element
;;; ------------------------------------------------------------------------

(defn element? [el]
  (instance? Element el))

(defn new-element
  ([^String name]
     (Element. name))
  ([^String name ^String uri]
     (Element. name uri)))

(defn add-attribute! [^Element el ^Attribute a]
  (doto el (.addAttribute a)))
(defn add-namespace-declaration! [^Element el prefix uri]
  (doto el (.addNamespaceDeclaration prefix uri)))
(defn attribute-at [^Element el index]
  (.getAttribute el (int index)))
(defn attribute
  ([^Element el ^String name]
     (.getAttribute el name))
  ([^Element el ^String name ^String ns-uri]
     (.getAttribute el name ns-uri)))
(defn attribute-count [^Element el]
  (.getAttributeCount el))
(defn child-elements
  ([^Element el]
     (.getChildElements el))
  ([^Element el ^String name]
     (.getChildElements el name))
  ([^Element el ^String name ^String ns-uri]
     (.getChildElements el name ns-uri)))
(defn first-child-element
  ([^Element el ^String name]
     (.getFirstChildElement el name))
  ([^Element el ^String name ^String ns-uri]
     (.getFirstChildElement el name ns-uri)))
(defn namespace-declaration-count [^Element el]
  (.getNamespaceDeclarationCount el))
(defn namespace-prefix-at
  ([^Element el index]
     (.getNamespacePrefix el index)))
(defn namespace-uri-for-prefix
  ([^Element el prefix]
     (.getNamespaceURI el prefix)))

(defn remove-attribute! [^Element el ^Attribute a]
  (.removeAttribute el a))
(defn remove-children! [^Element el]
  (.removeChildren el))
(defn remove-namespace-declaration! [^Element el ^String prefix]
  (doto el (.removeNamespaceDeclaration prefix)))
(defn set-namespace-prefix! [^Element el ^String prefix]
  (doto el (.setNamespacePrefix prefix)))
(defn set-namespace-uri! [^Element el ^String uri]
  (doto el (.setNamespaceURI el uri)))

(extend-type Element

  Named
  (local-name [self]
    (.getLocalName self))
  (qualified-name [self]
    (.getQualifiedName self))
  (namespace-uri [self]
    (.getNamespaceURI self))
  (namespace-prefix [self]
    (.getNamespacePrefix self))

  CanSetLocalName
  (set-local-name! [self name]
    (doto self (.setLocalName name))))


;;; ------------------------------------------------------------------------
;;; Elements
;;; ------------------------------------------------------------------------

(defn elements? [els]
  (instance? Elements els))

(extend-type Elements
  ImmutableIndexedList
  (size [self] (.size self))
  (get-at [self index] (.get self index)))


;;; ------------------------------------------------------------------------
;;; Attribute
;;; ------------------------------------------------------------------------

(defn attribute? [a]
  (instance? Attribute a))

(defn new-attribute
  ([^String name ^String value]
     (Attribute. name value))
  ([^String name ^String uri ^String value]
       (Attribute. name uri value)))

(defn attribute-type [^Attribute a]
  (.getType a))
(defn set-namespace! [^Attribute a ^String prefix ^String uri]
  (doto a (.setNamespace prefix uri)))
(defn set-attribute-type! [^Attribute a ^Attribute$Type atyp]
  (doto a (.setType atyp)))

(extend-type Attribute

  Named
  (local-name [self]
    (.getLocalName self))
  (qualified-name [self]
    (.getQualifiedName self))
  (namespace-uri [self]
    (.getNamespaceURI self))
  (namespace-prefix [self]
    (.getNamespacePrefix self))

  CanSetLocalName
  (set-local-name! [self name]
    (doto self (.setLocalName name)))

  CanSetValue
  (set-value! [self ^String content]
    (doto self (.setValue content))))


;;; ------------------------------------------------------------------------
;;; Text
;;; ------------------------------------------------------------------------

(defn text? [t]
  (instance? Text t))

(defn new-text [^String value]
  (Text. value))

(extend-type Text
  CanSetValue
  (set-value! [self ^String content]
    (doto self (.setValue content))))


;;; ------------------------------------------------------------------------
;;; Comment
;;; ------------------------------------------------------------------------

(defn comment? [c]
  (instance? Comment c))

(defn new-comment [^String value]
  (Comment. value))

(extend-type Comment
  CanSetValue
  (set-value! [self ^String content]
    (doto self (.setValue content))))


;;; ------------------------------------------------------------------------
;;; ProcessingInstruction
;;; ------------------------------------------------------------------------

(defn processing-instruction? [pi]
  (instance? ProcessingInstruction pi))

(defn new-processing-instruction [^String value]
  (ProcessingInstruction. value))

(defn target [^ProcessingInstruction pi]
  (.getTarget pi))
(defn set-target! [^ProcessingInstruction pi ^String target]
  (doto pi (.setTarget target)))

(extend-type ProcessingInstruction
  CanSetValue
  (set-value! [self ^String content]
    (doto self (.setValue content))))


;;; ------------------------------------------------------------------------
;;; XPathContext
;;; ------------------------------------------------------------------------

(defn new-xpath-context
  ([] (XPathContext.))
  ([^String prefix ^String uri & more]
     {:pre [(even? (count more))]}
     (let [x (XPathContext. prefix uri)]
       (doseq [[prefix uri] more]
         (.addNamespace x prefix uri))
       x)))

(defn lookup [^XPathContext self ^String prefix]
  (.lookup self prefix))
(defn add-namespace! [^XPathContext self ^String prefix ^String uri]
  (doto self (.addNamespace prefix uri)))
(defn make-namespace-context [^Element el]
  (XPathContext/makeNamespaceContext el))


;;; ------------------------------------------------------------------------
;;; Serializer
;;; ------------------------------------------------------------------------

(defn new-serializer
  ([^OutputStream out]
     (Serializer. out))
  ([^OutputStream out ^String encoding]
     (Serializer. out encoding)))

(defn flush-serializer [^Serializer ser]
  (.flush ser))
(defn encoding [^Serializer ser]
  (.getEncoding ser))
(defn indent [^Serializer ser]
  (.getIndent ser))
(defn line-separator [^Serializer ser]
  (.getLineSeparator ser))
(defn max-length [^Serializer ser]
  (.getMaxLength ser))
(defn preserve-base-uri? [^Serializer ser]
  (.getPreserveBaseURI ser))
(defn unicode-normalization-form-c? [^Serializer ser]
  (.getUnicodeNormalizationFormC ser))
(defn set-indent! [^Serializer ser indent]
  (doto ser (.setIndent indent)))
(defn set-line-separator! [^Serializer ser sep]
  (doto ser (.setLineSeparator sep)))
(defn set-max-length! [^Serializer ser length]
  (doto ser (.setMaxLength length)))
(defn set-preserve-base-uri! [^Serializer ser preserve?]
  (doto ser (.setPreserveBaseURI preserve?)))
(defn set-unicode-normalization-form-c! [^Serializer ser normc?]
  (doto ser (.setUnicodeNormalizationFormC normc?)))
(defn write [^Serializer ser ^Document doc]
  (doto ser (.write doc)))



;;; ------------------------------------------------------------------------
;;; Conveniences for I/O
;;; ------------------------------------------------------------------------

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
                (new-serializer out encoding)
                (new-serializer out))]
      (when indent (set-indent! ser indent))
      (when line-separator (set-line-separator! ser line-separator))
      (when max-line-length (set-max-length! ser max-line-length))
      (set-preserve-base-uri! ser (boolean preserve-base-uri))
      (set-unicode-normalization-form-c! ser (boolean normalize-unicode))
      (write ser doc)
      (flush-serializer ser))))

(defn serialize->string
  [doc & opts]
  (let [{:keys [encoding] :or {encoding "UTF-8"}} opts
        bos (ByteArrayOutputStream.)
        sw (StringWriter.)]
    (apply serialize doc bos opts)
    (io/copy (.toByteArray bos) sw :encoding encoding)
    (.toString sw)))

(defn ^:private xml-buildable [xml]
  (cond (and (string? xml) (re-find #"^\<" xml))  (StringReader. xml)
        (instance? Reader xml)                    xml
        :else                                     (io/input-stream xml)))

(defn ^:private make-builder [node-factory validate xml-reader]
  (let [node-factory (or node-factory (new-node-factory))
        validate     (boolean validate)]
    (if xml-reader
      (new-builder xml-reader validate node-factory)
      (new-builder validate node-factory))))

(defn parse
  [xml & {:keys [base-uri xml-reader node-factory validate builder]}]
  (let [builder (or builder (make-builder node-factory validate xml-reader))]
    (with-open [in (xml-buildable xml)]
      (if base-uri
        (build builder in base-uri)
        (build builder in)))))


