(defproject xombie "0.0.1-SNAPSHOT"
  :description "Teaching Clojure to love the XOM"
  :url "http://github.com/bpsm/xombie"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [xom "1.2.5" :exclusions [xml-apis/xml-apis
                                           xerces/xercesImpl
                                           xalan/xalan]]]
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  )
