(ns fominok.ideahelix.search
  (:import (com.intellij.openapi.editor Editor)
           (com.intellij.openapi.fileEditor FileEditorManager)
           (com.intellij.openapi.ui.popup JBPopupFactory)
           (com.intellij.psi.codeStyle NameUtil)
           (com.intellij.psi.search FilenameIndex GlobalSearchScope)
           (com.intellij.ui.components JBList JBScrollPane)
           (java.awt BorderLayout Dimension)
           (java.awt.event ActionEvent)
           (javax.swing AbstractAction DefaultListModel JPanel JTextField KeyStroke ListSelectionModel)
           (javax.swing.event DocumentListener)))


(defn filter-list [^JTextField input ^JBList list ^DefaultListModel model files]
  (let [query (.toLowerCase (.trim (.getText input)))
        matcher (.. (NameUtil/buildMatcher (str "*" query)) build)
        scored-items (map (fn [[path _]]
                            {:score (.matchingDegree matcher path)
                             :path  path})
                          files)
        filtered (->> scored-items
                      (sort-by :score >)
                      (take 20)
                      (map :path))]
    (.clear model)
    (doseq [item filtered]
      (.addElement model item))
    (when-not (empty? filtered)
      (.setSelectedIndex list 0))))


(defn search-file-name [project ^Editor parent]
  (let [files (into {} (keep (fn [name] (when-let [file (first (FilenameIndex/getVirtualFilesByName name (GlobalSearchScope/projectScope project)))]
                                           [(.getPath file) file]))
                              (FilenameIndex/getAllFilenames project)))
        model (doto (DefaultListModel.)
                (as-> m
                      (doseq [[path _] files] (.addElement m path))))
        list (doto (JBList. model)
               (.setSelectionMode ListSelectionModel/SINGLE_SELECTION)
               (.setSelectedIndex 0))
        input (JTextField.)
        panel (doto (JPanel. (BorderLayout. 0 5))
                (.add input BorderLayout/NORTH)
                (.add (JBScrollPane. list) BorderLayout/CENTER)
                (.setPreferredSize (Dimension. 600 400)))
        popup (-> (JBPopupFactory/getInstance)
                  (.createComponentPopupBuilder panel input)
                  (.setRequestFocus true)
                  (.setTitle "Find File")
                  (.setMovable true)
                  (.setResizable true)
                  (.createPopup))]

    (doto (.getInputMap input)
      (.put (KeyStroke/getKeyStroke "control N") "selectNext")
      (.put (KeyStroke/getKeyStroke "control P") "selectPrevious"))
    (doto (.getActionMap input)
      (.put "selectNext"
            (proxy [AbstractAction] []
              (actionPerformed [^ActionEvent _]
                (let [i (min (inc (.getSelectedIndex list)) (dec (.. list (getModel) (getSize))))]
                  (.setSelectedIndex list i)
                  (.ensureIndexIsVisible list i)))))
      (.put "selectPrevious"
            (proxy [AbstractAction] []
              (actionPerformed [^ActionEvent _]
                (let [i (max (dec (.getSelectedIndex list)) 0)]
                  (.setSelectedIndex list i)
                  (.ensureIndexIsVisible list i))))))

    (.addActionListener input
                        (proxy [java.awt.event.ActionListener] []
                          (actionPerformed [^ActionEvent _]
                            (let [selected (.getSelectedValue list)]
                              (.cancel popup)
                              (when-let [file (get files selected)]
                                (.openFile (FileEditorManager/getInstance project) file true))))))

    (.addDocumentListener (.getDocument input)
      (proxy [DocumentListener] []
        (insertUpdate [_] (filter-list input list model files))
        (removeUpdate [_] (filter-list input list model files))
        (changedUpdate [_] nil)))

    (.showInCenterOf popup (.getContentComponent parent))))