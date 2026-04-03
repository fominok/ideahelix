(ns fominok.ideahelix.search
  (:require
    [fominok.ideahelix.editor.jumplist :refer [capture-current-location! capture-current-location-later!]])
  (:import
    (com.intellij.openapi.application
      ModalityState
      ReadAction)
    (com.intellij.openapi.editor
      Editor)
    (com.intellij.openapi.fileEditor
      FileEditorManager)
    (com.intellij.openapi.project
      Project
      ProjectUtil
      ProjectUtilCore)
    (com.intellij.openapi.ui.popup
      JBPopupFactory)
    (com.intellij.psi.codeStyle
      NameUtil)
    (com.intellij.psi.search
      FilenameIndex
      GlobalSearchScope)
    (com.intellij.ui.components
      JBList
      JBScrollPane)
    (com.intellij.util
      Alarm
      Alarm$ThreadToUse)
    (com.intellij.util.concurrency
      AppExecutorUtil)
    (java.awt
      BorderLayout
      Dimension)
    (java.awt.event
      ActionEvent)
    (javax.swing
      AbstractAction
      DefaultListModel
      JPanel
      JTextField
      KeyStroke
      ListSelectionModel)
    (javax.swing.event
      DocumentListener)))


(defn filter-list
  [alarm ^JTextField input ^JBList list ^DefaultListModel model files]
  (.cancelAllRequests alarm)
  (.addRequest
    alarm
    (fn []
      (let [query (.trim (.getText input))
            matcher (.. (NameUtil/buildMatcher (str "*" query)) build)
            scored-items (map (fn [[path _]]
                                {:score (.matchingDegree matcher path)
                                 :path  path})
                              files)
            items
            (->> scored-items
                 (sort-by :score >)
                 (take 20)
                 (map :path))]

        (.clear model)
        (doseq [item items]
          (.addElement model item))
        (when-not (empty? items)
          (.setSelectedIndex list 0))))
    100
    true))


(defn relativize-path
  [project path]
  (let [base-path (some-> (ProjectUtil/guessProjectDir project) .getPath)]
    (if (and base-path (.startsWith path base-path) (not= path base-path))
      (.substring path (inc (count base-path)))
      path)))


(defn get-filenames!
  [project result on-loaded]
  (.. (^[Callable] ReadAction/nonBlocking
       (fn []
         (into {} (mapcat (fn [name]
                            (keep (fn [file]
                                    (when-let [file-path (some-> file .getPath)]
                                      (let [path (relativize-path project file-path)]
                                        [path file])))
                                  (FilenameIndex/getVirtualFilesByName name (GlobalSearchScope/projectScope project))))
                          (FilenameIndex/getAllFilenames project)))))
      (finishOnUiThread
        (ModalityState/any)
        #(do
           (vreset! result %)
           (on-loaded)))
      (submit (AppExecutorUtil/getAppExecutorService))))


(defn search-file-name
  [project state-atom ^Editor parent]
  (let [files (volatile! {})
        alarm (Alarm. Alarm$ThreadToUse/POOLED_THREAD project)
        model (DefaultListModel.)
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

    (get-filenames! project files #(filter-list alarm input list model @files))

    (doto (.getInputMap input)
      (.put (KeyStroke/getKeyStroke "control N") "selectNext")
      (.put (KeyStroke/getKeyStroke "control P") "selectPrevious"))
    (doto (.getActionMap input)
      (.put "selectNext"
            (proxy [AbstractAction] []
              (actionPerformed
                [^ActionEvent _]
                (let [i (min (inc (.getSelectedIndex list)) (dec (.. list (getModel) (getSize))))]
                  (.setSelectedIndex list i)
                  (.ensureIndexIsVisible list i)))))
      (.put "selectPrevious"
            (proxy [AbstractAction] []
              (actionPerformed
                [^ActionEvent _]
                (let [i (max (dec (.getSelectedIndex list)) 0)]
                  (.setSelectedIndex list i)
                  (.ensureIndexIsVisible list i))))))

    (.addActionListener input
                        (proxy [java.awt.event.ActionListener] []
                          (actionPerformed
                            [^ActionEvent _]
                            (let [selected (.getSelectedValue list)]
                              (when-let [file (get @files selected)]
                                (.cancel popup)
                                (capture-current-location! state-atom project)
                                (.openFile (FileEditorManager/getInstance project) file true)
                                (capture-current-location-later! state-atom project))))))

    (.addDocumentListener (.getDocument input)
                          (proxy [DocumentListener] []
                            (insertUpdate [_] (filter-list alarm input list model @files))

                            (removeUpdate [_] (filter-list alarm input list model @files))

                            (changedUpdate [_] nil)))

    (.showInCenterOf popup (.getContentComponent parent))))
