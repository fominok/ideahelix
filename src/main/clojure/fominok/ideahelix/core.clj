;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns fominok.ideahelix.core
  (:require
    [cider.nrepl :refer (cider-nrepl-handler)]
    [clojure.java.io :as io]
    [fominok.ideahelix.editor :refer [handle-editor-event state-atom quit-insert-mode]]
    [fominok.ideahelix.editor.selection :refer :all]
    [fominok.ideahelix.editor.ui :as ui]
    [nrepl.server :refer [start-server]]
    [toml-clj.core :as toml])
  (:import
    (com.intellij.openapi.editor
      Editor)
    (com.intellij.openapi.editor.event
      CaretListener)))


(set! *warn-on-reflection* true)


(defn- config-file
  ^java.io.File
  []
  (let [home (System/getProperty "user.home")
        os-name (System/getProperty "os.name" "")]
    (io/file home
             (if (.startsWith os-name "Windows")
               "_ideahelix.toml"
               ".ideahelix.toml"))))


(defn- read-config
  []
  (let [^java.io.File config-file (config-file)]
    (when (.exists config-file)
      (with-open [reader (io/reader config-file)]
        (toml/read reader)))))


(defn- configured-nrepl-port
  []
  (some-> (read-config)
          (get-in ["development" "nrepl_port"])))


(defn push-editor-event
  [project ^Editor editor event]
  (boolean
    (when-not (.isOneLineMode editor)
      (handle-editor-event project editor event))))


(defn current-mode
  [project]
  (:mode (or (get @state-atom project) {:mode :normal})))


(defn- caret-listener
  [editor]
  (reify CaretListener
    (caretPositionChanged
      [_ event]
      (ui/highlight-primary-caret editor event))))


(defn focus-editor
  [project ^Editor editor]
  (let [project-state (or (get @state-atom project) {:mode :normal})
        document (.getDocument editor)]
    (when-not (get-in project-state [:caret-listeners editor])
      (let [listener (caret-listener editor)
            _ (.. editor getCaretModel (addCaretListener listener))]
        (swap! state-atom assoc-in [project :caret-listeners editor] listener)))
    (ui/update-mode-panel! project project-state)
    (when (= (:mode project-state) :normal)
      (.. editor getCaretModel
          (runForEachCaret (fn [caret]
                             (-> (ihx-selection document caret)
                                 (ihx-apply-selection! document))))))))


(defonce -server
  (when-some [port (configured-nrepl-port)]
    (start-server :port port :handler cider-nrepl-handler)))
