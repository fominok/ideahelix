;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns fominok.ideahelix.core
  (:require
    [clojure.string :as str]
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


(declare ensure-nrepl-started!)


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
  (ensure-nrepl-started!)
  (boolean
    (when-not (.isOneLineMode editor)
      (handle-editor-event project editor event))))


(defn current-mode
  [project]
  (ensure-nrepl-started!)
  (:mode (or (get @state-atom project) {:mode :normal})))


(defn current-mode-display
  [project]
  (ensure-nrepl-started!)
  (let [project-state (or (get @state-atom project) {:mode :normal})
        mode-text (str/upper-case (name (or (:mode project-state) :normal)))]
    (str
      (when-let [prefix (:prefix project-state)]
        (format "(%s) " (apply str prefix)))
      mode-text)))


(defn- caret-listener
  [editor]
  (reify CaretListener
    (caretPositionChanged
      [_ event]
      (ui/highlight-primary-caret editor event))))


(defn focus-editor
  [project ^Editor editor]
  (ensure-nrepl-started!)
  (let [project-state (or (get @state-atom project) {:mode :normal})
        document (.getDocument editor)]
    (when-not (get @state-atom project)
      (swap! state-atom assoc project project-state))
    (when-not (get-in project-state [:caret-listeners editor])
      (let [listener (caret-listener editor)
            _ (.. editor getCaretModel (addCaretListener listener))]
        (swap! state-atom assoc-in [project :caret-listeners editor] listener)))
    (when (= (:mode project-state) :normal)
      (.. editor getCaretModel
          (runForEachCaret (fn [caret]
                             (-> (ihx-selection document caret)
                                 (ihx-apply-selection! document))))))))


(defn release-editor
  [project ^Editor editor]
  (ensure-nrepl-started!)
  (when-let [listener (get-in @state-atom [project :caret-listeners editor])]
    (.. editor getCaretModel (removeCaretListener listener))
    (swap! state-atom update project #(some-> %
                                              (update :caret-listeners dissoc editor)
                                              (update :per-editor dissoc editor)))))


(defonce nrepl-state-atom (atom ::unknown))


(defn ensure-nrepl-started!
  []
  (let [state @nrepl-state-atom]
    (cond
      (not= state ::unknown) (when-not (= state ::disabled) state)
      :else
      (let [configured-port (configured-nrepl-port)]
        (if-not configured-port
          (do
            (reset! nrepl-state-atom ::disabled)
            nil)
          (or (when-not (= @nrepl-state-atom ::unknown)
                (let [resolved-state @nrepl-state-atom]
                  (when-not (= resolved-state ::disabled) resolved-state)))
              (let [handler (requiring-resolve 'cider.nrepl/cider-nrepl-handler)
                    server (start-server :port configured-port :handler handler)]
                (reset! nrepl-state-atom server)
                server)))))))
