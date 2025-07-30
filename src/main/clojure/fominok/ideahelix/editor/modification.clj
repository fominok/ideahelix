;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns fominok.ideahelix.editor.modification
  (:require
    [fominok.ideahelix.editor.selection :refer :all]
    [fominok.ideahelix.editor.util
     :refer [get-caret-contents]])
  (:import
    (com.intellij.openapi.command
      CommandProcessor)
    (com.intellij.openapi.command.impl
      FinishMarkAction
      StartMarkAction)))


(defn finish-undo
  [project editor start-mark]
  (.. CommandProcessor getInstance
      (executeCommand
        project
        (fn [] (FinishMarkAction/finish project editor start-mark))
        "IHx: Insertion"
        nil)))


(defn delete-selection-contents
  [{:keys [anchor offset] :as selection} document]
  (let [[start end] (sort [anchor offset])]
    (.deleteString document start (min (.getTextLength document) (inc end)))
    (assoc selection :anchor start :offset start)))


(defn start-undo
  [project editor]
  (let [return (volatile! nil)]
    (.. CommandProcessor getInstance
        (executeCommand
          project
          (fn []
            (let [start (StartMarkAction/start editor project "IHx: Insertion")]
              (vreset! return start)))
          "IHx: Insertion"
          nil))
    @return))


(defn replace-selections
  [project-state project editor document & {:keys [register] :or {register \"}}]
  (let [start (start-undo project editor)
        carets (.. editor getCaretModel getAllCarets)
        register-contents
        (doall (for [caret carets
                     :let [text (get-caret-contents document caret)]]
                 (do
                   (-> (ihx-selection document caret)
                       (delete-selection-contents document)
                       (ihx-apply-selection! document))
                   text)))
        pre-selections (dump-drop-selections! editor document)]
    (-> project-state
        (assoc-in [:registers register] register-contents)
        (assoc-in [:per-editor editor :pre-selections] pre-selections)
        (assoc-in [:per-editor editor :mark-action] start)
        (assoc :mode :insert)
        (assoc :insertion-kind :prepend)
        (dissoc :prefix)
        (assoc :debounce true))))


(defn delete-selections
  [project-state editor document & {:keys [register] :or {register \"}}]
  (let [carets (.. editor getCaretModel getAllCarets)
        register-contents
        (doall (for [caret carets
                     :let [text (get-caret-contents document caret)]]
                 (do
                   (-> (ihx-selection document caret)
                       (delete-selection-contents document)
                       (ihx-apply-selection! document))
                   text)))]
    (-> project-state
        (assoc-in [:registers register] register-contents)
        (assoc :mode :normal)
        (assoc :prefix nil))))
