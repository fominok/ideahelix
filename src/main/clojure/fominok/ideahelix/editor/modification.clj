;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns fominok.ideahelix.editor.modification
  (:require
    [fominok.ideahelix.editor.movement :refer :all]
    [fominok.ideahelix.editor.selection :refer :all]
    [fominok.ideahelix.editor.util
     :refer [inc-within-bounds dec-within-bounds get-caret-contents]
     :rename {inc-within-bounds binc dec-within-bounds bdec}])
  (:import
    (com.intellij.openapi.command
      CommandProcessor)
    (com.intellij.openapi.command.impl
      FinishMarkAction
      StartMarkAction)))


(defn into-insert-mode-prepend
  [caret]
  (let [selection-start (.getSelectionStart caret)]
    (.moveToOffset caret selection-start)))


(defn finish-undo
  [project editor start-mark]
  (.. CommandProcessor getInstance
      (executeCommand
        project
        (fn [] (FinishMarkAction/finish project editor start-mark))
        "IHx: Insertion"
        nil)))


(defn leave-insert-mode
  [document caret]
  (if (.hasSelection caret)
    (when-not (or (degenerate? caret) (reversed? caret))
      (.moveToOffset caret (bdec (.getOffset caret))))
    (ensure-selection document caret)))


(defn backspace
  [document caret]
  (let [offset (.getOffset caret)]
    (.deleteString document (bdec offset) offset)))


(defn delete-selection-contents
  [document caret]
  (.deleteString document (.getSelectionStart caret) (.getSelectionEnd caret))
  (let [offset (.getOffset caret)]
    (.setSelection caret offset (binc document offset))))


(defn insert-newline
  [document caret]
  (let [offset (.getOffset caret)]
    (.insertString document offset "\n")
    (.moveToOffset caret (binc document offset))))


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


(defn ihx-insert-char
  [{:keys [offset in-append] :as selection} document char]
  (when-not (and (not= char \return \newline) (Character/isISOControl char))
    (.insertString document (cond-> offset
                              in-append inc)
                   (str char))
    (if in-append
      (ihx-move-forward selection 1)
      (ihx-nudge selection 1))))


(defn ihx-new-line-below
  [selection document]
  (let [new-selection
        (-> (ihx-make-forward selection)
            (ihx-move-relative! :lines 1)
            (ihx-move-line-start document)
            ihx-shrink-selection)]
    (.insertString document (:offset new-selection) "\n")
    new-selection))


(defn ihx-new-line-above
  [selection document]
  (let [new-selection
        (-> (ihx-make-forward selection)
            (ihx-move-line-start document)
            ihx-shrink-selection)]
    (.insertString document (:offset new-selection) "\n")
    new-selection))


(defn replace-selections
  [project-state project editor document & {:keys [register] :or {register \"}}]
  (let [start (start-undo project editor)
        carets (.. editor getCaretModel getAllCarets)
        register-contents
        (doall (for [caret carets
                     :let [text (get-caret-contents document caret)]]
                 (do
                   (delete-selection-contents document caret)
                   (into-insert-mode-prepend caret)
                   text)))]
    (-> project-state
        (assoc-in [:registers register] register-contents)
        (assoc-in [editor :mode] :insert)
        (assoc-in [editor :prefix] nil)
        (assoc-in [editor :mark-action] start))))


(defn delete-selections
  [project-state editor document & {:keys [register] :or {register \"}}]
  (let [carets (.. editor getCaretModel getAllCarets)
        register-contents
        (doall (for [caret carets
                     :let [text (get-caret-contents document caret)]]
                 (do
                   (delete-selection-contents document caret)
                   text)))]
    (-> project-state
        (assoc-in [:registers register] register-contents)
        (assoc-in [editor :prefix] nil))))
