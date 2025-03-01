;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns fominok.ideahelix.editor.modification
  (:require
    [fominok.ideahelix.editor.movement :refer :all]
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


(defn backspace
  [document caret]
  (let [offset (.getOffset caret)]
    (.deleteString document (max 0 offset) offset)))


(defn delete-selection-contents
  [{:keys [anchor offset] :as selection} document]
  (let [[start end] (sort [anchor offset])]
    (.deleteString document start (min (.getTextLength document) (inc end)))
    (assoc selection :anchor start :offset start)))


(defn insert-newline
  [{:keys [offset in-append] :as selection} document]
  (.insertString document (cond-> offset
                            in-append inc)
                 "\n")
  (if in-append
    (ihx-move-forward selection 1)
    (ihx-nudge selection 1)))


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
  [selection editor document]
  (let [new-selection
        (-> (ihx-make-forward selection)
            (ihx-move-relative! :lines 1)
            (ihx-move-line-start editor document)
            ihx-shrink-selection)]
    (.insertString document (:offset new-selection) "\n")
    new-selection))


(defn ihx-new-line-above
  [selection editor document]
  (let [new-selection
        (-> (ihx-make-forward selection)
            (ihx-move-line-start editor document)
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
                   (-> (ihx-selection document caret)
                       (delete-selection-contents document)
                       (ihx-apply-selection! document))
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
                   (-> (ihx-selection document caret)
                       (delete-selection-contents document)
                       (ihx-apply-selection! document))
                   text)))]
    (-> project-state
        (assoc-in [:registers register] register-contents)
        (assoc-in [editor :prefix] nil))))
