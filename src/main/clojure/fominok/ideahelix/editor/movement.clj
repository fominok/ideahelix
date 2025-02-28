;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns fominok.ideahelix.editor.movement
  "Movements that aren't defined by existing Idea actions"
  (:require
    [fominok.ideahelix.editor.selection :refer :all]
    [fominok.ideahelix.editor.util
     :refer [inc-within-bounds dec-within-bounds]
     :rename {inc-within-bounds binc dec-within-bounds bdec}])
  (:import
    (com.intellij.openapi.editor
      ScrollType)
    (com.intellij.openapi.editor.actions
      CaretStopPolicy
      EditorActionUtil)))


(defn scroll-to-primary-caret
  [editor]
  (.. editor getScrollingModel (scrollToCaret ScrollType/RELATIVE)))


(defn move-caret-line-start
  [document caret]
  (let [line-start-offset (.getLineStartOffset document (.. caret getLogicalPosition line))
        selection (ihx-selection document caret)]
    (assoc selection :offset line-start-offset)))


(defn move-caret-line-end
  [document caret]
  (let [line-end-offset (.getLineEndOffset document (.. caret getLogicalPosition line))
        selection (ihx-selection document caret)]
    (assoc selection :offset line-end-offset)))


(defn move-caret-down
  [document caret]
  (.moveCaretRelatively caret 0 1 false false)
  (ensure-selection document caret))


(defn move-caret-up
  [document caret]
  (.moveCaretRelatively caret 0 -1 false false)
  (ensure-selection document caret))


(defn ihx-move-relative!
  [{:keys [caret] :as selection} & {:keys [cols lines] :or {cols 0 lines 0}}]
  (.moveCaretRelatively caret cols lines false false)
  (assoc selection :offset (.getOffset caret)))


;; This modifies the caret
(defn ihx-word-forward-extending!
  [{:keys [caret] :as selection} editor]
  (.moveCaretRelatively caret 1 0 false false)
  (EditorActionUtil/moveToNextCaretStop editor CaretStopPolicy/WORD_START false true)
  (let [new-offset (max 0 (dec (.getOffset caret)))]
    (assoc selection :offset new-offset)))


(defn ihx-word-forward!
  [{:keys [caret offset] :as selection} editor]
  (EditorActionUtil/moveToNextCaretStop editor CaretStopPolicy/WORD_START false true)
  (let [new-offset (.getOffset caret)]
    (if (= new-offset (inc offset))
      (do
        (EditorActionUtil/moveToNextCaretStop editor CaretStopPolicy/WORD_START false true)
        (assoc selection :offset (max 0 (dec (.getOffset caret))) :anchor new-offset))
      (assoc selection :offset (max 0 (dec new-offset)) :anchor offset))))


(defn ihx-word-backward-extending!
  [{:keys [caret] :as selection} editor]
  (EditorActionUtil/moveToPreviousCaretStop editor CaretStopPolicy/WORD_START false true)
  (let [new-offset (.getOffset caret)]
    (assoc selection :offset new-offset)))


(defn ihx-word-backward!
  [{:keys [caret offset] :as selection} editor]
  (EditorActionUtil/moveToPreviousCaretStop editor CaretStopPolicy/WORD_START false true)
  (let [new-selection (assoc selection :offset (.getOffset caret) :anchor offset)]
    (EditorActionUtil/moveToNextCaretStop editor CaretStopPolicy/WORD_START false true)
    (if (= (.getOffset caret) offset)
      (assoc new-selection :anchor (max 0 (dec (.getOffset caret))))
      new-selection)))


(defn move-caret-line-n
  [editor document n]
  (let [line-n (dec (min n (.getLineCount document)))
        model (.getCaretModel editor)
        offset (.getLineStartOffset document line-n)]
    (.removeSecondaryCarets model)
    (.moveToOffset model offset)
    (ensure-selection document (.getPrimaryCaret model))))
