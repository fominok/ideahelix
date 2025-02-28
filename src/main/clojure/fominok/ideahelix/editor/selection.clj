;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns fominok.ideahelix.editor.selection
  (:require
    [fominok.ideahelix.editor.util
     :refer [inc-within-bounds dec-within-bounds]
     :rename {inc-within-bounds binc dec-within-bounds bdec}])
  (:import
    (com.intellij.openapi.editor
      ScrollType
      VisualPosition)
    (com.intellij.openapi.editor.actions
      CaretStopPolicy
      EditorActionUtil)
    (com.intellij.openapi.editor.impl
      CaretImpl
      DocumentImpl)
    (com.intellij.openapi.project
      Project)
    (com.intellij.openapi.ui
      Messages)))


;; Instead of counting positions between characters this wrapper
;; speaks in character indices, at least because when selection is getting
;; reversed on caret movement the pivot is a character in Helix rather than
;; a position between characters, meaning it will be kept selected unlike in
;; Idea
(defrecord IhxSelection
  [^CaretImpl caret anchor offset in-append])


(defn ihx-move-forward
  [selection n]
  (update selection :offset + n))


(defn ihx-move-backward
  [selection n]
  (update selection :offset - n))


(defn ihx-make-backward
  [{:keys [anchor offset] :as selection}]
  (if (> offset anchor)
    (assoc selection :anchor offset :offset anchor)
    selection))


(defn ihx-make-forward
  [{:keys [anchor offset] :as selection}]
  (if (< offset anchor)
    (assoc selection :anchor offset :offset anchor)
    selection))


(defn ihx-nudge
  [selection n]
  (-> selection
      (update :anchor + n)
      (update :offset + n)))


(defn ihx-append
  [selection]
  (assoc selection :in-append true))


(defn ihx-offset
  [selection offset]
  (assoc selection :offset offset))


(defn ihx-selection
  [^DocumentImpl document ^CaretImpl caret & {:keys [insert-mode] :or {insert-mode false}}]
  (let [start (.getSelectionStart caret)
        end (.getSelectionEnd caret)
        text-length (.getTextLength document)
        original-length (- end start)
        offset' (.getOffset caret)
        [in-append offset] (if (and insert-mode (= offset' end))
                             [true (dec offset')]
                             [false offset'])
        is-forward (or (< original-length 2) (not= start offset))
        is-broken (or (and (> text-length 0) (= original-length 0))
                      (and (not= offset start)
                           (not= offset (dec end))))
        anchor
        (cond
          is-broken offset
          is-forward start
          :default (max 0 (dec end)))]
    (->IhxSelection caret anchor offset in-append)))


;; This modifies the caret
(defn ihx-apply-selection!
  [{:keys [anchor offset caret in-append]} document]
  (let [[start end] (sort [anchor offset])
        text-length (.getTextLength document)
        adj #(max 0 (min % (dec text-length)))
        adjusted-offset (adj (cond-> offset
                               in-append inc))
        adjusted-start (adj start)]
    (.moveToOffset caret adjusted-offset)
    (.setSelection caret adjusted-start (max 0 (min (inc end) text-length)))))


(defn ihx-apply-selection-preserving
  [{:keys [anchor offset caret]} document]
  (let [[start end] (sort [anchor offset])
        adjusted-start (max 0 start)
        adjusted-end (min (.getTextLength document) (inc end))]
    (.setSelection caret adjusted-start adjusted-end)))


(defn ihx-shrink-selection
  [selection]
  (assoc selection :anchor (:offset selection)))


(defn ensure-selection
  "Keep at least one character selection"
  [document caret]
  (let [offset (.getOffset caret)
        selection-start (.getSelectionStart caret)
        selection-end (.getSelectionEnd caret)]
    (when-not (and (.hasSelection caret)
                   (or (= offset selection-start) (= offset (bdec selection-end))))
      (.setSelection caret offset (binc document offset)))))


(defn flip-selection
  [caret]
  (let [selection-start (.getSelectionStart caret)]
    (if (= (.getOffset caret) selection-start)
      (.moveToOffset caret (bdec (.getSelectionEnd caret)))
      (.moveToOffset caret selection-start))))


(defn ensure-selection-forward
  [caret]
  (let [selection-start (.getSelectionStart caret)]
    (when (= (.getOffset caret) selection-start)
      (.moveToOffset caret (bdec (.getSelectionEnd caret))))))


(defn keep-primary-selection
  [editor]
  (.. editor getCaretModel removeSecondaryCarets))


(defn reversed?
  [caret]
  (let [selection-start (.getSelectionStart caret)
        selection-end (.getSelectionEnd caret)
        offset (.getOffset caret)]
    (and
      (= offset selection-start)
      (< selection-start (bdec selection-end)))))


(defn degenerate?
  [caret]
  (let [selection-start (.getSelectionStart caret)
        selection-end (.getSelectionEnd caret)
        offset (.getOffset caret)]
    (and
      (= offset selection-start)
      (= offset (bdec selection-end)))))


(defn ihx-move-line-start
  [{:keys [offset] :as selection} editor document]
  (let [line-start-offset
        (.getLineStartOffset document (.line (.offsetToLogicalPosition editor offset)))]
    (assoc selection :offset line-start-offset)))


(defn ihx-move-line-end
  [{:keys [offset] :as selection} editor document]
  (let [line-end-offset
        (.getLineEndOffset document (.line (.offsetToLogicalPosition editor offset)))]
    (assoc selection :offset line-end-offset)))


(defn ihx-move-relative!
  [{:keys [caret] :as selection} & {:keys [cols lines] :or {cols 0 lines 0}}]
  (.moveCaretRelatively caret cols lines false false)
  (assoc selection :offset (.getOffset caret)))


(defn ihx-select-lines
  [{:keys [anchor offset] :as selection} editor document & {:keys [extend] :or {extend false}}]
  (let [new-selection
        (-> selection
            ihx-make-backward
            (ihx-move-line-start editor document)
            ihx-make-forward
            (ihx-move-line-end editor document))]
    (if (and extend (= (sort [anchor offset])
                       (sort [(:anchor new-selection) (:offset new-selection)])))
      (-> new-selection
          (ihx-move-relative! :lines 1)
          (ihx-move-line-end editor document))
      new-selection)))


(defn- line-length
  [document n]
  (let [start-offset (.getLineStartOffset document n)
        end-offset (.getLineEndOffset document n)]
    (- end-offset start-offset)))


(defn- scan-next-selection-placement
  [document height start end lines-count]
  (let [start-column (.column start)
        end-column (.column end)]
    (loop [line (+ height 1 (.line start))]
      (let [line-end (+ line height)]
        (when-not (>= line-end lines-count)
          (let [start-line-length (line-length document line)
                end-line-length (line-length document line-end)]
            (if (and (<= start-column start-line-length)
                     (<= end-column end-line-length))
              [line line-end]
              (recur (inc line)))))))))


(defn add-selection-below
  [editor caret]
  (let [model (.getCaretModel editor)
        document (.getDocument editor)
        selection-start (.offsetToLogicalPosition editor (.getSelectionStart caret))
        selection-end (.offsetToLogicalPosition editor (.getSelectionEnd caret))
        caret-col (.column (.offsetToLogicalPosition editor (.getOffset caret)))
        height (- (.line selection-end)
                  (.line selection-start))
        line-count (.. editor getDocument getLineCount)
        reversed (reversed? caret)]
    (when-let [[next-line-start next-line-end]
               (scan-next-selection-placement document height selection-start selection-end line-count)]
      (some-> (.addCaret model (VisualPosition.
                                 (if reversed
                                   next-line-start
                                   next-line-end) caret-col))
              (.setSelection (+ (.getLineStartOffset document next-line-start)
                                (.column selection-start))
                             (+ (.getLineStartOffset document next-line-end)
                                (.column selection-end)))))))


(defn select-buffer
  [editor document]
  (let [caret (.. editor getCaretModel getPrimaryCaret)
        length (.getTextLength document)]
    (.moveToOffset caret length)
    (.setSelection caret 0 length)))


(defn regex-matches-with-positions
  [pattern text]
  (let [matcher (re-matcher pattern text)]
    (loop [results []]
      (if (.find matcher)
        (recur (conj results {:start (.start matcher)
                              :end (.end matcher)}))
        results))))


(defn select-in-selections
  [^Project project editor document]
  (let [model (.getCaretModel editor)
        primary (.getPrimaryCaret model)
        input (Messages/showInputDialog
                project
                "select:"
                "Select in selections"
                (Messages/getQuestionIcon))
        pattern (when (not (empty? input)) (re-pattern input))
        matches
        (and pattern
             (->> (.getAllCarets model)
                  (map (fn [caret] [(.getSelectionStart caret) (.getText document (.getSelectionRange caret))]))
                  (map (fn [[offset text]]
                         (map #(update-vals % (partial + offset))
                              (regex-matches-with-positions pattern text))))
                  flatten))]
    (when-let [{:keys [start end]} (first matches)]
      (.removeSecondaryCarets model)
      (.moveToOffset primary (bdec end))
      (.setSelection primary start end))
    (doseq [{:keys [start end]} (rest matches)]
      (when-let [caret (.addCaret model (.offsetToVisualPosition editor (bdec end)))]
        (.setSelection caret start end)))))


(defn scroll-to-primary-caret
  [editor]
  (.. editor getScrollingModel (scrollToCaret ScrollType/RELATIVE)))


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


(defn ihx-move-caret-line-n
  [editor document n]
  (let [line-n (dec (min n (.getLineCount document)))
        model (.getCaretModel editor)
        caret (.getPrimaryCaret model)
        offset (.getLineStartOffset document line-n)]
    (.removeSecondaryCarets model)
    (-> (ihx-selection document caret)
        (ihx-offset offset))))


(defn ihx-move-file-end
  [editor document]
  (let [text-length (.getTextLength document)
        model (.getCaretModel editor)
        caret (.getPrimaryCaret model)]
    (.removeSecondaryCarets model)
    (-> (ihx-selection document caret)
        (ihx-offset text-length))))
