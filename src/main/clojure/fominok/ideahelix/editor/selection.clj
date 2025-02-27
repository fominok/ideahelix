;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns fominok.ideahelix.editor.selection
  (:require
    [fominok.ideahelix.editor.util
     :refer [inc-within-bounds dec-within-bounds for-each-caret]
     :rename {inc-within-bounds binc dec-within-bounds bdec}])
  (:import
    (com.intellij.openapi.editor
      VisualPosition)
    (com.intellij.openapi.editor.impl
      CaretImpl
      DocumentImpl)
    (com.intellij.openapi.project
      Project)
    (com.intellij.openapi.ui
      Messages)))


(defrecord IhxSelection
  [^CaretImpl caret anchor length])


(defn ihx-move-forward
  [selection n]
  (update selection :length + n))


(defn ihx-move-backward
  [selection n]
  (let [length (- (:length selection) n)]
    (if (= length 0)
      (-> selection
          (assoc :length -2)
          (update :anchor inc))
      (assoc selection :length length))))


(defn ihx-selection
  [^DocumentImpl document ^CaretImpl caret]
  (let [start (.getSelectionStart caret)
        end (.getSelectionEnd caret)
        text-length (.getTextLength document)
        original-length (- end start)
        offset (.getOffset caret)
        is-forward (or (< original-length 2) (not= start offset))
        is-broken (or (and (> text-length 0) (= original-length 0))
                      (and (not= offset start)
                           (not= offset (dec end))))
        [length anchor]
        (cond
          is-broken [1 (min offset (dec text-length))]
          is-forward [original-length start]
          :default [(- original-length) end])]
    (->IhxSelection caret anchor length)))


(defn ihx-apply-selection
  [{:keys [anchor length caret]} document]
  (let [[start end] (sort [anchor (+ anchor length)])
        adjusted-start (max 0 start)
        adjusted-end (min (.getTextLength document) end)
        offset (if (pos? length) (max 0 (dec adjusted-end)) start)]
    (.moveToOffset caret offset)
    (.setSelection caret adjusted-start adjusted-end)))


(defn ihx-shrink-selection
  [selection]
  (let [length (:length selection)
        pos (max 0 (+ (:anchor selection) length))]
    (-> selection
        (assoc :anchor (if (pos? length) (dec pos) pos))
        (assoc :length 1))))


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


(defn shrink-selection
  [document caret]
  (let [offset (.getOffset caret)]
    (.setSelection caret offset (binc document offset))))


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


(defn extending
  "Executes function f on the caret but extending the existing selection or creating a new one"
  ([document caret f]
   (let [selection-start (.getSelectionStart caret)
         selection-end (.getSelectionEnd caret)
         previous-offset (.getOffset caret)
         anchor (if (= selection-start previous-offset)
                  selection-end
                  selection-start)
         _ (f caret)
         new-offset (.getOffset caret)
         move-right (> new-offset previous-offset)]
     (if (and move-right (= new-offset anchor))
       (.setSelection caret (bdec anchor) (binc document new-offset))
       (if (>= new-offset anchor)
         (.setSelection caret anchor (binc document new-offset))
         (.setSelection caret new-offset anchor))))))


(defn select-lines
  [document ^CaretImpl caret & {:keys [extend] :or {extend false}}]
  (let [selection-start (.getSelectionStart caret)
        selection-end (.getSelectionEnd caret)
        line-start (.getLineNumber document selection-start)
        line-end (.getLineNumber document selection-end)
        start (.getLineStartOffset document line-start)
        end (.getLineEndOffset document line-end)
        extend? (and extend (= selection-start start) (= selection-end end))
        adjusted-end (if extend?
                       (binc document (.getLineEndOffset
                                        document
                                        (min (inc line-end)
                                             (dec (.getLineCount document)))))
                       (binc document end))]
    (.setSelection caret start adjusted-end)
    (.moveToOffset caret adjusted-end)))


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
