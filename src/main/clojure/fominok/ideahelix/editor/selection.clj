;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns fominok.ideahelix.editor.selection
  (:require
    [fominok.ideahelix.editor.util :refer [printable-char?]])
  (:import
    (com.intellij.openapi.editor
      Document
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
  (when (and caret (.isValid caret))
    (let [[start end] (sort [anchor offset])
          text-length (.getTextLength document)
          adj #(max 0 (min % (dec text-length)))
          adjusted-offset (adj (cond-> offset
                                 in-append inc))
          adjusted-start (adj start)]
      (.moveToOffset caret adjusted-offset)
      (.setSelection caret adjusted-start (max 0 (min (inc end) text-length))))))


(defn ihx-apply-selection-preserving
  [{:keys [anchor offset caret]} document]
  (let [[start end] (sort [anchor offset])
        adjusted-start (max 0 start)
        adjusted-end (min (.getTextLength document) (inc end))]
    (.setSelection caret adjusted-start adjusted-end)))


(defn ihx-shrink-selection
  [selection]
  (assoc selection :anchor (:offset selection)))


(defn flip-selection
  [{:keys [offset anchor] :as selection}]
  (if (> offset anchor)
    (ihx-make-backward selection)
    (ihx-make-forward selection)))


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
      (> (- selection-end selection-start) 1))))


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
    (-> (ihx-selection document caret)
        (ihx-offset 0)
        ihx-make-forward
        (ihx-offset length)
        (ihx-apply-selection! document))))


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
      (-> (->IhxSelection primary start (dec end) false)
          (ihx-apply-selection! document)))
    (doseq [{:keys [start end]} (rest matches)]
      (when-let [caret (.addCaret model (.offsetToVisualPosition editor (max 0 (dec end))))]
        (-> (->IhxSelection caret start (dec end) false)
            (ihx-apply-selection! document))))))


(defn scroll-to-primary-caret
  [editor]
  (.. editor getScrollingModel (scrollToCaret ScrollType/RELATIVE)))


(defn find-next-occurrence
  [^CharSequence text {:keys [pos neg]}]
  (first (keep-indexed #(when (or
                                (contains? pos %2)
                                (not (or (nil? neg) (contains? neg %2))))
                          %1) text)))


(defn find-char
  [state editor ^Document document char & {:keys [include]
                                           :or {include false}}]
  (when
    (or (Character/isLetterOrDigit char) ((into #{} "!@#$%^&*()_+-={}[]|;:<>.,?~`") char))
    (doseq [caret (.. editor getCaretModel getAllCarets)]
      (let [text (.getCharsSequence document)
            len (.length text)
            expand (= (:previous-mode state) :select)
            sub (.subSequence text (+ 2 (.getOffset caret)) len)]
        (when-let [delta (find-next-occurrence sub {:pos #{char}})]
          (cond-> (ihx-selection document caret)
            (not expand) (ihx-shrink-selection)
            true (ihx-move-forward (inc delta))
            include (ihx-move-forward 1)
            true (ihx-apply-selection! document)))))
    (assoc state :mode (:previous-mode state))))


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


(defn ihx-long-word-forward!
  [{:keys [_ offset] :as selection} document extending?]
  (let [text (.getCharsSequence document)
        blank-chars (set " \t\n\r")
        len (.length text)
        offset (cond-> offset
                 ;; if we are at space right before a word, we shift the offset
                 (and
                   (contains? blank-chars (.charAt text offset))
                   (not (contains? blank-chars (.charAt text (inc offset)))))
                 inc
                 ;; if we are at the end of the line, we skip to next line
                 (= \newline (.charAt text (inc offset)))
                 (+ 2))
        sub (.subSequence text offset len)
        end-offset (+ offset (or (find-next-occurrence sub {:pos blank-chars}) (.length sub)))
        sub (.subSequence text end-offset len)
        end-offset (+ end-offset (or (find-next-occurrence sub {:pos #{} :neg (set " \t")}) (.length sub)))
        end-offset (dec end-offset)]
    (if extending?
      (assoc selection :offset end-offset)
      (assoc selection :offset end-offset :anchor offset))))


(defn ihx-word-end-extending!
  [{:keys [caret] :as selection} editor]
  (.moveCaretRelatively caret 1 0 false false)
  (EditorActionUtil/moveToNextCaretStop editor CaretStopPolicy/WORD_END false true)
  (let [new-offset (max 0 (dec (.getOffset caret)))]
    (assoc selection :offset new-offset)))


(defn ihx-word-end!
  [{:keys [caret offset] :as selection} editor]
  (EditorActionUtil/moveToNextCaretStop editor CaretStopPolicy/WORD_END false true)
  (let [new-offset (.getOffset caret)]
    (if (= new-offset (inc offset))
      (do
        (EditorActionUtil/moveToNextCaretStop editor CaretStopPolicy/WORD_END false true)
        (assoc selection :offset (max 0 (dec (.getOffset caret))) :anchor new-offset))
      (assoc selection :offset (max 0 (dec new-offset)) :anchor offset))))


(defn ihx-long-word-end!
  [{:keys [_ offset] :as selection} document extending?]
  (let [text (.getCharsSequence document)
        blank-chars (set " \t\n\r")
        len (.length text)
        offset (cond-> offset
                 ;; if we are at space right before a word, we shift the offset
                 (and
                   (not (contains? blank-chars (.charAt text offset)))
                   (contains? blank-chars (.charAt text (inc offset))))
                 inc
                 ;; if we are at the end of the line, we skip to next line
                 (= \newline (.charAt text (inc offset)))
                 (+ 2))
        sub (.subSequence text offset len)
        end-offset (+ offset (or (find-next-occurrence sub {:pos #{} :neg blank-chars}) (.length sub)))
        sub (.subSequence text end-offset len)
        end-offset (+ end-offset (or (find-next-occurrence sub {:pos blank-chars}) (.length sub)))
        end-offset (dec end-offset)]
    (if extending?
      (assoc selection :offset end-offset)
      (assoc selection :offset end-offset :anchor offset))))


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


(defn ihx-long-word-backward!
  [{:keys [_ offset] :as selection} document extending?]
  (let [text (-> (.getCharsSequence document) (.subSequence 0 offset) StringBuilder. .reverse)
        blank-chars (set " \t\n\r")
        len (.length text)
        start (cond-> 1
                (and
                  (not (contains? blank-chars (.charAt text 0)))
                  (contains? blank-chars (.charAt text 1)))
                inc
                (= \newline (.charAt text 1))
                (+ 2))
        sub (.subSequence text start len)
        end-offset (+ start (or (find-next-occurrence sub {:pos #{\newline} :neg blank-chars}) (.length sub)))
        sub (.subSequence text end-offset len)
        end-offset (+ end-offset (or (find-next-occurrence sub {:pos blank-chars}) (.length sub)))
        end-offset (dec end-offset)]
    (if extending?
      (assoc selection :offset (- offset end-offset 1))
      (assoc selection :offset (- offset end-offset 1) :anchor (- offset start)))))


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


(defn dump-drop-selections!
  [editor document]
  (let [carets (.. editor getCaretModel getAllCarets)
        ihx-selections (doall (map (partial ihx-selection document) carets))]
    (doseq [caret carets
            :let [offset (.getOffset caret)]]
      (.setSelection caret offset offset))
    ihx-selections))


(defn- restore-saved-selections
  [pre-selections insertion-kind document carets]
  (when-not (empty? pre-selections)
    (let [delta (- (.getOffset (first carets)) (:offset (first pre-selections))
                   (if (= insertion-kind :append) 1 0))
          increasing-deltas (map (partial * delta) (range))]
      (doseq [[{:keys [caret anchor] :as selection} delta-anchor]
              (map vector pre-selections increasing-deltas)
              :let [new-offset (.getOffset caret)
                    new-anchor (+ anchor delta-anchor)]]
        (case insertion-kind
          :append (if (>= new-offset new-anchor)
                    (-> selection
                        (assoc :anchor new-anchor)
                        (assoc :offset (dec new-offset))
                        (ihx-apply-selection! document))
                    (-> (ihx-selection document caret)
                        (ihx-move-backward 1)
                        (ihx-apply-selection! document)))
          (-> selection
              (ihx-nudge (+ delta delta-anchor))
              (ihx-apply-selection! document)))))))


(defn restore-selections
  [pre-selections insertion-kind editor document]
  (let [carets (.. editor getCaretModel getAllCarets)
        saved-carets (into #{} (map :caret pre-selections))
        free-carets (filter (complement saved-carets) carets)]
    (restore-saved-selections pre-selections insertion-kind document carets)
    (doseq [caret free-carets]
      (-> (ihx-selection document caret)
          (ihx-apply-selection! document)))))


(def char-match
  {\( {:match \) :direction :open}
   \) {:match \( :direction :close}
   \[ {:match \] :direction :open}
   \] {:match \[ :direction :close}
   \{ {:match \} :direction :open}
   \} {:match \{ :direction :close}
   \< {:match \> :direction :open}
   \> {:match \< :direction :close}})


(defn next-match
  [text offset opener target]
  (loop [to-find 1 text (.subSequence text offset (.length text)) acc-offset offset]
    (let [target-offset (find-next-occurrence text {:pos (set [opener target])})
          found (.charAt text target-offset)
          text (.subSequence text (inc target-offset) (.length text))
          acc-offset (inc acc-offset)]
      (if (= found target) ; must check first if we found match in case match = char
        (if (= to-find 1)
          (+ acc-offset target-offset -1)
          (recur (dec to-find) text (+ acc-offset target-offset)))
        (recur (inc to-find) text (+ acc-offset target-offset))))))


(defn previous-match
  [text offset opener target]
  (let [len (.length text)
        idx (- len offset 1)
        text (-> (StringBuilder. text) .reverse)
        res (next-match text idx opener target)]
    (- len res 1)))


(defn get-open-close-chars
  [char]
  (let [match-info (get char-match char)]
    (cond (nil? match-info) {:open-char char :close-char char}
          (= (:direction match-info) :open) {:open-char char :close-char (:match match-info)}
          :else {:open-char (:match match-info) :close-char char})))


(defn ihx-surround-add
  [{:keys [offset anchor] :as selection} document char]
  (when (printable-char? char)
    (let [{:keys [open-char close-char]} (get-open-close-chars char)]
    (cond (> offset anchor)
          (do (.insertString document (inc offset) (str close-char))
              (.insertString document anchor (str open-char))
              (assoc selection :offset (+ 2 offset) :anchor anchor))
          :else
          (do (.insertString document (inc anchor) (str close-char))
              (.insertString document offset (str open-char))
              (assoc selection :offset offset :anchor (+ 2 anchor)))))))


(defn ihx-surround-delete
  [{:keys [offset anchor] :as selection} document char]
  (when (printable-char? char)
    (let [text (.getCharsSequence document)
          {:keys [open-char close-char]} (get-open-close-chars char)
          curr-char (.charAt text offset)]
      (if (and (not= open-char curr-char) (not= close-char curr-char))
        (let [left (previous-match text offset close-char open-char)
              right (next-match text offset open-char close-char)
              anchor (if (> anchor left)
                       (dec anchor)
                       anchor)]
          (.deleteString document right (inc right))
          (.deleteString document left (inc left))
          (assoc selection :offset (dec offset) :anchor anchor))
        (cond
          (= open-char close-char) nil
          (= open-char curr-char) (let [left offset
                                        right (next-match text (inc offset) open-char close-char)
                                        anchor (if (> anchor offset) (dec anchor) anchor)]
                                    (.deleteString document right (inc right))
                                    (.deleteString document left (inc left))
                                    (assoc selection :offset offset :anchor anchor))
          (= close-char curr-char) (let [left (previous-match text (dec offset) close-char open-char)
                                         right offset
                                         anchor (dec anchor)]
                                     (.deleteString document right (inc right))
                                     (.deleteString document left (inc left))
                                     (assoc selection :offset (dec offset) :anchor anchor)))))))


(defn ihx-goto-matching
  [{:keys [_ offset] :as selection} document]
  (let [text (.getCharsSequence document)
        opener (.charAt text offset)
        {:keys [match direction]} (get char-match opener)]
    (when (and match direction)
      (let [offset (case direction
                     :open (next-match text (inc offset) opener match)
                     :close (previous-match text (dec offset) opener match))]
        (assoc selection :offset offset)))))
