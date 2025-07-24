;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns fominok.ideahelix.editor
  (:require
    [fominok.ideahelix.editor.action :refer [actions]]
    [fominok.ideahelix.editor.jumplist :refer :all]
    [fominok.ideahelix.editor.modification :refer :all]
    [fominok.ideahelix.editor.registers :refer :all]
    [fominok.ideahelix.editor.selection :refer :all]
    [fominok.ideahelix.editor.ui :as ui]
    [fominok.ideahelix.editor.util :refer [get-editor-height]]
    [fominok.ideahelix.keymap :refer [defkeymap]]
    [fominok.ideahelix.search :refer :all])
  (:import
    (com.intellij.codeInsight.lookup
      LookupManager)
    (com.intellij.codeInsight.lookup.impl
      LookupImpl)
    (com.intellij.ide.actions.searcheverywhere
      SearchEverywhereManager)
    (com.intellij.openapi.actionSystem
      ActionPlaces
      AnActionEvent
      IdeActions)
    (com.intellij.openapi.command.impl
      StartMarkAction$AlreadyStartedException)
    (com.intellij.openapi.editor
      ScrollType)
    (com.intellij.openapi.editor.impl
      EditorImpl)
    (java.awt.event
      KeyEvent)))


(defonce state-atom (atom {}))


(def get-prefix
  (memoize
    (fn [state]
      (if-let [prefix-vec (get state :prefix)]
        (min 10000 (Integer/parseInt (apply str prefix-vec)))
        1))))


(defn quit-insert-mode
  [project state document]
  (doseq [[editor {:keys [mark-action pre-selections]}] (:per-editor state)]
    (restore-selections pre-selections (:insertion-kind state) editor document)
    (finish-undo project editor mark-action))
  (assoc state :mode :normal :prefix nil :per-editor nil))


(defn- into-insert-mode
  [project
   state
   editor
   & {:keys [dump-selections insertion-kind]
      :or {dump-selections true insertion-kind :prepend}}]
  (let [pre-selections (when dump-selections (dump-drop-selections! editor (.getDocument editor)))]
    (-> state
        (assoc-in [:per-editor editor :mark-action] (start-undo project editor))
        (assoc-in [:per-editor editor :pre-selections] pre-selections)
        (assoc :mode :insert
               :debounce true
               :insertion-kind insertion-kind
               :prefix nil))))


(defkeymap
  editor-handler

  (:any
    (KeyEvent/VK_ESCAPE
      "Back to normal mode"
      [state project editor document]
      (if (= :insert (:mode state))
        (quit-insert-mode project state document)
        (assoc state :mode :normal :prefix nil :pre-selections nil :insertion-kind nil))))

  (:find-char
    (_ [state editor document char event]
       (let [include (:find-char-include state)]
         (find-char state editor document char :include include))))

  ((:or :normal :select)
   (\space
     "Space menu"
     [state] (assoc state :mode :space))
   (\z
     "View menu"
     [state] (assoc state :mode :view :previous-mode (:mode state)))
   ((:or (:ctrl \w) (:ctrl \u0017))
    "Window menu"
    [state] (assoc state :mode :window))
   (\t
     "Find till char"
     [state] (assoc state :mode :find-char
                    :find-char-include false
                    :previous-mode (:mode state)))
   (\f
     "Find including char"
     [state] (assoc state :mode :find-char
                    :find-char-include true
                    :previous-mode (:mode state)))
   (\u
     "Undo"
     [editor] (actions editor IdeActions/ACTION_UNDO)
     [document editor caret] (-> (ihx-selection document caret)
                                 (ihx-apply-selection! document)))
   ((:shift \U)
    "Redo"
    [editor] (actions editor IdeActions/ACTION_REDO)
    [document editor caret] (-> (ihx-selection document caret)
                                (ihx-apply-selection! document)))
   (\y
     "Yank"
     [state editor document]
     (let [registers (copy-to-register (:registers state) editor document)]
       (assoc state :registers registers)))
   (\o
     "New line below" :write :scroll
     [editor document caret]
     (do (-> (ihx-selection document caret)
             (ihx-move-line-end editor document)
             (ihx-apply-selection! document))
         (.setSelection caret (.getSelectionEnd caret) (.getSelectionEnd caret)))
     [project editor state]
     (let [new-state (into-insert-mode project state editor :dump-selections false)]
       (actions editor IdeActions/ACTION_EDITOR_ENTER)
       new-state))
   ((:shift \O)
    "New line above" :write :scroll
    [editor document caret]
    (do (-> (ihx-selection document caret)
            (ihx-move-relative! :lines -1)
            (ihx-move-line-end editor document)
            (ihx-apply-selection! document))
        (.setSelection caret (.getSelectionEnd caret) (.getSelectionEnd caret)))
    [project editor state]
    (let [new-state (into-insert-mode project state editor :dump-selections false)]
      (actions editor IdeActions/ACTION_EDITOR_ENTER)
      new-state))
   ((:shift \%)
    "Select whole buffer"
    [editor document] (select-buffer editor document))
   (\s
     "Select in selections"
     [project editor document] (select-in-selections project editor document))
   (Character/isDigit
     "Add prefix arg" :keep-prefix
     [char state] (update state :prefix (fnil conj []) char))
   (\d
     "Delete selections" :undoable :write
     [state editor document]
     (delete-selections state editor document))
   (\c
     "Replace selections" :write
     [state project editor document]
     (replace-selections state project editor document))
   (\a
     "Append to selections"
     [document editor caret]
     (-> (ihx-selection document caret)
         ihx-make-forward
         (ihx-apply-selection! document))
     [project editor state document]
     (let [new-state (into-insert-mode project state editor :insertion-kind :append)]
       (actions editor IdeActions/ACTION_EDITOR_MOVE_CARET_RIGHT)
       new-state))
   ((:shift \A)
    "Append to line"
    [editor document caret]
    (-> (ihx-selection document caret)
        (ihx-move-line-end editor document)
        ihx-shrink-selection
        (ihx-apply-selection! document))
    [project editor state]
    (into-insert-mode project state editor))
   (\i
     "Prepend to selections"
     [document editor caret]
     (-> (ihx-selection document caret)
         ihx-make-backward
         (ihx-apply-selection! document))
     [project editor state]
     (into-insert-mode project state editor))
   ((:shift \I)
    "Prepend to lines"
    [editor document caret]
    (-> (ihx-selection document caret)
        (ihx-move-line-start editor document)
        ihx-shrink-selection
        (ihx-apply-selection! document))
    [project editor state]
    (into-insert-mode project state editor))
   ((:or (:alt \;) (:alt \u2026))
    "Flip selection" :scroll
    [document editor caret] (-> (ihx-selection document caret)
                                flip-selection
                                (ihx-apply-selection! document)))
   ((:or (:alt \:) (:alt \u00DA))
    "Make selections forward"
    [document editor caret]
    (-> (ihx-selection document caret)
        ihx-make-forward
        (ihx-apply-selection! document)))
   (\;
     "Shrink selections to 1 char"
     [document editor caret]
     (-> (ihx-selection document caret)
         ihx-shrink-selection
         (ihx-apply-selection! document)))
   (\,
     "Drop all selections but primary"
     [editor] (keep-primary-selection editor))
   (\x
     "Select whole lines extending" :scroll
     [state editor document caret]
     (dotimes [_ (get-prefix state)]
       (-> (ihx-selection document caret)
           (ihx-select-lines editor document :extend true)
           (ihx-apply-selection! document))))
   ((:shift \X)
    "Select whole lines" :scroll
    [editor document caret]
    (-> (ihx-selection document caret)
        (ihx-select-lines editor document)
        (ihx-apply-selection! document)))
   ((:shift \C)
    "Add selections below"
    [state editor caret]
    (add-selection-below editor caret))
   ((:or (:ctrl \o) (:ctrl \u000f))
    "Jump backward"
    [state project editor document]
    (jumplist-backward! state project))
   ((:or (:ctrl \i) (:ctrl \u0009))
    "Jump forward"
    [state project editor document]
    (jumplist-forward! state project))
   ((:or (:ctrl \s) (:ctrl \u0013))
    "Add to jumplist"
    [state project document]
    (jumplist-add project state)))

  (:normal
    (\g "Goto mode" :keep-prefix [state] (assoc state :mode :goto))
    (\v "Selection mode" [state] (assoc state :mode :select))
    ((:or (:ctrl \d) (:ctrl \u0004))
     "Move down half page" :scroll
     [editor document caret]
     (let [n-lines (quot (get-editor-height editor) 2)]
       (-> (ihx-selection document caret)
           (ihx-move-relative! :lines n-lines)
           (ihx-shrink-selection)
           (ihx-apply-selection! document))))
    ((:or (:ctrl \d) (:ctrl \u0015))
     "Move up half page extending" :scroll
     [editor document caret]
     (let [n-lines (quot (get-editor-height editor) 2)]
       (-> (ihx-selection document caret)
           (ihx-move-relative! :lines (- n-lines))
           (ihx-shrink-selection)
           (ihx-apply-selection! document))))
    (\p
      "Paste" :undoable :write
      [state editor document]
      (paste-register (:registers state) editor document :select true))
    (\w
      "Select word forward" :scroll
      [state editor document caret]
      (dotimes [_ (get-prefix state)]
        (-> (ihx-selection document caret)
            (ihx-word-forward! editor)
            (ihx-apply-selection! document))))
    ((:shift \W)
     "Select WORD forward" :scroll
     [state editor document caret]
     (dotimes [_ (get-prefix state)]
       (-> (ihx-selection document caret)
           (ihx-long-word-forward! document false)
           (ihx-apply-selection! document))))
    (\e
      "Select word end" :scroll
      [state editor document caret]
      (dotimes [_ (get-prefix state)]
        (-> (ihx-selection document caret)
            (ihx-word-end! editor)
            (ihx-apply-selection! document))))
    ((:shift \E)
     "Select WORD end" :scroll
     [state document editor caret]
     (dotimes [_ (get-prefix state)]
       (-> (ihx-selection document caret)
           (ihx-long-word-end! document false)
           (ihx-apply-selection! document))))
    (\b
      "Select word backward" :scroll
      [state document editor caret]
      (dotimes [_ (get-prefix state)]
        (-> (ihx-selection document caret)
            (ihx-word-backward! editor)
            (ihx-apply-selection! document))))
    ((:shift \B)
     "Select WORD backward" :scroll
     [state document editor caret]

     (dotimes [_ (get-prefix state)]
       (-> (ihx-selection document caret)
           (ihx-long-word-backward! document false)
           (ihx-apply-selection! document))))
    ((:or \j KeyEvent/VK_DOWN)
     "Move carets down" :scroll
     [state document editor caret]
     (dotimes [_ (get-prefix state)]
       (-> (ihx-selection document caret)
           (ihx-move-relative! :lines 1)
           ihx-shrink-selection
           (ihx-apply-selection-preserving document))))
    ((:or \k KeyEvent/VK_UP)
     "Move carets up" :scroll
     [state document editor caret]
     (dotimes [_ (get-prefix state)]
       (-> (ihx-selection document caret)
           (ihx-move-relative! :lines -1)
           ihx-shrink-selection
           (ihx-apply-selection-preserving document))))
    ((:or \h KeyEvent/VK_LEFT)
     "Move carets left" :scroll
     [state document editor caret]
     (-> (ihx-selection document caret)
         (ihx-move-backward (get-prefix state))
         ihx-shrink-selection
         (ihx-apply-selection! document)))
    ((:or \l KeyEvent/VK_RIGHT)
     "Move carets right" :scroll
     [state document editor caret]
     (-> (ihx-selection document caret)
         (ihx-move-forward (get-prefix state))
         ihx-shrink-selection
         (ihx-apply-selection! document)))
    (KeyEvent/VK_HOME
      "Move carets to line start" :scroll
      [editor document caret]
      (-> (ihx-selection document caret)
          (ihx-move-line-start editor document)
          ihx-shrink-selection
          (ihx-apply-selection! document)))
    (KeyEvent/VK_END
      "Move carets to line end" :scroll
      [editor document caret]
      (-> (ihx-selection document caret)
          (ihx-move-line-end editor document)
          (ihx-move-backward 1)
          ihx-shrink-selection
          (ihx-apply-selection! document)))
    ((:shift \G)
     "Move to line number" :scroll :jumplist-add
     [state editor document]
     (do (-> (ihx-move-caret-line-n editor document (get-prefix state))
             ihx-shrink-selection
             (ihx-apply-selection! document))
         (assoc state :mode :normal)))
    (\m
      "Match menu"
      [state] (assoc state :mode :match)))

  (:select
    (\g
      "Goto mode extending" :keep-prefix
      [state] (assoc state :mode :select-goto))
    (\v
      "Back to normal mode" [state] (assoc state :mode :normal))
    ((:or (:ctrl \d) (:ctrl \u0004))
     "Move down half page extending" :scroll
     [editor document caret]
     (let [n-lines (quot (get-editor-height editor) 2)]
       (-> (ihx-selection document caret)
           (ihx-move-relative! :lines n-lines)
           (ihx-apply-selection! document))))
    ((:or (:ctrl \d) (:ctrl \u0015))
     "Move up half page extending" :scroll
     [editor document caret]
     (let [n-lines (quot (get-editor-height editor) 2)]
       (-> (ihx-selection document caret)
           (ihx-move-relative! :lines (- n-lines))
           (ihx-apply-selection! document))))
    (\p
      "Paste" :undoable :write
      [state editor document]
      (paste-register (:registers state) editor document))
    (\w
      "Select word forward extending" :scroll
      [state document editor caret]
      (dotimes [_ (get-prefix state)]
        (-> (ihx-selection document caret)
            (ihx-word-forward-extending! editor)
            (ihx-apply-selection! document))))
    ((:shift \W)
     "Select WORD forward extending" :scroll
     [state editor document caret]
     (dotimes [_ (get-prefix state)]
       (-> (ihx-selection document caret)
           (ihx-long-word-forward! document true)
           (ihx-apply-selection! document))))
    (\e
      "Select word end extending" :scroll
      [state document editor caret]
      (dotimes [_ (get-prefix state)]
        (-> (ihx-selection document caret)
            (ihx-word-end-extending! editor)
            (ihx-apply-selection! document))))
    ((:shift \E)
     "Select WORD end extending" :scroll
     [state editor document caret]
     (dotimes [_ (get-prefix state)]
       (-> (ihx-selection document caret)
           (ihx-long-word-end! document true)
           (ihx-apply-selection! document))))
    (\b
      "Select word backward extending" :scroll
      [state document editor caret]
      (dotimes [_ (get-prefix state)]
        (-> (ihx-selection document caret)
            (ihx-word-backward-extending! editor)
            (ihx-apply-selection! document))))
    ((:shift \B)
     "Select WORD backward extending" :scroll
     [state editor document caret]
     (dotimes [_ (get-prefix state)]
       (-> (ihx-selection document caret)
           (ihx-long-word-backward! document true)
           (ihx-apply-selection! document))))
    ((:or \j KeyEvent/VK_DOWN)
     "Move carets down extending" :scroll
     [state document editor caret]
     (dotimes [_ (get-prefix state)]
       (-> (ihx-selection document caret)
           (ihx-move-relative! :lines 1)
           (ihx-apply-selection-preserving document))))
    ((:or \k KeyEvent/VK_UP)
     "Move carets up extending" :scroll
     [state document editor caret]
     (dotimes [_ (get-prefix state)]
       (-> (ihx-selection document caret)
           (ihx-move-relative! :lines -1)
           (ihx-apply-selection-preserving document))))
    ((:or \h KeyEvent/VK_LEFT)
     "Move carets left extending" :scroll
     [state document editor caret]
     (-> (ihx-selection document caret)
         (ihx-move-backward (get-prefix state))
         (ihx-apply-selection! document)))
    ((:or \l KeyEvent/VK_RIGHT)
     "Move carets right extending" :scroll
     [state document editor caret]
     (-> (ihx-selection document caret)
         (ihx-move-forward (get-prefix state))
         (ihx-apply-selection! document)))
    (KeyEvent/VK_HOME
      "Move carets to line start" :scroll
      [editor document caret]
      (-> (ihx-selection document caret)
          (ihx-move-line-start editor document)
          (ihx-apply-selection! document)))
    (KeyEvent/VK_END
      "Move carets to line end" :scroll
      [editor document caret]
      (-> (ihx-selection document caret)
          (ihx-move-line-end editor document)
          (ihx-move-backward 1)
          (ihx-apply-selection! document)))
    ((:shift \G)
     "Move to line number" :scroll :jumplist-add
     [state editor document]
     (do (-> (ihx-move-caret-line-n editor document (get-prefix state))
             (ihx-apply-selection! document))
         (assoc state :mode :select)))
    (\m
      "Match menu"
      [state] (assoc state :mode :select-match)))

  (:goto
    (Character/isDigit
      "Add prefix arg" :keep-prefix [char state] (update state :prefix conj char))
    (\d
      "Goto declaration" :jumplist-add
      [editor]
      (actions editor IdeActions/ACTION_GOTO_DECLARATION)
      [state] (assoc state :mode :normal))
    (\n
      "Next tab"
      [editor]
      (actions editor IdeActions/ACTION_NEXT_TAB)
      [state]
      (assoc state :mode :normal))
    (\p
      "Previous tab"
      [editor]
      (actions editor IdeActions/ACTION_PREVIOUS_TAB)
      [state]
      (assoc state :mode :normal))
    (\h
      "Move carets to line start" :scroll
      [editor document caret]
      (-> (ihx-selection document caret)
          (ihx-move-line-start editor document)
          ihx-shrink-selection
          (ihx-apply-selection! document))
      [state] (assoc state :mode :normal))
    (\l
      "Move carets to line end" :scroll
      [editor document caret]
      (-> (ihx-selection document caret)
          (ihx-move-line-end editor document)
          (ihx-move-backward 1)
          ihx-shrink-selection
          (ihx-apply-selection! document))
      [state] (assoc state :mode :normal))
    (\g
      "Move to line number" :scroll :jumplist-add
      [state editor document]
      (do (-> (ihx-move-caret-line-n editor document (get-prefix state))
              ihx-shrink-selection
              (ihx-apply-selection! document))
          (assoc state :mode :normal)))
    (\e
      "Move to file end" :scroll :jumplist-add
      [state editor document]
      (do (-> (ihx-move-file-end editor document)
              ihx-shrink-selection
              (ihx-apply-selection! document))
          (assoc state :mode :normal)))
    (_ [state] (assoc state :mode :normal)))

  (:select-goto
    (Character/isDigit
      "Add prefix arg" :keep-prefix [char state] (update state :prefix conj char))
    (\h
      "Move carets to line start extending" :scroll
      [editor document caret]
      (-> (ihx-selection document caret)
          (ihx-move-line-start editor document)
          (ihx-apply-selection! document))
      [state] (assoc state :mode :select))
    (\l
      "Move carets to line end extending" :scroll
      [editor document caret]
      (-> (ihx-selection document caret)
          (ihx-move-line-end editor document)
          (ihx-apply-selection! document))
      [state] (assoc state :mode :select))
    (\g
      "Move to line number" :scroll :jumplist-add
      [state editor document]
      (do (-> (ihx-move-caret-line-n editor document (get-prefix state))
              (ihx-apply-selection! document))
          (assoc state :mode :select)))
    (\e
      "Move to file end" :scroll :jumplist-add
      [state editor document]
      (do (-> (ihx-move-file-end editor document)
              (ihx-apply-selection! document))
          (assoc state :mode :select)))
    (_ [state] (assoc state :mode :select)))

  (:space
    (\f
      "File finder"
      [state project editor]
      (do
        (search-file-name project editor)
        (assoc state :mode :normal)))
    (\r
      "Rename symbol"
      [project editor document state]
      (do
        (actions editor IdeActions/ACTION_RENAME)
        (assoc state :mode :normal)))
    (\/
      "Global search"
      [state project editor]
      (let [manager (.. SearchEverywhereManager (getInstance project))
            data-context (.getDataContext editor)
            action-event (AnActionEvent/createFromDataContext
                           ActionPlaces/KEYBOARD_SHORTCUT nil data-context)]
        (.show manager "TextSearchContributor" nil action-event)
        (assoc state :mode :normal))))

  (:view
    ((:or \z \c)
     "Center screen"
     [editor state]
     (let [caret (.. editor getCaretModel getPrimaryCaret)
           scrolling-model (.getScrollingModel editor)]
       (.scrollTo scrolling-model
                  (.offsetToLogicalPosition editor (.. caret getOffset))
                  ScrollType/CENTER)
       (assoc state :mode (:previous-mode state)))))

  (:window
    (\v
      "Split vertical"
      [editor] (actions editor "SplitVertically")
      [state] (assoc state :mode :normal))
    ((:or \w (:ctrl \u0017))
     "Switch split"
     [editor] (actions editor "NextSplitter")
     [state] (assoc state :mode :normal))
    (\o
      [editor] (actions editor "UnsplitAll")
      [state] (assoc state :mode :normal))
    (_ [state] (assoc state :mode :normal)))

  (:insert
    ((:or (:ctrl \n) (:ctrl \u000e))
     "Select next completion item"
     [state editor event]
     (when (= (.getID event) KeyEvent/KEY_PRESSED)
       (when-let [^LookupImpl lookup (LookupManager/getActiveLookup editor)]
         (.setSelectedIndex lookup (inc (.getSelectedIndex lookup)))
         state)))
    ((:or (:ctrl \p) (:ctrl \u0010))
     "Select next completion item"
     [state editor event]
     (when (= (.getID event) KeyEvent/KEY_PRESSED)
       (when-let [^LookupImpl lookup (LookupManager/getActiveLookup editor)]
         (.setSelectedIndex lookup (dec (.getSelectedIndex lookup)))
         state)))
    (_ [state] (assoc state :pass true)))


  (:match
    (\i
      [state] (assoc state :mode :match-inside))
    (\a
      [state] (assoc state :mode :match-around)))

  (:select-match
    (\i
      [state] (assoc state :mode :select-match-inside))
    (\a
      [state] (assoc state :mode :select-match-around)))

  (:match-inside
    (_
      "Select inside"
      [project state document caret char]
      (-> (ihx-selection document caret)
          (ihx-select-inside document char)
          (ihx-apply-selection! document))
      [state] (assoc state :mode :normal)))

  (:select-match-inside
    (_
      "Select inside"
      [project state document caret char]
      (-> (ihx-selection document caret)
          (ihx-select-inside document char)
          (ihx-apply-selection! document))
      [state] (assoc state :mode :select)))

  (:match-around
    (_
      "Select around"
      [project state document caret char]
      (-> (ihx-selection document caret)
          (ihx-select-around document char)
          (ihx-apply-selection! document))
      [state] (assoc state :mode :normal)))

  (:select-match-around
    (_
      "Select around"
      [project state document caret char]
      (-> (ihx-selection document caret)
          (ihx-select-around document char)
          (ihx-apply-selection! document))))

  ((:or :match :select-match)
   (\s
     [state] (assoc state :mode :match-surround-add))
   (\d
     [state] (assoc state :mode :match-surround-delete))
   (\r
     [state] (assoc state :mode :match-find)))

  (:match
    (\m
      "Goto matching bracket"
      [project document editor caret]
      (-> (ihx-selection document caret)
          (ihx-goto-matching document)
          ihx-shrink-selection
          (ihx-apply-selection! document))
      [state] (assoc state :mode :normal)))

  (:select-match
    (\m
      "Goto matching bracket"
      [document editor caret]
      (-> (ihx-selection document caret)
          (ihx-goto-matching document)
          (ihx-apply-selection! document))
      [state] (assoc state :mode :select)))

  (:match-surround-add
    (_
      "Surround add" :write
      [project state document editor caret char]
      (-> (ihx-selection document caret)
          (ihx-surround-add document char)
          (ihx-apply-selection! document))))

  (:match-surround-delete
    (_
      "Surround delete" :write
      [document editor caret char]
      (-> (ihx-selection document caret)
          (ihx-surround-delete document char)
          (ihx-apply-selection! document))
      [state] (assoc state :mode :normal)))

  (:match-find
    (_
      "Find the matching brackets"
      [state editor document char]
      (ihx-surround-find state editor document char)))

  (:match-replace
    (_
      "Replace the matching brackets" :write
      [state editor document char]
      (ihx-surround-replace state editor document char))))


(defn handle-editor-event
  [project ^EditorImpl editor ^KeyEvent event]
  (let [project-state (or (get @state-atom project) {:mode :normal})
        mode (:mode project-state)
        debounce (:debounce project-state)
        result-fn (partial editor-handler project project-state editor event)
        result
        (try
          (if (= mode :insert)
            (cond
              (and (= (.getID event) KeyEvent/KEY_PRESSED)
                   (= (.getKeyCode event) KeyEvent/VK_ESCAPE)) (result-fn)
              (and (not= (.getID event) KeyEvent/KEY_PRESSED)
                   (= (.getKeyCode event) KeyEvent/VK_ESCAPE)) nil
              (and debounce (= (.getID event) KeyEvent/KEY_TYPED))
              (assoc project-state :debounce false)
              :else (result-fn))
            (if (and (= (.getID event) KeyEvent/KEY_PRESSED)
                     (not (#{KeyEvent/VK_SHIFT KeyEvent/VK_CONTROL KeyEvent/VK_ALT KeyEvent/VK_META} (.getKeyCode event))))
              (result-fn)
              nil))
          (catch StartMarkAction$AlreadyStartedException e
            (quit-insert-mode project project-state (.getDocument editor))
            (throw e)))]
    (cond
      (:pass result) false
      (map? result) (do
                      (.consume event)
                      (let [new-state (merge project-state result)]
                        (swap! state-atom assoc project new-state)
                        (ui/update-mode-panel! project new-state))
                      true)
      :default (do
                 (.consume event)
                 true))))
