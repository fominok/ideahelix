;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns fominok.ideahelix.editor
  (:require
    [fominok.ideahelix.editor.action :refer [actions]]
    [fominok.ideahelix.editor.modification :refer :all]
    [fominok.ideahelix.editor.movement :refer :all]
    [fominok.ideahelix.editor.registers :refer :all]
    [fominok.ideahelix.editor.selection :refer :all]
    [fominok.ideahelix.editor.ui :as ui]
    [fominok.ideahelix.editor.util :refer [for-each-caret]]
    [fominok.ideahelix.keymap :refer [defkeymap]])
  (:import
    (com.intellij.openapi.actionSystem
      IdeActions)
    (com.intellij.openapi.editor.event
      CaretListener)
    (com.intellij.openapi.editor.impl
      EditorImpl)
    (java.awt.event
      KeyEvent)))


;; We're allowed to use "thread-unsafe" mutable state since events are coming within 1 thread
;; which is blocked until it is decided what to do with an event.
(defonce state (volatile! {}))


(def get-prefix
  (memoize
    (fn [state]
      (if-let [prefix-vec (get state :prefix)]
        (min 10000 (Integer/parseInt (apply str prefix-vec)))
        1))))


(defkeymap
  editor-handler

  (:any
    (KeyEvent/VK_ESCAPE
      "Back to normal mode"
      [state document caret]
      (when (= :insert (:mode state)) (leave-insert-mode document caret))
      [state project editor]
      (let [new-state (assoc state :mode :normal :prefix nil)]
        (if (= :insert (:mode state))
          (do (finish-undo project editor (:mark-action state))
              (dissoc new-state :mark-action))
          new-state))))

  ((:or :normal :select)
   (\u
     "Undo"
     [editor] (actions editor IdeActions/ACTION_UNDO))
   ((:shift \U)
    "Redo"
    [editor] (actions editor IdeActions/ACTION_REDO))
   (\y
     "Yank"
     [project-state editor document]
     (let [registers (copy-to-register (:registers project-state) editor document)]
       (assoc project-state :registers registers)))
   (\o
     "New line below" :write
     [editor document caret] (do (insert-new-line-below editor document caret)
                                 (into-insert-mode-prepend caret))
     [project editor state]
     (assoc state :mode :insert :prefix nil :mark-action (start-undo project editor)))
   ((:shift \O)
    "New line above" :write
    [document caret] (do (insert-new-line-above document caret)
                         (into-insert-mode-prepend caret))
    [project editor state]
    (assoc state :mode :insert :prefix nil :mark-action (start-undo project editor)))

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
     [project-state editor document]
     (delete-selections project-state editor document))
   (\c
     "Replace selections" :write
     [project-state project editor document]
     (replace-selections project-state project editor document))
   (\a
     "Append to selections"
     [caret] (into-insert-mode-append caret)
     [project editor state]
     (assoc state :mode :insert :prefix nil :mark-action (start-undo project editor)))
   ((:shift \A)
    "Append to line"
    [document caret] (do (move-caret-line-end document caret)
                         (into-insert-mode-append caret))
    [project editor state]
    (assoc state :mode :insert :prefix nil :mark-action (start-undo project editor)))
   (\i
     "Prepend to selections"
     [caret] (into-insert-mode-prepend caret)
     [project editor state]
     (assoc state :mode :insert :prefix nil :mark-action (start-undo project editor)))
   ((:shift \I)
    "Prepend to lines"
    [document caret] (do (move-caret-line-start document caret)
                         (into-insert-mode-prepend caret))
    [project editor state]
    (assoc state :mode :insert :prefix nil :mark-action (start-undo project editor)))
   ((:or (:alt \;) (:alt \u2026))
    "Flip selection" :undoable
    [caret] (flip-selection caret))
   ((:or (:alt \:) (:alt \u00DA))
    "Make selections forward" :undoable
    [caret] (ensure-selection-forward caret))
   (\;
     "Shrink selections to 1 char" :undoable
     [document caret]
     (-> (ihx-selection document caret)
         ihx-shrink-selection
         (ihx-apply-selection! document)))
   (\,
     "Drop all selections but primary" :undoable
     [editor] (keep-primary-selection editor))
   (\x
     "Select whole lines extending" :undoable :scroll
     [state document caret]
     (dotimes [_ (min 10000 (get-prefix state))] (select-lines document caret :extend true)))
   ((:shift \X)
    "Select whole lines" :undoable :scroll
    [document caret] (select-lines document caret :extend false))
   ((:shift \C)
    "Add selections below" :undoable
    [state editor caret]
    (add-selection-below editor caret)))

  (:normal
    (\g "Goto mode" :keep-prefix [state] (assoc state :mode :goto))
    (\v "Selection mode" [state] (assoc state :mode :select))
    (\p
      "Paste" :undoable :write
      [project-state editor document]
      (paste-register (:registers project-state) editor document :select true))
    (\w
      "Select word forward" :undoable :scroll
      [state editor document caret]
      (dotimes [_ (get-prefix state)]
        (-> (ihx-selection document caret)
            (ihx-word-forward! editor)
            (ihx-apply-selection! document))))
    (\b
      "Select word backward" :undoable :scroll
      [state document editor caret]
      (dotimes [_ (get-prefix state)]
        (-> (ihx-selection document caret)
            (ihx-word-backward! editor)
            (ihx-apply-selection! document))))
    ((:or \j KeyEvent/VK_DOWN)
     "Move carets down" :undoable :scroll
     [state document caret]
     (dotimes [_ (min 10000 (get-prefix state))] (move-caret-down document caret)))
    ((:or \k KeyEvent/VK_UP)
     "Move carets up" :undoable :scroll
     [state document caret]
     (dotimes [_ (min 10000 (get-prefix state))] (move-caret-up document caret)))
    ((:or \h KeyEvent/VK_LEFT)
     "Move carets left" :undoable :scroll
     [state document caret]
     (-> (ihx-selection document caret)
         (ihx-move-backward (get-prefix state))
         ihx-shrink-selection
         (ihx-apply-selection! document)))
    ((:or \l KeyEvent/VK_RIGHT)
     "Move carets right" :undoable :scroll
     [state document caret]
     (-> (ihx-selection document caret)
         (ihx-move-forward (get-prefix state))
         ihx-shrink-selection
         (ihx-apply-selection! document)))
    ((:shift \G)
     "Move to line number" :undoable :scroll
     [state editor document] (move-caret-line-n editor document (get-prefix state))
     [state] (assoc state :mode :normal)))

  (:select
    (\g
      "Goto mode extending" :undoable :keep-prefix
      [state] (assoc state :mode :select-goto))
    (\v
      "Back to normal mode" [state] (assoc state :mode :normal))
    (\p
      "Paste" :undoable :write
      [project-state editor document]
      (paste-register (:registers project-state) editor document))
    (\w
      "Select word forward extending" :undoable :scroll
      [state document editor caret]
      (dotimes [_ (get-prefix state)]
        (-> (ihx-selection document caret)
            (ihx-word-forward-extending! editor)
            (ihx-apply-selection! document))))
    (\b
      "Select word backward extending" :undoable :scroll
      [state document editor caret]
      (dotimes [_ (get-prefix state)]
        (-> (ihx-selection document caret)
            (ihx-word-backward-extending! editor)
            (ihx-apply-selection! document))))
    ((:or \j KeyEvent/VK_DOWN)
     "Move carets down extending" :undoable :scroll
     [state document caret]
     (dotimes [_ (min 10000 (get-prefix state))] (extending document caret (partial move-caret-down document))))
    ((:or \k KeyEvent/VK_UP)
     "Move carets up extending" :undoable :scroll
     [state document caret]
     (dotimes [_ (min 10000 (get-prefix state))] (extending document caret (partial move-caret-up document))))
    ((:or \h KeyEvent/VK_LEFT)
     "Move carets left extending" :undoable :scroll
     [state document caret]
     (-> (ihx-selection document caret)
         (ihx-move-backward (get-prefix state))
         (ihx-apply-selection! document)))
    ((:or \l KeyEvent/VK_RIGHT)
     "Move carets right extending" :undoable :scroll
     [state document caret]
     (-> (ihx-selection document caret)
         (ihx-move-forward (get-prefix state))
         (ihx-apply-selection! document)))
    ((:shift \G)
     "Move to line number" :undoable :scroll
     [state editor document]
     (let [caret (.. editor getCaretModel getPrimaryCaret)]
       (extending document caret (fn [_] (move-caret-line-n editor document (get-prefix state))))
       (assoc state :mode :select))))

  (:goto
    (Character/isDigit
      "Add prefix arg" :keep-prefix [char state] (update state :prefix conj char))
    (\h
      "Move carets to line start" :undoable :scroll
      [document caret] (-> (move-caret-line-start document caret)
                           ihx-shrink-selection
                           (ihx-apply-selection! document))
      [state] (assoc state :mode :normal))
    (\l
      "Move carets to line end" :undoable :scroll
      [document caret] (-> (move-caret-line-end document caret)
                           ihx-shrink-selection
                           (ihx-apply-selection! document))
      [state] (assoc state :mode :normal))
    (\g
      "Move to line number" :undoable :scroll
      [state editor document] (move-caret-line-n editor document (get-prefix state))
      [state] (assoc state :mode :normal))
    (\e "Move to file end" :undoable :scroll
        [state editor document]
        (move-caret-line-n editor document (.getLineCount document))
        [state] (assoc state :mode :normal))
    (_ [state] (assoc state :mode :normal)))

  (:select-goto
    (Character/isDigit
      "Add prefix arg" :keep-prefix [char state] (update state :prefix conj char))
    (\h
      "Move carets to line start extending" :undoable :scroll
      [document caret] (-> (move-caret-line-start document caret)
                           (ihx-apply-selection! document))
      [state] (assoc state :mode :select))
    (\l
      "Move carets to line end extending" :undoable :scroll
      [document caret] (-> (move-caret-line-end document caret)
                           (ihx-apply-selection! document))
      [state] (assoc state :mode :select))
    (\g
      "Move to line number" :undoable :scroll
      [state editor document]
      (let [caret (.. editor getCaretModel getPrimaryCaret)]
        (extending document caret (fn [_] (move-caret-line-n editor document (get-prefix state))))
        (assoc state :mode :select)))
    (\e
      "Move to file end" :undoable :scroll
      [state editor document]
      (let [caret (.. editor getCaretModel getPrimaryCaret)]
        (extending document caret (fn [_] (move-caret-line-n editor document (.getLineCount document))))
        (assoc state :mode :select)))
    (_ [state] (assoc state :mode :select)))

  (:insert
    #_((:or (:ctrl \a) (:ctrl \u0001)) [document caret] (move-caret-line-start document caret))
    #_((:or (:ctrl \e) (:ctrl \u0005)) [document caret] (move-caret-line-end document caret))
    (KeyEvent/VK_BACK_SPACE :write [document caret] (backspace document caret))
    (KeyEvent/VK_ENTER :write [document caret] (insert-newline document caret))
    (_ :write [document caret char] (insert-char document caret char))))


(defn- caret-listener
  [editor]
  (reify CaretListener
    (caretPositionChanged
      [_ event]
      (ui/highlight-primary-caret editor event))))


(defn handle-editor-event
  [project ^EditorImpl editor ^KeyEvent event]
  (let [project-state (or (get @state project) {project {editor {:mode :normal}}})
        editor-state (get project-state editor)]
    (try
      (let [result (editor-handler project project-state editor-state editor event)]
        (when-not (:caret-listener editor-state)
          (let [listener (caret-listener editor)]
            (.. editor getCaretModel (addCaretListener listener))
            (vswap! state assoc-in [project editor :caret-listener] listener)))
        (cond
          (= :pass result) false

          (map? result) (do
                          (.consume event)
                          (vswap! state assoc project result)
                          (ui/update-mode-panel! project (get-in @state [project editor]))
                          true)
          :default (do
                     (.consume event)
                     true)))
      (catch Exception e
        (when-let [mark (:mark-action editor-state)]
          (finish-undo project editor mark))
        (vswap! state assoc-in [project editor :mark-action] nil)
        (throw e)))))
