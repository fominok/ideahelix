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
      (when (= :insert (:mode state))
        (-> (ihx-selection document caret :insert-mode true)
            ihx-append-quit
            (ihx-apply-selection! document)))
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
     "New line below" :write :scroll
     [editor document caret]
     (-> (ihx-selection document caret)
         (ihx-new-line-below editor document)
         (ihx-apply-selection! document))
     [project editor state]
     (assoc state :mode :insert :prefix nil :mark-action (start-undo project editor)))
   ((:shift \O)
    "New line above" :write :scroll
    [editor document caret]
    (-> (ihx-selection document caret)
        (ihx-new-line-above editor document)
        (ihx-apply-selection! document))
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
     [document caret]
     (-> (ihx-selection document caret)
         ihx-make-forward
         ihx-append
         (ihx-apply-selection! document))
     [project editor state]
     (assoc state :mode :insert :prefix nil :mark-action (start-undo project editor)))
   ((:shift \A)
    "Append to line"
    [editor document caret]
    (-> (ihx-selection document caret)
        (ihx-move-line-end editor document)
        ihx-shrink-selection
        (ihx-apply-selection! document))
    [project editor state]
    (assoc state :mode :insert :prefix nil :mark-action (start-undo project editor)))
   (\i
     "Prepend to selections"
     [document caret]
     (-> (ihx-selection document caret)
         ihx-make-backward
         (ihx-apply-selection! document))
     [project editor state]
     (assoc state :mode :insert :prefix nil :mark-action (start-undo project editor)))
   ((:shift \I)
    "Prepend to lines"
    [editor document caret]
    (-> (ihx-selection document caret)
        (ihx-move-line-start editor document)
        ihx-shrink-selection
        (ihx-apply-selection! document))
    [project editor state]
    (assoc state :mode :insert :prefix nil :mark-action (start-undo project editor)))
   ((:or (:alt \;) (:alt \u2026))
    "Flip selection" :scroll
    [document caret] (-> (ihx-selection document caret)
                         flip-selection
                         (ihx-apply-selection! document)))
   ((:or (:alt \:) (:alt \u00DA))
    "Make selections forward"
    [document caret]
    (-> (ihx-selection document caret)
        ihx-make-forward
        (ihx-apply-selection! document)))
   (\;
     "Shrink selections to 1 char"
     [document caret]
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
    [project-state project editor document]
    (jumplist-backward! project-state project editor document))
   ((:or (:ctrl \i) (:ctrl \u0009))
    "Jump forward"
    [project-state project editor document]
    (jumplist-forward! project-state project editor document))
   ((:or (:ctrl \s) (:ctrl \u0013))
    "Add to jumplist"
    [project-state editor document]
    (jumplist-add project-state editor document)))

  (:normal
    (\g "Goto mode" :keep-prefix [state] (assoc state :mode :goto))
    (\v "Selection mode" [state] (assoc state :mode :select))
    (\p
      "Paste" :undoable :write
      [project-state editor document]
      (paste-register (:registers project-state) editor document :select true))
    (\w
      "Select word forward" :scroll
      [state editor document caret]
      (dotimes [_ (get-prefix state)]
        (-> (ihx-selection document caret)
            (ihx-word-forward! editor)
            (ihx-apply-selection! document))))
    (\b
      "Select word backward" :scroll
      [state document editor caret]
      (dotimes [_ (get-prefix state)]
        (-> (ihx-selection document caret)
            (ihx-word-backward! editor)
            (ihx-apply-selection! document))))
    ((:or \j KeyEvent/VK_DOWN)
     "Move carets down" :scroll
     [state document caret]
     (dotimes [_ (get-prefix state)]
       (-> (ihx-selection document caret)
           (ihx-move-relative! :lines 1)
           ihx-shrink-selection
           (ihx-apply-selection-preserving document))))
    ((:or \k KeyEvent/VK_UP)
     "Move carets up" :scroll
     [state document caret]
     (dotimes [_ (get-prefix state)]
       (-> (ihx-selection document caret)
           (ihx-move-relative! :lines -1)
           ihx-shrink-selection
           (ihx-apply-selection-preserving document))))
    ((:or \h KeyEvent/VK_LEFT)
     "Move carets left" :scroll
     [state document caret]
     (-> (ihx-selection document caret)
         (ihx-move-backward (get-prefix state))
         ihx-shrink-selection
         (ihx-apply-selection! document)))
    ((:or \l KeyEvent/VK_RIGHT)
     "Move carets right" :scroll
     [state document caret]
     (-> (ihx-selection document caret)
         (ihx-move-forward (get-prefix state))
         ihx-shrink-selection
         (ihx-apply-selection! document)))
    ((:shift \G)
     "Move to line number" :scroll :jumplist-add
     [state editor document]
     (do (-> (ihx-move-caret-line-n editor document (get-prefix state))
             ihx-shrink-selection
             (ihx-apply-selection! document))
         (assoc state :mode :normal))))

  (:select
    (\g
      "Goto mode extending" :keep-prefix
      [state] (assoc state :mode :select-goto))
    (\v
      "Back to normal mode" [state] (assoc state :mode :normal))
    (\p
      "Paste" :undoable :write
      [project-state editor document]
      (paste-register (:registers project-state) editor document))
    (\w
      "Select word forward extending" :scroll
      [state document editor caret]
      (dotimes [_ (get-prefix state)]
        (-> (ihx-selection document caret)
            (ihx-word-forward-extending! editor)
            (ihx-apply-selection! document))))
    (\b
      "Select word backward extending" :scroll
      [state document editor caret]
      (dotimes [_ (get-prefix state)]
        (-> (ihx-selection document caret)
            (ihx-word-backward-extending! editor)
            (ihx-apply-selection! document))))
    ((:or \j KeyEvent/VK_DOWN)
     "Move carets down extending" :scroll
     [state document caret]
     (dotimes [_ (get-prefix state)]
       (-> (ihx-selection document caret)
           (ihx-move-relative! :lines 1)
           (ihx-apply-selection-preserving document))))
    ((:or \k KeyEvent/VK_UP)
     "Move carets up extending" :scroll
     [state document caret]
     (dotimes [_ (get-prefix state)]
       (-> (ihx-selection document caret)
           (ihx-move-relative! :lines -1)
           (ihx-apply-selection-preserving document))))
    ((:or \h KeyEvent/VK_LEFT)
     "Move carets left extending" :scroll
     [state document caret]
     (-> (ihx-selection document caret)
         (ihx-move-backward (get-prefix state))
         (ihx-apply-selection! document)))
    ((:or \l KeyEvent/VK_RIGHT)
     "Move carets right extending" :scroll
     [state document caret]
     (-> (ihx-selection document caret)
         (ihx-move-forward (get-prefix state))
         (ihx-apply-selection! document)))
    ((:shift \G)
     "Move to line number" :scroll :jumplist-add
     [state editor document]
     (do (-> (ihx-move-caret-line-n editor document (get-prefix state))
             (ihx-apply-selection! document))
         (assoc state :mode :select))))

  (:goto
    (Character/isDigit
      "Add prefix arg" :keep-prefix [char state] (update state :prefix conj char))
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

  (:insert
    #_((:or (:ctrl \a) (:ctrl \u0001)) [document caret] (move-caret-line-start document caret))
    #_((:or (:ctrl \e) (:ctrl \u0005)) [document caret] (move-caret-line-end document caret))
    (KeyEvent/VK_BACK_SPACE :write [document caret] (backspace document caret))
    (KeyEvent/VK_ENTER :write
                       [document caret]
                       (-> (ihx-selection document caret :insert-mode true)
                           (insert-newline document)
                           (ihx-apply-selection! document)))
    (_ :write
       [document caret char]
       (-> (ihx-selection document caret :insert-mode true)
           (ihx-insert-char document char)
           (ihx-apply-selection! document)))))


(defn- caret-listener
  [editor]
  (reify CaretListener
    (caretPositionChanged
      [_ event]
      (ui/highlight-primary-caret editor event))))


(defn handle-editor-event
  [project ^EditorImpl editor ^KeyEvent event]
  (let [project-state (or (get @state project) {project {editor {:mode :normal}}})
        editor-state (get project-state editor)
        result (editor-handler project project-state editor-state editor event)]
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
                 true))))
