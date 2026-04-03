;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns fominok.ideahelix.editor.action
  (:import
    (com.intellij.openapi.actionSystem
      ActionManager
      ActionPlaces
      AnActionEvent)
    (com.intellij.openapi.command.undo
      UndoManager)
    (com.intellij.openapi.editor.impl
      EditorImpl)
    (com.intellij.openapi.fileEditor.impl.text
      TextEditorProvider)))


(defn actions
  [^EditorImpl editor & action-names]
  (let [data-context (.getDataContext editor)]
    (doseq [action-name action-names]
      (let [action (.getAction (ActionManager/getInstance) action-name)]
        (when action
          (let [event (AnActionEvent/createFromAnAction
                        action
                        nil
                        ActionPlaces/KEYBOARD_SHORTCUT
                        data-context)]
            (.beforeActionPerformedUpdate action event)
            (when (.. event getPresentation isEnabledAndVisible)
              (.actionPerformed action event))))))))


(defn- action-available?
  [^EditorImpl editor availability-check]
  (when-let [project (.getProject editor)]
    (when-let [file-editor (.. TextEditorProvider getInstance (getTextEditor editor))]
      (availability-check (UndoManager/getInstance project) file-editor))))


(defn undo-available?
  [^EditorImpl editor]
  (action-available? editor #(.isUndoAvailable %1 %2)))


(defn redo-available?
  [^EditorImpl editor]
  (action-available? editor #(.isRedoAvailable %1 %2)))
