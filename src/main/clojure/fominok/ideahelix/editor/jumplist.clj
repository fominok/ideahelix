;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns fominok.ideahelix.editor.jumplist
  (:require
    [fominok.ideahelix.editor.selection :refer :all])
  (:import
    (com.intellij.openapi.fileEditor
      FileEditorManager)
    (com.intellij.openapi.fileEditor.impl
      EditorWindow
      FileEditorManagerImpl
      FileEditorOpenOptions)
    (com.intellij.openapi.vfs
      VirtualFile)
    (com.intellij.util.ui
      UIUtil)))


(defn- serialize-selection
  [selection]
  (dissoc selection :caret :in-append))


(defn- ensure-project-state
  [project-state]
  (or project-state {:mode :normal}))


(defn jumplist-add
  [project project-state]
  (if-let [editor (.. (FileEditorManager/getInstance project) getSelectedTextEditor)]
    (let [document (.getDocument editor)
          {:keys [stack pointer] :or {pointer -1}} (:jumplist project-state)
          model (.getCaretModel editor)
          primary-caret (.getPrimaryCaret model)
          secondary-carets (filter (partial not= primary-caret) (.getAllCarets model))
          new-stack (into [] (take (inc pointer) stack))
          serialize (comp serialize-selection (partial ihx-selection document))
          entry {:file (.getVirtualFile editor)
                 :primary-caret    (serialize primary-caret)
                 :secondary-carets (doall (map serialize secondary-carets))}
          last-entry (peek new-stack)]
      (if (= entry last-entry)
        (-> project-state
            (assoc-in [:jumplist :stack] new-stack)
            (assoc-in [:jumplist :pointer] (dec (count new-stack))))
        (-> project-state
            (assoc-in [:jumplist :stack] (conj new-stack entry))
            (assoc-in [:jumplist :pointer] (count new-stack)))))
    project-state))


(defn capture-current-location!
  [state-atom project]
  (swap! state-atom update project #(jumplist-add project (ensure-project-state %))))


(defn capture-current-location-later!
  [state-atom project]
  (UIUtil/invokeLaterIfNeeded
    (fn []
      (capture-current-location! state-atom project))))


(defn- open-file-current-window
  [project file]
  (let [manager (FileEditorManager/getInstance project)
        window (.getCurrentWindow manager)]
    (.openFile ^FileEditorManagerImpl manager ^VirtualFile file ^EditorWindow window (FileEditorOpenOptions.))
    (UIUtil/invokeLaterIfNeeded
      (fn [] (.. manager getSelectedTextEditor getContentComponent requestFocus)))))


(defn- jumplist-apply!
  [project-state project new-pointer]
  (let [stack (get-in project-state [:jumplist :stack])
        {:keys [primary-caret secondary-carets file]} (nth stack new-pointer)
        _  (open-file-current-window project file)
        editor (.. (FileEditorManager/getInstance project) getSelectedTextEditor)
        document (.getDocument editor)
        model (.getCaretModel editor)
        primary (.getPrimaryCaret model)
        text-length (.getTextLength document)]
    (-> (->IhxSelection primary
                        (:anchor primary-caret)
                        (:offset primary-caret)
                        false)
        (ihx-apply-selection! document))
    (doseq [{:keys [anchor offset]} secondary-carets]
      (when-let [caret (.addCaret model (.offsetToVisualPosition editor (min text-length offset)))]
        (-> (->IhxSelection caret anchor offset false)
            (ihx-apply-selection! document))))
    (assoc-in project-state [:jumplist :pointer] new-pointer)))


(defn jumplist-backward!
  [project-state project]
  (let [{:keys [pointer] :or {pointer -1}} (:jumplist project-state)
        new-pointer (dec pointer)]
    (if (>= new-pointer 0)
      (jumplist-apply! project-state project new-pointer)
      project-state)))


(defn jumplist-forward!
  [project-state project]
  (let [{:keys [pointer stack] :or {pointer -1}} (:jumplist project-state)
        new-pointer (inc pointer)]
    (if (< new-pointer (count stack))
      (assoc-in (jumplist-apply! project-state project new-pointer)
                [:jumplist :pointer] new-pointer)
      project-state)))
