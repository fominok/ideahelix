;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns fominok.ideahelix.bridge
  (:require
    [fominok.ideahelix.core :as core])
  (:import
    (clojure.lang
      Keyword)
    (com.intellij.openapi.editor
      Editor)
    (com.intellij.openapi.project
      Project)
    (java.awt.event
      KeyEvent))
  (:gen-class
    :name fominok.ideahelix.Bridge
    :methods [^{:static true} [pushEditorEvent [com.intellij.openapi.project.Project com.intellij.openapi.editor.Editor java.awt.event.KeyEvent] boolean]
              ^{:static true} [focusEditor [com.intellij.openapi.project.Project com.intellij.openapi.editor.Editor] void]
              ^{:static true} [releaseEditor [com.intellij.openapi.project.Project com.intellij.openapi.editor.Editor] void]
              ^{:static true} [currentMode [com.intellij.openapi.project.Project] java.lang.String]
              ^{:static true} [currentModeDisplay [com.intellij.openapi.project.Project] java.lang.String]]))


(defn -pushEditorEvent
  [^Project project ^Editor editor ^KeyEvent event]
  (boolean (core/push-editor-event project editor event)))


(defn -focusEditor
  [^Project project ^Editor editor]
  (core/focus-editor project editor))


(defn -releaseEditor
  [^Project project ^Editor editor]
  (core/release-editor project editor))


(defn -currentMode
  [^Project project]
  (let [value (core/current-mode project)]
    (cond
      (instance? Keyword value) (name value)
      (nil? value) "normal"
      :else (str value))))


(defn -currentModeDisplay
  [^Project project]
  (core/current-mode-display project))
