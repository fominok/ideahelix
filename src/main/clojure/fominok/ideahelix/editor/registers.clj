;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns fominok.ideahelix.editor.registers
  (:require
    [fominok.ideahelix.editor.selection :refer :all]))


(defn copy-to-register
  [registers editor document & {:keys [register] :or {register \"}}]
  (let [model (.getCaretModel editor)
        strings
        (let [strings* (transient [])]
          (.runForEachCaret
            model
            (fn [caret] (conj! strings* (.getText document (.getSelectionRange caret)))))
          (persistent! strings*))]
    (assoc registers register strings)))


(defn paste-register
  [registers editor document & {:keys [register select] :or {register \" select false}}]
  (let [register-contents (get registers register)
        strings (concat register-contents (some-> (last register-contents) repeat))
        pairs (map (fn [caret string] [(ihx-selection document caret) string])
                   (.. editor getCaretModel getAllCarets)
                   strings)]
    (when (not (empty? strings))
      (doseq [[selection string] pairs
              :let [pos (min (.getTextLength document)
                             (inc (max (:anchor selection) (:offset selection))))]]
        (.insertString document pos string)
        (-> selection
            (assoc :anchor pos)
            (assoc :offset (dec (+ pos (count string))))
            (ihx-apply-selection! document))))))
