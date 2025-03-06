;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns fominok.ideahelix.editor.util)


(defn deep-merge
  [& maps]
  (reduce (fn [m1 m2]
            (merge-with (fn [v1 v2]
                          (if (and (map? v1) (map? v2))
                            (deep-merge v1 v2)
                            v2))
                        m1 m2))
          maps))


(defn get-caret-contents
  [document caret]
  (.getText document (.getSelectionRange caret)))
