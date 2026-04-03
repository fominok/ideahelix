;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns fominok.ideahelix.editor.ui
  (:import
    (com.intellij.openapi.editor
      CaretVisualAttributes
      CaretVisualAttributes$Weight)
    (com.intellij.ui
      JBColor)))


(defn highlight-primary-caret
  [editor event]
  (let [primary-caret (.. editor getCaretModel getPrimaryCaret)
        primary-attributes
        (CaretVisualAttributes. JBColor/GRAY CaretVisualAttributes$Weight/HEAVY)
        secondary-attributes
        (CaretVisualAttributes. JBColor/BLACK CaretVisualAttributes$Weight/HEAVY)
        caret (.getCaret event)]
    (.setVisualAttributes caret
                          (if (= caret primary-caret)
                            primary-attributes
                            secondary-attributes))))
