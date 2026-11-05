;;; SPDX-FileCopyrightText: 2021 Arthur A. Gleckler
;;; SPDX-License-Identifier: MIT

(define-library (srfi 146 gleckler vector-edit)
  (import (scheme base))
  (export vector-edit vector-replace-one vector-without)
  (include "vector-edit.scm"))
