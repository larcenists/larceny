(define-library (srfi 130)

  (export 

    string-cursor?
    string-cursor-start    string-cursor-end
    string-cursor-next     string-cursor-prev
    string-cursor-forward  string-cursor-back
    string-cursor=?
    string-cursor<?        string-cursor>?
    string-cursor<=?       string-cursor>=?
    string-cursor-diff
    string-cursor->index   string-index->cursor

    string-null? 
    string-every string-any

    string-tabulate
    string-unfold   string-unfold-right

    string->list/cursors string->vector/cursors
    reverse-list->string string-join

    string-ref/cursor
    substring/cursors  string-copy/cursors
    string-take        string-take-right
    string-drop        string-drop-right
    string-pad         string-pad-right 
    string-trim        string-trim-right string-trim-both

    string-prefix-length    string-suffix-length
    string-prefix?          string-suffix?    

    string-index     string-index-right
    string-skip      string-skip-right
    string-contains  string-contains-right

    string-reverse
    string-concatenate  string-concatenate-reverse
    string-fold         string-fold-right
    string-for-each-cursor
    string-replicate    string-count
    string-replace      string-split
    string-filter       string-remove 

    )

  (import (scheme base)
          (scheme case-lambda)
          (only (rnrs base) div mod assertion-violation)
          (only (srfi 1) last-pair)
          (except (srfi 13)
                  string-index
                  string-index-right
                  string-skip
                  string-skip-right
                  string-map
                  string-for-each)
          (prefix (only (srfi 13)
                        string-index
                        string-index-right
                        string-skip
                        string-skip-right)
                  srfi-13:)
          (primitives errmsg))

  (include "130.body.scm"))
