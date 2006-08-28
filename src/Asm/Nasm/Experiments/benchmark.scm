;; This is intended to benchmark certain effects of code layout.
;; Something like this:
;;
;; struct TreeNode {
;;     int key;
;;     struct TreeNode *left;
;;     struct TreeNode *right;
;;     struct TreeNode *nextTree;
;;     int nextKey;
;;     int (*nextFun)(struct TreeNode *, int, int);
;;     int dummy;
;; };
;;
;; main(struct TreeNode *tree, int key, count) {
;;     while (tree->key != key)
;;         tree = key < tree->key ? tree->left : tree->right;
;;
;;     tree->dummy = count;
;;
;;     if (count == 0)
;;         return tree->nextKey;
;;     else
;;         return tree->nextFun(tree->nextTree, tree->nextKey, count - 1);
;; }
;;
;; This is designed to be called from benchmark.c or winbenchmark.c.
;;
;; C calling convention sort of, but with sneaky tail calls.
;;
(sassy-make-bin "benchmark.i"
  (sassy
    '((macro c-arg
             (lambda (n)
               `(& esp ,(* 4 (+ 1 n)))))

      (macro t:key       0)
      (macro t:left      4)
      (macro t:right     8)
      (macro t:nextTree 12)
      (macro t:nextKey  16)
      (macro t:nextFun  20)
      (macro t:dummy    24)

      (text
        (label main
          (mov eax (c-arg 1))         ; eax := key
          (mov ecx (c-arg 0))         ; ecx := tree

          (while
            (begin
              (mov edx (& ecx t:key))
              (!= eax edx))
            (if (< eax edx)
              (mov ecx (& ecx t:left))
              (mov ecx (& ecx t:right))))

          ;; Should expand to something like:
;          (locals (loop left break)
;            (label loop)              ; loop:
;            (mov edx (& ecx t:key))   ; edx := tree->key
;            (cmp eax edx)             ; if (key == tree->key)
;            (je break)                ;   break
;            (jl left)                 ; if (key < tree->key) goto left;
;            (mov ecx (& ecx t:right)) ; tree := tree->right
;            (jmp loop)                ; goto loop
;            (label left)              ; left:
;            (mov ecx (& ecx t:left))  ; tree := tree->left
;            (jmp loop)                ; goto loop
;            (label break))

          (mov eax (c-arg 2))
          (mov (& ecx t:dummy) eax)

          (if
            (= eax 0)
            (begin
              (mov eax (& ecx t:nextKey))
              (ret))
            (begin
              (dec eax)
              (mov (c-arg 2) eax)
              (mov eax (& ecx t:nextKey))
              (mov (c-arg 1) eax)
              (mov eax (& ecx t:nextTree))
              (mov (c-arg 0) eax)
              (jmp (& ecx t:nextFun)))))))))

