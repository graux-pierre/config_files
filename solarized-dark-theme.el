;;; solarized.el --- Solarized for Emacs.

;; Copyright (C) 2011-2016 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; Author: Thomas Frössman <thomasf@jossystem.se>
;; URL: http://github.com/bbatsov/solarized-emacs
;; Version: 1.2.2

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A port of Solarized to Emacs.
;;
;;; Installation:
;;
;;   Drop the `solarized-theme.el` somewhere in your `load-path` and
;; the two themes in a folder that is on `custom-theme-load-path'
;; and enjoy!
;;
;; Don't forget that the theme requires Emacs 24.
;;
;;; Credits
;;
;; Ethan Schoonover created the original theme for vim on such this port
;; is based.
;;
;;; Code:

;;; dash.el --- A modern list library for Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2012-2015 Free Software Foundation, Inc.

;; Author: Magnar Sveen <magnars@gmail.com>
;; Version: 2.12.1
;; Keywords: lists

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A modern list api for Emacs.
;;
;; See documentation on https://github.com/magnars/dash.el#functions
;;
;; **Please note** The lexical binding in this file is not utilised at the
;; moment. We will take full advantage of lexical binding in an upcoming 3.0
;; release of Dash. In the meantime, we've added the pragma to avoid a bug that
;; you can read more about in https://github.com/magnars/dash.el/issues/130.
;;

;;; Code:

(defgroup dash ()
  "Customize group for dash.el"
  :group 'lisp
  :prefix "dash-")

(defun dash--enable-fontlock (symbol value)
  (when value
    (dash-enable-font-lock))
  (set-default symbol value))

(defcustom dash-enable-fontlock nil
  "If non-nil, enable fontification of dash functions, macros and
special values."
  :type 'boolean
  :set 'dash--enable-fontlock
  :group 'dash)

(defmacro !cons (car cdr)
  "Destructive: Set CDR to the cons of CAR and CDR."
  `(setq ,cdr (cons ,car ,cdr)))

(defmacro !cdr (list)
  "Destructive: Set LIST to the cdr of LIST."
  `(setq ,list (cdr ,list)))

(defmacro --each (list &rest body)
  "Anaphoric form of `-each'."
  (declare (debug (form body))
           (indent 1))
  (let ((l (make-symbol "list")))
    `(let ((,l ,list)
           (it-index 0))
       (while ,l
         (let ((it (car ,l)))
           ,@body)
         (setq it-index (1+ it-index))
         (!cdr ,l)))))

(defmacro -doto (eval-initial-value &rest forms)
  "Eval a form, then insert that form as the 2nd argument to other forms.
The EVAL-INITIAL-VALUE form is evaluated once. Its result is
passed to FORMS, which are then evaluated sequentially. Returns
the target form."
  (declare (indent 1))
  (let ((retval (make-symbol "value")))
    `(let ((,retval ,eval-initial-value))
       ,@(mapcar (lambda (form)
                   (if (sequencep form)
                       `(,(-first-item form) ,retval ,@(cdr form))
                     `(funcall form ,retval)))
                 forms)
       ,retval)))

(defun -each (list fn)
  "Call FN with every item in LIST. Return nil, used for side-effects only."
  (--each list (funcall fn it)))

(put '-each 'lisp-indent-function 1)

(defalias '--each-indexed '--each)

(defun -each-indexed (list fn)
  "Call (FN index item) for each item in LIST.

In the anaphoric form `--each-indexed', the index is exposed as `it-index`.

See also: `-map-indexed'."
  (--each list (funcall fn it-index it)))

(defmacro --each-while (list pred &rest body)
  "Anaphoric form of `-each-while'."
  (declare (debug (form form body))
           (indent 2))
  (let ((l (make-symbol "list"))
        (c (make-symbol "continue")))
    `(let ((,l ,list)
           (,c t)
           (it-index 0))
       (while (and ,l ,c)
         (let ((it (car ,l)))
           (if (not ,pred) (setq ,c nil) ,@body))
         (setq it-index (1+ it-index))
         (!cdr ,l)))))

(defun -each-while (list pred fn)
  "Call FN with every item in LIST while (PRED item) is non-nil.
Return nil, used for side-effects only."
  (--each-while list (funcall pred it) (funcall fn it)))

(put '-each-while 'lisp-indent-function 2)

(defmacro --dotimes (num &rest body)
  "Repeatedly executes BODY (presumably for side-effects) with `it` bound to integers from 0 through NUM-1."
  (declare (debug (form body))
           (indent 1))
  (let ((n (make-symbol "num")))
    `(let ((,n ,num)
           (it 0))
       (while (< it ,n)
         ,@body
         (setq it (1+ it))))))

(defun -dotimes (num fn)
  "Repeatedly calls FN (presumably for side-effects) passing in integers from 0 through NUM-1."
  (--dotimes num (funcall fn it)))

(put '-dotimes 'lisp-indent-function 1)

(defun -map (fn list)
  "Return a new list consisting of the result of applying FN to the items in LIST."
  (mapcar fn list))

(defmacro --map (form list)
  "Anaphoric form of `-map'."
  (declare (debug (form form)))
  `(mapcar (lambda (it) ,form) ,list))

(defmacro --reduce-from (form initial-value list)
  "Anaphoric form of `-reduce-from'."
  (declare (debug (form form form)))
  `(let ((acc ,initial-value))
     (--each ,list (setq acc ,form))
     acc))

(defun -reduce-from (fn initial-value list)
  "Return the result of applying FN to INITIAL-VALUE and the
first item in LIST, then applying FN to that result and the 2nd
item, etc. If LIST contains no items, return INITIAL-VALUE and
FN is not called.

In the anaphoric form `--reduce-from', the accumulated value is
exposed as `acc`.

See also: `-reduce', `-reduce-r'"
  (--reduce-from (funcall fn acc it) initial-value list))

(defmacro --reduce (form list)
  "Anaphoric form of `-reduce'."
  (declare (debug (form form)))
  (let ((lv (make-symbol "list-value")))
    `(let ((,lv ,list))
       (if ,lv
           (--reduce-from ,form (car ,lv) (cdr ,lv))
         (let (acc it) ,form)))))

(defun -reduce (fn list)
  "Return the result of applying FN to the first 2 items in LIST,
then applying FN to that result and the 3rd item, etc. If LIST
contains no items, FN must accept no arguments as well, and
reduce return the result of calling FN with no arguments. If
LIST has only 1 item, it is returned and FN is not called.

In the anaphoric form `--reduce', the accumulated value is
exposed as `acc`.

See also: `-reduce-from', `-reduce-r'"
  (if list
      (-reduce-from fn (car list) (cdr list))
    (funcall fn)))

(defun -reduce-r-from (fn initial-value list)
  "Replace conses with FN, nil with INITIAL-VALUE and evaluate
the resulting expression. If LIST is empty, INITIAL-VALUE is
returned and FN is not called.

Note: this function works the same as `-reduce-from' but the
operation associates from right instead of from left.

See also: `-reduce-r', `-reduce'"
  (if (not list) initial-value
    (funcall fn (car list) (-reduce-r-from fn initial-value (cdr list)))))

(defmacro --reduce-r-from (form initial-value list)
  "Anaphoric version of `-reduce-r-from'."
  (declare (debug (form form form)))
  `(-reduce-r-from (lambda (&optional it acc) ,form) ,initial-value ,list))

(defun -reduce-r (fn list)
  "Replace conses with FN and evaluate the resulting expression.
The final nil is ignored. If LIST contains no items, FN must
accept no arguments as well, and reduce return the result of
calling FN with no arguments. If LIST has only 1 item, it is
returned and FN is not called.

The first argument of FN is the new item, the second is the
accumulated value.

Note: this function works the same as `-reduce' but the operation
associates from right instead of from left.

See also: `-reduce-r-from', `-reduce'"
  (cond
   ((not list) (funcall fn))
   ((not (cdr list)) (car list))
   (t (funcall fn (car list) (-reduce-r fn (cdr list))))))

(defmacro --reduce-r (form list)
  "Anaphoric version of `-reduce-r'."
  (declare (debug (form form)))
  `(-reduce-r (lambda (&optional it acc) ,form) ,list))

(defmacro --filter (form list)
  "Anaphoric form of `-filter'."
  (declare (debug (form form)))
  (let ((r (make-symbol "result")))
    `(let (,r)
       (--each ,list (when ,form (!cons it ,r)))
       (nreverse ,r))))

(defun -filter (pred list)
  "Return a new list of the items in LIST for which PRED returns a non-nil value.

Alias: `-select'

See also: `-keep'"
  (--filter (funcall pred it) list))

(defalias '-select '-filter)
(defalias '--select '--filter)

(defmacro --remove (form list)
  "Anaphoric form of `-remove'."
  (declare (debug (form form)))
  `(--filter (not ,form) ,list))

(defun -remove (pred list)
  "Return a new list of the items in LIST for which PRED returns nil.

Alias: `-reject'"
  (--remove (funcall pred it) list))

(defalias '-reject '-remove)
(defalias '--reject '--remove)

(defun -remove-first (pred list)
  "Return a new list with the first item matching PRED removed.

Alias: `-reject-first'

See also: `-remove', `-map-first'"
  (let (front)
    (while (and list (not (funcall pred (car list))))
      (push (car list) front)
      (!cdr list))
    (if list
        (-concat (nreverse front) (cdr list))
      (nreverse front))))

(defmacro --remove-first (form list)
  "Anaphoric form of `-remove-first'."
  (declare (debug (form form)))
  `(-remove-first (lambda (it) ,form) ,list))

(defalias '-reject-first '-remove-first)
(defalias '--reject-first '--remove-first)

(defun -remove-last (pred list)
  "Return a new list with the last item matching PRED removed.

Alias: `-reject-last'

See also: `-remove', `-map-last'"
  (nreverse (-remove-first pred (reverse list))))

(defmacro --remove-last (form list)
  "Anaphoric form of `-remove-last'."
  (declare (debug (form form)))
  `(-remove-last (lambda (it) ,form) ,list))

(defalias '-reject-last '-remove-last)
(defalias '--reject-last '--remove-last)

(defun -remove-item (item list)
  "Remove all occurences of ITEM from LIST.

Comparison is done with `equal'."
  (--remove (equal it item) list))

(defmacro --keep (form list)
  "Anaphoric form of `-keep'."
  (declare (debug (form form)))
  (let ((r (make-symbol "result"))
        (m (make-symbol "mapped")))
    `(let (,r)
       (--each ,list (let ((,m ,form)) (when ,m (!cons ,m ,r))))
       (nreverse ,r))))

(defun -keep (fn list)
  "Return a new list of the non-nil results of applying FN to the items in LIST.

If you want to select the original items satisfying a predicate use `-filter'."
  (--keep (funcall fn it) list))

(defun -non-nil (list)
  "Return all non-nil elements of LIST."
  (-remove 'null list))

(defmacro --map-indexed (form list)
  "Anaphoric form of `-map-indexed'."
  (declare (debug (form form)))
  (let ((r (make-symbol "result")))
    `(let (,r)
       (--each ,list
         (!cons ,form ,r))
       (nreverse ,r))))

(defun -map-indexed (fn list)
  "Return a new list consisting of the result of (FN index item) for each item in LIST.

In the anaphoric form `--map-indexed', the index is exposed as `it-index`.

See also: `-each-indexed'."
  (--map-indexed (funcall fn it-index it) list))

(defmacro --map-when (pred rep list)
  "Anaphoric form of `-map-when'."
  (declare (debug (form form form)))
  (let ((r (make-symbol "result")))
    `(let (,r)
       (--each ,list (!cons (if ,pred ,rep it) ,r))
       (nreverse ,r))))

(defun -map-when (pred rep list)
  "Return a new list where the elements in LIST that does not match the PRED function
are unchanged, and where the elements in LIST that do match the PRED function are mapped
through the REP function.

Alias: `-replace-where'

See also: `-update-at'"
  (--map-when (funcall pred it) (funcall rep it) list))

(defalias '-replace-where '-map-when)
(defalias '--replace-where '--map-when)

(defun -map-first (pred rep list)
  "Replace first item in LIST satisfying PRED with result of REP called on this item.

See also: `-map-when', `-replace-first'"
  (let (front)
    (while (and list (not (funcall pred (car list))))
      (push (car list) front)
      (!cdr list))
    (if list
        (-concat (nreverse front) (cons (funcall rep (car list)) (cdr list)))
      (nreverse front))))

(defmacro --map-first (pred rep list)
  "Anaphoric form of `-map-first'."
  `(-map-first (lambda (it) ,pred) (lambda (it) (ignore it) ,rep) ,list))

(defun -map-last (pred rep list)
  "Replace first item in LIST satisfying PRED with result of REP called on this item.

See also: `-map-when', `-replace-last'"
  (nreverse (-map-first pred rep (reverse list))))

(defmacro --map-last (pred rep list)
  "Anaphoric form of `-map-last'."
  `(-map-last (lambda (it) ,pred) (lambda (it) (ignore it) ,rep) ,list))

(defun -replace (old new list)
  "Replace all OLD items in LIST with NEW.

Elements are compared using `equal'.

See also: `-replace-at'"
  (--map-when (equal it old) new list))

(defun -replace-first (old new list)
  "Replace the first occurence of OLD with NEW in LIST.

Elements are compared using `equal'.

See also: `-map-first'"
  (--map-first (equal old it) new list))

(defun -replace-last (old new list)
  "Replace the last occurence of OLD with NEW in LIST.

Elements are compared using `equal'.

See also: `-map-last'"
  (--map-last (equal old it) new list))

(defmacro --mapcat (form list)
  "Anaphoric form of `-mapcat'."
  (declare (debug (form form)))
  `(apply 'append (--map ,form ,list)))

(defun -mapcat (fn list)
  "Return the concatenation of the result of mapping FN over LIST.
Thus function FN should return a list."
  (--mapcat (funcall fn it) list))

(defun -flatten (l)
  "Take a nested list L and return its contents as a single, flat list.

Note that because `nil' represents a list of zero elements (an
empty list), any mention of nil in L will disappear after
flattening.  If you need to preserve nils, consider `-flatten-n'
or map them to some unique symbol and then map them back.

Conses of two atoms are considered \"terminals\", that is, they
aren't flattened further.

See also: `-flatten-n'"
  (if (and (listp l) (listp (cdr l)))
      (-mapcat '-flatten l)
    (list l)))

(defmacro --iterate (form init n)
  "Anaphoric version of `-iterate'."
  (declare (debug (form form form)))
  `(-iterate (lambda (it) ,form) ,init ,n))

(defun -flatten-n (num list)
  "Flatten NUM levels of a nested LIST.

See also: `-flatten'"
  (-last-item (--iterate (--mapcat (-list it) it) list (1+ num))))

(defun -concat (&rest lists)
  "Return a new list with the concatenation of the elements in the supplied LISTS."
  (apply 'append lists))

(defalias '-copy 'copy-sequence
  "Create a shallow copy of LIST.")

(defun -splice (pred fun list)
  "Splice lists generated by FUN in place of elements matching PRED in LIST.

FUN takes the element matching PRED as input.

This function can be used as replacement for `,@' in case you
need to splice several lists at marked positions (for example
with keywords).

See also: `-splice-list', `-insert-at'"
  (let (r)
    (--each list
      (if (funcall pred it)
          (let ((new (funcall fun it)))
            (--each new (!cons it r)))
        (!cons it r)))
    (nreverse r)))

(defmacro --splice (pred form list)
  "Anaphoric form of `-splice'."
  `(-splice (lambda (it) ,pred) (lambda (it) ,form) ,list))

(defun -splice-list (pred new-list list)
  "Splice NEW-LIST in place of elements matching PRED in LIST.

See also: `-splice', `-insert-at'"
  (-splice pred (lambda (_) new-list) list))

(defmacro --splice-list (pred new-list list)
  "Anaphoric form of `-splice-list'."
  `(-splice-list (lambda (it) ,pred) ,new-list ,list))

(defun -cons* (&rest args)
  "Make a new list from the elements of ARGS.

The last 2 members of ARGS are used as the final cons of the
result so if the final member of ARGS is not a list the result is
a dotted list."
  (-reduce-r 'cons args))

(defun -snoc (list elem &rest elements)
  "Append ELEM to the end of the list.

This is like `cons', but operates on the end of list.

If ELEMENTS is non nil, append these to the list as well."
  (-concat list (list elem) elements))

(defmacro --first (form list)
  "Anaphoric form of `-first'."
  (declare (debug (form form)))
  (let ((n (make-symbol "needle")))
    `(let (,n)
       (--each-while ,list (not ,n)
         (when ,form (setq ,n it)))
       ,n)))

(defun -first (pred list)
  "Return the first x in LIST where (PRED x) is non-nil, else nil.

To get the first item in the list no questions asked, use `car'.

Alias: `-find'"
  (--first (funcall pred it) list))

(defalias '-find '-first)
(defalias '--find '--first)

(defmacro --some (form list)
  "Anaphoric form of `-some'."
  (declare (debug (form form)))
  (let ((n (make-symbol "needle")))
    `(let (,n)
       (--each-while ,list (not ,n)
         (setq ,n ,form))
       ,n)))

(defun -some (pred list)
  "Return (PRED x) for the first LIST item where (PRED x) is non-nil, else nil.

Alias: `-any'"
  (--some (funcall pred it) list))

(defalias '-any '-some)
(defalias '--any '--some)

(defmacro --last (form list)
  "Anaphoric form of `-last'."
  (declare (debug (form form)))
  (let ((n (make-symbol "needle")))
    `(let (,n)
       (--each ,list
         (when ,form (setq ,n it)))
       ,n)))

(defun -last (pred list)
  "Return the last x in LIST where (PRED x) is non-nil, else nil."
  (--last (funcall pred it) list))

(defalias '-first-item 'car
  "Return the first item of LIST, or nil on an empty list.")

(defun -last-item (list)
  "Return the last item of LIST, or nil on an empty list."
  (car (last list)))

(defun -butlast (list)
  "Return a list of all items in list except for the last."
  ;; no alias as we don't want magic optional argument
  (butlast list))

(defmacro --count (pred list)
  "Anaphoric form of `-count'."
  (declare (debug (form form)))
  (let ((r (make-symbol "result")))
    `(let ((,r 0))
       (--each ,list (when ,pred (setq ,r (1+ ,r))))
       ,r)))

(defun -count (pred list)
  "Counts the number of items in LIST where (PRED item) is non-nil."
  (--count (funcall pred it) list))

(defun ---truthy? (val)
  (not (null val)))

(defmacro --any? (form list)
  "Anaphoric form of `-any?'."
  (declare (debug (form form)))
  `(---truthy? (--first ,form ,list)))

(defun -any? (pred list)
  "Return t if (PRED x) is non-nil for any x in LIST, else nil.

Alias: `-any-p', `-some?', `-some-p'"
  (--any? (funcall pred it) list))

(defalias '-some? '-any?)
(defalias '--some? '--any?)
(defalias '-any-p '-any?)
(defalias '--any-p '--any?)
(defalias '-some-p '-any?)
(defalias '--some-p '--any?)

(defmacro --all? (form list)
  "Anaphoric form of `-all?'."
  (declare (debug (form form)))
  (let ((a (make-symbol "all")))
    `(let ((,a t))
       (--each-while ,list ,a (setq ,a ,form))
       (---truthy? ,a))))

(defun -all? (pred list)
  "Return t if (PRED x) is non-nil for all x in LIST, else nil.

Alias: `-all-p', `-every?', `-every-p'"
  (--all? (funcall pred it) list))

(defalias '-every? '-all?)
(defalias '--every? '--all?)
(defalias '-all-p '-all?)
(defalias '--all-p '--all?)
(defalias '-every-p '-all?)
(defalias '--every-p '--all?)

(defmacro --none? (form list)
  "Anaphoric form of `-none?'."
  (declare (debug (form form)))
  `(--all? (not ,form) ,list))

(defun -none? (pred list)
  "Return t if (PRED x) is nil for all x in LIST, else nil.

Alias: `-none-p'"
  (--none? (funcall pred it) list))

(defalias '-none-p '-none?)
(defalias '--none-p '--none?)

(defmacro --only-some? (form list)
  "Anaphoric form of `-only-some?'."
  (declare (debug (form form)))
  (let ((y (make-symbol "yes"))
        (n (make-symbol "no")))
    `(let (,y ,n)
       (--each-while ,list (not (and ,y ,n))
         (if ,form (setq ,y t) (setq ,n t)))
       (---truthy? (and ,y ,n)))))

(defun -only-some? (pred list)
  "Return `t` if at least one item of LIST matches PRED and at least one item of LIST does not match PRED.
Return `nil` both if all items match the predicate or if none of the items match the predicate.

Alias: `-only-some-p'"
  (--only-some? (funcall pred it) list))

(defalias '-only-some-p '-only-some?)
(defalias '--only-some-p '--only-some?)

(defun -slice (list from &optional to step)
  "Return copy of LIST, starting from index FROM to index TO.

FROM or TO may be negative.  These values are then interpreted
modulo the length of the list.

If STEP is a number, only each STEPth item in the resulting
section is returned.  Defaults to 1."
  (let ((length (length list))
        (new-list nil))
    ;; to defaults to the end of the list
    (setq to (or to length))
    (setq step (or step 1))
    ;; handle negative indices
    (when (< from 0)
      (setq from (mod from length)))
    (when (< to 0)
      (setq to (mod to length)))

    ;; iterate through the list, keeping the elements we want
    (--each-while list (< it-index to)
      (when (and (>= it-index from)
                 (= (mod (- from it-index) step) 0))
        (push it new-list)))
    (nreverse new-list)))

(defun -take (n list)
  "Return a new list of the first N items in LIST, or all items if there are fewer than N.

See also: `-take-last'"
  (let (result)
    (--dotimes n
      (when list
        (!cons (car list) result)
        (!cdr list)))
    (nreverse result)))

(defun -take-last (n list)
  "Return the last N items of LIST in order.

See also: `-take'"
  (copy-sequence (last list n)))

(defalias '-drop 'nthcdr
  "Return the tail of LIST without the first N items.

See also: `-drop-last'")

(defun -drop-last (n list)
  "Remove the last N items of LIST and return a copy.

See also: `-drop'"
  ;; No alias because we don't want magic optional argument
  (butlast list n))

(defmacro --take-while (form list)
  "Anaphoric form of `-take-while'."
  (declare (debug (form form)))
  (let ((r (make-symbol "result")))
    `(let (,r)
       (--each-while ,list ,form (!cons it ,r))
       (nreverse ,r))))

(defun -take-while (pred list)
  "Return a new list of successive items from LIST while (PRED item) returns a non-nil value."
  (--take-while (funcall pred it) list))

(defmacro --drop-while (form list)
  "Anaphoric form of `-drop-while'."
  (declare (debug (form form)))
  (let ((l (make-symbol "list")))
    `(let ((,l ,list))
       (while (and ,l (let ((it (car ,l))) ,form))
         (!cdr ,l))
       ,l)))

(defun -drop-while (pred list)
  "Return the tail of LIST starting from the first item for which (PRED item) returns nil."
  (--drop-while (funcall pred it) list))

(defun -split-at (n list)
  "Return a list of ((-take N LIST) (-drop N LIST)), in no more than one pass through the list."
  (let (result)
    (--dotimes n
      (when list
        (!cons (car list) result)
        (!cdr list)))
    (list (nreverse result) list)))

(defun -rotate (n list)
  "Rotate LIST N places to the right.  With N negative, rotate to the left.
The time complexity is O(n)."
  (if (> n 0)
      (append (last list n) (butlast list n))
    (append (-drop (- n) list) (-take (- n) list))))

(defun -insert-at (n x list)
  "Return a list with X inserted into LIST at position N.

See also: `-splice', `-splice-list'"
  (let ((split-list (-split-at n list)))
    (nconc (car split-list) (cons x (cadr split-list)))))

(defun -replace-at (n x list)
  "Return a list with element at Nth position in LIST replaced with X.

See also: `-replace'"
  (let ((split-list (-split-at n list)))
    (nconc (car split-list) (cons x (cdr (cadr split-list))))))

(defun -update-at (n func list)
  "Return a list with element at Nth position in LIST replaced with `(func (nth n list))`.

See also: `-map-when'"
  (let ((split-list (-split-at n list)))
    (nconc (car split-list) (cons (funcall func (car (cadr split-list))) (cdr (cadr split-list))))))

(defmacro --update-at (n form list)
  "Anaphoric version of `-update-at'."
  (declare (debug (form form form)))
  `(-update-at ,n (lambda (it) ,form) ,list))

(defun -remove-at (n list)
  "Return a list with element at Nth position in LIST removed.

See also: `-remove-at-indices', `-remove'"
  (-remove-at-indices (list n) list))

(defun -remove-at-indices (indices list)
  "Return a list whose elements are elements from LIST without
elements selected as `(nth i list)` for all i
from INDICES.

See also: `-remove-at', `-remove'"
  (let* ((indices (-sort '< indices))
         (diffs (cons (car indices) (-map '1- (-zip-with '- (cdr indices) indices))))
         r)
    (--each diffs
      (let ((split (-split-at it list)))
        (!cons (car split) r)
        (setq list (cdr (cadr split)))))
    (!cons list r)
    (apply '-concat (nreverse r))))

(defmacro --split-with (pred list)
  "Anaphoric form of `-split-with'."
  (declare (debug (form form)))
  (let ((l (make-symbol "list"))
        (r (make-symbol "result"))
        (c (make-symbol "continue")))
    `(let ((,l ,list)
           (,r nil)
           (,c t))
       (while (and ,l ,c)
         (let ((it (car ,l)))
           (if (not ,pred)
               (setq ,c nil)
             (!cons it ,r)
             (!cdr ,l))))
       (list (nreverse ,r) ,l))))

(defun -split-with (pred list)
  "Return a list of ((-take-while PRED LIST) (-drop-while PRED LIST)), in no more than one pass through the list."
  (--split-with (funcall pred it) list))

(defmacro -split-on (item list)
  "Split the LIST each time ITEM is found.

Unlike `-partition-by', the ITEM is discarded from the results.
Empty lists are also removed from the result.

Comparison is done by `equal'.

See also `-split-when'"
  (declare (debug (form form)))
  `(-split-when (lambda (it) (equal it ,item)) ,list))

(defmacro --split-when (form list)
  "Anaphoric version of `-split-when'."
  (declare (debug (form form)))
  `(-split-when (lambda (it) ,form) ,list))

(defun -split-when (fn list)
  "Split the LIST on each element where FN returns non-nil.

Unlike `-partition-by', the \"matched\" element is discarded from
the results.  Empty lists are also removed from the result.

This function can be thought of as a generalization of
`split-string'."
  (let (r s)
    (while list
      (if (not (funcall fn (car list)))
          (push (car list) s)
        (when s (push (nreverse s) r))
        (setq s nil))
      (!cdr list))
    (when s (push (nreverse s) r))
    (nreverse r)))

(defmacro --separate (form list)
  "Anaphoric form of `-separate'."
  (declare (debug (form form)))
  (let ((y (make-symbol "yes"))
        (n (make-symbol "no")))
    `(let (,y ,n)
       (--each ,list (if ,form (!cons it ,y) (!cons it ,n)))
       (list (nreverse ,y) (nreverse ,n)))))

(defun -separate (pred list)
  "Return a list of ((-filter PRED LIST) (-remove PRED LIST)), in one pass through the list."
  (--separate (funcall pred it) list))

(defun ---partition-all-in-steps-reversed (n step list)
  "Private: Used by -partition-all-in-steps and -partition-in-steps."
  (when (< step 1)
    (error "Step must be a positive number, or you're looking at some juicy infinite loops."))
  (let ((result nil))
    (while list
      (!cons (-take n list) result)
      (setq list (-drop step list)))
    result))

(defun -partition-all-in-steps (n step list)
  "Return a new list with the items in LIST grouped into N-sized sublists at offsets STEP apart.
The last groups may contain less than N items."
  (nreverse (---partition-all-in-steps-reversed n step list)))

(defun -partition-in-steps (n step list)
  "Return a new list with the items in LIST grouped into N-sized sublists at offsets STEP apart.
If there are not enough items to make the last group N-sized,
those items are discarded."
  (let ((result (---partition-all-in-steps-reversed n step list)))
    (while (and result (< (length (car result)) n))
      (!cdr result))
    (nreverse result)))

(defun -partition-all (n list)
  "Return a new list with the items in LIST grouped into N-sized sublists.
The last group may contain less than N items."
  (-partition-all-in-steps n n list))

(defun -partition (n list)
  "Return a new list with the items in LIST grouped into N-sized sublists.
If there are not enough items to make the last group N-sized,
those items are discarded."
  (-partition-in-steps n n list))

(defmacro --partition-by (form list)
  "Anaphoric form of `-partition-by'."
  (declare (debug (form form)))
  (let ((r (make-symbol "result"))
        (s (make-symbol "sublist"))
        (v (make-symbol "value"))
        (n (make-symbol "new-value"))
        (l (make-symbol "list")))
    `(let ((,l ,list))
       (when ,l
         (let* ((,r nil)
                (it (car ,l))
                (,s (list it))
                (,v ,form)
                (,l (cdr ,l)))
           (while ,l
             (let* ((it (car ,l))
                    (,n ,form))
               (unless (equal ,v ,n)
                 (!cons (nreverse ,s) ,r)
                 (setq ,s nil)
                 (setq ,v ,n))
               (!cons it ,s)
               (!cdr ,l)))
           (!cons (nreverse ,s) ,r)
           (nreverse ,r))))))

(defun -partition-by (fn list)
  "Apply FN to each item in LIST, splitting it each time FN returns a new value."
  (--partition-by (funcall fn it) list))

(defmacro --partition-by-header (form list)
  "Anaphoric form of `-partition-by-header'."
  (declare (debug (form form)))
  (let ((r (make-symbol "result"))
        (s (make-symbol "sublist"))
        (h (make-symbol "header-value"))
        (b (make-symbol "seen-body?"))
        (n (make-symbol "new-value"))
        (l (make-symbol "list")))
    `(let ((,l ,list))
       (when ,l
         (let* ((,r nil)
                (it (car ,l))
                (,s (list it))
                (,h ,form)
                (,b nil)
                (,l (cdr ,l)))
           (while ,l
             (let* ((it (car ,l))
                    (,n ,form))
               (if (equal ,h ,n)
                   (when ,b
                     (!cons (nreverse ,s) ,r)
                     (setq ,s nil)
                     (setq ,b nil))
                 (setq ,b t))
               (!cons it ,s)
               (!cdr ,l)))
           (!cons (nreverse ,s) ,r)
           (nreverse ,r))))))

(defun -partition-by-header (fn list)
  "Apply FN to the first item in LIST. That is the header
value. Apply FN to each item in LIST, splitting it each time FN
returns the header value, but only after seeing at least one
other value (the body)."
  (--partition-by-header (funcall fn it) list))

(defmacro --group-by (form list)
  "Anaphoric form of `-group-by'."
  (declare (debug t))
  (let ((n (make-symbol "n"))
        (k (make-symbol "k"))
        (grp (make-symbol "grp")))
    `(nreverse
      (-map
       (lambda (,n)
         (cons (car ,n)
               (nreverse (cdr ,n))))
       (--reduce-from
        (let* ((,k (,@form))
               (,grp (assoc ,k acc)))
          (if ,grp
              (setcdr ,grp (cons it (cdr ,grp)))
            (push
             (list ,k it)
             acc))
          acc)
        nil ,list)))))

(defun -group-by (fn list)
  "Separate LIST into an alist whose keys are FN applied to the
elements of LIST.  Keys are compared by `equal'."
  (--group-by (funcall fn it) list))

(defun -interpose (sep list)
  "Return a new list of all elements in LIST separated by SEP."
  (let (result)
    (when list
      (!cons (car list) result)
      (!cdr list))
    (while list
      (setq result (cons (car list) (cons sep result)))
      (!cdr list))
    (nreverse result)))

(defun -interleave (&rest lists)
  "Return a new list of the first item in each list, then the second etc."
  (let (result)
    (while (-none? 'null lists)
      (--each lists (!cons (car it) result))
      (setq lists (-map 'cdr lists)))
    (nreverse result)))

(defmacro --zip-with (form list1 list2)
  "Anaphoric form of `-zip-with'.

The elements in list1 is bound as `it`, the elements in list2 as `other`."
  (declare (debug (form form form)))
  (let ((r (make-symbol "result"))
        (l1 (make-symbol "list1"))
        (l2 (make-symbol "list2")))
    `(let ((,r nil)
           (,l1 ,list1)
           (,l2 ,list2))
       (while (and ,l1 ,l2)
         (let ((it (car ,l1))
               (other (car ,l2)))
           (!cons ,form ,r)
           (!cdr ,l1)
           (!cdr ,l2)))
       (nreverse ,r))))

(defun -zip-with (fn list1 list2)
  "Zip the two lists LIST1 and LIST2 using a function FN.  This
function is applied pairwise taking as first argument element of
LIST1 and as second argument element of LIST2 at corresponding
position.

The anaphoric form `--zip-with' binds the elements from LIST1 as `it`,
and the elements from LIST2 as `other`."
  (--zip-with (funcall fn it other) list1 list2))

(defun -zip (&rest lists)
  "Zip LISTS together.  Group the head of each list, followed by the
second elements of each list, and so on. The lengths of the returned
groupings are equal to the length of the shortest input list.

If two lists are provided as arguments, return the groupings as a list
of cons cells. Otherwise, return the groupings as a list of lists.

Please note! This distinction is being removed in an upcoming 2.0
release of Dash. If you rely on this behavior, use -zip-pair instead."
  (let (results)
    (while (-none? 'null lists)
      (setq results (cons (mapcar 'car lists) results))
      (setq lists (mapcar 'cdr lists)))
    (setq results (nreverse results))
    (if (= (length lists) 2)
        ;; to support backward compatability, return
        ;; a cons cell if two lists were provided
        (--map (cons (car it) (cadr it)) results)
      results)))

(defalias '-zip-pair '-zip)

(defun -zip-fill (fill-value &rest lists)
  "Zip LISTS, with FILL-VALUE padded onto the shorter lists. The
lengths of the returned groupings are equal to the length of the
longest input list."
  (apply '-zip (apply '-pad (cons fill-value lists))))

(defun -cycle (list)
  "Return an infinite copy of LIST that will cycle through the
elements and repeat from the beginning."
  (let ((newlist (-map 'identity list)))
    (nconc newlist newlist)))

(defun -pad (fill-value &rest lists)
  "Appends FILL-VALUE to the end of each list in LISTS such that they
will all have the same length."
  (let* ((annotations (-annotate 'length lists))
         (n (-max (-map 'car annotations))))
    (--map (append (cdr it) (-repeat (- n (car it)) fill-value)) annotations)))

(defun -annotate (fn list)
  "Return a list of cons cells where each cell is FN applied to each
element of LIST paired with the unmodified element of LIST."
  (-zip (-map fn list) list))

(defmacro --annotate (form list)
  "Anaphoric version of `-annotate'."
  (declare (debug (form form)))
  `(-annotate (lambda (it) ,form) ,list))

(defun dash--table-carry (lists restore-lists &optional re)
  "Helper for `-table' and `-table-flat'.

If a list overflows, carry to the right and reset the list."
  (while (not (or (car lists)
                  (equal lists '(nil))))
    (setcar lists (car restore-lists))
    (pop (cadr lists))
    (!cdr lists)
    (!cdr restore-lists)
    (when re
      (push (nreverse (car re)) (cadr re))
      (setcar re nil)
      (!cdr re))))

(defun -table (fn &rest lists)
  "Compute outer product of LISTS using function FN.

The function FN should have the same arity as the number of
supplied lists.

The outer product is computed by applying fn to all possible
combinations created by taking one element from each list in
order.  The dimension of the result is (length lists).

See also: `-table-flat'"
  (let ((restore-lists (copy-sequence lists))
        (last-list (last lists))
        (re (make-list (length lists) nil)))
    (while (car last-list)
      (let ((item (apply fn (-map 'car lists))))
        (push item (car re))
        (setcar lists (cdar lists)) ;; silence byte compiler
        (dash--table-carry lists restore-lists re)))
    (nreverse (car (last re)))))

(defun -table-flat (fn &rest lists)
  "Compute flat outer product of LISTS using function FN.

The function FN should have the same arity as the number of
supplied lists.

The outer product is computed by applying fn to all possible
combinations created by taking one element from each list in
order.  The results are flattened, ignoring the tensor structure
of the result.  This is equivalent to calling:

  (-flatten-n (1- (length lists)) (-table fn lists))

but the implementation here is much more efficient.

See also: `-flatten-n', `-table'"
  (when lists                           ;Just in case.
    (let* ((list1 (pop lists))
           (restore-lists (copy-sequence lists))
           (last-list (last lists))
           re)
      (while (car last-list)
        (let ((tail (-map #'car lists)))
          (dolist (head list1)
            (push (apply fn head tail) re)))
        (pop (car lists))
        (dash--table-carry lists restore-lists))
      (nreverse re))))

(defun -partial (fn &rest args)
  "Take a function FN and fewer than the normal arguments to FN,
and return a fn that takes a variable number of additional ARGS.
When called, the returned function calls FN with ARGS first and
then additional args."
  (apply 'apply-partially fn args))

(defun -elem-index (elem list)
  "Return the index of the first element in the given LIST which
is equal to the query element ELEM, or nil if there is no
such element."
  (car (-elem-indices elem list)))

(defun -elem-indices (elem list)
  "Return the indices of all elements in LIST equal to the query
element ELEM, in ascending order."
  (-find-indices (-partial 'equal elem) list))

(defun -find-indices (pred list)
  "Return the indices of all elements in LIST satisfying the
predicate PRED, in ascending order."
  (apply 'append (--map-indexed (when (funcall pred it) (list it-index)) list)))

(defmacro --find-indices (form list)
  "Anaphoric version of `-find-indices'."
  (declare (debug (form form)))
  `(-find-indices (lambda (it) ,form) ,list))

(defun -find-index (pred list)
  "Take a predicate PRED and a LIST and return the index of the
first element in the list satisfying the predicate, or nil if
there is no such element.

See also `-first'."
  (car (-find-indices pred list)))

(defmacro --find-index (form list)
  "Anaphoric version of `-find-index'."
  (declare (debug (form form)))
  `(-find-index (lambda (it) ,form) ,list))

(defun -find-last-index (pred list)
  "Take a predicate PRED and a LIST and return the index of the
last element in the list satisfying the predicate, or nil if
there is no such element.

See also `-last'."
  (-last-item (-find-indices pred list)))

(defmacro --find-last-index (form list)
  "Anaphoric version of `-find-last-index'."
  `(-find-last-index (lambda (it) ,form) ,list))

(defun -select-by-indices (indices list)
  "Return a list whose elements are elements from LIST selected
as `(nth i list)` for all i from INDICES."
  (let (r)
    (--each indices
      (!cons (nth it list) r))
    (nreverse r)))

(defun -select-columns (columns table)
  "Select COLUMNS from TABLE.

TABLE is a list of lists where each element represents one row.
It is assumed each row has the same length.

Each row is transformed such that only the specified COLUMNS are
selected.

See also: `-select-column', `-select-by-indices'"
  (--map (-select-by-indices columns it) table))

(defun -select-column (column table)
  "Select COLUMN from TABLE.

TABLE is a list of lists where each element represents one row.
It is assumed each row has the same length.

The single selected column is returned as a list.

See also: `-select-columns', `-select-by-indices'"
  (--mapcat (-select-by-indices (list column) it) table))

(defmacro -> (x &optional form &rest more)
  "Thread the expr through the forms. Insert X as the second item
in the first form, making a list of it if it is not a list
already. If there are more forms, insert the first form as the
second item in second form, etc."
  (cond
   ((null form) x)
   ((null more) (if (listp form)
                    `(,(car form) ,x ,@(cdr form))
                  (list form x)))
   (:else `(-> (-> ,x ,form) ,@more))))

(defmacro ->> (x &optional form &rest more)
  "Thread the expr through the forms. Insert X as the last item
in the first form, making a list of it if it is not a list
already. If there are more forms, insert the first form as the
last item in second form, etc."
  (cond
   ((null form) x)
   ((null more) (if (listp form)
                    `(,@form ,x)
                  (list form x)))
   (:else `(->> (->> ,x ,form) ,@more))))

(defmacro --> (x form &rest more)
  "Thread the expr through the forms. Insert X at the position
signified by the token `it' in the first form. If there are more
forms, insert the first form at the position signified by `it' in
in second form, etc."
  (if (null more)
      (if (listp form)
          (--map-when (eq it 'it) x form)
        (list form x))
    `(--> (--> ,x ,form) ,@more)))

(defmacro -some-> (x &optional form &rest more)
  "When expr is non-nil, thread it through the first form (via `->'),
and when that result is non-nil, through the next form, etc."
  (if (null form) x
    (let ((result (make-symbol "result")))
      `(-some-> (-when-let (,result ,x)
                  (-> ,result ,form))
                ,@more))))

(defmacro -some->> (x &optional form &rest more)
  "When expr is non-nil, thread it through the first form (via `->>'),
and when that result is non-nil, through the next form, etc."
  (if (null form) x
    (let ((result (make-symbol "result")))
      `(-some->> (-when-let (,result ,x)
                   (->> ,result ,form))
                 ,@more))))

(defmacro -some--> (x &optional form &rest more)
  "When expr in non-nil, thread it through the first form (via `-->'),
and when that result is non-nil, through the next form, etc."
  (if (null form) x
    (let ((result (make-symbol "result")))
      `(-some--> (-when-let (,result ,x)
                   (--> ,result ,form))
                 ,@more))))

(defun -grade-up (comparator list)
  "Grade elements of LIST using COMPARATOR relation, yielding a
permutation vector such that applying this permutation to LIST
sorts it in ascending order."
  ;; ugly hack to "fix" lack of lexical scope
  (let ((comp `(lambda (it other) (funcall ',comparator (car it) (car other)))))
    (->> (--map-indexed (cons it it-index) list)
         (-sort comp)
         (-map 'cdr))))

(defun -grade-down (comparator list)
  "Grade elements of LIST using COMPARATOR relation, yielding a
permutation vector such that applying this permutation to LIST
sorts it in descending order."
  ;; ugly hack to "fix" lack of lexical scope
  (let ((comp `(lambda (it other) (funcall ',comparator (car other) (car it)))))
    (->> (--map-indexed (cons it it-index) list)
         (-sort comp)
         (-map 'cdr))))

(defvar dash--source-counter 0
  "Monotonic counter for generated symbols.")

(defun dash--match-make-source-symbol ()
  "Generate a new dash-source symbol.

All returned symbols are guaranteed to be unique."
  (prog1 (make-symbol (format "--dash-source-%d--" dash--source-counter))
    (setq dash--source-counter (1+ dash--source-counter))))

(defun dash--match-ignore-place-p (symbol)
  "Return non-nil if SYMBOL is a symbol and starts with _."
  (and (symbolp symbol)
       (eq (aref (symbol-name symbol) 0) ?_)))

(defun dash--match-cons-skip-cdr (skip-cdr source)
  "Helper function generating idiomatic shifting code."
  (cond
   ((= skip-cdr 0)
    `(pop ,source))
   (t
    `(prog1 ,(dash--match-cons-get-car skip-cdr source)
       (setq ,source ,(dash--match-cons-get-cdr (1+ skip-cdr) source))))))

(defun dash--match-cons-get-car (skip-cdr source)
  "Helper function generating idiomatic code to get nth car."
  (cond
   ((= skip-cdr 0)
    `(car ,source))
   ((= skip-cdr 1)
    `(cadr ,source))
   (t
    `(nth ,skip-cdr ,source))))

(defun dash--match-cons-get-cdr (skip-cdr source)
  "Helper function generating idiomatic code to get nth cdr."
  (cond
   ((= skip-cdr 0)
    source)
   ((= skip-cdr 1)
    `(cdr ,source))
   (t
    `(nthcdr ,skip-cdr ,source))))

(defun dash--match-cons (match-form source)
  "Setup a cons matching environment and call the real matcher."
  (let ((s (dash--match-make-source-symbol))
        (n 0)
        (m match-form))
    (while (and (consp m)
                (dash--match-ignore-place-p (car m)))
      (setq n (1+ n)) (!cdr m))
    (cond
     ;; when we only have one pattern in the list, we don't have to
     ;; create a temporary binding (--dash-source--) for the source
     ;; and just use the input directly
     ((and (consp m)
           (not (cdr m)))
      (dash--match (car m) (dash--match-cons-get-car n source)))
     ;; handle other special types
     ((> n 0)
      (dash--match m (dash--match-cons-get-cdr n source)))
     ;; this is the only entry-point for dash--match-cons-1, that's
     ;; why we can't simply use the above branch, it would produce
     ;; infinite recursion
     (t
      (cons (list s source) (dash--match-cons-1 match-form s))))))

(defun dash--match-cons-1 (match-form source &optional props)
  "Match MATCH-FORM against SOURCE.

MATCH-FORM is a proper or improper list.  Each element of
MATCH-FORM is either a symbol, which gets bound to the respective
value in source or another match form which gets destructured
recursively.

If the cdr of last cons cell in the list is `nil', matching stops
there.

SOURCE is a proper or improper list."
  (let ((skip-cdr (or (plist-get props :skip-cdr) 0)))
    (cond
     ((consp match-form)
      (cond
       ((cdr match-form)
        (cond
         ((and (symbolp (car match-form))
               (memq (car match-form) '(&keys &plist &alist &hash)))
          (dash--match-kv match-form (dash--match-cons-get-cdr skip-cdr source)))
         ((dash--match-ignore-place-p (car match-form))
          (dash--match-cons-1 (cdr match-form) source
                              (plist-put props :skip-cdr (1+ skip-cdr))))
         (t
          (-concat (dash--match (car match-form) (dash--match-cons-skip-cdr skip-cdr source))
                   (dash--match-cons-1 (cdr match-form) source)))))
       (t ;; Last matching place, no need for shift
        (dash--match (car match-form) (dash--match-cons-get-car skip-cdr source)))))
     ((eq match-form nil)
      nil)
     (t ;; Handle improper lists.  Last matching place, no need for shift
      (dash--match match-form (dash--match-cons-get-cdr skip-cdr source))))))

(defun dash--vector-tail (seq start)
  "Return the tail of SEQ starting at START."
  (cond
   ((vectorp seq)
    (let* ((re-length (- (length seq) start))
           (re (make-vector re-length 0)))
      (--dotimes re-length (aset re it (aref seq (+ it start))))
      re))
   ((stringp seq)
    (substring seq start))))

(defun dash--match-vector (match-form source)
  "Setup a vector matching environment and call the real matcher."
  (let ((s (dash--match-make-source-symbol)))
    (cond
     ;; don't bind `s' if we only have one sub-pattern
     ((= (length match-form) 1)
      (dash--match (aref match-form 0) `(aref ,source 0)))
     ;; if the source is a symbol, we don't need to re-bind it
     ((symbolp source)
      (dash--match-vector-1 match-form source))
     ;; don't bind `s' if we only have one sub-pattern which is not ignored
     ((let* ((ignored-places (mapcar 'dash--match-ignore-place-p match-form))
             (ignored-places-n (length (-remove 'null ignored-places))))
        (when (= ignored-places-n (1- (length match-form)))
          (let ((n (-find-index 'null ignored-places)))
            (dash--match (aref match-form n) `(aref ,source ,n))))))
     (t
      (cons (list s source) (dash--match-vector-1 match-form s))))))

(defun dash--match-vector-1 (match-form source)
  "Match MATCH-FORM against SOURCE.

MATCH-FORM is a vector.  Each element of MATCH-FORM is either a
symbol, which gets bound to the respective value in source or
another match form which gets destructured recursively.

If second-from-last place in MATCH-FORM is the symbol &rest, the
next element of the MATCH-FORM is matched against the tail of
SOURCE, starting at index of the &rest symbol.  This is
conceptually the same as the (head . tail) match for improper
lists, where dot plays the role of &rest.

SOURCE is a vector.

If the MATCH-FORM vector is shorter than SOURCE vector, only
the (length MATCH-FORM) places are bound, the rest of the SOURCE
is discarded."
  (let ((i 0)
        (l (length match-form))
        (re))
    (while (< i l)
      (let ((m (aref match-form i)))
        (push (cond
               ((and (symbolp m)
                     (eq m '&rest))
                (prog1 (dash--match
                        (aref match-form (1+ i))
                        `(dash--vector-tail ,source ,i))
                  (setq i l)))
               ((and (symbolp m)
                     ;; do not match symbols starting with _
                     (not (eq (aref (symbol-name m) 0) ?_)))
                (list (list m `(aref ,source ,i))))
               ((not (symbolp m))
                (dash--match m `(aref ,source ,i))))
              re)
        (setq i (1+ i))))
    (-flatten-n 1 (nreverse re))))

(defun dash--match-kv (match-form source)
  "Setup a kv matching environment and call the real matcher.

kv can be any key-value store, such as plist, alist or hash-table."
  (let ((s (dash--match-make-source-symbol)))
    (cond
     ;; don't bind `s' if we only have one sub-pattern (&type key val)
     ((= (length match-form) 3)
      (dash--match-kv-1 (cdr match-form) source (car match-form)))
     ;; if the source is a symbol, we don't need to re-bind it
     ((symbolp source)
      (dash--match-kv-1 (cdr match-form) source (car match-form)))
     (t
      (cons (list s source) (dash--match-kv-1 (cdr match-form) s (car match-form)))))))

(defun dash--match-kv-1 (match-form source type)
  "Match MATCH-FORM against SOURCE of type TYPE.

MATCH-FORM is a proper list of the form (key1 place1 ... keyN
placeN).  Each placeK is either a symbol, which gets bound to the
value of keyK retrieved from the key-value store, or another
match form which gets destructured recursively.

SOURCE is a key-value store of type TYPE, which can be a plist,
an alist or a hash table.

TYPE is a token specifying the type of the key-value store.
Valid values are &plist, &alist and &hash."
  (-flatten-n 1 (-map
                 (lambda (kv)
                   (let* ((k (car kv))
                          (v (cadr kv))
                          (getter (cond
                                   ((or (eq type '&plist) (eq type '&keys))
                                    `(plist-get ,source ,k))
                                   ((eq type '&alist)
                                    `(cdr (assoc ,k ,source)))
                                   ((eq type '&hash)
                                    `(gethash ,k ,source)))))
                     (cond
                      ((symbolp v)
                       (list (list v getter)))
                      (t (dash--match v getter)))))
                 (-partition 2 match-form))))

(defun dash--match-symbol (match-form source)
  "Bind a symbol.

This works just like `let', there is no destructuring."
  (list (list match-form source)))

(defun dash--match (match-form source)
  "Match MATCH-FORM against SOURCE.

This function tests the MATCH-FORM and dispatches to specific
matchers based on the type of the expression.

Key-value stores are disambiguated by placing a token &plist,
&alist or &hash as a first item in the MATCH-FORM."
  (cond
   ((symbolp match-form)
    (dash--match-symbol match-form source))
   ((consp match-form)
    (cond
     ;; Handle the "x &as" bindings first.
     ((and (consp (cdr match-form))
           (symbolp (car match-form))
           (eq '&as (cadr match-form)))
      (let ((s (car match-form)))
        (cons (list s source)
              (dash--match (cddr match-form) s))))
     ((memq (car match-form) '(&keys &plist &alist &hash))
      (dash--match-kv match-form source))
     (t (dash--match-cons match-form source))))
   ((vectorp match-form)
    ;; We support the &as binding in vectors too
    (cond
     ((and (> (length match-form) 2)
           (symbolp (aref match-form 0))
           (eq '&as (aref match-form 1)))
      (let ((s (aref match-form 0)))
        (cons (list s source)
              (dash--match (dash--vector-tail match-form 2) s))))
     (t (dash--match-vector match-form source))))))

(defmacro -let* (varlist &rest body)
  "Bind variables according to VARLIST then eval BODY.

VARLIST is a list of lists of the form (PATTERN SOURCE).  Each
PATTERN is matched against the SOURCE structurally.  SOURCE is
only evaluated once for each PATTERN.

Each SOURCE can refer to the symbols already bound by this
VARLIST.  This is useful if you want to destructure SOURCE
recursively but also want to name the intermediate structures.

See `-let' for the list of all possible patterns."
  (declare (debug ((&rest (sexp form)) body))
           (indent 1))
  (let ((bindings (--mapcat (dash--match (car it) (cadr it)) varlist)))
    `(let* ,bindings
       ,@body)))

(defmacro -let (varlist &rest body)
  "Bind variables according to VARLIST then eval BODY.

VARLIST is a list of lists of the form (PATTERN SOURCE).  Each
PATTERN is matched against the SOURCE \"structurally\".  SOURCE
is only evaluated once for each PATTERN.  Each PATTERN is matched
recursively, and can therefore contain sub-patterns which are
matched against corresponding sub-expressions of SOURCE.

All the SOURCEs are evalled before any symbols are
bound (i.e. \"in parallel\").

If VARLIST only contains one (PATTERN SOURCE) element, you can
optionally specify it using a vector and discarding the
outer-most parens.  Thus

  (-let ((PATTERN SOURCE)) ..)

becomes

  (-let [PATTERN SOURCE] ..).

`-let' uses a convention of not binding places (symbols) starting
with _ whenever it's possible.  You can use this to skip over
entries you don't care about.  However, this is not *always*
possible (as a result of implementation) and these symbols might
get bound to undefined values.

Following is the overview of supported patterns.  Remember that
patterns can be matched recursively, so every a, b, aK in the
following can be a matching construct and not necessarily a
symbol/variable.

Symbol:

  a - bind the SOURCE to A.  This is just like regular `let'.

Conses and lists:

  (a) - bind `car' of cons/list to A

  (a . b) - bind car of cons to A and `cdr' to B

  (a b) - bind car of list to A and `cadr' to B

  (a1 a2 a3  ...) - bind 0th car of list to A1, 1st to A2, 2nd to A3 ...

  (a1 a2 a3 ... aN . rest) - as above, but bind the Nth cdr to REST.

Vectors:

  [a] - bind 0th element of a non-list sequence to A (works with
        vectors, strings, bit arrays...)

  [a1 a2 a3 ...] - bind 0th element of non-list sequence to A0, 1st to
                   A1, 2nd to A2, ...
                   If the PATTERN is shorter than SOURCE, the values at
                   places not in PATTERN are ignored.
                   If the PATTERN is longer than SOURCE, an `error' is
                   thrown.

  [a1 a2 a3 ... &rest rest] - as above, but bind the rest of
                              the sequence to REST.  This is
                              conceptually the same as improper list
                              matching (a1 a2 ... aN . rest)

Key/value stores:

  (&plist key0 a0 ... keyN aN) - bind value mapped by keyK in the
                                 SOURCE plist to aK.  If the
                                 value is not found, aK is nil.

  (&alist key0 a0 ... keyN aN) - bind value mapped by keyK in the
                                 SOURCE alist to aK.  If the
                                 value is not found, aK is nil.

  (&hash key0 a0 ... keyN aN) - bind value mapped by keyK in the
                                SOURCE hash table to aK.  If the
                                value is not found, aK is nil.

Further, special keyword &keys supports \"inline\" matching of
plist-like key-value pairs, similarly to &keys keyword of
`cl-defun'.

  (a1 a2 ... aN &keys key1 b1 ... keyN bK)

This binds N values from the list to a1 ... aN, then interprets
the cdr as a plist (see key/value matching above).

You can name the source using the syntax SYMBOL &as PATTERN.
This syntax works with lists (proper or improper), vectors and
all types of maps.

  (list &as a b c) (list 1 2 3)

binds A to 1, B to 2, C to 3 and LIST to (1 2 3).

Similarly:

  (bounds &as beg . end) (cons 1 2)

binds BEG to 1, END to 2 and BOUNDS to (1 . 2).

  (items &as first . rest) (list 1 2 3)

binds FIRST to 1, REST to (2 3) and ITEMS to (1 2 3)

  [vect &as _ b c] [1 2 3]

binds B to 2, C to 3 and VECT to [1 2 3] (_ avoids binding as usual).

  (plist &as &plist :b b) (list :a 1 :b 2 :c 3)

binds B to 2 and PLIST to (:a 1 :b 2 :c 3).  Same for &alist and &hash.

This is especially useful when we want to capture the result of a
computation and destructure at the same time.  Consider the
form (function-returning-complex-structure) returning a list of
two vectors with two items each.  We want to capture this entire
result and pass it to another computation, but at the same time
we want to get the second item from each vector.  We can achieve
it with pattern

  (result &as [_ a] [_ b]) (function-returning-complex-structure)

Note: Clojure programmers may know this feature as the \":as
binding\".  The difference is that we put the &as at the front
because we need to support improper list binding."
  (declare (debug ([&or (&rest (sexp form))
                        (vector [&rest [sexp form]])]
                   body))
           (indent 1))
  (if (vectorp varlist)
      `(let* ,(dash--match (aref varlist 0) (aref varlist 1))
         ,@body)
    (let* ((inputs (--map-indexed (list (make-symbol (format "input%d" it-index)) (cadr it)) varlist))
           (new-varlist (--map (list (caar it) (cadr it)) (-zip varlist inputs))))
      `(let ,inputs
         (-let* ,new-varlist ,@body)))))

(defmacro -lambda (match-form &rest body)
  "Return a lambda which destructures its input as MATCH-FORM and executes BODY.

Note that you have to enclose the MATCH-FORM in a pair of parens,
such that:

  (-lambda (x) body)
  (-lambda (x y ...) body)

has the usual semantics of `lambda'.  Furthermore, these get
translated into normal lambda, so there is no performance
penalty.

See `-let' for the description of destructuring mechanism."
  (declare (doc-string 2) (indent defun)
           (debug (&define sexp
                           [&optional stringp]
                           [&optional ("interactive" interactive)]
                           def-body)))
  (cond
   ((not (consp match-form))
    (signal 'wrong-type-argument "match-form must be a list"))
   ;; no destructuring, so just return regular lambda to make things faster
   ((-all? 'symbolp match-form)
    `(lambda ,match-form ,@body))
   (t
    (let* ((inputs (--map-indexed (list it (make-symbol (format "input%d" it-index))) match-form)))
      ;; TODO: because inputs to the lambda are evaluated only once,
      ;; -let* need not to create the extra bindings to ensure that.
      ;; We should find a way to optimize that.  Not critical however.
      `(lambda ,(--map (cadr it) inputs)
         (-let* ,inputs ,@body))))))

(defmacro -if-let* (vars-vals then &rest else)
  "If all VALS evaluate to true, bind them to their corresponding
VARS and do THEN, otherwise do ELSE. VARS-VALS should be a list
of (VAR VAL) pairs.

Note: binding is done according to `-let*'.  VALS are evaluated
sequentially, and evaluation stops after the first nil VAL is
encountered."
  (declare (debug ((&rest (sexp form)) form body))
           (indent 2))
  (->> vars-vals
       (--mapcat (dash--match (car it) (cadr it)))
       (--reduce-r-from
        (let ((var (car it))
              (val (cadr it)))
          `(let ((,var ,val))
             (if ,var ,acc ,@else)))
        then)))

(defmacro -if-let (var-val then &rest else)
  "If VAL evaluates to non-nil, bind it to VAR and do THEN,
otherwise do ELSE. VAR-VAL should be a (VAR VAL) pair.

Note: binding is done according to `-let'."
  (declare (debug ((sexp form) form body))
           (indent 2))
  `(-if-let* (,var-val) ,then ,@else))

(defmacro --if-let (val then &rest else)
  "If VAL evaluates to non-nil, bind it to `it' and do THEN,
otherwise do ELSE."
  (declare (debug (form form body))
           (indent 2))
  `(-if-let (it ,val) ,then ,@else))

(defmacro -when-let* (vars-vals &rest body)
  "If all VALS evaluate to true, bind them to their corresponding
VARS and execute body. VARS-VALS should be a list of (VAR VAL)
pairs.

Note: binding is done according to `-let*'.  VALS are evaluated
sequentially, and evaluation stops after the first nil VAL is
encountered."
  (declare (debug ((&rest (sexp form)) body))
           (indent 1))
  `(-if-let* ,vars-vals (progn ,@body)))

(defmacro -when-let (var-val &rest body)
  "If VAL evaluates to non-nil, bind it to VAR and execute body.
VAR-VAL should be a (VAR VAL) pair.

Note: binding is done according to `-let'."
  (declare (debug ((sexp form) body))
           (indent 1))
  `(-if-let ,var-val (progn ,@body)))

(defmacro --when-let (val &rest body)
  "If VAL evaluates to non-nil, bind it to `it' and execute
body."
  (declare (debug (form body))
           (indent 1))
  `(--if-let ,val (progn ,@body)))

(defvar -compare-fn nil
  "Tests for equality use this function or `equal' if this is nil.
It should only be set using dynamic scope with a let, like:

  (let ((-compare-fn #'=)) (-union numbers1 numbers2 numbers3)")

(defun -distinct (list)
  "Return a new list with all duplicates removed.
The test for equality is done with `equal',
or with `-compare-fn' if that's non-nil.

Alias: `-uniq'"
  (let (result)
    (--each list (unless (-contains? result it) (!cons it result)))
    (nreverse result)))

(defalias '-uniq '-distinct)

(defun -union (list list2)
  "Return a new list containing the elements of LIST1 and elements of LIST2 that are not in LIST1.
The test for equality is done with `equal',
or with `-compare-fn' if that's non-nil."
  ;; We fall back to iteration implementation if the comparison
  ;; function isn't one of `eq', `eql' or `equal'.
  (let* ((result (reverse list))
         ;; TODO: get rid of this dynamic variable, pass it as an
         ;; argument instead.
         (-compare-fn (if (bound-and-true-p -compare-fn)
                          -compare-fn
                        'equal)))
    (if (memq -compare-fn '(eq eql equal))
        (let ((ht (make-hash-table :test -compare-fn)))
          (--each list (puthash it t ht))
          (--each list2 (unless (gethash it ht) (!cons it result))))
      (--each list2 (unless (-contains? result it) (!cons it result))))
    (nreverse result)))

(defun -intersection (list list2)
  "Return a new list containing only the elements that are members of both LIST and LIST2.
The test for equality is done with `equal',
or with `-compare-fn' if that's non-nil."
  (--filter (-contains? list2 it) list))

(defun -difference (list list2)
  "Return a new list with only the members of LIST that are not in LIST2.
The test for equality is done with `equal',
or with `-compare-fn' if that's non-nil."
  (--filter (not (-contains? list2 it)) list))

(defun -contains? (list element)
  "Return non-nil if LIST contains ELEMENT.

The test for equality is done with `equal', or with `-compare-fn'
if that's non-nil.

Alias: `-contains-p'"
  (not
   (null
    (cond
     ((null -compare-fn)    (member element list))
     ((eq -compare-fn 'eq)  (memq element list))
     ((eq -compare-fn 'eql) (memql element list))
     (t
      (let ((lst list))
        (while (and lst
                    (not (funcall -compare-fn element (car lst))))
          (setq lst (cdr lst)))
        lst))))))

(defalias '-contains-p '-contains?)

(defun -same-items? (list list2)
  "Return true if LIST and LIST2 has the same items.

The order of the elements in the lists does not matter.

Alias: `-same-items-p'"
  (let ((length-a (length list))
        (length-b (length list2)))
    (and
     (= length-a length-b)
     (= length-a (length (-intersection list list2))))))

(defalias '-same-items-p '-same-items?)

(defun -is-prefix? (prefix list)
  "Return non-nil if PREFIX is prefix of LIST.

Alias: `-is-prefix-p'"
  (--each-while list (equal (car prefix) it)
    (!cdr prefix))
  (not prefix))

(defun -is-suffix? (suffix list)
  "Return non-nil if SUFFIX is suffix of LIST.

Alias: `-is-suffix-p'"
  (-is-prefix? (reverse suffix) (reverse list)))

(defun -is-infix? (infix list)
  "Return non-nil if INFIX is infix of LIST.

This operation runs in O(n^2) time

Alias: `-is-infix-p'"
  (let (done)
    (while (and (not done) list)
      (setq done (-is-prefix? infix list))
      (!cdr list))
    done))

(defalias '-is-prefix-p '-is-prefix?)
(defalias '-is-suffix-p '-is-suffix?)
(defalias '-is-infix-p '-is-infix?)

(defun -sort (comparator list)
  "Sort LIST, stably, comparing elements using COMPARATOR.
Return the sorted list.  LIST is NOT modified by side effects.
COMPARATOR is called with two elements of LIST, and should return non-nil
if the first element should sort before the second."
  (sort (copy-sequence list) comparator))

(defmacro --sort (form list)
  "Anaphoric form of `-sort'."
  (declare (debug (form form)))
  `(-sort (lambda (it other) ,form) ,list))

(defun -list (&rest args)
  "Return a list with ARGS.

If first item of ARGS is already a list, simply return ARGS.  If
not, return a list with ARGS as elements."
  (let ((arg (car args)))
    (if (listp arg) arg args)))

(defun -repeat (n x)
  "Return a list with X repeated N times.
Return nil if N is less than 1."
  (let (ret)
    (--dotimes n (!cons x ret))
    ret))

(defun -sum (list)
  "Return the sum of LIST."
  (apply '+ list))

(defun -product (list)
  "Return the product of LIST."
  (apply '* list))

(defun -max (list)
  "Return the largest value from LIST of numbers or markers."
  (apply 'max list))

(defun -min (list)
  "Return the smallest value from LIST of numbers or markers."
  (apply 'min list))

(defun -max-by (comparator list)
  "Take a comparison function COMPARATOR and a LIST and return
the greatest element of the list by the comparison function.

See also combinator `-on' which can transform the values before
comparing them."
  (--reduce (if (funcall comparator it acc) it acc) list))

(defun -min-by (comparator list)
  "Take a comparison function COMPARATOR and a LIST and return
the least element of the list by the comparison function.

See also combinator `-on' which can transform the values before
comparing them."
  (--reduce (if (funcall comparator it acc) acc it) list))

(defmacro --max-by (form list)
  "Anaphoric version of `-max-by'.

The items for the comparator form are exposed as \"it\" and \"other\"."
  (declare (debug (form form)))
  `(-max-by (lambda (it other) ,form) ,list))

(defmacro --min-by (form list)
  "Anaphoric version of `-min-by'.

The items for the comparator form are exposed as \"it\" and \"other\"."
  (declare (debug (form form)))
  `(-min-by (lambda (it other) ,form) ,list))

(defun -iterate (fun init n)
  "Return a list of iterated applications of FUN to INIT.

This means a list of form:

  (init (fun init) (fun (fun init)) ...)

N is the length of the returned list."
  (if (= n 0) nil
    (let ((r (list init)))
      (--dotimes (1- n)
        (push (funcall fun (car r)) r))
      (nreverse r))))

(defun -fix (fn list)
  "Compute the (least) fixpoint of FN with initial input LIST.

FN is called at least once, results are compared with `equal'."
  (let ((re (funcall fn list)))
    (while (not (equal list re))
      (setq list re)
      (setq re (funcall fn re)))
    re))

(defmacro --fix (form list)
  "Anaphoric form of `-fix'."
  `(-fix (lambda (it) ,form) ,list))

(defun -unfold (fun seed)
  "Build a list from SEED using FUN.

This is \"dual\" operation to `-reduce-r': while -reduce-r
consumes a list to produce a single value, `-unfold' takes a
seed value and builds a (potentially infinite!) list.

FUN should return `nil' to stop the generating process, or a
cons (A . B), where A will be prepended to the result and B is
the new seed."
  (let ((last (funcall fun seed)) r)
    (while last
      (push (car last) r)
      (setq last (funcall fun (cdr last))))
    (nreverse r)))

(defmacro --unfold (form seed)
  "Anaphoric version of `-unfold'."
  (declare (debug (form form)))
  `(-unfold (lambda (it) ,form) ,seed))

(defun -cons-pair? (con)
  "Return non-nil if CON is true cons pair.
That is (A . B) where B is not a list."
  (and (listp con)
       (not (listp (cdr con)))))

(defun -cons-to-list (con)
  "Convert a cons pair to a list with `car' and `cdr' of the pair respectively."
  (list (car con) (cdr con)))

(defun -value-to-list (val)
  "Convert a value to a list.

If the value is a cons pair, make a list with two elements, `car'
and `cdr' of the pair respectively.

If the value is anything else, wrap it in a list."
  (cond
   ((-cons-pair? val) (-cons-to-list val))
   (t (list val))))

(defun -tree-mapreduce-from (fn folder init-value tree)
  "Apply FN to each element of TREE, and make a list of the results.
If elements of TREE are lists themselves, apply FN recursively to
elements of these nested lists.

Then reduce the resulting lists using FOLDER and initial value
INIT-VALUE. See `-reduce-r-from'.

This is the same as calling `-tree-reduce-from' after `-tree-map'
but is twice as fast as it only traverse the structure once."
  (cond
   ((not tree) nil)
   ((-cons-pair? tree) (funcall fn tree))
   ((listp tree)
    (-reduce-r-from folder init-value (mapcar (lambda (x) (-tree-mapreduce-from fn folder init-value x)) tree)))
   (t (funcall fn tree))))

(defmacro --tree-mapreduce-from (form folder init-value tree)
  "Anaphoric form of `-tree-mapreduce-from'."
  (declare (debug (form form form form)))
  `(-tree-mapreduce-from (lambda (it) ,form) (lambda (it acc) ,folder) ,init-value ,tree))

(defun -tree-mapreduce (fn folder tree)
  "Apply FN to each element of TREE, and make a list of the results.
If elements of TREE are lists themselves, apply FN recursively to
elements of these nested lists.

Then reduce the resulting lists using FOLDER and initial value
INIT-VALUE. See `-reduce-r-from'.

This is the same as calling `-tree-reduce' after `-tree-map'
but is twice as fast as it only traverse the structure once."
  (cond
   ((not tree) nil)
   ((-cons-pair? tree) (funcall fn tree))
   ((listp tree)
    (-reduce-r folder (mapcar (lambda (x) (-tree-mapreduce fn folder x)) tree)))
   (t (funcall fn tree))))

(defmacro --tree-mapreduce (form folder tree)
  "Anaphoric form of `-tree-mapreduce'."
  (declare (debug (form form form)))
  `(-tree-mapreduce (lambda (it) ,form) (lambda (it acc) ,folder) ,tree))

(defun -tree-map (fn tree)
  "Apply FN to each element of TREE while preserving the tree structure."
  (cond
   ((not tree) nil)
   ((-cons-pair? tree) (funcall fn tree))
   ((listp tree)
    (mapcar (lambda (x) (-tree-map fn x)) tree))
   (t (funcall fn tree))))

(defmacro --tree-map (form tree)
  "Anaphoric form of `-tree-map'."
  (declare (debug (form form)))
  `(-tree-map (lambda (it) ,form) ,tree))

(defun -tree-reduce-from (fn init-value tree)
  "Use FN to reduce elements of list TREE.
If elements of TREE are lists themselves, apply the reduction recursively.

FN is first applied to INIT-VALUE and first element of the list,
then on this result and second element from the list etc.

The initial value is ignored on cons pairs as they always contain
two elements."
  (cond
   ((not tree) nil)
   ((-cons-pair? tree) tree)
   ((listp tree)
    (-reduce-r-from fn init-value (mapcar (lambda (x) (-tree-reduce-from fn init-value x)) tree)))
   (t tree)))

(defmacro --tree-reduce-from (form init-value tree)
  "Anaphoric form of `-tree-reduce-from'."
  (declare (debug (form form form)))
  `(-tree-reduce-from (lambda (it acc) ,form) ,init-value ,tree))

(defun -tree-reduce (fn tree)
  "Use FN to reduce elements of list TREE.
If elements of TREE are lists themselves, apply the reduction recursively.

FN is first applied to first element of the list and second
element, then on this result and third element from the list etc.

See `-reduce-r' for how exactly are lists of zero or one element handled."
  (cond
   ((not tree) nil)
   ((-cons-pair? tree) tree)
   ((listp tree)
    (-reduce-r fn (mapcar (lambda (x) (-tree-reduce fn x)) tree)))
   (t tree)))

(defmacro --tree-reduce (form tree)
  "Anaphoric form of `-tree-reduce'."
  (declare (debug (form form)))
  `(-tree-reduce (lambda (it acc) ,form) ,tree))

(defun -tree-map-nodes (pred fun tree)
  "Call FUN on each node of TREE that satisfies PRED.

If PRED returns nil, continue descending down this node.  If PRED
returns non-nil, apply FUN to this node and do not descend
further."
  (if (funcall pred tree)
      (funcall fun tree)
    (if (and (listp tree)
             (not (-cons-pair? tree)))
        (-map (lambda (x) (-tree-map-nodes pred fun x)) tree)
      tree)))

(defmacro --tree-map-nodes (pred form tree)
  "Anaphoric form of `-tree-map-nodes'."
  `(-tree-map-nodes (lambda (it) ,pred) (lambda (it) ,form) ,tree))

(defun -tree-seq (branch children tree)
  "Return a sequence of the nodes in TREE, in depth-first search order.

BRANCH is a predicate of one argument that returns non-nil if the
passed argument is a branch, that is, a node that can have children.

CHILDREN is a function of one argument that returns the children
of the passed branch node.

Non-branch nodes are simply copied."
  (cons tree
        (when (funcall branch tree)
          (-mapcat (lambda (x) (-tree-seq branch children x))
                   (funcall children tree)))))

(defmacro --tree-seq (branch children tree)
  "Anaphoric form of `-tree-seq'."
  `(-tree-seq (lambda (it) ,branch) (lambda (it) ,children) ,tree))

(defun -clone (list)
  "Create a deep copy of LIST.
The new list has the same elements and structure but all cons are
replaced with new ones.  This is useful when you need to clone a
structure such as plist or alist."
  (-tree-map 'identity list))

(defun dash-enable-font-lock ()
  "Add syntax highlighting to dash functions, macros and magic values."
  (eval-after-load "lisp-mode"
    '(progn
       (let ((new-keywords '(
                             "-each"
                             "--each"
                             "-each-while"
                             "--each-while"
                             "-dotimes"
                             "--dotimes"
                             "-map"
                             "--map"
                             "-reduce-from"
                             "--reduce-from"
                             "-reduce"
                             "--reduce"
                             "-reduce-r-from"
                             "--reduce-r-from"
                             "-reduce-r"
                             "--reduce-r"
                             "-filter"
                             "--filter"
                             "-select"
                             "--select"
                             "-remove"
                             "--remove"
                             "-reject"
                             "--reject"
                             "-remove-first"
                             "--remove-first"
                             "-reject-first"
                             "--reject-first"
                             "-remove-last"
                             "--remove-last"
                             "-reject-last"
                             "--reject-last"
                             "-remove-item"
                             "-non-nil"
                             "-keep"
                             "--keep"
                             "-map-indexed"
                             "--map-indexed"
                             "-splice"
                             "--splice"
                             "-splice-list"
                             "--splice-list"
                             "-map-when"
                             "--map-when"
                             "-replace-where"
                             "--replace-where"
                             "-map-first"
                             "--map-first"
                             "-map-last"
                             "--map-last"
                             "-replace"
                             "-replace-first"
                             "-replace-last"
                             "-flatten"
                             "-flatten-n"
                             "-concat"
                             "-mapcat"
                             "--mapcat"
                             "-copy"
                             "-cons*"
                             "-snoc"
                             "-first"
                             "--first"
                             "-find"
                             "--find"
                             "-some"
                             "--some"
                             "-any"
                             "--any"
                             "-last"
                             "--last"
                             "-first-item"
                             "-last-item"
                             "-butlast"
                             "-count"
                             "--count"
                             "-any?"
                             "--any?"
                             "-some?"
                             "--some?"
                             "-any-p"
                             "--any-p"
                             "-some-p"
                             "--some-p"
                             "-all?"
                             "--all?"
                             "-every?"
                             "--every?"
                             "-all-p"
                             "--all-p"
                             "-every-p"
                             "--every-p"
                             "-none?"
                             "--none?"
                             "-none-p"
                             "--none-p"
                             "-only-some?"
                             "--only-some?"
                             "-only-some-p"
                             "--only-some-p"
                             "-slice"
                             "-take"
                             "-drop"
                             "-take-while"
                             "--take-while"
                             "-drop-while"
                             "--drop-while"
                             "-split-at"
                             "-rotate"
                             "-insert-at"
                             "-replace-at"
                             "-update-at"
                             "--update-at"
                             "-remove-at"
                             "-remove-at-indices"
                             "-split-with"
                             "--split-with"
                             "-split-on"
                             "-split-when"
                             "--split-when"
                             "-separate"
                             "--separate"
                             "-partition-all-in-steps"
                             "-partition-in-steps"
                             "-partition-all"
                             "-partition"
                             "-partition-by"
                             "--partition-by"
                             "-partition-by-header"
                             "--partition-by-header"
                             "-group-by"
                             "--group-by"
                             "-interpose"
                             "-interleave"
                             "-zip-with"
                             "--zip-with"
                             "-zip"
                             "-zip-fill"
                             "-cycle"
                             "-pad"
                             "-annotate"
                             "--annotate"
                             "-table"
                             "-table-flat"
                             "-partial"
                             "-elem-index"
                             "-elem-indices"
                             "-find-indices"
                             "--find-indices"
                             "-find-index"
                             "--find-index"
                             "-find-last-index"
                             "--find-last-index"
                             "-select-by-indices"
                             "-select-columns"
                             "-select-column"
                             "-grade-up"
                             "-grade-down"
                             "->"
                             "->>"
                             "-->"
                             "-when-let"
                             "-when-let*"
                             "--when-let"
                             "-if-let"
                             "-if-let*"
                             "--if-let"
                             "-let*"
                             "-let"
                             "-lambda"
                             "-distinct"
                             "-uniq"
                             "-union"
                             "-intersection"
                             "-difference"
                             "-contains?"
                             "-contains-p"
                             "-same-items?"
                             "-same-items-p"
                             "-is-prefix-p"
                             "-is-prefix?"
                             "-is-suffix-p"
                             "-is-suffix?"
                             "-is-infix-p"
                             "-is-infix?"
                             "-sort"
                             "--sort"
                             "-list"
                             "-repeat"
                             "-sum"
                             "-product"
                             "-max"
                             "-min"
                             "-max-by"
                             "--max-by"
                             "-min-by"
                             "--min-by"
                             "-iterate"
                             "--iterate"
                             "-fix"
                             "--fix"
                             "-unfold"
                             "--unfold"
                             "-cons-pair?"
                             "-cons-to-list"
                             "-value-to-list"
                             "-tree-mapreduce-from"
                             "--tree-mapreduce-from"
                             "-tree-mapreduce"
                             "--tree-mapreduce"
                             "-tree-map"
                             "--tree-map"
                             "-tree-reduce-from"
                             "--tree-reduce-from"
                             "-tree-reduce"
                             "--tree-reduce"
                             "-tree-seq"
                             "--tree-seq"
                             "-tree-map-nodes"
                             "--tree-map-nodes"
                             "-clone"
                             "-rpartial"
                             "-juxt"
                             "-applify"
                             "-on"
                             "-flip"
                             "-const"
                             "-cut"
                             "-orfn"
                             "-andfn"
                             "-iteratefn"
                             "-fixfn"
                             "-prodfn"
                             ))
             (special-variables '(
                                  "it"
                                  "it-index"
                                  "acc"
                                  "other"
                                  )))
         (font-lock-add-keywords 'emacs-lisp-mode `((,(concat "\\_<" (regexp-opt special-variables 'paren) "\\_>")
                                                     1 font-lock-variable-name-face)) 'append)
         (font-lock-add-keywords 'emacs-lisp-mode `((,(concat "(\\s-*" (regexp-opt new-keywords 'paren) "\\_>")
                                                     1 font-lock-keyword-face)) 'append))
       (--each (buffer-list)
         (with-current-buffer it
           (when (and (eq major-mode 'emacs-lisp-mode)
                      (boundp 'font-lock-mode)
                      font-lock-mode)
             (font-lock-refresh-defaults)))))))
(require 'color)

;;; Options

(defgroup solarized nil
  "Solarized theme options.
The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom solarized-distinct-fringe-background nil
  "Make the fringe background different from the normal background color.
Also affects `linum-mode' background."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-distinct-doc-face nil
  "Make `font-lock-doc-face' stand out more.
Related discussion: https://github.com/bbatsov/solarized-emacs/issues/158"
  :type 'boolean
  :group 'solarized)

(defcustom solarized-use-variable-pitch t
  "Use variable pitch face for some headings and titles."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-use-less-bold nil
  "Use bold weight less often."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-use-more-italic nil
  "Use italic slant more often."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-emphasize-indicators t
  "Use more colors for indicators such as git:gutter, flycheck and similar."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-high-contrast-mode-line nil
  "Make the active/inactive mode line stand out more."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'solarized)

(defcustom solarized-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'solarized)

(defcustom solarized-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'solarized)

(defcustom solarized-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'solarized)

(defcustom solarized-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'solarized)

(defcustom solarized-scale-org-headlines t
  "Whether scaling of outline-headlines should apply to `org-mode' headlines."
  :type 'boolean
  :group 'solarized)

;;; Utilities

;;;###autoload
(defun solarized-color-blend (color1 color2 alpha)
  "Blends COLOR1 onto COLOR2 with ALPHA.

COLOR1 and COLOR2 should be color names (e.g. \"white\") or RGB
triplet strings (e.g. \"#ff12ec\").

Alpha should be a float between 0 and 1."
  (apply 'color-rgb-to-hex
         (-zip-with '(lambda (it other)
                       (+ (* alpha it) (* other (- 1 alpha))))
                    (color-name-to-rgb color1)
                    (color-name-to-rgb color2))))

;;; Setup Start
(defmacro solarized-with-color-variables (variant &rest body)
  (declare (indent 0))
  `(let* ((class '((class color) (min-colors 89)))
         (variant ,variant)
         (s-base03    "#002b36")
         (s-base02    "#073642")
         ;; emphasized content
         (s-base01    "#586e75")
         ;; primary content
         (s-base00    "#657b83")
         (s-base0     "#839496")
         ;; comments
         (s-base1     "#93a1a1")
         ;; background highlight light
         (s-base2     "#eee8d5")
         ;; background light
         (s-base3     "#fdf6e3")

         ;; Solarized accented colors
         (yellow    "#b58900")
         (orange    "#cb4b16")
         (red       "#dc322f")
         (magenta   "#d33682")
         (violet    "#6c71c4")
         (blue      "#268bd2")
         (cyan      "#2aa198")
         (green     "#859900")

         ;; Darker and lighter accented colors
         ;; Only use these in exceptional circumstances!
         (yellow-d  "#7B6000")
         (yellow-l  "#DEB542")
         (orange-d  "#8B2C02")
         (orange-l  "#F2804F")
         (red-d     "#990A1B")
         (red-l     "#FF6E64")
         (magenta-d "#93115C")
         (magenta-l "#F771AC")
         (violet-d  "#3F4D91")
         (violet-l  "#9EA0E5")
         (blue-d    "#00629D")
         (blue-l    "#69B7F0")
         (cyan-d    "#00736F")
         (cyan-l    "#69CABF")
         (green-d   "#546E00")
         (green-l   "#B4C342")

         ;; Solarized palette names, use these instead of -fg -bg...
         (base0 (if (eq variant 'light) s-base00 s-base0))
         (base00 (if (eq variant 'light) s-base0 s-base00))
         (base1 (if (eq variant 'light) s-base01 s-base1))
         (base01 (if (eq variant 'light) s-base1 s-base01))
         (base2 (if (eq variant 'light) s-base02 s-base2))
         (base02 (if (eq variant 'light) s-base2 s-base02))
         (base3 (if (eq variant 'light) s-base03 s-base3))
         (base03 (if (eq variant 'light) s-base3 s-base03))

         ;; Line drawing color
         ;;
         ;; NOTE only use this for very thin lines that are hard to see using base02, in low
         ;; color displayes base02 might be used instead
         (s-line (if (eq variant 'light) "#cccec4" "#284b54"))

         ;; Light/Dark adaptive higher/lower contrast accented colors
         ;;
         ;; NOTE Only use these in exceptional cirmumstances!
         (yellow-hc (if (eq variant 'light) yellow-d yellow-l))
         (yellow-lc (if (eq variant 'light) yellow-l yellow-d))
         (orange-hc (if (eq variant 'light) orange-d orange-l))
         (orange-lc (if (eq variant 'light) orange-l orange-d))
         (red-hc (if (eq variant 'light) red-d red-l))
         (red-lc (if (eq variant 'light) red-l red-d))
         (magenta-hc (if (eq variant 'light) magenta-d magenta-l))
         (magenta-lc (if (eq variant 'light) magenta-l magenta-d))
         (violet-hc (if (eq variant 'light) violet-d violet-l))
         (violet-lc (if (eq variant 'light) violet-l violet-d))
         (blue-hc (if (eq variant 'light) blue-d blue-l))
         (blue-lc (if (eq variant 'light) blue-l blue-d))
         (cyan-hc (if (eq variant 'light) cyan-d cyan-l))
         (cyan-lc (if (eq variant 'light) cyan-l cyan-d))
         (green-hc (if (eq variant 'light) green-d green-l))
         (green-lc (if (eq variant 'light) green-l green-d))

         ;; customize based face properties
         (s-maybe-bold (if solarized-use-less-bold
                           'unspecified 'bold))
         (s-maybe-italic (if solarized-use-more-italic
                             'italic 'normal))
         (s-variable-pitch (if solarized-use-variable-pitch
                               'variable-pitch 'default))
         (s-fringe-bg (if solarized-distinct-fringe-background
                          base02 base03))
         (s-fringe-fg base01)

         (s-header-line-fg (if solarized-high-contrast-mode-line
                               base1 base0))
         (s-header-line-bg (if solarized-high-contrast-mode-line
                               base02 base03))
         (s-header-line-underline (if solarized-high-contrast-mode-line
                                      nil base02))

         (s-mode-line-fg (if solarized-high-contrast-mode-line
                             base03 base0))
         (s-mode-line-bg (if solarized-high-contrast-mode-line
                             base0 base02))
         (s-mode-line-underline (if solarized-high-contrast-mode-line
                                    nil s-line))

         (s-mode-line-buffer-id-fg (if solarized-high-contrast-mode-line
                                       'unspecified base1))
         (s-mode-line-inactive-fg (if solarized-high-contrast-mode-line
                                      base0 base01))
         (s-mode-line-inactive-bg (if solarized-high-contrast-mode-line
                                      base02 base03))
         (s-mode-line-inactive-bc (if solarized-high-contrast-mode-line
                                      base02 base02))
         )
     ,@body))

(defun create-solarized-theme (variant theme-name &optional childtheme)
  "Create a VARIANT of the theme named THEME-NAME.

When optional argument CHILDTHEME function is supplied it's invoked to further
customize the resulting theme."
;;; Color palette
  (solarized-with-color-variables variant
;;; Theme Faces
    (custom-theme-set-faces
     theme-name
;;;; Built-in
;;;;; basic coloring
     `(default ((,class (:foreground ,base0 :background ,base03))))
     `(shadow ((,class (:foreground ,base01))))
     `(match ((,class (:background ,base02 :foreground ,base1 :weight bold))))
     `(cursor ((,class (:foreground ,base03 :background ,base0
                                    :inverse-video t))))
     `(escape-glyph ((,class (:foreground ,violet))))
     `(fringe ((,class (:foreground ,s-fringe-fg :background ,s-fringe-bg))))
     `(highlight ((,class (:background ,base02))))
     `(link ((,class (:foreground ,yellow :underline t :weight bold))))
     `(link-visited ((,class (:foreground ,yellow :underline t :weight normal))))
     `(success ((,class (:foreground ,green ))))
     `(warning ((,class (:foreground ,yellow ))))
     `(error ((,class (:foreground ,orange))))
     `(lazy-highlight ((,class (:foreground ,base03 :background ,yellow
                                            :weight normal))))
     `(widget-field ((,class (:background ,base02))))
     '(button ((t (:underline t))))
;;;;; compilation
     `(compilation-column-face ((,class (:foreground ,cyan :underline nil))))
     `(compilation-column-number ((,class (:inherit font-lock-doc-face :foreground ,cyan
                                                    :underline nil))))
     `(compilation-enter-directory-face ((,class (:foreground ,green :underline nil))))
     `(compilation-error ((,class (:inherit error :underline nil))))
     `(compilation-error-face ((,class (:foreground ,red : :underline nil))))
     `(compilation-face ((,class (:foreground ,base0 :underline nil))))
     `(compilation-info ((,class (:foreground ,base01 :underline nil :bold nil))))
     `(compilation-info-face ((,class (:foreground ,blue :underline nil))))
     `(compilation-leave-directory-face ((,class (:foreground ,green :underline nil))))
     `(compilation-line-face ((,class (:foreground ,green :underline nil))))
     `(compilation-line-number ((,class (:foreground ,green :underline nil))))
     `(compilation-warning ((,class (:inherit warning :underline nil))))
     `(compilation-warning-face ((,class (:foreground ,yellow :weight normal :underline nil))))

     `(compilation-mode-line-exit
       ((,class (:foreground unspecified :weight bold))))
     `(compilation-mode-line-fail
       ((,class (:inherit compilation-error :foreground ,red :weight bold))))
     `(compilation-mode-line-run ((,class (:foreground ,orange :weight bold))))
;;;;; cua
     `(cua-global-mark ((,class (:background ,yellow :foreground ,base03))))
     `(cua-rectangle ((,class (:inherit region
                                        :background ,magenta :foreground ,base03))))
     `(cua-rectangle-noselect ((,class (:inherit region :background ,base02
                                                 :foreground ,base01))))
;;;;; debbugs
     `(debbugs-gnu-archived ((,class (:inverse-video t))))
     `(debbugs-gnu-done ((,class (:foreground ,base01))))
     `(debbugs-gnu-handled ((,class (:foreground ,green))))
     `(debbugs-gnu-new ((,class (:foreground ,blue))))
     `(debbugs-gnu-pending ((,class (:foreground ,cyan))))
     `(debbugs-gnu-stale ((,class (:foreground ,yellow))))
     `(debbugs-gnu-tagged ((,class (:foreground ,base1 :weight bold))))

;;;;; diary
     `(diary ((,class (:foreground ,yellow))))
;;;;; dired
     `(dired-directory ((,class (:foreground ,blue :weight normal))))
     `(dired-flagged ((,class (:foreground ,red))))
     `(dired-header ((,class (:foreground ,base03 :background ,blue))))
     `(dired-ignored ((,class (:inherit shadow))))
     `(dired-mark ((,class (:foreground ,yellow :weight bold))))
     `(dired-marked ((,class (:foreground ,magenta :weight bold))))
     `(dired-perm-write ((,class (:foreground ,base0 :underline t))))
     `(dired-symlink ((,class (:foreground ,cyan :weight normal :slant italic))))
     `(dired-warning ((,class (:foreground ,orange :underline t))))
;;;;; dired-async
     `(dired-async-message ((,class (:background ,(if (eq variant 'light) yellow-l yellow) ))))
     `(dired-async-mode-message
       ((,class (:background ,(if (eq variant 'light) red-l red) ))))
;;;;; dired-efap
     `(dired-efap-face ((,class (:box nil
                                      :background ,base02
                                      :foreground ,base1
                                      :underline ,s-line
                                      :weight bold))))
;;;;; dropdown
     `(dropdown-list-face ((,class (:background ,base02 :foreground ,cyan))))
     `(dropdown-list-selection-face ((,class (:background ,cyan-lc :foreground ,cyan-hc))))
;;;;; ecb
     `(ecb-default-highlight-face ((,class (:background ,blue :foreground ,base03))))
     `(ecb-history-bucket-node-dir-soure-path-face
       ((,class (:inherit ecb-history-bucket-node-face :foreground ,yellow))))
     `(ecb-source-in-directories-buffer-face ((,class (:inherit ecb-directories-general-face
                                                                :foreground ,base0))))
     `(ecb-history-dead-buffer-face ((,class (:inherit ecb-history-general-face
                                                       :foreground ,base01))))
     `(ecb-directory-not-accessible-face ((,class (:inherit ecb-directories-general-face
                                                            :foreground ,base01))))
     `(ecb-bucket-node-face ((,class (:inherit ecb-default-general-face :weight normal
                                               :foreground ,blue))))
     `(ecb-tag-header-face ((,class (:background ,base02))))
     `(ecb-analyse-bucket-element-face ((,class (:inherit ecb-analyse-general-face
                                                          :foreground ,green))))
     `(ecb-directories-general-face ((,class (:inherit ecb-default-general-face :height 1.0))))
     `(ecb-method-non-semantic-face ((,class (:inherit ecb-methods-general-face
                                                       :foreground ,cyan))))
     `(ecb-mode-line-prefix-face ((,class (:foreground ,green))))
     `(ecb-tree-guide-line-face ((,class (:inherit ecb-default-general-face
                                                   :foreground ,base02 :height 1.0))))
;;;;; ee
     `(ee-bookmarked ((,class (:foreground ,base1))))
     `(ee-category ((,class (:foreground ,blue))))
     `(ee-link ((,class (:inherit link))))
     `(ee-link-visited ((,class (:inherit link-visited))))
     `(ee-marked ((,class (:foreground ,magenta :weight bold))))
     `(ee-omitted ((,class (:foreground ,base01))))
     `(ee-shadow ((,class (:inherit shadow))))
;;;;; enh-ruby-mode
     `(enh-ruby-string-delimiter-face ((,class (:foreground ,yellow))))
     `(enh-ruby-heredoc-delimiter-face ((,class (:inherit enh-ruby-string-delimiter-face))))
     `(enh-ruby-regexp-delimiter-face ((,class (:inherit enh-ruby-string-delimiter-face))))
     `(enh-ruby-op-face ((,class (:foreground ,base0))))
     `(erm-syn-errline ((,class (:inherit flymake-errline))))
     `(erm-syn-warnline ((,class (:inherit flymake-warnline))))
;;;;; completions
     `(completions-annotations ((t (:foreground ,base01))))
;;;;; grep
     `(grep-context-face ((,class (:foreground ,base0))))
     `(grep-error-face ((,class (:foreground ,red :weight bold :underline t))))
     `(grep-hit-face ((,class (:foreground ,blue))))
     `(grep-match-face ((,class (:foreground ,orange :weight bold))))
;;;;; isearch
     `(isearch ((,class (:foreground ,base03 :background ,magenta :weight normal))))
     `(isearch-fail ((,class (:foreground ,red :background ,base03 :bold t))))
;;;;; man
     `(Man-overstrike ((,class (:foreground ,blue :weight bold))))
     `(Man-reverse ((,class (:foreground ,orange))))
     `(Man-underline ((,class (:foreground ,green :underline t))))
;;;;; misc faces
     `(menu ((,class (:foreground ,base0 :background ,base03))))
     `(minibuffer-prompt ((,class (:foreground ,base0))))
     `(mode-line
       ((,class (:inverse-video unspecified
                                :overline ,s-mode-line-bg
                                :underline ,s-mode-line-underline
                                :foreground ,s-mode-line-fg
                                :background ,s-mode-line-bg
                                :box (:line-width 1 :color ,s-mode-line-bg
                                                  :style unspecified)
                                ))))
     `(mode-line-buffer-id ((,class (:foreground ,s-mode-line-buffer-id-fg :weight bold))))
     `(mode-line-inactive
       ((,class (:inverse-video unspecified
                                :overline ,s-mode-line-inactive-bc
                                :underline ,s-mode-line-underline
                                :foreground ,s-mode-line-inactive-fg
                                :background ,s-mode-line-inactive-bg
                                :box (:line-width 1 :color ,s-mode-line-inactive-bg
                                                  :style unspecified)
                                ))))
     `(header-line
       ((,class (:inverse-video unspecified
                                :overline nil
                                :underline ,s-header-line-underline
                                :foreground ,s-header-line-fg
                                :background ,s-header-line-bg
                                :box (:line-width 2 :color ,s-header-line-bg
                                                  :style unspecified)
                                ))))
     `(region ((,class (:foreground ,base03 :background ,base1))))
     `(secondary-selection ((,class (:background ,base02))))

     `(trailing-whitespace ((,class (:background ,red))))
     `(vertical-border ((,class (:foreground ,s-line))))
;;;;; font lock
     `(font-lock-builtin-face ((,class (:foreground ,base0 :weight ,s-maybe-bold
                                                    :slant ,s-maybe-italic))))
     `(font-lock-comment-delimiter-face
       ((,class (:foreground ,base01 :slant ,s-maybe-italic))))
     `(font-lock-comment-face ((,class (:foreground ,base01))))
     `(font-lock-constant-face ((,class (:foreground ,blue :weight bold))))
     `(font-lock-doc-face ((,class (:foreground ,(if solarized-distinct-doc-face violet cyan)
                                                :slant ,s-maybe-italic))))
     `(font-lock-function-name-face ((,class (:foreground ,blue))))
     `(font-lock-keyword-face ((,class (:foreground ,green :weight ,s-maybe-bold))))
     `(font-lock-negation-char-face ((,class (:foreground ,yellow :weight bold))))
     `(font-lock-preprocessor-face ((,class (:foreground ,blue))))
     `(font-lock-regexp-grouping-construct ((,class (:foreground ,yellow :weight bold))))
     `(font-lock-regexp-grouping-backslash ((,class (:foreground ,green :weight bold))))
     `(font-lock-string-face ((,class (:foreground ,cyan))))
     `(font-lock-type-face ((,class (:foreground ,yellow))))
     `(font-lock-variable-name-face ((,class (:foreground ,blue))))
     `(font-lock-warning-face ((,class (:inherit error :weight bold))))
     `(c-annotation-face ((,class (:inherit font-lock-constant-face))))
;;;; Third-party
;;;;; ace-jump-mode
     `(ace-jump-face-background
       ((,class (:foreground ,base01 :background ,base03
                             :inverse-video nil))))
     `(ace-jump-face-foreground
       ((,class (:foreground ,red :background ,base03 :inverse-video nil :weight bold))))
;;;;; auctex
     `(font-latex-bold-face ((,class (:inherit bold :foreground ,base1))))
     `(font-latex-doctex-documentation-face ((,class (:background unspecified))))
     `(font-latex-doctex-preprocessor-face ((,class
                                             (:inherit (font-latex-doctex-documentation-face
                                                        font-lock-builtin-face
                                                        font-lock-preprocessor-face)))))
     `(font-latex-italic-face ((,class (:inherit italic :foreground ,base1))))
     `(font-latex-math-face ((,class (:foreground ,violet))))
     `(font-latex-sectioning-0-face ((,class (:inherit font-latex-sectioning-1-face
                                                       :height ,solarized-height-plus-1))))
     `(font-latex-sectioning-1-face ((,class (:inherit font-latex-sectioning-2-face
                                                       :height ,solarized-height-plus-1))))
     `(font-latex-sectioning-2-face ((,class (:inherit font-latex-sectioning-3-face
                                                       :height ,solarized-height-plus-1))))
     `(font-latex-sectioning-3-face ((,class (:inherit font-latex-sectioning-4-face
                                                       :height ,solarized-height-plus-1))))
     `(font-latex-sectioning-4-face ((,class (:inherit font-latex-sectioning-5-face
                                                       :height ,solarized-height-plus-1))))
     `(font-latex-sectioning-5-face ((,class (:inherit ,s-variable-pitch :foreground ,yellow
                                                       :weight bold))))
     `(font-latex-sedate-face ((,class (:foreground ,base1))))
     `(font-latex-slide-title-face ((,class (:inherit (,s-variable-pitch font-lock-type-face)
                                                      :weight bold :height ,solarized-height-plus-3))))
     `(font-latex-string-face ((,class (:foreground ,cyan))))
     `(font-latex-subscript-face ((,class (:height ,solarized-height-minus-1))))
     `(font-latex-superscript-face ((,class (:height ,solarized-height-minus-1))))
     `(font-latex-verbatim-face ((,class (:inherit fixed-pitch :foreground ,base0
                                                   :slant italic))))
     `(font-latex-warning-face ((,class (:inherit bold :foreground ,orange))))
;;;;; auto-complete
     `(ac-candidate-face ((,class (:background ,base02 :foreground ,cyan))))
     `(ac-selection-face ((,class (:background ,cyan-lc :foreground ,cyan-hc))))
     `(ac-candidate-mouse-face ((,class (:background ,cyan-hc :foreground ,cyan-lc))))
     `(ac-completion-face ((,class (:foreground ,base1 :underline t))))
     `(ac-gtags-candidate-face ((,class (:background ,base02 :foreground ,blue))))
     `(ac-gtags-selection-face ((,class (:background ,blue-lc :foreground ,blue-hc))))
     `(ac-yasnippet-candidate-face ((,class (:background ,base02 :foreground ,yellow))))
     `(ac-yasnippet-selection-face ((,class (:background ,yellow-lc :foreground ,yellow-hc))))
;;;;; auto highlight symbol
     `(ahs-definition-face ((,class (:foreground ,magenta :background unspecified
                                                 :slant normal))))
     `(ahs-edit-mode-face ((,class (:foreground ,base03 :background ,magenta))))
     `(ahs-face ((,class (:foreground ,magenta :background unspecified))))
     `(ahs-plugin-bod-face ((,class (:foreground ,magenta :background unspecified ))))
     `(ahs-plugin-defalt-face ((,class (:foreground ,magenta :background unspecified))))
     `(ahs-plugin-whole-buffer-face ((,class (:foreground ,magenta  :background unspecified))))
     `(ahs-warning-face ((,class (:foreground ,red :weight bold))))
;;;;; android mode
     `(android-mode-debug-face ((,class (:foreground ,green))))
     `(android-mode-error-face ((,class (:foreground ,orange :weight bold))))
     `(android-mode-info-face ((,class (:foreground ,base0))))
     `(android-mode-verbose-face ((,class (:foreground ,base01))))
     `(android-mode-warning-face ((,class (:foreground ,yellow))))
;;;;; anzu-mode
     `(anzu-mode-line ((,class (:foreground ,yellow :weight bold))))
;;;;; avy-mode
     `(avy-lead-face ((,class (:inherit isearch))))
     `(avy-lead-face-0 ((,class (:inherit isearch :background ,violet))))
     `(avy-lead-face-1 ((,class (:inherit isearch :background ,orange))))
     `(avy-lead-face-2 ((,class (:inherit isearch :background ,cyan))))
     `(avy-background-face ((,class (:inherit font-lock-comment-face))))
;;;;; bm
     `(bm-face ((,class (:overline ,base0))))
     `(bm-fringe-face ((,class (:overline ,base0))))
     `(bm-fringe-persistent-face ((,class (:overline ,base0))))
     `(bm-persistent-face ((,class (:overline ,base0))))
;;;;; calfw
     `(cfw:face-day-title ((,class (:background ,base02))))
     `(cfw:face-annotation ((,class (:inherit cfw:face-day-title :foreground ,yellow))))
     `(cfw:face-default-content ((,class (:foreground ,green))))
     `(cfw:face-default-day ((,class (:inherit cfw:face-day-title :weight bold))))
     `(cfw:face-disable ((,class (:inherit cfw:face-day-title
                                           :foreground ,base01))))
     `(cfw:face-grid ((,class (:foreground ,base01))))
     `(cfw:face-header ((,class (:foreground ,blue-hc :background ,blue-lc :weight bold))))
     `(cfw:face-holiday ((,class (:background nil :foreground ,red :weight bold))))
     `(cfw:face-periods ((,class (:foreground ,magenta))))
     `(cfw:face-select ((,class (:background ,magenta-lc :foreground ,magenta-hc))))
     `(cfw:face-saturday ((,class (:foreground ,cyan-hc :background ,cyan-lc))))
     `(cfw:face-sunday ((,class (:foreground ,red-hc :background ,red-lc :weight bold))))
     `(cfw:face-title ((,class (:inherit ,s-variable-pitch :foreground ,yellow
                                         :weight bold :height ,solarized-height-plus-4))))
     `(cfw:face-today ((,class (:weight bold :background ,base02 :foreground nil))))
     `(cfw:face-today-title ((,class (:background ,yellow-lc
                                                  :foreground ,yellow-hc :weight bold))))
     `(cfw:face-toolbar ((,class (:background ,base02 :foreground ,base0))))
     `(cfw:face-toolbar-button-off ((,class (:background ,yellow-lc :foreground ,yellow-hc
                                                         :weight bold))))
     `(cfw:face-toolbar-button-on ((,class (:background ,yellow-hc :foreground ,yellow-lc
                                                        :weight bold))))
;;;;; cider
     `(cider-result-overlay-face ((t (:background unspecified))))
     `(cider-enlightened-face ((t (:box (:color ,magenta :line-width -1)))))
     `(cider-enlightened-local-face ((t (:weight bold :foreground ,green-l))))
     `(cider-deprecated-face ((t (:background ,yellow))))
     `(cider-instrumented-face ((t (:box (:color ,red-l :line-width -1)))))
     `(cider-traced-face ((t (:box (:color ,cyan :line-width -1)))))
;;;;; cider-repl-mode
     `(cider-repl-err-output-face ((t (:inherit ,font-lock-warning-face :underline nil))))
;;;;; cider-test-mode
     `(cider-test-failure-face ((t (:foreground ,orange :weight bold :underline t))))
     `(cider-test-error-face ((t (:foreground ,red :weight bold :underline t))))
     `(cider-test-success-face ((t (:foreground ,green :weight bold :underline t))))
;;;;; company-mode
     `(company-template-field ((,class (:background ,yellow :foreground ,base02))))
     `(company-tooltip ((,class (:background ,base02 :foreground ,cyan))))
     `(company-tooltip-selection ((,class (:background ,cyan-lc :foreground ,cyan-hc))))
     `(company-tooltip-mouse ((,class (:background ,cyan-hc :foreground ,cyan-lc))))
     `(company-tooltip-common ((,class (:foreground ,base1 :underline t))))
     `(company-tooltip-common-selection ((,class (:foreground ,base1 :underline t))))
     `(company-tooltip-annotation ((,class (:foreground ,base1 :background ,base02))))
     `(company-scrollbar-fg ((,class (:foreground ,base03 :background ,base0))))
     `(company-scrollbar-bg ((,class (:background ,base02 :foreground ,cyan))))
     `(company-preview ((,class (:background ,base02 :foreground ,cyan))))
     `(company-preview-common ((,class (:foreground ,base1 :underline t))))
;;;;; column-enforce-mode
     `(column-enforce-face ((,class (:background unspecified :foreground ,magenta
                                                 :inverse-video unspecified))))
;;;;; cscope
     `(cscope-file-face ((,class (:foreground ,green :weight bold))))
     `(cscope-function-face ((,class (:foreground ,blue))))
     `(cscope-line-number-face ((,class (:foreground ,yellow))))
     `(cscope-line-face ((,class (:foreground ,base0))))
     `(cscope-mouse-face ((,class (:background ,blue :foreground ,base0))))
;;;;; ctable
     `(ctbl:face-cell-select ((,class (:background ,base02 :foreground ,base1
                                                   :underline ,base1 :weight bold))))
     `(ctbl:face-continue-bar ((,class (:background ,base02 :foreground ,yellow))))
     `(ctbl:face-row-select ((,class (:background ,base02 :foreground ,base0
                                                  :underline t))))
;;;;; coffee
     `(coffee-mode-class-name ((,class (:foreground ,yellow :weight bold))))
     `(coffee-mode-function-param ((,class (:foreground ,violet :slant italic))))

;;;;; cperl-mode
     `(cperl-array-face ((,class (:background unspecified :foreground ,blue))))
     `(cperl-hash-face ((,class (:background unspecified :foreground ,blue))))
     `(cperl-nonoverridable-face ((,class (:foreground ,base0 :weight bold))))

;;;;; custom
     `(custom-face-tag ((,class (:inherit ,s-variable-pitch :height ,solarized-height-plus-3
                                          :foreground ,violet :weight normal))))
     `(custom-variable-tag ((,class (:inherit ,s-variable-pitch
                                              :foreground ,cyan :height ,solarized-height-plus-3))))
     `(custom-comment-tag ((,class (:foreground ,base01))))
     `(custom-group-tag ((,class (:inherit ,s-variable-pitch :foreground ,blue :height ,solarized-height-plus-3))))
     `(custom-group-tag-1 ((,class (:inherit ,s-variable-pitch :foreground ,red :height ,solarized-height-plus-3))))
     `(custom-state ((,class (:foreground ,green))))
     `(custom-button ((,class (:background ,base02 :foreground ,base1
                                           :box (:line-width 2 :style released-button)))))
     `(custom-button-mouse ((,class (:background ,base01 :foreground ,base02
                                                 :box (:line-width 2 :style released-button)))))
     `(custom-button-pressed ((,class (:background ,base01 :foreground ,base1
                                                   :box (:line-width 2 :style pressed-button)))))
     `(custom-button-unraised ((,class (:inherit underline))))
     `(custom-button-pressed-unraised ((,class (:inherit custom-button-unraised :foreground ,magenta))))
;;;;; diff
     `(diff-added   ((,class (:foreground ,green))))
     `(diff-changed ((,class (:foreground ,blue))))
     `(diff-removed ((,class (:foreground ,red))))
     `(diff-refine-added
       ((((class color) (background light))
         (:background ,(solarized-color-blend "#ddffdd" green 0.7)))
        (((class color) (background dark))
         (:background ,(solarized-color-blend "#446644" green 0.7)))))
     `(diff-refine-changed
       ((((class color) (background light))
         (:background ,(solarized-color-blend "#ddddff" blue 0.7)))
        (((class color) (background dark))
         (:background ,(solarized-color-blend "#444466" blue 0.7)))))
     `(diff-refine-removed
       ((((class color) (background light))
         (:background ,(solarized-color-blend "#ffdddd" red 0.7)))
        (((class color) (background dark))
         (:background ,(solarized-color-blend "#664444" red 0.7)))))
     `(diff-header  ((,class (:background ,base03))))
     `(diff-file-header
       ((,class (:background ,base03 :foreground ,base0 :weight bold))))
;;;;; ediff
     `(ediff-fine-diff-A ((,class (:background ,orange-lc))))
     `(ediff-fine-diff-B ((,class (:background ,green-lc))))
     `(ediff-fine-diff-C ((,class (:background ,yellow-lc))))

     `(ediff-current-diff-C ((,class (:background ,blue-lc))))

     `(ediff-even-diff-A ((,class (:background ,base01
                                               :foreground ,base3 ))))
     `(ediff-odd-diff-A ((,class (:background ,base01
                                              :foreground ,base03 ))))
     `(ediff-even-diff-B ((,class (:background ,base01
                                               :foreground ,base03 ))))
     `(ediff-odd-diff-B ((,class (:background ,base01
                                              :foreground ,base03 ))))
     `(ediff-even-diff-C ((,class (:background ,base01
                                               :foreground ,base0 ))))
     `(ediff-odd-diff-C ((,class (:background ,base01
                                              :foreground ,base03 ))))

;;;;;; alternative ediff (not finished)
     ;; `(ediff-fine-diff-A ((,class (
     ;;                               :background ,(solarized-color-blend blue base03 0.25))
     ;;                              )))
     ;; `(ediff-fine-diff-B ((,class (
     ;;                               :background ,(solarized-color-blend violet base03 0.25))
     ;;                              )))
     ;; `(ediff-fine-diff-C ((,class (
     ;;                               :background ,(solarized-color-blend yellow base03 0.25))
     ;;                              )))
     ;; `(ediff-current-diff-A ((,class (
     ;;                                  :background ,(solarized-color-blend blue base03 0.15)
     ;;                                              ))))
     ;; `(ediff-current-diff-B ((,class (
     ;;                                   :background ,(solarized-color-blend violet base03 0.15)
     ;;                                              ))))
     ;; `(ediff-current-diff-C ((,class (
     ;;                                  :background ,(solarized-color-blend yellow base03 0.15)
     ;;                                              ))))
     ;; `(ediff-even-diff-A ((,class (
     ;;                                ;; :background ,(solarized-color-blend base0 base03 0.15)
     ;;                               :background ,base02
     ;;                               ;; :foreground ,base2
     ;;                                ;; :background ,(solarized-color-blend green base02 0.15)
     ;;                                           ))))
     ;; `(ediff-even-diff-B ((,class (
     ;;                               ;; :background ,base01
     ;;                               :background ,base02
     ;;                               ;; :foreground ,base2
     ;;                                           ))))
     ;; `(ediff-even-diff-C ((,class (
     ;;                               ;; :background ,base01
     ;;                               :background ,base02
     ;;                                           ;; :foreground ,base2
     ;;                                           ))))
     ;; `(ediff-odd-diff-A ((,class (
     ;;                              ;; :background ,base01
     ;;                                          :background ,base02
     ;;                                          ))))
     ;; `(ediff-odd-diff-B ((,class (
     ;;                              ;; :background ,base01
     ;;                                          :background ,base02
     ;;                                          ))))
     ;; `(ediff-odd-diff-C ((,class (
     ;;                              ;; :background ,base01
     ;;                                          :background ,base03
     ;;                                          ))))
     ;; `(ediff-current-diff-Ancestor ((,class (:background "VioletRed" :foreground "Black"))))
     ;; `(ediff-even-diff-Ancestor ((,class (:background "Grey" :foreground "White"))))
     ;; `(ediff-fine-diff-Ancestor ((,class (:background "Green" :foreground "Black"))))
     ;; `(ediff-odd-diff-Ancestor ((,class (:background "gray40" :foreground "cyan3"))))
     ;; `(ediff-even-diff-A ((,class (:underline ,base01))))
     ;; `(ediff-odd-diff-A ((,class (:underline ,base01
     ;;                                          ))))
     ;; `(ediff-even-diff-B ((,class (:background ,base01
     ;;                                           :foreground ,base03
     ;;                                           ))))
     ;; `(ediff-odd-diff-B ((,class (:background ,base01
     ;;                                          :foreground ,base03
     ;;                                          ))))
     ;; `(ediff-even-diff-C ((,class (:background ,base01
     ;;                                           :foreground ,base0
     ;;                                           ))))
     ;; `(ediff-odd-diff-C ((,class (:background ,base01
     ;;                                          :foreground ,base03
     ;;                                          ))))
;;;;; diff-hl
     `(diff-hl-change ((,class (:background ,blue-lc  :foreground ,blue-hc))))
     `(diff-hl-delete ((,class (:background ,red-lc  :foreground ,red-hc))))
     `(diff-hl-insert ((,class (:background ,green-lc  :foreground ,green-hc))))
     `(diff-hl-unknown ((,class (:background ,cyan-lc   :foreground ,cyan-hc))))
;;;;; edts
     `(edts-face-error-line
       ((,(append '((supports :underline (:style line))) class)
         (:underline (:style line :color ,(if (eq variant 'light) red-l red)) :inherit unspecified))
        (,class (:foreground ,red-hc :background ,red-lc :weight bold :underline t))))
     `(edts-face-warning-line
       ((,(append '((supports :underline (:style line))) class)
         (:underline (:style line :color ,(if (eq variant 'light) yellow-l yellow)) :inherit unspecified))
        (,class (:foreground ,yellow-hc :background ,yellow-lc :weight bold :underline t))))
     `(edts-face-error-fringe-bitmap
       ((,class (:foreground ,(if (eq variant 'light) red-l red) :background unspecified :weight bold))))
     `(edts-face-warning-fringe-bitmap
       ((,class (:foreground ,(if (eq variant 'light) yellow-l yellow) :background unspecified :weight bold))))
     `(edts-face-error-mode-line
       ((,class (:background ,(if (eq variant 'light) red-l red) :foreground unspecified))))
     `(edts-face-warning-mode-line
       ((,class (:background ,(if (eq variant 'light) yellow-l yellow) :foreground unspecified))))
;;;;; elfeed
     `(elfeed-search-date-face ((,class (:foreground ,base01))))
     `(elfeed-search-feed-face ((,class (:foreground ,base01))))
     `(elfeed-search-tag-face ((,class (:foreground ,base0))))
     `(elfeed-search-title-face ((,class (:foreground ,base0))))

;;;;; elscreen
     `(elscreen-tab-background-face ((,class (:background ,base03))))
     `(elscreen-tab-current-screen-face ((,class (:background ,base1 :foreground ,base03)) (t (:underline t))))
     `(elscreen-tab-other-screen-face ((,class (:background ,base02 :foreground ,base01))))
     `(elscreen-tab-control-face ((,class (:background ,base03 :foreground ,base0))))
;;;;; epa
     `(epa-mark ((,class (:foreground ,magenta :weight bold))))
     `(epa-string ((,class (:foreground ,violet))))
     `(epa-validity-disabled ((,class (:inverse-video t :slant italic))))
     `(epa-validity-high ((,class (:weight bold))))
     `(epa-validity-low ((,class (:slant italic))))
     `(epa-validity-medium ((,class (:slant italic))))
;;;;; epc
     `(epc:face-title ((,class (:foreground ,blue :background ,base03
                                            :weight normal :underline nil))))
;;;;; eshell
     `(eshell-prompt ((,class (:foreground ,yellow :weight bold))))
     `(eshell-ls-archive ((,class (:foreground ,red :weight bold))))
     `(eshell-ls-backup ((,class (:inherit font-lock-comment-face))))
     `(eshell-ls-clutter ((,class (:inherit font-lock-comment-face))))
     `(eshell-ls-directory ((,class (:foreground ,blue :weight bold))))
     `(eshell-ls-executable ((,class (:foreground ,red :weight bold))))
     `(eshell-ls-unreadable ((,class (:foreground ,base0))))
     `(eshell-ls-missing ((,class (:inherit font-lock-warning-face))))
     `(eshell-ls-product ((,class (:inherit font-lock-doc-face))))
     `(eshell-ls-special ((,class (:foreground ,yellow :weight bold))))
     `(eshell-ls-symlink ((,class (:foreground ,cyan :weight bold))))
;;;;; evil-search-highlight-persist
     `(evil-search-highlight-persist-highlight-face ((,class (:background ,(if (eq variant 'light) green-lc violet-lc)))))
;;;;; fic
     `(fic-author-face ((,class (:background ,base03 :foreground ,orange
                                             :underline t :slant italic))))
     `(fic-face ((,class (:background ,base03 :foreground ,orange
                                      :weight normal :slant italic))))
     `(font-lock-fic-face ((,class (:background ,base03 :foreground ,orange
                                      :weight normal :slant italic))))
;;;;; fixmee
     `(fixmee-notice-face ((,class (:background nil :foreground ,base1
                                                :underline nil :slant italic :weight bold))))

;;;;; flx
     `(flx-highlight-face ((,class (:foreground ,blue
                                                :weight normal :underline nil))))
;;;;; flymake
     `(flymake-errline
       ((,(append '((supports :underline (:style wave))) class)
         (:underline (:style wave :color ,red) :inherit unspecified
                     :foreground unspecified :background unspecified))
        (,class (:foreground ,red-hc :background ,red-lc :weight bold :underline t))))
     `(flymake-infoline
       ((,(append '((supports :underline (:style wave))) class)
         (:underline (:style wave :color ,green) :inherit unspecified
                     :foreground unspecified :background unspecified))
        (,class (:foreground ,green-hc :background ,green-lc))))
     `(flymake-warnline
       ((,(append '((supports :underline (:style wave))) class)
         (:underline (:style wave :color ,yellow) :inherit unspecified
                     :foreground unspecified :background unspecified))
        (,class (:foreground ,yellow-hc :background ,yellow-lc :weight bold :underline t))))
;;;;; flycheck
     `(flycheck-error
       ((,(append '((supports :underline (:style wave))) class)
         (:underline (:style wave :color ,red) :inherit unspecified))
        (,class (:foreground ,red-hc :background ,red-lc :weight bold :underline t))))
     `(flycheck-warning
       ((,(append '((supports :underline (:style wave))) class)
         (:underline (:style wave :color ,yellow) :inherit unspecified))
        (,class (:foreground ,yellow-hc :background ,yellow-lc :weight bold :underline t))))
     `(flycheck-info
       ((,(append '((supports :underline (:style wave))) class)
         (:underline (:style wave :color ,(if solarized-emphasize-indicators
                                              blue base03)) :inherit unspecified))
        (,class (:foreground ,blue-hc :background ,blue-lc :weight bold :underline t))))
     `(flycheck-fringe-error
       ((,class (:foreground ,(if solarized-emphasize-indicators
                                  red-hc red)
                             :background ,(if solarized-emphasize-indicators
                                              red-lc base03) :weight bold))))
     `(flycheck-fringe-warning
       ((,class (:foreground ,(if solarized-emphasize-indicators
                                  yellow-hc yellow)
                             :background ,(if solarized-emphasize-indicators
                                              yellow-lc base03) :weight bold))))
     `(flycheck-fringe-info
       ((,class (:foreground ,(if solarized-emphasize-indicators
                                  blue-hc base01)
                             :background ,(if solarized-emphasize-indicators
                                              blue-lc base03) :weight bold))))
;;;;; flyspell
     `(flyspell-duplicate
       ((,(append '((supports :underline (:style wave))) class)
         (:underline (:style wave :color ,yellow) :inherit unspecified))
        (,class (:foreground ,yellow :weight bold :underline t))))
     `(flyspell-incorrect
       ((,(append '((supports :underline (:style wave))) class)
         (:underline (:style wave :color ,red) :inherit unspecified))
        (,class (:foreground ,red :weight bold :underline t))))
;;;;; erc
     `(erc-action-face ((,class (:inherit erc-default-face))))
     `(erc-bold-face ((,class (:weight bold))))
     `(erc-current-nick-face ((,class (:foreground ,blue :weight bold))))
     `(erc-dangerous-host-face ((,class (:inherit font-lock-warning-face))))
     `(erc-default-face ((,class (:foreground ,base0))))
     `(erc-highlight-face ((,class (:inherit erc-default-face
                                             :background ,base02))))
     `(erc-direct-msg-face ((,class (:inherit erc-default-face))))
     `(erc-error-face ((,class (:inherit font-lock-warning-face))))
     `(erc-fool-face ((,class (:inherit erc-default-face))))
     `(erc-input-face ((,class (:foreground ,yellow))))
     `(erc-keyword-face ((,class (:foreground ,blue :weight bold))))
     `(erc-nick-default-face ((,class (:foreground ,yellow :weight bold))))
     `(erc-my-nick-face ((,class (:foreground ,red :weight bold))))
     `(erc-nick-msg-face ((,class (:inherit erc-default-face))))
     `(erc-notice-face ((,class (:foreground ,green))))
     `(erc-pal-face ((,class (:foreground ,orange :weight bold))))
     `(erc-prompt-face ((,class (:foreground ,orange :background ,base03 :weight bold))))
     `(erc-timestamp-face ((,class (:foreground ,green))))
     `(erc-underline-face ((t (:underline t))))
;;;;; git-commit
     `(git-commit-comment-action  ((,class (:foreground ,base0  :weight bold))))
     `(git-commit-comment-branch  ((,class (:foreground ,blue   :weight bold))))
     `(git-commit-comment-heading ((,class (:foreground ,yellow :weight bold))))
;;;;; git-gutter
     `(git-gutter:added
         ((,class (:weight normal
                           :foreground ,(if solarized-emphasize-indicators
                                            green s-fringe-fg)
                         :background ,s-fringe-bg
                         ))))
     `(git-gutter:deleted
         ((,class (:weight normal
                           :foreground ,(if solarized-emphasize-indicators
                                            red s-fringe-fg)
                         :background ,s-fringe-bg
                         ))))
     `(git-gutter:modified
       ((,class (:weight normal
                         :foreground ,(if solarized-emphasize-indicators
                                          blue s-fringe-fg)
                         :background ,s-fringe-bg
                         ))))
     `(git-gutter:unchanged
       ((,class (:weight normal
                         :foreground ,(if solarized-emphasize-indicators
                                          base01 s-fringe-fg)
                         :background ,s-fringe-bg
                         ))))
;;;;; git-gutter-fr
     `(git-gutter-fr:added ((,class (:foreground ,green  :weight bold))))
     `(git-gutter-fr:deleted ((,class (:foreground ,red :weight bold))))
     `(git-gutter-fr:modified ((,class (:foreground ,blue :weight bold))))
;;;;; git-gutter+ and git-gutter+-fr
     `(git-gutter+-added ((,class (:background ,green :foreground ,base03
                                               :weight bold))))
     `(git-gutter+-deleted ((,class (:background ,red :foreground ,base03
                                                 :weight bold))))
     `(git-gutter+-modified ((,class (:background ,blue :foreground ,base03
                                                  :weight bold))))
     `(git-gutter+-unchanged ((,class (:background ,base02
                                                   :foreground ,base03
                                                   :weight bold))))
     `(git-gutter-fr+-added ((,class (:foreground ,green :weight bold))))
     `(git-gutter-fr+-deleted ((,class (:foreground ,red :weight bold))))
     `(git-gutter-fr+-modified ((,class (:foreground ,blue :weight bold))))
;;;;; git-rebase
     `(git-rebase-hash ((,class (:foreground ,base01))))
;;;;; git-timemachine
     `(git-timemachine-minibuffer-author-face ((,class (:foreground ,orange))))
     `(git-timemachine-minibuffer-detail-face ((,class (:foreground ,yellow))))
;;;;; go-direx
     `(go-direx-header ((,class (:foreground ,blue))))
     `(go-direx-label ((,class (:foreground ,green))))
     `(go-direx-package ((,class (:foreground ,base1 :weight bold))))

;;;;;; go-mode
     `(go-coverage-0 ((,class (:foreground ,orange))))
     `(go-coverage-1 ((,class (:foreground ,(solarized-color-blend blue yellow (/ 2.0 6))))))
     `(go-coverage-2 ((,class (:foreground ,(solarized-color-blend blue yellow (/ 3.0 6))))))
     `(go-coverage-3 ((,class (:foreground ,(solarized-color-blend blue yellow (/ 4.0 6))))))
     `(go-coverage-4 ((,class (:foreground ,(solarized-color-blend blue yellow (/ 5.0 6))))))
     `(go-coverage-5 ((,class (:foreground ,blue))))
     `(go-coverage-6 ((,class (:foreground ,(solarized-color-blend cyan blue (/ 2.0 6))))))
     `(go-coverage-7 ((,class (:foreground ,(solarized-color-blend cyan blue (/ 3.0 6))))))
     `(go-coverage-8 ((,class (:foreground ,(solarized-color-blend cyan blue (/ 4.0 6))))))
     `(go-coverage-9 ((,class (:foreground ,(solarized-color-blend cyan blue (/ 5.0 6))))))
     `(go-coverage-10 ((,class (:foreground ,cyan))))
     `(go-coverage-covered ((,class (:foreground ,green))))
     `(go-coverage-untracked ((,class (:foreground ,base01))))

;;;;; guide-key
     `(guide-key/highlight-command-face ((,class (:foreground ,blue))))
     `(guide-key/key-face ((,class (:foreground ,base01))))
     `(guide-key/prefix-command-face ((,class (:foreground ,green))))
;;;;; gnus
     `(gnus-group-mail-1 ((,class (:weight bold :inherit gnus-group-mail-1-empty))))
     `(gnus-group-mail-1-empty ((,class (:inherit gnus-group-news-1-empty))))
     `(gnus-group-mail-2 ((,class (:weight bold :inherit gnus-group-mail-2-empty))))
     `(gnus-group-mail-2-empty ((,class (:inherit gnus-group-news-2-empty))))
     `(gnus-group-mail-3 ((,class (:weight bold :inherit gnus-group-mail-3-empty))))
     `(gnus-group-mail-3-empty ((,class (:inherit gnus-group-news-3-empty))))
     `(gnus-group-mail-low ((,class (:weight bold :inherit gnus-group-mail-low-empty))))
     `(gnus-group-mail-low-empty ((,class (:inherit gnus-group-news-low-empty))))
     `(gnus-group-news-1 ((,class (:weight bold :inherit gnus-group-news-1-empty))))
     `(gnus-group-news-2 ((,class (:weight bold :inherit gnus-group-news-2-empty))))
     `(gnus-group-news-3 ((,class (:weight bold :inherit gnus-group-news-3-empty))))
     `(gnus-group-news-4 ((,class (:weight bold :inherit gnus-group-news-4-empty))))
     `(gnus-group-news-5 ((,class (:weight bold :inherit gnus-group-news-5-empty))))
     `(gnus-group-news-6 ((,class (:weight bold :inherit gnus-group-news-6-empty))))
     `(gnus-group-news-low ((,class (:weight bold :inherit gnus-group-news-low-empty))))
     `(gnus-header-content ((,class (:inherit message-header-other))))
     `(gnus-header-from ((,class (:inherit message-header-other))))
     `(gnus-header-name ((,class (:inherit message-header-name))))
     `(gnus-header-newsgroups ((,class (:inherit message-header-other))))
     `(gnus-header-subject ((,class (:inherit message-header-subject))))
     `(gnus-summary-cancelled ((,class (:foreground ,orange))))
     `(gnus-summary-high-ancient ((,class (:foreground ,blue :weight bold))))
     `(gnus-summary-high-read ((,class (:foreground ,green :weight bold))))
     `(gnus-summary-high-ticked ((,class (:foreground ,orange :weight bold))))
     `(gnus-summary-high-unread ((,class (:foreground ,base0 :weight bold))))
     `(gnus-summary-low-ancient ((,class (:foreground ,blue))))
     `(gnus-summary-low-read ((t (:foreground ,green))))
     `(gnus-summary-low-ticked ((,class (:foreground ,orange))))
     `(gnus-summary-low-unread ((,class (:foreground ,base0))))
     `(gnus-summary-normal-ancient ((,class (:foreground ,blue))))
     `(gnus-summary-normal-read ((,class (:foreground ,green))))
     `(gnus-summary-normal-ticked ((,class (:foreground ,orange))))
     `(gnus-summary-normal-unread ((,class (:foreground ,base0))))
     `(gnus-summary-selected ((,class (:foreground ,yellow :weight bold))))
     `(gnus-cite-1 ((,class (:foreground ,blue))))
     `(gnus-cite-2 ((,class (:foreground ,blue))))
     `(gnus-cite-3 ((,class (:foreground ,blue))))
     `(gnus-cite-4 ((,class (:foreground ,green))))
     `(gnus-cite-5 ((,class (:foreground ,green))))
     `(gnus-cite-6 ((,class (:foreground ,green))))
     `(gnus-cite-7 ((,class (:foreground ,red))))
     `(gnus-cite-8 ((,class (:foreground ,red))))
     `(gnus-cite-9 ((,class (:foreground ,red))))
     `(gnus-cite-10 ((,class (:foreground ,yellow))))
     `(gnus-cite-11 ((,class (:foreground ,yellow))))
     `(gnus-group-news-1-empty ((,class (:foreground ,yellow))))
     `(gnus-group-news-2-empty ((,class (:foreground ,green))))
     `(gnus-group-news-3-empty ((,class (:foreground ,green))))
     `(gnus-group-news-4-empty ((,class (:foreground ,blue))))
     `(gnus-group-news-5-empty ((,class (:foreground ,blue))))
     `(gnus-group-news-6-empty ((,class (:foreground ,blue-lc))))
     `(gnus-group-news-low-empty ((,class (:foreground ,base01))))
     `(gnus-signature ((,class (:foreground ,yellow))))
     `(gnus-x-face ((,class (:background ,base0 :foreground ,base03))))
;;;;; helm
     ;; These probably needs tweaking.
     `(helm-apt-deinstalled ((,class (:foreground ,base01))))
     `(helm-apt-installed ((,class (:foreground ,green))))
     `(helm-bookmark-directory ((,class (:inherit helm-ff-directory))))
     `(helm-bookmark-file ((,class (:foreground ,base0))))
     `(helm-bookmark-gnus ((,class (:foreground ,cyan))))
     `(helm-bookmark-info ((,class (:foreground ,green))))
     `(helm-bookmark-man ((,class (:foreground ,violet))))
     `(helm-bookmark-w3m ((,class (:foreground ,yellow))))
     `(helm-bookmarks-su ((,class (:foreground ,orange))))
     `(helm-buffer-not-saved ((,class (:foreground ,orange))))
     `(helm-buffer-saved-out ((,class (:foreground ,red :background ,base03
                                                   :inverse-video t))))
     `(helm-buffer-size ((,class (:foreground ,base01))))
     `(helm-candidate-number ((,class (:background ,base02 :foreground ,base1
                                                   :bold t))))
     `(helm-ff-directory ((,class (:background ,base03  :foreground ,blue))))
     `(helm-ff-executable ((,class (:foreground ,green))))
     `(helm-ff-file ((,class (:background ,base03 :foreground ,base0))))
     `(helm-ff-invalid-symlink ((,class (:background ,base03 :foreground ,orange
                                                     :slant italic))))
     `(helm-ff-prefix ((,class (:background ,yellow :foreground ,base03))))
     `(helm-ff-symlink ((,class (:foreground ,cyan))))
     `(helm-grep-file ((,class (:foreground ,cyan :underline t))))
     `(helm-grep-finish ((,class (:foreground ,green))))
     `(helm-grep-lineno ((,class (:foreground ,orange))))
     `(helm-grep-match ((,class (:inherit match))))
     `(helm-grep-running ((,class (:foreground ,red))))
     `(helm-header ((,class (:inherit header-line))))
     `(helm-header-line-left-margin ((,class (:inherit header-line))))
     `(helm-lisp-completion-info ((,class (:foreground ,base0))))
     `(helm-lisp-show-completion ((,class (:foreground ,yellow  :background ,base02
                                                       :bold t))))
     `(helm-M-x-key ((,class (:foreground ,orange :underline t))))
     `(helm-moccur-buffer ((,class (:foreground ,cyan :underline t))))
     `(helm-match ((,class (:inherit match))))
     `(helm-selection ((,class (:background ,base02 :underline t))))
     `(helm-selection-line ((,class (:background ,base02 :foreground ,base1
                                                 :underline nil))))
     `(helm-separator ((,class (:foreground ,red))))
     `(helm-source-header ((,class (:background ,blue-lc :foreground ,base03
                                                :underline nil))))
     `(helm-time-zone-current ((,class (:foreground ,green))))
     `(helm-time-zone-home ((,class (:foreground ,red))))
     `(helm-visible-mark ((,class (:background ,base03 :foreground ,magenta :bold t))))
;;;;; helm-css-scss
     `(helm-css-scss-selector-depth-face-1 ((,class (:foreground ,base0))))
     `(helm-css-scss-selector-depth-face-2 ((,class (:foreground ,blue))))
     `(helm-css-scss-selector-depth-face-3 ((,class (:foreground ,cyan))))
     `(helm-css-scss-selector-depth-face-4 ((,class (:foreground ,green))))
     `(helm-css-scss-selector-depth-face-5 ((,class (:foreground ,yellow))))
     `(helm-css-scss-selector-depth-face-6 ((,class (:foreground ,violet))))
     `(helm-css-scss-target-line-face ((,class (:background unspecified :foreground ,magenta))))
;;;;; helm-go-package
     `(helm-source-go-package-godoc-description ((,class (:foreground ,base01))))
;;;;; helm-swoop
     `(helm-swoop-target-line-face ((,class (:foreground unspecified :background ,base02))))
     `(helm-swoop-target-line-block-face ((,class (:foreground unspecified :background ,base02))))
     `(helm-swoop-target-word-face ((,class (:foreground ,magenta :background unspecified))))
;;;;; hi-lock-mode
     `(hi-yellow ((,class (:foreground ,(solarized-color-blend yellow base1 0.5)
                                       :background,(solarized-color-blend yellow base03 0.15)))))
     `(hi-pink ((,class (:foreground ,(solarized-color-blend magenta base1 0.5)
                                       :background,(solarized-color-blend magenta base03 0.15)))))
     `(hi-green ((,class (:foreground ,(solarized-color-blend green base1 0.5)
                                       :background,(solarized-color-blend green base03 0.15)))))
     `(hi-blue ((,class (:foreground ,(solarized-color-blend blue base1 0.5)
                                       :background,(solarized-color-blend blue base03 0.15)))))
     `(hi-black-b ((,class (:foreground ,base1
                                        :background ,base03
                                        :weight bold))))
     `(hi-blue-b ((,class (:weight bold
                                   :foreground ,(solarized-color-blend cyan base1 0.7)
                                   :background ,(solarized-color-blend cyan base03 0.2)))))
     `(hi-green-b ((,class (:weight bold
                           :foreground ,(solarized-color-blend green base1 0.7)
                           :background ,(solarized-color-blend green base03 0.2)))))
     `(hi-red-b ((,class (:weight bold
                                  :foreground ,(solarized-color-blend red base1 0.7)
                                  :background ,(solarized-color-blend red base03 0.2)))))
     `(hi-black-hb ((,class (:weight bold
                                     :foreground ,base1
                                     :background ,base02))))
;;;;; highlight-changes
     `(highlight-changes ((,class (:foreground ,orange))))
     `(highlight-changes-delete ((,class (:foreground ,red :underline t))))
;;;;; highlight-indentation
     `(highlight-indentation-face ((,class (:background ,base02))))
     `(highlight-indentation-current-column-face((,class (:background ,base02))))
;;;;; highlight-numbers
     `(highlight-numbers-number ((,class (:foreground ,violet :bold nil))))
;;;;; highlight-symbol
     `(highlight-symbol-face ((,class (:foreground ,magenta))))
;;;;; hl-line-mode
     `(hl-line ((,class (:background ,base02))))
     `(hl-line-face ((,class (:background ,base02))))
;;;;; hydra
     `(hydra-face-red ((,class (:foreground ,red))))
     `(hydra-face-blue ((,class (:foreground ,blue))))
     `(hydra-face-amaranth ((,class (:foreground ,orange))))
     `(hydra-face-pink ((,class (:foreground ,magenta))))
     `(hydra-face-teal ((,class (:foreground ,cyan))))
;;;;; ido-mode
     `(ido-first-match ((,class (:foreground ,yellow :weight normal))))
     `(ido-only-match ((,class (:foreground ,base03 :background ,yellow :weight normal))))
     `(ido-subdir ((,class (:foreground ,blue))))
     `(ido-incomplete-regexp ((,class (:foreground ,red :weight bold ))))
     `(ido-indicator ((,class (:background ,red :foreground ,base03 :width condensed))))
     `(ido-virtual ((,class (:foreground ,cyan))))
;;;;; iedit-mode
     `(iedit-occurrence ((,class (:background ,base03 :foreground ,magenta :bold t))))
;;;;; info
     `(info-title-1 ((,class (:foreground ,base1 :weight bold))))
     `(info-title-2 ((,class (:foreground ,base1 :weight bold))))
     `(info-title-3 ((,class (:weight bold))))
     `(info-title-4 ((,class (:weight bold))))
     `(info-node ((,class (:foreground ,base1 :slant italic :weight bold))))
     `(info-header-node ((,class (:inherit info-node))))
     `(info-header-xref ((,class (:inherit info-xref))))
     `(info-index-match ((,class (:inherit match))))
     `(info-menu-header ((,class (:inherit variable-pitch :weight bold))))
     `(info-menu-star ((,class (:foreground ,orange))))
     `(info-xref ((,class (:inherit link))))
     `(info-xref-visited ((,class (:inherit (link-visited info-xref)))))
;;;;; info+
     `(info-file
       ((,class (:foreground ,yellow :background ,base02))))
     `(info-menu
       ((,class (:foreground ,violet :background ,base02))))
     `(info-single-quote
       ((,class (:foreground ,cyan :inherit font-lock-string-face))))
     `(info-quoted-name
       ((,class (:foreground ,orange :inherit font-lock-string-face))))
     `(info-string
       ((,class (:foreground ,blue :inherit font-lock-string-face))))
     `(info-command-ref-item
       ((,class (:foreground ,green :background ,base02))))
     `(info-constant-ref-item
       ((,class (:foreground ,red :background ,base02))))
     `(info-function-ref-item
       ((,class (:foreground ,cyan :background ,base02))))
     `(info-macro-ref-item
       ((,class (:foreground ,green :background ,base02))))
     `(info-reference-item
       ((,class (:background ,base02))))
     `(info-special-form-ref-item
       ((,class (:foreground ,magenta :background ,base02))))
     `(info-syntax-class-item
       ((,class (:foreground ,magenta :background ,base02))))
     `(info-user-option-ref-item
       ((,class (:foreground ,orange :background ,base02))))
;;;;; ivy
     `(ivy-confirm-face ((,class (:foreground ,green))))
     `(ivy-current-match ((,class (:weight bold :background ,base02))))
     `(ivy-match-required-face ((,class (:foreground ,red))))
     `(ivy-minibuffer-match-face-1 ((,class (:foreground ,base1))))
     `(ivy-minibuffer-match-face-2 ((,class (:foreground ,yellow))))
     `(ivy-minibuffer-match-face-3 ((,class (:foreground ,yellow))))
     `(ivy-minibuffer-match-face-4 ((,class (:foreground ,yellow))))
     `(ivy-remote ((,class (:foreground ,blue))))
;;;;; jabber
     `(jabber-activity-face ((,class (:weight bold :foreground ,red))))
     `(jabber-activity-personal-face ((,class (:weight bold :foreground ,blue))))
     `(jabber-chat-error ((,class (:weight bold :foreground ,red))))
     `(jabber-chat-prompt-foreign ((,class (:weight bold :foreground ,red))))
     `(jabber-chat-prompt-local ((,class (:weight bold :foreground ,blue))))
     `(jabber-chat-prompt-system ((,class (:weight bold :foreground ,green))))
     `(jabber-chat-text-foreign ((,class (:foreground ,base1))))
     `(jabber-chat-text-local ((,class (:foreground ,base0))))
     `(jabber-chat-rare-time-face ((,class (:underline t :foreground ,green))))
     `(jabber-roster-user-away ((,class (:slant italic :foreground ,green))))
     `(jabber-roster-user-chatty ((,class (:weight bold :foreground ,orange))))
     `(jabber-roster-user-dnd ((,class (:slant italic :foreground ,red))))
     `(jabber-roster-user-error ((,class (:weight light :slant italic :foreground ,red))))
     `(jabber-roster-user-offline ((,class (:foreground ,base01))))
     `(jabber-roster-user-online ((,class (:weight bold :foreground ,blue))))
     `(jabber-roster-user-xa ((,class (:slant italic :foreground ,magenta))))
;;;;; js2-mode colors
     `(js2-error ((,class (:foreground ,red))))
     `(js2-external-variable ((,class (:foreground ,orange))))
     `(js2-function-param ((,class (:foreground ,green))))
     `(js2-instance-member ((,class (:foreground ,magenta))))
     `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,cyan))))
     `(js2-jsdoc-html-tag-name ((,class (:foreground ,orange))))
     `(js2-jsdoc-tag ((,class (:foreground ,cyan))))
     `(js2-jsdoc-type ((,class (:foreground ,blue))))
     `(js2-jsdoc-value ((,class (:foreground ,violet))))
     `(js2-magic-paren ((,class (:underline t))))
     `(js2-private-function-call ((,class (:foreground ,yellow))))
     `(js2-private-member ((,class (:foreground ,blue))))
     `(js2-warning ((,class (:underline ,orange))))
;;;;; jedi
     `(jedi:highlight-function-argument ((,class (:inherit bold))))
;;;;; kite
     ;; Sadly kite is not very stable for me so these faces might miss out things.
     `(bg:kite-dataReceived ((,class (:background ,magenta))))
     `(bg:kite-receiveHeadersEnd ((,class (:background ,green))))
     `(bg:kite-requestStart ((,class (:background ,red))))
     `(bg:kite-sendEnd ((,class (:background ,cyan))))
     `(bg:kite-table-head ((,class (:background ,base02))))
     `(bg:kite-tick ((,class (:background ,base02))))
     `(kite-css-computed-proprietary-unused-property ((,class (:inherit kite-css-proprietary-property :foreground ,blue))))
     `(kite-css-computed-unused-property ((,class (:inherit kite-css-property :foreground ,blue))))
     `(kite-css-value-widget-error ((,class (:background ,orange-lc :foreground ,orange-hc))))
     `(kite-css-value-widget-modified ((,class (:background ,base02 :foreground ,yellow))))
     `(kite-delimited-data-face ((,class (:foreground ,green))))
     `(kite-delimiter-face ((,class (:foreground ,base1))))
     `(kite-modified-attribute-local-name-face ((,class (:inherit kite-attribute-local-name-face :background ,base02))))
     `(kite-modified-attribute-value-face ((,class (:inherit kite-attribute-value-face :background ,base02))))
     `(kite-modified-element-local-name-face ((,class (:inherit kite-element-local-name-face :background ,base02))))
     `(kite-name-face ((,class (:foreground ,blue))))
     `(kite-proto-property-name ((,class (:inherit default :foreground ,base02))))
     `(kite-ref-face ((,class (:foreground ,cyan))))
     `(kite-session-closed ((,class (:inherit default :background ,red))))
     `(kite-text-face ((,class (:background nil :foreground ,base01))))
     `(kite-node-highlight-face ((,class (:background ,base02))))
     `(bg:kite-pageStart ((,class nil)))
     `(kite-attribute-colon-face ((,class (:inherit kite-name-face))))
     `(kite-attribute-local-name-face ((,class (:inherit kite-name-face))))
     `(kite-attribute-prefix-face ((,class (:inherit kite-name-face))))
     `(kite-attribute-value-delimiter-face ((,class (:inherit kite-delimiter-face))))
     `(kite-attribute-value-face ((,class (:inherit kite-delimited-data-face))))
     `(kite-boolean ((,class (:inherit nxml-char-ref-number))))
     `(kite-cdata-section-CDATA-face ((,class (:inherit kite-name-face))))
     `(kite-cdata-section-content-face ((,class (:inherit kite-text-face))))
     `(kite-cdata-section-delimiter-face ((,class (:inherit kite-delimiter-face))))
     `(kite-char-ref-delimiter-face ((,class (:inherit kite-ref-face))))
     `(kite-char-ref-number-face ((,class (:inherit kite-ref-face))))
     `(kite-comment-content-face ((,class (:slant italic))))
     `(kite-comment-delimiter-face ((,class (:inherit kite-delimiter-face))))
     `(kite-console-prompt-face ((,class (:inherit default))))
     `(kite-css-property ((,class (:inherit css-property))))
     `(kite-css-proprietary-property ((,class (:inherit css-proprietary-property))))
     `(kite-css-selected-overlay ((,class (:inherit secondary-selection))))
     `(kite-css-selector ((,class (:inherit css-selector))))
     `(kite-element-colon-face ((,class (:inherit kite-name-face))))
     `(kite-element-local-name-face ((,class (:inherit kite-name-face))))
     `(kite-element-prefix-face ((,class (:inherit kite-name-face))))
     `(kite-entity-ref-delimiter-face ((,class (:inherit kite-ref-face))))
     `(kite-entity-ref-name-face ((,class (:inherit kite-ref-face))))
     `(kite-hash-face ((,class (:inherit kite-name-face))))
     `(kite-link-face ((,class (:inherit change-log-file))))
     `(kite-loading ((,class (:inherit font-lock-comment))))
     `(kite-log-debug ((,class (:inherit font-lock-comment))))
     `(kite-log-error ((,class (:inherit error))))
     `(kite-log-log ((,class (:inherit default))))
     `(kite-log-warning ((,class (:inherit warning))))
     `(kite-markup-declaration-delimiter-face ((,class (:inherit kite-delimiter-face))))
     `(kite-namespace-attribute-colon-face ((,class (:inherit kite-name-face))))
     `(kite-namespace-attribute-prefix-face ((,class (:inherit kite-name-face))))
     `(kite-namespace-attribute-value-delimiter-face ((,class (:inherit kite-attribute-value-delimiter-face))))
     `(kite-namespace-attribute-value-face ((,class (:inherit kite-attribute-value-face))))
     `(kite-namespace-attribute-xmlns-face ((,class (:inherit kite-name-face))))
     `(kite-null ((,class (:inherit nxml-char-ref-number))))
     `(kite-number ((,class (:inherit nxml-char-ref-number))))
     `(kite-object ((,class (:inherit font-lock-variable-name))))
     `(kite-processing-instruction-content-face ((,class (:inherit kite-delimited-data-face))))
     `(kite-processing-instruction-delimiter-face ((,class (:inherit kite-delimiter-face))))
     `(kite-processing-instruction-target-face ((,class (:inherit kite-name-face))))
     `(kite-prolog-keyword-face ((,class (:inherit kite-name-face))))
     `(kite-prolog-literal-content-face ((,class (:inherit kite-delimited-data-face))))
     `(kite-prolog-literal-delimiter-face ((,class (:inherit kite-delimiter-face))))
     `(kite-property-name ((,class (:inherit default))))
     `(kite-quote ((,class (:inherit font-lock-keyword))))
     `(kite-stack-column-number ((,class (:inherit kite-number))))
     `(kite-stack-error-message ((,class (:inherit default))))
     `(kite-stack-error-type ((,class (:inherit error))))
     `(kite-stack-file-name ((,class (:inherit link))))
     `(kite-stack-function-name ((,class (:inherit font-lock-function-name-face))))
     `(kite-stack-line-number ((,class (:inherit kite-number))))
     `(kite-stack-pseudo-file-name ((,class (:inherit default))))
     `(kite-string ((,class (:inherit font-lock-string))))
     `(kite-table-head ((,class (:inherit highlight))))
     `(kite-tag-delimiter-face ((,class (:inherit kite-delimiter-face))))
     `(kite-tag-slash-face ((,class (:inherit kite-name-face))))
     `(kite-undefined ((,class (:inherit nxml-char-ref-number))))
;;;;; ledger-mode
     `(ledger-font-payee-uncleared-face ((t (:foreground ,red))))
     `(ledger-font-payee-cleared-face ((t (:foreground ,green :weight normal))))
     `(ledger-font-xact-highlight-face ((t (:background ,base02))))
     `(ledger-font-pending-face ((t (:foreground ,yellow weight: normal))))
     `(ledger-font-other-face ((t (:foreground ,base0))))
     `(ledger-font-posting-account-face ((t (:foreground ,cyan))))
     `(ledger-font-posting-account-cleared-face ((t (:foreground ,base0))))
     `(ledger-font-posting-account-pending-face ((t (:foreground ,yellow))))
     `(ledger-font-posting-amount-face ((t (:foreground ,yellow))))
     `(ledger-occur-narrowed-face ((t (:foreground ,base3 :invisible t))))
     `(ledger-occur-xact-face ((t (:background ,base02))))
     `(ledger-font-comment-face ((t (:foreground ,base01))))
     `(ledger-font-reconciler-uncleared-face ((t (:foreground ,red :weight bold))))
     `(ledger-font-reconciler-cleared-face ((t (:foreground ,base0 :weight normal))))
     `(ledger-font-reconciler-pending-face ((t (:foreground ,yellow :weight normal))))
     `(ledger-font-report-clickable-face ((t (:foreground ,yellow :weight normal))))
;;;;; linum-mode
     `(linum ((,class (:foreground ,s-fringe-fg :background ,s-fringe-bg))))
;;;;; lusty-explorer
     `(lusty-directory-face ((,class (:inherit dired-directory))))
     `(lusty-file-face ((,class nil)))
     `(lusty-match-face ((,class (:inherit ido-first-match))))
     `(lusty-slash-face ((,class (:foreground ,cyan :weight bold))))
;;;;; magit
;;;;;; headings and diffs
     `(magit-section-highlight           ((t (:background ,base02))))
     `(magit-section-heading             ((t (:foreground ,yellow :weight bold))))
     `(magit-section-heading-selection   ((t (:foreground ,orange :weight bold))))
     `(magit-diff-file-heading           ((t (:weight bold))))
     `(magit-diff-file-heading-highlight ((t (:background ,base02 :weight bold))))
     `(magit-diff-file-heading-selection ((t (:background ,base02
                                              :foreground ,orange :weight bold))))
     `(magit-diff-hunk-heading
       ((t (:background ,(solarized-color-blend yellow base03 0.1)))))
     `(magit-diff-hunk-heading-highlight
       ((t (:background ,(solarized-color-blend yellow base02 0.1)))))
     `(magit-diff-hunk-heading-selection
       ((t (:background ,(solarized-color-blend yellow base02 0.1)
            :foreground ,orange
            :weight bold))))
     `(magit-diff-lines-heading          ((t (:background ,orange
                                              :foreground ,base3))))
     `(magit-diff-context-highlight      ((t (:background ,base02))))
     `(magit-diffstat-added              ((t (:foreground ,green))))
     `(magit-diffstat-removed            ((t (:foreground ,red))))
;;;;;; popup
     `(magit-popup-heading             ((t (:foreground ,base1 :weight normal))))
     `(magit-popup-key                 ((t (:foreground ,base1 :weight bold))))
     `(magit-popup-argument            ((t (:foreground ,base1 :weight bold))))
     `(magit-popup-disabled-argument   ((t (:foreground ,base01 :weight normal))))
     `(magit-popup-option-value        ((t (:foreground ,base1 :weight bold))))
;;;;;; process
     `(magit-process-ok    ((t (:foreground ,green :weight bold))))
     `(magit-process-ng    ((t (:foreground ,red   :weight bold))))
;;;;;; log
     `(magit-log-author    ((t (:foreground ,base01 :weight bold))))
     `(magit-log-date      ((t (:foreground ,base01))))
     `(magit-log-graph     ((t (:foreground ,base1))))
;;;;;; sequence
     `(magit-sequence-pick ((t (:foreground ,yellow-d))))
     `(magit-sequence-stop ((t (:foreground ,green))))
     `(magit-sequence-part ((t (:foreground ,yellow))))
     `(magit-sequence-head ((t (:foreground ,blue))))
     `(magit-sequence-drop ((t (:foreground ,red))))
     `(magit-sequence-done ((t (:foreground ,base01))))
     `(magit-sequence-onto ((t (:foreground ,base01))))
;;;;;; bisect
     `(magit-bisect-good ((t (:foreground ,green))))
     `(magit-bisect-skip ((t (:foreground ,yellow))))
     `(magit-bisect-bad  ((t (:foreground ,red))))
;;;;;; blame
     `(magit-blame-heading ((t (:background ,base02 :foreground ,violet
                                            :weight bold :slant normal :box (:color ,base02 :line-width 2)))))
     `(magit-blame-hash    ((t (:background ,base02 :foreground ,violet
                                            :weight normal :slant normal :box (:color ,base02 :line-width 2)))))
     `(magit-blame-name    ((t (:background ,base02 :foreground ,violet
                                            :weight normal :slant normal :box (:color ,base02 :line-width 2)))))
     `(magit-blame-date    ((t (:background ,base02 :foreground ,violet
                                            :weight bold :slant normal :box (:color ,base02 :line-width 2)))))
     `(magit-blame-summary ((t (:background ,base02 :foreground ,base0
                                            :weight bold :slant normal :box (:color ,base02 :line-width 2)))))
;;;;;; references etc.
     `(magit-dimmed         ((t (:foreground ,base01))))
     `(magit-hash           ((t (:foreground ,base01))))
     `(magit-tag            ((t (:foreground ,cyan :weight bold))))
     `(magit-branch-remote  ((t (:foreground ,green  :weight bold))))
     `(magit-branch-local   ((t (:foreground ,blue   :weight bold))))
     `(magit-branch-current ((t (:foreground ,blue   :weight bold :box t))))
     `(magit-head           ((t (:foreground ,blue   :weight bold))))
     `(magit-refname        ((t (:background ,base02 :foreground ,base01 :weight bold))))
     `(magit-refname-stash  ((t (:background ,base02 :foreground ,base01 :weight bold))))
     `(magit-refname-wip    ((t (:background ,base02 :foreground ,base01 :weight bold))))
     `(magit-signature-good      ((t (:foreground ,green))))
     `(magit-signature-bad       ((t (:foreground ,red))))
     `(magit-signature-untrusted ((t (:foreground ,yellow))))
     `(magit-cherry-unmatched    ((t (:foreground ,cyan))))
     `(magit-cherry-equivalent   ((t (:foreground ,magenta))))
     `(magit-reflog-commit       ((t (:foreground ,green))))
     `(magit-reflog-amend        ((t (:foreground ,magenta))))
     `(magit-reflog-merge        ((t (:foreground ,green))))
     `(magit-reflog-checkout     ((t (:foreground ,blue))))
     `(magit-reflog-reset        ((t (:foreground ,red))))
     `(magit-reflog-rebase       ((t (:foreground ,magenta))))
     `(magit-reflog-cherry-pick  ((t (:foreground ,green))))
     `(magit-reflog-remote       ((t (:foreground ,cyan))))
     `(magit-reflog-other        ((t (:foreground ,cyan))))
;;;;; markdown-mode
     `(markdown-blockquote-face ((,class (:inherit font-lock-doc-face))))
     `(markdown-bold-face ((,class (:inherit bold))))
     `(markdown-comment-face ((,class (:foreground ,base01 :strike-through t))))
     `(markdown-footnote-face ((,class (:inherit default))))
     `(markdown-header-delimiter-face ((,class (:foreground ,base01))))
     `(markdown-header-face ((,class (:foreground ,blue))))
     `(markdown-header-face-1 ((,class (:inherit markdown-header-face))))
     `(markdown-header-face-2 ((,class (:inherit markdown-header-face))))
     `(markdown-header-face-3 ((,class (:inherit markdown-header-face))))
     `(markdown-header-face-4 ((,class (:inherit markdown-header-face))))
     `(markdown-header-face-5 ((,class (:inherit markdown-header-face))))
     `(markdown-header-face-6 ((,class (:inherit markdown-header-face))))
     `(markdown-header-rule-face ((,class (:foreground ,base01))))
     `(markdown-inline-code-face ((,class (:foreground ,base01))))
     `(markdown-italic-face ((,class (:inherit italic))))
     `(markdown-language-keyword-face ((,class (:inherit default))))
     `(markdown-line-break-face ((,class (:inherit default :underline t))))
     `(markdown-link-face ((,class (:inherit default :foreground ,yellow))))
     `(markdown-link-title-face ((,class (:inherit font-lock-comment-face))))
     `(markdown-list-face ((,class (:inherit font-lock-builtin-face))))
     `(markdown-math-face ((,class (:inherit font-lock-string-face))))
     `(markdown-metadata-key-face ((,class (:inherit font-lock-comment-face))))
     `(markdown-metadata-value-face ((,class (:inherit default))))
     `(markdown-missing-link-face ((,class (:inherit font-lock-warning-face))))
     `(markdown-pre-face ((,class (:foreground ,base01))))
     `(markdown-reference-face ((,class (:inherit default :foreground ,base01))))
     `(markdown-url-face ((,class (:foreground ,base01))))
;;;;; multiple-cursors
     `(mc/cursor-face ((,class (:inherit cursor :inverse-video nil))))
;;;;; message-mode
     `(message-cited-text ((,class (:foreground ,base01))))
     `(message-header-name ((,class (:foreground ,base01))))
     `(message-header-other ((,class (:foreground ,base0 :weight normal))))
     `(message-header-to ((,class (:foreground ,base0 :weight normal))))
     `(message-header-cc ((,class (:foreground ,base0 :weight normal))))
     `(message-header-newsgroups ((,class (:foreground ,yellow :weight bold))))
     `(message-header-subject ((,class (:foreground ,cyan :weight normal))))
     `(message-header-xheader ((,class (:foreground ,cyan))))
     `(message-mml ((,class (:foreground ,yellow :weight bold))))
     `(message-separator ((,class (:foreground ,base01 :slant italic))))
;;;;; mew
     `(mew-face-header-subject ((,class (:foreground ,orange))))
     `(mew-face-header-from ((,class (:foreground ,yellow))))
     `(mew-face-header-date ((,class (:foreground ,green))))
     `(mew-face-header-to ((,class (:foreground ,red))))
     `(mew-face-header-key ((,class (:foreground ,green))))
     `(mew-face-header-private ((,class (:foreground ,green))))
     `(mew-face-header-important ((,class (:foreground ,blue))))
     `(mew-face-header-marginal ((,class (:foreground ,base0 :weight bold))))
     `(mew-face-header-warning ((,class (:foreground ,red))))
     `(mew-face-header-xmew ((,class (:foreground ,green))))
     `(mew-face-header-xmew-bad ((,class (:foreground ,red))))
     `(mew-face-body-url ((,class (:foreground ,orange))))
     `(mew-face-body-comment ((,class (:foreground ,base0 :slant italic))))
     `(mew-face-body-cite1 ((,class (:foreground ,green))))
     `(mew-face-body-cite2 ((,class (:foreground ,blue))))
     `(mew-face-body-cite3 ((,class (:foreground ,orange))))
     `(mew-face-body-cite4 ((,class (:foreground ,yellow))))
     `(mew-face-body-cite5 ((,class (:foreground ,red))))
     `(mew-face-mark-review ((,class (:foreground ,blue))))
     `(mew-face-mark-escape ((,class (:foreground ,green))))
     `(mew-face-mark-delete ((,class (:foreground ,red))))
     `(mew-face-mark-unlink ((,class (:foreground ,yellow))))
     `(mew-face-mark-refile ((,class (:foreground ,green))))
     `(mew-face-mark-unread ((,class (:foreground ,red))))
     `(mew-face-eof-message ((,class (:foreground ,green))))
     `(mew-face-eof-part ((,class (:foreground ,yellow))))
;;;;; mic-paren
     `(paren-face-match
       ((,class (:foreground ,magenta :background unspecified
                             :weight ,s-maybe-bold))))
     `(paren-face-mismatch
       ((,class (:foreground ,base02 :background ,red
                             :weight ,s-maybe-bold))))
     `(paren-face-no-match
       ((,class (:foreground ,base02 :background ,red
                             :weight ,s-maybe-bold))))
;;;;; mingus
     `(mingus-directory-face ((,class (:foreground ,blue))))
     `(mingus-pausing-face ((,class (:foreground ,magenta))))
     `(mingus-playing-face ((,class (:foreground ,cyan))))
     `(mingus-playlist-face ((,class (:foreground ,cyan ))))
     `(mingus-song-file-face ((,class (:foreground ,yellow))))
     `(mingus-stopped-face ((,class (:foreground ,red))))
;;;;; moccur
     `(moccur-current-line-face ((,class (:underline t))))
     `(moccur-edit-done-face ((,class
                               (:foreground ,base01
                                            :background ,base03
                                            :slant italic))))
     `(moccur-edit-face
       ((,class (:background ,yellow :foreground ,base03))))
     `(moccur-edit-file-face ((,class (:background ,base02))))
     `(moccur-edit-reject-face ((,class (:foreground ,red))))
     `(moccur-face ((,class (:background ,base02 :foreground ,base1
                                         :weight bold))))
     `(search-buffers-face ((,class (:background ,base02 :foreground ,base1
                                                 :weight bold))))
     `(search-buffers-header-face ((,class (:background ,base02 :foreground ,yellow
                                                        :weight bold))))
;;;;; mu4e
     `(mu4e-cited-1-face ((,class (:foreground ,green :slant italic :weight normal))))
     `(mu4e-cited-2-face ((,class (:foreground ,blue :slant italic :weight normal))))
     `(mu4e-cited-3-face ((,class (:foreground ,orange :slant italic :weight normal))))
     `(mu4e-cited-4-face ((,class (:foreground ,yellow :slant italic :weight normal))))
     `(mu4e-cited-5-face ((,class (:foreground ,cyan :slant italic :weight normal))))
     `(mu4e-cited-6-face ((,class (:foreground ,green :slant italic :weight normal))))
     `(mu4e-cited-7-face ((,class (:foreground ,blue :slant italic :weight normal))))
     `(mu4e-flagged-face ((,class (:foreground ,blue :weight normal))))
     `(mu4e-unread-face ((,class (:foreground ,green :weight normal))))
     `(mu4e-view-url-number-face ((,class (:foreground ,yellow :weight normal))))
     `(mu4e-warning-face ((,class (:foreground ,red :slant normal :weight bold))))
     `(mu4e-header-highlight-face
       ((,class (:inherit unspecified :foreground unspecified :background ,base02
                          :underline unspecified  :weight unspecified))))
     `(mu4e-view-contact-face ((,class (:foreground ,base0  :weight normal))))
     `(mu4e-view-header-key-face ((,class (:inherit message-header-name :weight normal))))
     `(mu4e-view-header-value-face ((,class (:foreground ,cyan :weight normal :slant normal))))
     `(mu4e-view-link-face ((,class (:inherit link))))
     `(mu4e-view-special-header-value-face ((,class (:foreground ,blue :weight normal :underline nil))))
;;;;; mumamo
     `(mumamo-background-chunk-submode1 ((,class (:background ,base02))))
;;;;; nav
     `(nav-face-heading ((,class (:foreground ,yellow))))
     `(nav-face-button-num ((,class (:foreground ,cyan))))
     `(nav-face-dir ((,class (:foreground ,green))))
     `(nav-face-hdir ((,class (:foreground ,red))))
     `(nav-face-file ((,class (:foreground ,base0))))
     `(nav-face-hfile ((,class (:foreground ,red))))
;;;;; nav-flash
     ;; `(nav-flash-face ((,class (:background ,base02))))
     `(nav-flash-face ((,class (:foreground
                                ,(apply 'solarized-color-blend
                                        (if
                                            (eq variant 'light)
                                            (list yellow base1 0.2)
                                          (list cyan base1 0.1)))
                                :background
                                ,(apply 'solarized-color-blend
                                        (if
                                            (eq variant 'light)
                                            (list yellow base03 0.2)
                                          (list cyan base03 0.3)))))))
;;;;; navi2ch
     `(navi2ch-list-category-face ((,class (:foreground ,blue ))))
     `(navi2ch-list-add-board-name-face ((,class (:foreground ,yellow))))
     `(navi2ch-list-board-name-face ((,class (:foreground ,blue))))
     `(navi2ch-list-change-board-name-face ((,class (:foreground ,green :weight bold))))
     `(navi2ch-bm-unread-face ((,class (:foreground ,green))))
     `(navi2ch-bm-view-face ((,class (:foreground ,yellow))))
     `(navi2ch-bm-cache-face ((,class (:foreground ,blue))))
     `(navi2ch-bm-update-face ((,class (:foreground ,orange))))
     `(navi2ch-bm-down-face ((,class (:foreground ,base1))))
     `(navi2ch-bm-mark-face ((,class (:foreground ,red))))
     `(navi2ch-bm-new-unread-face ((,class (:foreground ,green))))
     `(navi2ch-bm-new-view-face ((,class (:foreground ,yellow))))
     `(navi2ch-bm-new-cache-face ((,class (:foreground ,blue))))
     `(navi2ch-bm-new-update-face ((,class (:foreground ,orange))))
     `(navi2ch-bm-new-mark-face ((,class (:foreground ,red))))
     `(navi2ch-bm-updated-unread-face ((,class (:foreground ,green))))
     `(navi2ch-bm-updated-view-face ((,class (:foreground ,yellow))))
     `(navi2ch-bm-updated-cache-face ((,class (:foreground ,blue))))
     `(navi2ch-bm-updated-update-face ((,class (:foreground ,orange))))
     `(navi2ch-bm-updated-navi2ch-bm-updated-mark-facemark-face ((,class (:foreground ,red))))
     `(navi2ch-bm-seen-unread-face ((,class (:foreground ,green))))
     `(navi2ch-bm-seen-view-face ((,class (:foreground ,yellow))))
     `(navi2ch-bm-seen-cache-face ((,class (:foreground ,blue))))
     `(navi2ch-bm-seen-update-face ((,class (:foreground ,orange))))
     `(navi2ch-bm-seen-mark-face ((,class (:foreground ,red))))
     `(navi2ch-article-header-face ((,class (:foreground ,base1))))
     `(navi2ch-article-header-contents-face ((,class (:foreground ,blue))))
     `(navi2ch-article-header-fusianasan-face ((,class (:foreground ,blue :underline t))))
     `(navi2ch-article-link-face ((,class (:weight bold))))
     `(navi2ch-article-url-face ((,class (:weight bold))))
     `(navi2ch-article-citation-face ((,class (:foreground ,yellow))))
     `(navi2ch-article-auto-decode-face ((,class (:foreground ,base03))))
     `(navi2ch-article-message-separator-face ((,class (:foreground ,green))))
     `(navi2ch-splash-screen-face ((,class (:foreground ,cyan))))
     `(navi2ch-message-link-face ((,class (:weight bold))))
     `(navi2ch-message-url-face ((,class (:weight bold))))
     `(navi2ch-message-citation-face ((,class (:foreground ,magenta))))
;;;;; neotree
     `(neo-banner-face ((,class (:foreground ,base01))))
     `(neo-header-face ((,class (:foreground ,blue))))
     `(neo-root-dir-face ((,class (:foreground ,base1 :weight bold))))
     `(neo-dir-link-face ((,class (:foreground ,blue))))
     `(neo-file-link-face ((,class (:foreground ,base0))))
     `(neo-expand-btn-face ((,class (:foreground ,base01))))
;;;;; org-mode
     `(org-agenda-structure
       ((,class (:foreground ,base1 :background ,base02
                             :weight bold :slant normal :inverse-video nil :height ,solarized-height-plus-1
                             :underline nil
                             :box (:line-width 2 :color ,base03)))))
     `(org-agenda-calendar-event ((,class (:foreground ,base1))))
     `(org-agenda-calendar-sexp ((,class (:foreground ,base0 :slant italic))))
     `(org-agenda-date
       ((,class (:foreground ,base01 :background ,base03 :weight normal
                             :box (:line-width 2 :color ,base03)
                             :inverse-video nil :overline nil :slant normal :height 1.0))))
     `(org-agenda-date-weekend
       ((,class (:inherit org-agenda-date :inverse-video nil :background unspecified
                          :foreground ,base01 :weight unspecified
                          :underline t :overline nil :box unspecified))))
     `(org-agenda-date-today
       ((,class (:inherit org-agenda-date :inverse-video t :weight bold
                          :underline unspecified :overline nil :box unspecified
                          :foreground ,blue :background ,base03))))
     `(org-agenda-done ((,class (:foreground ,base01 :slant italic))))
     `(org-archived ((,class (:foreground ,base01 :weight normal))))
     `(org-block ((,class (:foreground ,base01))))
     `(org-block-begin-line ((,class (:foreground ,base01 :slant italic))))
     `(org-checkbox ((,class (:background ,base03 :foreground ,base0
                                          :box (:line-width 1 :style released-button)))))
     `(org-code ((,class (:foreground ,base01))))
     `(org-date ((,class (:foreground ,blue :underline t))))
     `(org-done ((,class (:weight bold :foreground ,green))))
     `(org-ellipsis ((,class (:foreground ,base01))))
     `(org-formula ((,class (:foreground ,yellow))))
     `(org-headline-done ((,class (:foreground ,green))))
     `(org-hide ((,class (:foreground ,base03))))
     `(org-level-1 ((,class (:inherit ,s-variable-pitch :foreground ,orange
                             ,@(when solarized-scale-org-headlines
                                 (list :height solarized-height-plus-4))))))
     `(org-level-2 ((,class (:inherit ,s-variable-pitch :foreground ,green
                             ,@(when solarized-scale-org-headlines
                                 (list :height solarized-height-plus-3))))))
     `(org-level-3 ((,class (:inherit ,s-variable-pitch :foreground ,blue
                             ,@(when solarized-scale-org-headlines
                                 (list :height solarized-height-plus-2))))))
     `(org-level-4 ((,class (:inherit ,s-variable-pitch :foreground ,yellow
                             ,@(when solarized-scale-org-headlines
                                 (list :height solarized-height-plus-1))))))
     `(org-level-5 ((,class (:inherit ,s-variable-pitch
                                      :foreground ,cyan))))
     `(org-level-6 ((,class (:inherit ,s-variable-pitch
                                      :foreground ,green))))
     `(org-level-7 ((,class (:inherit ,s-variable-pitch
                                      :foreground ,red))))
     `(org-level-8 ((,class (:inherit ,s-variable-pitch
                                      :foreground ,blue))))
     `(org-link ((,class (:foreground ,yellow :underline t))))
     `(org-sexp-date ((,class (:foreground ,violet))))
     `(org-scheduled ((,class (:foreground ,green))))
     `(org-scheduled-previously ((,class (:foreground ,cyan))))
     `(org-scheduled-today ((,class (:foreground ,blue :weight normal))))
     `(org-special-keyword ((,class (:foreground ,base01 :weight bold))))
     `(org-table ((,class (:foreground ,green))))
     `(org-tag ((,class (:weight bold))))
     `(org-time-grid ((,class (:foreground ,base01))))
     `(org-todo ((,class (:foreground ,cyan :weight bold))))
     `(org-upcoming-deadline ((,class (:foreground ,yellow  :weight normal :underline nil))))
     `(org-warning ((,class (:foreground ,orange :weight normal :underline nil))))
     ;; org-habit
     ;; (clear=blue, ready=green, alert=yellow, overdue=red. future=lower contrast)
     `(org-habit-clear-face ((,class (:background ,blue-lc :foreground ,blue-hc))))
     `(org-habit-clear-future-face ((,class (:background ,blue-lc))))
     `(org-habit-ready-face ((,class (:background ,green-lc :foreground ,green))))
     `(org-habit-ready-future-face ((,class (:background ,green-lc))))
     `(org-habit-alert-face ((,class (:background ,yellow :foreground ,yellow-lc))))
     `(org-habit-alert-future-face ((,class (:background ,yellow-lc))))
     `(org-habit-overdue-face ((,class (:background ,red :foreground ,red-lc))))
     `(org-habit-overdue-future-face ((,class (:background ,red-lc))))
     ;; latest additions
     `(org-agenda-dimmed-todo-face ((,class (:foreground ,base01))))
     `(org-agenda-restriction-lock ((,class (:background ,yellow))))
     `(org-clock-overlay ((,class (:background ,yellow))))
     `(org-column ((,class (:background ,base02 :strike-through nil
                                        :underline nil :slant normal :weight normal :inherit default))))
     `(org-column-title ((,class (:background ,base02 :underline t :weight bold))))
     `(org-date-selected ((,class (:foreground ,red :inverse-video t))))
     `(org-document-info ((,class (:foreground ,base0))))
     `(org-document-title ((,class (:foreground ,base1  :weight bold :height ,solarized-height-plus-4))))
     `(org-drawer ((,class (:foreground ,cyan))))
     `(org-footnote ((,class (:foreground ,magenta :underline t))))
     `(org-latex-and-export-specials ((,class (:foreground ,orange))))
     `(org-mode-line-clock-overrun ((,class (:inherit mode-line :background ,red))))
;;;;; outline
     `(outline-1 ((,class (:inherit org-level-1 :height ,solarized-height-plus-4))))
     `(outline-2 ((,class (:inherit org-level-2 :height ,solarized-height-plus-3))))
     `(outline-3 ((,class (:inherit org-level-3 :height ,solarized-height-plus-2))))
     `(outline-4 ((,class (:inherit org-level-4 :height ,solarized-height-plus-1))))
     `(outline-5 ((,class (:inherit org-level-5))))
     `(outline-6 ((,class (:inherit org-level-6))))
     `(outline-7 ((,class (:inherit org-level-7))))
     `(outline-8 ((,class (:inherit org-level-8))))
;;;;; paren-face
     `(paren-face  ((,class (:foreground ,base01))))
;;;;; perspective
     `(persp-selected-face ((,class (:foreground ,yellow))))
;;;;; pretty-mode
     `(pretty-mode-symbol-face  ((,class (:foreground ,yellow :weight normal))))
;;;;; prodigy
     `(prodigy-green-face ((,class (:foreground ,green))))
     `(prodigy-red-face ((,class (:foreground ,orange))))
     `(prodigy-yellow-face ((,class (:foreground ,yellow))))
     `(prodigy-line-face ((,class (:background ,base02))))
;;;;; popup
     `(popup-face ((,class (:background ,base02 :foreground ,base0))))
     `(popup-isearch-match ((,class (:background ,yellow :foreground ,base03))))
     `(popup-menu-face ((,class (:background ,base02 :foreground ,base0))))
     `(popup-menu-mouse-face ((,class (:background ,blue :foreground ,base03))))
     `(popup-menu-selection-face ((,class (:background ,magenta :foreground ,base03))))
     `(popup-scroll-bar-background-face ((,class (:background ,base01))))
     `(popup-scroll-bar-foreground-face ((,class (:background ,base1))))
     `(popup-tip-face ((,class (:background ,base02 :foreground ,base0))))
;;;;; pophint
     `(pophint:tip-face ((,class (:background ,magenta :foreground ,base03))))
     `(pophint:match-face ((,class (:background ,blue :foreground ,base03))))
     `(pophint:pos-tip-face ((,class (:background ,base02 :foreground ,base0))))
;;;;; powerline
     `(powerline-active1 ((,class ,(if solarized-high-contrast-mode-line
                                       `(:background ,base00 :foreground ,base03)
                                       `(:background ,base03 :foreground ,base00)))))
     `(powerline-active2 ((,class ,(if solarized-high-contrast-mode-line
                                       `(:background ,base01 :foreground ,base03)
                                       `(:background ,base02 :foreground ,base00)))))
     `(powerline-inactive1 ((,class ,(if solarized-high-contrast-mode-line
                                         `(:background ,base03 :foreground ,base1)
                                         `(:background ,base02 :foreground ,base01)))))
     `(powerline-inactive2 ((,class ,(if solarized-high-contrast-mode-line
                                         `(:background ,base02 :foreground ,base1)
                                         `(:background ,base03 :foreground ,base01)))))
;;;;; rainbow-blocks
     `(rainbow-blocks-depth-1-face ((,class (:foreground ,cyan))))
     `(rainbow-blocks-depth-2-face ((,class (:foreground ,yellow))))
     `(rainbow-blocks-depth-3-face ((,class (:foreground ,blue))))
     `(rainbow-blocks-depth-4-face ((,class (:foreground ,violet))))
     `(rainbow-blocks-depth-5-face ((,class (:foreground ,green))))
     `(rainbow-blocks-depth-6-face ((,class (:foreground ,yellow))))
     `(rainbow-blocks-depth-7-face ((,class (:foreground ,blue))))
     `(rainbow-blocks-depth-8-face ((,class (:foreground ,violet))))
     `(rainbow-blocks-depth-9-face ((,class (:foreground ,green))))
     `(rainbow-blocks-unmatched-face ((,class (:foreground ,red))))
;;;;; rainbow-delimiters
     `(rainbow-delimiters-depth-1-face ((,class (:foreground ,cyan))))
     `(rainbow-delimiters-depth-2-face ((,class (:foreground ,yellow))))
     `(rainbow-delimiters-depth-3-face ((,class (:foreground ,blue))))
     `(rainbow-delimiters-depth-4-face ((,class (:foreground ,violet))))
     `(rainbow-delimiters-depth-5-face ((,class (:foreground ,green))))
     `(rainbow-delimiters-depth-6-face ((,class (:foreground ,yellow))))
     `(rainbow-delimiters-depth-7-face ((,class (:foreground ,blue))))
     `(rainbow-delimiters-depth-8-face ((,class (:foreground ,violet))))
     `(rainbow-delimiters-depth-9-face ((,class (:foreground ,green))))
     `(rainbow-delimiters-depth-10-face ((,class (:foreground ,yellow))))
     `(rainbow-delimiters-depth-11-face ((,class (:foreground ,blue))))
     `(rainbow-delimiters-depth-12-face ((,class (:foreground ,violet))))
     `(rainbow-delimiters-unmatched-face
       ((,class (:foreground ,base0 :background ,base03 :inverse-video t))))
;;;;; rst-mode
     `(rst-level-1 ((,class (:inherit org-level-1))))
     `(rst-level-2 ((,class (:inherit org-level-2))))
     `(rst-level-3 ((,class (:inherit org-level-3))))
     `(rst-level-4 ((,class (:inherit org-level-4))))
     `(rst-level-5 ((,class (:inherit org-level-5))))
     `(rst-level-6 ((,class (:inherit org-level-6))))
;;;;; rpm-mode
     `(rpm-spec-dir-face ((,class (:foreground ,green))))
     `(rpm-spec-doc-face ((,class (:foreground ,green))))
     `(rpm-spec-ghost-face ((,class (:foreground ,red))))
     `(rpm-spec-macro-face ((,class (:foreground ,yellow))))
     `(rpm-spec-obsolete-tag-face ((,class (:foreground ,red))))
     `(rpm-spec-package-face ((,class (:foreground ,red))))
     `(rpm-spec-section-face ((,class (:foreground ,yellow))))
     `(rpm-spec-tag-face ((,class (:foreground ,blue))))
     `(rpm-spec-var-face ((,class (:foreground ,red))))
;;;;; sh-mode
     `(sh-quoted-exec ((,class (:foreground ,violet :weight bold))))
     `(sh-escaped-newline ((,class (:foreground ,yellow :weight bold))))
     `(sh-heredoc ((,class (:foreground ,yellow :weight bold))))
;;;;; skewer-mode
     `(skewer-error-face ((,class (:foreground ,orange :underline nil))))
     `(skewer-repl-log-face ((,class (:foreground ,violet))))
;;;;; smart-mode-line
     ;; use (setq sml/theme nil) to enable Solarized for sml
     `(sml/filename ((,class (:foreground ,base1 :weight bold))))
     `(sml/prefix ((,class (:foreground unspecified))))
     `(sml/git ((,class (:foreground unspecified))))
     `(sml/process ((,class (:weight bold))))
     `(sml/sudo ((,class  (:foreground ,orange :weight bold))))
     `(sml/read-only ((,class (:foreground ,cyan))))
     `(sml/outside-modified ((,class (:foreground , cyan))))
     `(sml/modified ((,class (:foreground ,cyan))))
     `(sml/vc-edited ((,class (:foreground ,green))))
     `(sml/charging ((,class (:foreground ,base1))))
     `(sml/discharging ((,class (:foreground ,base1 :weight bold))))
;;;;; smartparens
     `(sp-pair-overlay-face ((,class (:background ,base02))))
     `(sp-wrap-overlay-face ((,class (:background ,base02))))
     `(sp-wrap-tag-overlay-face ((,class (:background ,base02))))
     `(sp-show-pair-enclosing ((,class (:inherit highlight))))
     `(sp-show-pair-match-face
       ((,class (:background unspecified :foreground ,magenta
                             :weight ,s-maybe-bold))))
     `(sp-show-pair-mismatch-face
       ((,class (:foreground ,base02 :background ,red
                             :weight ,s-maybe-bold))))
;;;;; show-paren
     `(show-paren-match
       ((,class (:foreground ,magenta :background unspecified
                             :weight ,s-maybe-bold))))
     `(show-paren-mismatch
       ((,class (:foreground ,base02 :background ,red
                             :weight ,s-maybe-bold))))
;;;;; swiper
     `(swiper-line-face ((,class (:background ,base02))))
     `(swiper-match-face-1 ((,class (:weight bold :foreground ,base1))))
     `(swiper-match-face-2 ((,class (:weight bold :foreground ,yellow))))
     `(swiper-match-face-3 ((,class (:weight bold :foreground ,yellow))))
     `(swiper-match-face-4 ((,class (:weight bold :foreground ,yellow))))
;;;;; sx (WIP)
     `(sx-custom-button ((,class (:background ,base02 :foreground ,base1
                                              :box (:line-width 2 :style released-button :height 0.9)))))
     `(sx-question-list-answers ((,class (:inherit sx-question-list-parent :foreground ,green :height 1.0))))
     `(sx-question-list-answers-accepted ((,class (:inherit sx-question-list-answers :weight bold :underline t))))
     `(sx-question-list-bounty ((,class (:foreground ,cyan))))
     `(sx-question-list-date ((,class (:inherit font-lock-comment-face))))
     `(sx-question-list-favorite ((,class (:inherit sx-question-list-score-upvoted))))
     `(sx-question-list-parent ((,class (:inherit default))))
     `(sx-question-list-read-question ((,class (:inherit sx-question-list-parent :height 1.0))))
     `(sx-question-list-score ((,class (:inherit sx-question-list-parent :foreground ,base01 :height 1.0))))
     `(sx-question-list-score-upvoted ((,class (:inherit sx-question-list-score :weight bold))))
     `(sx-question-list-unread-question ((,class (:inherit sx-question-list-read-question :weight bold))))
     `(sx-question-mode-accepted ((,class (:inherit sx-question-mode-title :foreground ,green :height 1.3))))
     `(sx-question-mode-closed ((,class (:inherit font-lock-warning-face :box 2))))
     `(sx-question-mode-closed-reason ((,class (:inherit sx-question-mode-title :box (:line-width 2 :color ,yellow)))))
     ;; TODO: sx-question-mode-content-faceexposes a general problem that's hard to deal with,
     ;; if base02 is used as bg some things are not visible enough.. It might be a good idea to
     ;; introduce yet another special color that goes a little furhter towards netural gray and
     ;; ensures readability as a bg for all solarized faces. If it's possible, that is.
     `(sx-question-mode-content-face ((,class (:background unspecified))))
     `(sx-question-mode-date ((,class (:inherit font-lock-string-face))))
     `(sx-question-mode-header ((,class (:inherit message-header-name :weight normal))))
     `(sx-question-mode-kbd-tag ((,class (:box (:line-width 3 :style released-button :color ,base02) :weight semibold :height 0.9))))
     `(sx-question-mode-score ((,class nil)))
     `(sx-question-mode-score-downvoted ((,class (:inherit (font-lock-warning-face sx-question-mode-score)))))
     `(sx-question-mode-score-upvoted ((,class (:inherit (font-lock-function-name-face sx-question-mode-score) :weight bold))))
     `(sx-question-mode-sub-sup-tag ((,class (:height 0.7))))
     `(sx-question-mode-title ((,class (:inherit default :weight bold))))
     `(sx-question-mode-title-comments ((,class (:inherit sx-question-mode-title))))
     `(sx-tag ((,class (:foreground ,base0))))
     `(sx-user-accept-rate ((,class nil)))
     `(sx-user-name ((,class (:inherit font-lock-builtin-face))))
     `(sx-user-reputation ((,class (:inherit font-lock-comment-face))))

;;;;; syslog-mode
     `(syslog-ip ((,class (:background unspecified
                                       :foreground ,green
                                       :underline nil
                                       :weight normal
                                       :slant normal))))
     `(syslog-hour ((,class (:background unspecified
                                         :foreground ,yellow))))
     `(syslog-error ((,class (:background unspecified
                                          :foreground ,orange
                                          :weight bold))))
     `(syslog-warn ((,class (:background unspecified
                                         :foreground ,yellow
                                         :weight bold))))
     `(syslog-info ((,class (:background unspecified
                                         :foreground ,blue
                                         :weight bold))))
     `(syslog-debug ((,class (:background unspecified
                                          :foreground ,cyan
                                          :weight bold))))
     `(syslog-su ((,class (:background unspecified
                                       :foreground ,violet
                                       :weight normal))))
;;;;; slime
     `(slime-repl-inputed-output-face ((,class (:foreground ,red))))
;;;;; speedbar
     `(speedbar-button-face ((,class (:inherit ,s-variable-pitch
                                               :foreground ,base01))))
     `(speedbar-directory-face ((,class (:inherit ,s-variable-pitch :foreground ,blue))))
     `(speedbar-file-face ((,class (:inherit ,s-variable-pitch :foreground ,base0))))
     `(speedbar-highlight-face ((,class (:inherit ,s-variable-pitch :background ,base02))))
     `(speedbar-selected-face ((,class (:inherit ,s-variable-pitch
                                                 :foreground ,yellow :underline t))))
     `(speedbar-separator-face ((,class (:inherit ,s-variable-pitch
                                                  :background ,blue :foreground ,base03
                                                  :overline ,cyan-lc))))
     `(speedbar-tag-face ((,class (:inherit ,s-variable-pitch :foreground ,green))))
;;;;; stripe-buffer
     `(stripe-highlight ((,class (:background ,base02))))
;;;;; structured-haskell
     `(shm-current-face ((,class (:background ,base02))))
     `(shm-quarantine-face ((,class (:background ,base01))))
;;;;; sunrise commander
;;;;;; headings
     `(sr-active-path-face ((,class (:background ,blue :foreground ,base03
                                                 :height ,solarized-height-plus-1  :weight bold))))
     `(sr-editing-path-face ((,class (:background ,yellow :foreground ,base03
                                                  :weight bold :height ,solarized-height-plus-1))))
     `(sr-highlight-path-face ((,class (:background ,green :foreground ,base03
                                                    :weight bold :height ,solarized-height-plus-1))))
     `(sr-passive-path-face ((,class (:background ,base01 :foreground ,base03
                                                  :weight bold :height ,solarized-height-plus-1))))
;;;;;; marked
     `(sr-marked-dir-face ((,class (:inherit dired-marked))))
     `(sr-marked-file-face ((,class (:inherit dired-marked))))
     `(sr-alt-marked-dir-face ((,class (:background ,magenta :foreground ,base03
                                                    :weight bold))))
     `(sr-alt-marked-file-face ((,class (:background ,magenta :foreground ,base03
                                                     :weight bold))))
;;;;;; fstat
     `(sr-directory-face ((,class (:inherit dired-directory :weight normal))))
     `(sr-symlink-directory-face ((,class (:inherit dired-directory
                                                    :slant italic :weight normal))))
     `(sr-symlink-face ((,class (:inherit dired-symlink :slant italic :weight normal))))
     `(sr-broken-link-face ((,class (:inherit dired-warning :slant italic :weight normal))))
;;;;;; file types
     `(sr-compressed-face ((,class (:foreground ,base0))))
     `(sr-encrypted-face ((,class (:foreground ,base0))))
     `(sr-log-face ((,class (:foreground ,base0))))
     `(sr-packaged-face ((,class (:foreground ,base0))))
     `(sr-html-face ((,class (:foreground ,base0))))
     `(sr-xml-face ((,class (:foreground ,base0))))
;;;;;; misc
     `(sr-clex-hotchar-face ((,class (:background ,red  :foreground ,base03
                                                  :weight bold))))
;;;;; swoop
     `(swoop-face-header-format-line ((,class (:foreground ,yellow :weight bold
                                                           :height unspecified))))
     `(swoop-face-line-buffer-name ((,class (:background ,base02 :foreground ,base1
                                                         :weight bold :height unspecified))))
     `(swoop-face-line-number ((,class (:foreground ,base01))))
     `(swoop-face-target-line ((,class (:background ,base02 :foreground unspecified))))
     `(swoop-face-target-words ((,class (:background unspecified :foreground ,magenta))))
;;;;; table
     `(table-cell ((,class (:foreground ,base0 :background ,base02))))
;;;;; term
     `(term ((t ( :background ,base03 :foreground ,base0))))
     `(term-color-black ((t (:foreground ,base02 :background ,base02))))
     `(term-color-red ((t (:foreground ,red :background ,red))))
     `(term-color-green ((t (:foreground ,green :background ,green))))
     `(term-color-yellow ((t (:foreground ,yellow :background ,yellow))))
     `(term-color-blue ((t (:foreground ,blue :background ,blue))))
     `(term-color-magenta ((t (:foreground ,magenta :background ,magenta))))
     `(term-color-cyan ((t (:foreground ,cyan :background ,cyan))))
     `(term-color-white ((t (:foreground ,base2 :background ,base2))))
;;;;; todotxt
     `(todotxt-priority-a-face ((,class (:foreground ,orange))))
     `(todotxt-priority-b-face ((,class (:foreground ,yellow))))
     `(todotxt-priority-c-face ((,class (:foreground ,violet))))
;;;;; tooltip
     ;; NOTE: This setting has no effect on the os widgets for me
     ;; zencoding uses this.
     `(tooltip ((,class (:background ,yellow-lc :foreground ,yellow-hc
                                     :inherit ,s-variable-pitch))))
;;;;; tuareg
     `(tuareg-font-lock-governing-face ((,class (:foreground ,magenta :weight bold))))
     `(tuareg-font-lock-multistage-face ((,class (:foreground ,blue :background ,base02
                                                              :weight bold))))
     `(tuareg-font-lock-operator-face ((,class (:foreground ,base1))))
     `(tuareg-font-lock-error-face ((,class (:foreground ,yellow :background ,red
                                                         :weight bold))))
     `(tuareg-font-lock-interactive-output-face ((,class (:foreground ,cyan))))
     `(tuareg-font-lock-interactive-error-face ((,class (:foreground ,red))))
;;;;; undo-tree
     `(undo-tree-visualizer-default-face
       ((,class (:foreground ,base01 :background ,base03))))
     `(undo-tree-visualizer-unmodified-face ((,class (:foreground ,green))))
     `(undo-tree-visualizer-current-face ((,class (:foreground ,blue :inverse-video t))))
     `(undo-tree-visualizer-active-branch-face
       ((,class (:foreground ,base1 :background ,base03 :weight bold))))
     `(undo-tree-visualizer-register-face ((,class (:foreground ,yellow))))
;;;;; volatile highlights
     `(vhl/default-face ((,class (:background ,green-lc :foreground ,green-hc))))
;;;;; w3m
     `(w3m-anchor ((,class (:inherit link))))
     `(w3m-arrived-anchor ((,class (:inherit link-visited))))
     `(w3m-form ((,class (:background ,base03 :foreground ,base0))))
     `(w3m-header-line-location-title
       ((,class (:background ,base02 :foreground ,yellow))))
     `(w3m-header-line-location-content
       ((,class (:background ,base02 :foreground ,base0))))
     `(w3m-bold ((,class (:foreground ,base1 :weight bold))))
     `(w3m-image-anchor ((,class (:background ,base03 :foreground ,cyan :inherit link))))
     `(w3m-image ((,class (:background ,base03 :foreground ,cyan))))
     `(w3m-lnum-minibuffer-prompt ((,class (:foreground ,base1))))
     `(w3m-lnum-match ((,class (:background ,base02))))
     `(w3m-lnum ((,class (:underline nil :bold nil :foreground ,red))))
     `(w3m-session-select ((,class (:foreground ,base0))))
     `(w3m-session-selected ((,class (:foreground ,base1 :bold t :underline t))))
     `(w3m-tab-background ((,class (:background ,base03 :foreground ,base0))))
     `(w3m-tab-selected-background
       ((,class (:background ,base03 :foreground ,base0))))
     `(w3m-tab-mouse ((,class (:background ,base02 :foreground ,yellow))))
     `(w3m-tab-selected ((,class (:background ,base02 :foreground ,base1
                                              :bold t))))
     `(w3m-tab-unselected ((,class (:background ,base02 :foreground ,base0))))
     `(w3m-tab-selected-retrieving ((,class (:background ,base02 :foreground ,red))))
     `(w3m-tab-unselected-retrieving
       ((,class (:background ,base02 :foreground ,orange))))
     `(w3m-tab-unselected-unseen ((,class (:background ,base02 :foreground ,violet))))
;;;;; web-mode
     `(web-mode-builtin-face ((,class (:inherit font-lock-builtin-face))))
     `(web-mode-comment-face ((,class (:foreground ,base01))))
     `(web-mode-constant-face ((,class (:foreground ,blue :weight bold))))
     `(web-mode-current-element-highlight-face ((,class
                                                 (:underline unspecified :weight unspecified
                                                             :background ,base02))))
     `(web-mode-css-at-rule-face ((,class (:foreground ,violet :slant italic))))
     `(web-mode-css-pseudo-class-face ((,class (:foreground ,green :slant italic))))
     `(web-mode-doctype-face ((,class (:foreground ,base01
                                                   :slant italic :weight bold))))
     `(web-mode-folded-face ((,class (:underline t))))
     `(web-mode-function-name-face ((,class (:foreground ,blue))))
     `(web-mode-html-attr-name-face ((,class (:foreground ,blue :slant normal))))
     `(web-mode-html-attr-value-face ((,class (:foreground ,cyan :slant italic))))
     `(web-mode-html-tag-face ((,class (:foreground ,green))))
     `(web-mode-keyword-face ((,class (:foreground ,yellow :weight normal))))
     `(web-mode-preprocessor-face ((,class (:foreground ,yellow :slant normal :weight unspecified))))
     `(web-mode-string-face ((,class (:foreground ,cyan))))
     `(web-mode-type-face ((,class (:foreground ,yellow))))
     `(web-mode-variable-name-face ((,class (:foreground ,blue))))
     `(web-mode-warning-face ((,class (:inherit font-lock-warning-face))))
     `(web-mode-block-attr-name-face ((,class (:inherit web-mode-html-attr-name-face))))
     `(web-mode-block-attr-value-face ((,class (:inherit web-mode-html-attr-value-face))))
     `(web-mode-block-comment-face ((,class (:inherit web-mode-comment-face))))
     `(web-mode-block-control-face ((,class (:inherit font-lock-preprocessor-face))))
     `(web-mode-block-face ((,class (:background unspecified))))
     `(web-mode-block-string-face ((,class (:inherit web-mode-string-face))))
     `(web-mode-comment-keyword-face ((,class (:box 1 :weight bold))))
     `(web-mode-css-color-face ((,class (:inherit font-lock-builtin-face))))
     `(web-mode-css-function-face ((,class (:inherit font-lock-builtin-face))))
     `(web-mode-css-priority-face ((,class (:inherit font-lock-builtin-face))))
     `(web-mode-css-property-name-face ((,class (:inherit font-lock-variable-name-face))))
     `(web-mode-css-selector-face ((,class (:inherit font-lock-keyword-face))))
     `(web-mode-css-string-face ((,class (:inherit web-mode-string-face))))
     `(web-mode-javascript-string-face ((,class (:inherit web-mode-string-face))))
     `(web-mode-json-context-face ((,class (:foreground ,violet))))
     `(web-mode-json-key-face ((,class (:foreground ,violet))))
     `(web-mode-json-string-face ((,class (:inherit web-mode-string-face))))
     `(web-mode-param-name-face ((,class (:foreground ,base0))))
     `(web-mode-part-comment-face ((,class (:inherit web-mode-comment-face))))
     `(web-mode-part-face ((,class (:inherit web-mode-block-face))))
     `(web-mode-part-string-face ((,class (:inherit web-mode-string-face))))
     `(web-mode-symbol-face ((,class (:foreground ,yellow))))
     `(web-mode-whitespace-face ((,class (:background ,red))))
     `(web-mode-html-tag-bracket-face ((,class (:foreground ,base01))))
     `(web-mode-block-delimiter-face ((,class (:inherit font-lock-preprocessor-face))))
     `(web-mode-css-comment-face ((,class (:inherit web-mode-comment-face))))
     `(web-mode-css-variable-face ((,class (:inherit web-mode-variable-name-face :slant italic))))
     `(web-mode-error-face ((,class (:background ,red))))
     `(web-mode-function-call-face ((,class (:inherit font-lock-function-name-face))))
     `(web-mode-html-attr-custom-face ((,class (:inherit web-mode-html-attr-name-face))))
     `(web-mode-html-attr-engine-face ((,class (:inherit web-mode-html-attr-custom-face))))
     `(web-mode-html-attr-equal-face ((,class (:inherit web-mode-html-attr-name-face))))
     `(web-mode-html-tag-custom-face ((,class (:inherit web-mode-html-tag-face))))
     `(web-mode-javascript-comment-face ((,class (:inherit web-mode-comment-face))))
     `(web-mode-json-comment-face ((,class (:inherit web-mode-comment-face))))
;;;;; weather-metno
     `(weather-metno-date ((,class (:foreground ,yellow :height ,solarized-height-plus-3))))
     `(weather-metno-date-range ((,class (:foreground ,blue))))
     `(weather-metno-entry ((,class (:foreground ,cyan))))
     `(weather-metno-footer ((,class (:inherit font-lock-comment-face))))
     `(weather-metno-header ((,class (:inherit header-line))))
;;;;; wgrep
     `(wgrep-delete-face ((,class (:background unspecified :foreground ,blue))))
     `(wgrep-done-face ((,class (:foreground ,green))))
     `(wgrep-face ((,class (:background unspecified :foreground ,blue))))
     `(wgrep-file-face ((,class (:background unspecified :foreground ,magenta))))
     `(wgrep-reject-face ((,class (:foreground ,red :weight unspecified))))
;;;;; whitespace-mode
     `(whitespace-space ((,class (:background unspecified :foreground ,base01
                                              :inverse-video unspecified :slant italic))))
     `(whitespace-hspace ((,class (:background unspecified :foreground ,base1
                                               :inverse-video unspecified))))
     `(whitespace-tab ((,class (:background unspecified :foreground ,red
                                            :inverse-video t))))
     `(whitespace-newline ((,class(:background unspecified :foreground ,base01
                                               :inverse-video unspecified))))
     `(whitespace-trailing ((,class (:background unspecified :foreground ,orange-lc
                                                 :inverse-video t))))
     `(whitespace-line ((,class (:background unspecified :foreground ,magenta
                                             :inverse-video unspecified))))
     `(whitespace-space-before-tab ((,class (:background ,red-lc :foreground unspecified
                                                         :inverse-video unspecified))))
     `(whitespace-indentation ((,class (:background unspecified :foreground ,yellow
                                                    :inverse-video unspecified :weight bold))))
     `(whitespace-empty ((,class (:background unspecified :foreground ,red-lc
                                              :inverse-video t))))
     `(whitespace-space-after-tab ((,class (:background unspecified :foreground ,orange
                                                        :inverse-video t :weight bold))))
;;;;; wanderlust
     `(wl-highlight-folder-few-face ((,class (:foreground ,red))))
     `(wl-highlight-folder-many-face ((,class (:foreground ,red))))
     `(wl-highlight-folder-path-face ((,class (:foreground ,orange))))
     `(wl-highlight-folder-unread-face ((,class (:foreground ,blue))))
     `(wl-highlight-folder-zero-face ((,class (:foreground ,base0))))
     `(wl-highlight-folder-unknown-face ((,class (:foreground ,blue))))
     `(wl-highlight-message-citation-header ((,class (:foreground ,red))))
     `(wl-highlight-message-cited-text-1 ((,class (:foreground ,red))))
     `(wl-highlight-message-cited-text-2 ((,class (:foreground ,green))))
     `(wl-highlight-message-cited-text-3 ((,class (:foreground ,blue))))
     `(wl-highlight-message-cited-text-4 ((,class (:foreground ,blue))))
     `(wl-highlight-message-header-contents-face ((,class (:foreground ,green))))
     `(wl-highlight-message-headers-face ((,class (:foreground ,red))))
     `(wl-highlight-message-important-header-contents ((,class (:foreground ,green))))
     `(wl-highlight-message-header-contents ((,class (:foreground ,green))))
     `(wl-highlight-message-important-header-contents2 ((,class (:foreground ,green))))
     `(wl-highlight-message-signature ((,class (:foreground ,green))))
     `(wl-highlight-message-unimportant-header-contents ((,class (:foreground ,base0))))
     `(wl-highlight-summary-answered-face ((,class (:foreground ,blue))))
     `(wl-highlight-summary-disposed-face ((,class (:foreground ,base0 :slant italic))))
     `(wl-highlight-summary-new-face ((,class (:foreground ,blue))))
     `(wl-highlight-summary-normal-face ((,class (:foreground ,base0))))
     `(wl-highlight-summary-thread-top-face ((,class (:foreground ,yellow))))
     `(wl-highlight-thread-indent-face ((,class (:foreground ,magenta))))
     `(wl-highlight-summary-refiled-face ((,class (:foreground ,base0))))
     `(wl-highlight-summary-displaying-face ((,class (:underline t :weight bold))))
;;;;; weechat
     `(weechat-error-face ((,class (:inherit error))))
     `(weechat-highlight-face ((,class (:foreground ,base0 :weight bold))))
     `(weechat-nick-self-face ((,class (:foreground ,base01 :weight unspecified))))
     `(weechat-prompt-face ((,class (:inherit minibuffer-prompt))))
     `(weechat-time-face ((,class (:foreground ,base01))))
;;;;; which-func-mode
     `(which-func ((,class (:foreground ,green))))
;;;;; window-number-mode
     `(window-number-face ((,class (:foreground ,green))))
;;;;; yascroll
     `(yascroll:thumb-text-area
       ((,class (:foreground ,base01 :background ,base01))))
     `(yascroll:thumb-fringe
       ((,class (:foreground ,base01 :background ,base01))))
;;;;; yasnippet
     `(yas-field-highlight-face ((,class (:inherit secondary-selection))))
;;;;; zencoding
     `(zencoding-preview-input ((,class (:background ,base02 :box ,base1))))
;;;;; ztree
     `(ztreep-arrow-face ((,class (:foreground ,base01))))
     `(ztreep-diff-header-face ((,class (:foreground ,base01 :weight bold :height 1.2))))
     `(ztreep-diff-header-small-face ((,class (:foreground ,base01 :weight bold))))
     `(ztreep-diff-model-add-face ((,class (:foreground ,blue))))
     `(ztreep-diff-model-diff-face ((,class (:foreground ,red))))
     `(ztreep-diff-model-normal-face ((,class (:foreground ,base0))))
     `(ztreep-expand-sign-face ((,class (:foreground ,base01))))
     `(ztreep-header-face ((,class (:foreground ,base01 :weight bold :height 1.2))))
     `(ztreep-leaf-face ((,class (:foreground  ,base0))))
     `(ztreep-node-face ((,class (:foreground ,blue))))
     ) ; END custom-theme-set-faces
;;; Theme Variables
    (custom-theme-set-variables
     theme-name
;;;;; ansi-colors
     `(ansi-color-names-vector
       [,base02 ,red ,green ,yellow ,blue ,magenta ,cyan ,base00])
;;;;; compilation
     `(compilation-message-face 'default)
;;;;; cua
     `(cua-normal-cursor-color ,base0)
     `(cua-read-only-cursor-color ,green)
     `(cua-global-mark-cursor-color ,cyan)
     `(cua-overwrite-cursor-color ,yellow)
;;;;; fill-column-indicator
     `(fci-rule-color ,base02)
;;;;; magit
     `(magit-diff-use-overlays nil)
;;;;; nrepl-client
     `(nrepl-message-colors
       '(,red ,orange ,yellow ,green-d ,green-l
                      ,blue-d ,cyan ,magenta ,violet))
;;;;; highlight-changes
     `(highlight-changes-colors '(,magenta ,violet))
;;;;; highlight-symbol
     `(highlight-symbol-foreground-color ,base1)
     `(highlight-symbol-colors
       (--map (solarized-color-blend it ,base03 0.25)
              '(,yellow ,cyan ,red ,violet ,green ,orange ,blue)))
;;;;; highlight-tail
     `(highlight-tail-colors
       '((,base02 . 0)(,green-lc . 20)(,cyan-lc . 30)(,blue-lc . 50)
         (,yellow-lc . 60)(,orange-lc . 70)(,magenta-lc . 85)(,base02 . 100)))
;;;;; hl-anything
     `(hl-fg-colors '(,base03 ,base03 ,base03 ,base03 ,base03 ,base03
                              ,base03 ,base03))
     `(hl-bg-colors '(,yellow-lc ,orange-lc ,red-lc ,magenta-lc
                                 ,violet-lc ,blue-lc ,cyan-lc ,green-lc))
;;;;; pos-tip
     `(pos-tip-foreground-color ,base1)
     `(pos-tip-background-color ,base02)
;;;;; smartrep
     `(smartrep-mode-line-active-bg (solarized-color-blend ,green ,s-mode-line-bg 0.2))
;;;;; term
     `(term-default-fg-color ,base0) ;; @deprecated24.3
     `(term-default-bg-color ,base03) ;; @deprecated24.3
;;;;; vc
     `(vc-annotate-color-map
       '((20 . ,red)
         (40 . ,(solarized-color-blend yellow red (/ 2.0 4)))
         (60 . ,(solarized-color-blend yellow red (/ 3.0 4)))
         (80 . ,yellow)
         (100 . ,(solarized-color-blend green yellow (/ 2.0 6)))
         (120 . ,(solarized-color-blend green yellow (/ 3.0 6)))
         (140 . ,(solarized-color-blend green yellow (/ 4.0 6)))
         (160 . ,(solarized-color-blend green yellow (/ 5.0 6)))
         (180 . ,green)
         (200 . ,(solarized-color-blend cyan green (/ 2.0 6)))
         (220 . ,(solarized-color-blend cyan green (/ 3.0 6)))
         (240 . ,(solarized-color-blend cyan green (/ 4.0 6)))
         (260 . ,(solarized-color-blend cyan green (/ 5.0 6)))
         (280 . ,cyan)
         (300 . ,(solarized-color-blend blue cyan (/ 2.0 5)))
         (320 . ,(solarized-color-blend blue cyan (/ 3.0 5)))
         (340 . ,(solarized-color-blend blue cyan (/ 4.0 5)))
         (360 . ,blue)))
     `(vc-annotate-very-old-color nil)
     `(vc-annotate-background nil)
;;;;; weechat
     `(weechat-color-list
       '(unspecified ,base03 ,base02
                     ,red-d ,red
                     ,green-d ,green
                     ,yellow-d ,yellow
                     ,blue-d ,blue
                     ,magenta-d ,magenta
                     ,cyan-d ,cyan
                     ,base0 ,base00))
;;;;; xterm-color
     `(xterm-color-names [,base02 ,red ,green ,yellow
                                  ,blue ,magenta ,cyan ,base2])
     `(xterm-color-names-bright [,base03 ,orange ,base01 ,base00
                                         ,base0 ,violet ,base1 ,base3]))
;;; Setup End
     (when childtheme
       (funcall childtheme))
     ) ; END custom-theme-set-variables
  )    ; END defun create-solarized-theme

;;; Footer

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; Local Variables:
;; no-byte-compile: t
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode 1))
;; indent-tabs-mode: nil
;; fill-column: 95
;; End:
;;; solarized.el ends here

(deftheme solarized-dark "The dark variant of the Solarized colour theme")

(create-solarized-theme 'dark 'solarized-dark)

(provide-theme 'solarized-dark)
