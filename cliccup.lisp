(defpackage :fwoar.cliccup
  (:use :cl )
  (:export #:base-template
           #:with-template
           #:to-dom
           #:make-element
           #:set-attribute
           #:make-text-node))
(in-package :fwoar.cliccup)

(defun unpack-vector (vec)
  (let* ((length (length vec))
         (raw-tag (elt vec 0))
         (tag (etypecase raw-tag
                (string raw-tag)
                (symbol (serapeum:string-invert-case raw-tag))))
         (attrs-p (and (> length 1)
                       (listp (elt vec 1))))
         (attrs (when attrs-p
                  (elt vec 1)))
         (children (subseq vec
                           (if attrs-p
                               2
                               1))))
    (values tag attrs children)))

(defgeneric make-element (parent tag))
(defgeneric set-attribute (root name value))
(defgeneric make-text-node (tag text))

(defmethod make-element ((parent plump:nesting-node) (tag string))
  (plump:make-element parent tag))
(defmethod set-attribute ((parent plump:element) (name string) (value string))
  (plump:set-attribute parent name value))
(defmethod make-text-node ((parent plump:element) (text string))
  (plump:make-text-node parent text))

(defun parse-tag (tag)
  (when (> (count #\@ tag) 1)
    (error "too many ids"))
  (destructuring-bind (head tail)
      (fwoar.string-utils:partition #\@ tag)
    (let* ((tail-parsed (when tail
                          (fwoar.string-utils:split #\. tail)))
           (id (when tail
                 (elt tail-parsed 0)))
           (head-parsed (fwoar.string-utils:split #\. head))
           (tag (elt head-parsed 0))
           (classes (concatenate 'list
                                 (subseq head-parsed 1)
                                 (when tail
                                   (subseq tail-parsed 1)))))
      (values tag
              id
              classes))))

(defun to-dom (parent vec)
  (multiple-value-bind (tag attrs children)
      (unpack-vector vec)
    (multiple-value-bind (tag id classes) (parse-tag tag)
      (let ((tag (make-element parent tag)))
        (prog1 tag
          (when id
            (set-attribute tag "id" id))
          (when classes
            (set-attribute tag
                           "class"
                           (serapeum:string-join classes #\space)))
          (loop for (key value) on attrs by 'cddr
                do
                   (set-attribute tag
                                  (typecase key
                                    (string key)
                                    (symbol (serapeum:string-invert-case key)))
                                  value))
          (map nil
               (lambda (child)
                 (typecase child
                   (string (make-text-node tag child))
                   (sequence (to-dom tag child))))
               children))))))

(defun base-template (title &rest body)
  `(:html (:lang "en")
          (:head ()
                 (:link (:rel "stylesheet"
                         :href "/static/style.css"))
                 (:title ,title))
          (:body ()
                 (:main ()
                        (:h1 () ,title)
                        ,@body))))

(defmacro with-template ((template title) &rest body)
  `(,template ,title
              ,@body))
