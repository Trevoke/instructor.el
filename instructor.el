;; -*- lexical-binding: t; -*-

(cl-defun instructor-call (&key llm type prompt)
  "Generate instances of TYPE from LLM based on PROMPT."
  (let* ((function-call (instructor--make-function-spec type))
         (chat-prompt (llm-make-chat-prompt
                       prompt
                       :functions `(,function-call)))
         (results (llm-chat llm chat-prompt)))
    ;; Extract the structs from the results
    (mapcar #'cdr results)))

(defmacro instructor-struct (name &rest slots)
  "Define a struct NAME and record its slot names, types, and descriptions for LLM usage."
  (let ((slot-defs '())
        (metadata '()))
    (dolist (slot slots)
      (cond
       ;; If the slot is a symbol (no options)
       ((symbolp slot)
        (push slot slot-defs)
        (push (list slot nil nil) metadata))
       ;; If the slot is a list starting with the slot name
       ((and (listp slot) (symbolp (car slot)))
        (let* ((slot-name (car slot))
               (rest (cdr slot))
               (default (when (and rest (not (keywordp (car rest))))
                          (pop rest)))
               (cl-options '())
               (type nil)
               (description nil))
          ;; Process options
          (while rest
            (let ((key (pop rest)))
              (cond
               ((eq key :type)
                (setq type (pop rest)))
               ((eq key :description)
                (setq description (pop rest)))
               ;; Collect any other options for cl-defstruct
               (t
                (push key cl-options)
                (push (pop rest) cl-options)))))
          (setq cl-options (reverse cl-options))
          ;; Build slot definition
          (let ((slot-def (if default
                              (cons slot-name (cons default cl-options))
                            (cons slot-name cl-options))))
            (push slot-def slot-defs))
          ;; Store metadata
          (push (list slot-name type description) metadata)))
       (t
        (error "Invalid slot definition: %s" slot))))
    `(progn
       ;; Define the struct
       (cl-defstruct ,name ,@(reverse slot-defs))
       ;; Record the slots with their types and descriptions
       (put ',name 'llm-struct-slots ',(reverse metadata)))))

(defun instructor-struct-to-function-args (struct-name)
  "Generate llm-function-arg definitions from STRUCT-NAME's slots."
  (let ((slots (get struct-name 'llm-struct-slots)))
    (mapcar (lambda (slot)
              (let ((name (symbol-name (nth 0 slot)))
                    (type (nth 1 slot))
                    (description (nth 2 slot)))
                (make-llm-function-arg
                 :name name
                 :type type
                 :description description
                 :required t)))
            slots)))

(defun instructor--struct-constructor (struct-name)
  "Create a lambda function to construct instances of STRUCT-NAME."
  (let* ((slots (get struct-name 'llm-struct-slots))
         (constructor (intern (concat "make-" (symbol-name struct-name)))))
    (lambda (&rest args)
      ;; Map positional arguments to keyword arguments
      (let ((keys (mapcar (lambda (slot)
                            (intern (concat ":" (symbol-name (nth 0 slot)))))
                          slots)))
        (apply constructor (cl-mapcan #'list keys args))))))

(defun instructor--make-function-spec (struct-name)
  "Create an llm-function-call for STRUCT-NAME."
  (let ((args (instructor-struct-to-function-args struct-name))
        (function (instructor--struct-constructor struct-name))
        (function-name (format "make-%s" (symbol-name struct-name))))
    (make-llm-function-call
     :function function
     :name function-name
     :description (format "Construct %s instances." (symbol-name struct-name))
     :args args)))

(provide 'instructor)
