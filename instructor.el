;;; instructor.el --- Type-based LLM interaction -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2024 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; Homepage: https://github.com/Trevoke/org-gtd.el
;; Package-Requires: ((emacs "29.1") (llm "0.17"))
;; Package-Version: 0.1.0

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Define structs and use them to seamlessly transition from LLM response
;; back to your code.
;; (setq llm (make-llm-ollama :chat-model "llama3.1"))
;; ;; for valid types see the help for `make-llm-function-arg' and look at the type.
;; (instructor-defstruct dnd-character
;;                       (name nil :type string :description "The character's name")
;;                       (class nil :type string :description "The character's class")
;;                       (race nil :type string :description "The character's race in the D&D setting")
;;                       (skills nil :type (list string) :description "The skills the character has"))

;; (instructor-call :llm llm
;;                  :type 'dnd-character ;; note, same name as type created above
;;                  :prompt "Make a starter character for D&D 5th edition.")

;; ;; (#s(dnd-character :name "Eilif Stonefist" :class "Fighter" :race "Human" :skills
;; ;;                   ["Acrobatics" "Athletics"]))
;;
;;; Code:

(require 'llm)
(require 'cl-lib)

(cl-defun instructor-call (&key llm type prompt)
  "Generate instances of TYPE from LLM based on PROMPT."
  (let* ((function-call (instructor--make-function-spec type))
         (chat-prompt (llm-make-chat-prompt
                       prompt
                       :functions `(,function-call)))
         (results (llm-chat llm chat-prompt)))
    ;; Extract the structs from the results
    (mapcar #'cdr results)))

(defmacro instructor-defstruct (name &rest slots)
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

(defvar instructor-primitive-types '(string integer float boolean)
  "List of primitive types for instructor structs.")

(defvar instructor-complex-types '(enum list or)
  "List of complex types for instructor structs.")

(defun instructor--validate-value (type value field-name)
  "Validate VALUE against TYPE for FIELD-NAME."
  (cond
   ;; Primitive types
   ((member type instructor-primitive-types)
    (cond
     ((eq type 'string) (stringp value))
     ((eq type 'integer) (integerp value))
     ((eq type 'float) (floatp value))
     ((eq type 'boolean) (booleanp value))
     (t t)))  ; Assume valid for other types
   ;; Enum types
   ((and (listp type) (eq (car type) 'enum))
    (let ((allowed-values (cdr type)))
      (if (member value allowed-values)
          t
        (error "Invalid value '%s' for field '%s'. Allowed values are: %s" value field-name allowed-values))))
   ;; List types
   ((and (listp type) (eq (car type) 'list))
    (and (listp value)
         (cl-every (lambda (elem)
                     (instructor--validate-value (cadr type) elem field-name))
                   value)))
   ;; Nested structs
   ((and (symbolp type) (get type 'llm-struct-slots))
    (instructor--validate-struct type value))
   (t
    (error "Unknown type '%s' for field '%s'" type field-name))))

(defun instructor--validate-struct (struct-name value)
  "Validate VALUE against the struct STRUCT-NAME."
  (let ((slots (get struct-name 'llm-struct-slots)))
    (unless (hash-table-p value)
      (error "Expected an object for struct '%s', got '%s'" struct-name value))
    (cl-loop for slot in slots do
             (let* ((field-name (nth 0 slot))
                    (type (nth 1 slot))
                    (field-value (gethash (symbol-name field-name) value))
                    (required t))  ; Assume all fields are required for simplicity
               (when (and required (null field-value))
                 (error "Missing required field '%s' in struct '%s'" field-name struct-name))
               (when field-value
                 (instructor--validate-value type field-value (symbol-name field-name)))))))


;; (defun instructor-struct-to-function-args (struct-name)
;;   "Generate llm-function-arg definitions from STRUCT-NAME's slots."
;;   (let ((slots (get struct-name 'llm-struct-slots))
;;         (primitive-types '(string integer float boolean)))
;;     (mapcar
;;      (lambda (slot)
;;        (let ((name (symbol-name (nth 0 slot)))
;;              (type (nth 1 slot))
;;              (description (nth 2 slot)))
;;          (cond
;;           ((member type primitive-types)
;;            (make-llm-function-arg :name name :type type
;;                                   :description description :required t))
;;           ((and (listp type) (member (car type) '(enum list or)))
;;            (make-llm-function-arg :name name :type type
;;             :description description :required t))
;;           ;; Nested struct
;;           ((and (symbolp type) (get type 'llm-struct-slots))
;;            (make-llm-function-arg :name name
;;             :type `(object ,@(instructor-struct-to-function-args type))
;;             :description description :required t))
;;           (t
;;            (error "Unknown type: %s" type)))))
;;      slots)))

(defun instructor-struct-to-function-args (struct-name)
  "Generate llm-function-arg definitions from STRUCT-NAME's slots."
  (let ((slots (get struct-name 'llm-struct-slots))
        (primitive-types '(string integer float boolean)))
    (mapcar
     (lambda (slot)
       (let ((name (symbol-name (nth 0 slot)))
             (type (nth 1 slot))
             (description (nth 2 slot)))
         (cond
          ;; Check for primitive types
          ((member type primitive-types)
           (make-llm-function-arg
            :name name
            :type type
            :description description
            :required t))
          ;; Check for list types
          ((and (listp type) (eq (car type) 'list))
           (let ((element-type (cadr type)))
             (if (and (symbolp element-type) (get element-type 'llm-struct-slots))
                 ;; The list elements are structs, expand them
                 (make-llm-function-arg
                  :name name
                  :type `(list (object ,@(instructor-struct-to-function-args element-type)))
                  :description description
                  :required t)
               ;; The list elements are basic types or complex types
               (make-llm-function-arg
                :name name
                :type type
                :description description
                :required t))))
          ;; Check for complex types like (enum ...) or (or ...)
          ((and (listp type) (member (car type) '(enum or)))
           (make-llm-function-arg
            :name name
            :type type                  ; Pass the type as is
            :description description
            :required t))
          ;; Nested struct
          ((and (symbolp type) (get type 'llm-struct-slots))
           (make-llm-function-arg
            :name name
            :type `(object ,@(instructor-struct-to-function-args type))
            :description description
            :required t))
          (t
           (error "Unknown type: %s" type)))))
     slots)))


(defun instructor--struct-constructor (struct-name)
  "Create a lambda function to construct instances of STRUCT-NAME."
  (let* ((slots (get struct-name 'llm-struct-slots))
         (constructor (intern (concat "make-" (symbol-name struct-name)))))
    (lambda (&rest args)
      ;; If args is a single element which is a list, flatten it
      (when (and (= (length args) 1) (listp (car args)) (not (keywordp (caar args))))
        (setq args (car args)))
      (let ((values
             (cl-mapcar
              (lambda (slot arg)
                (let ((field-name (nth 0 slot))
                      (type (nth 1 slot)))
                  ;; Validate the field value
                  (instructor--validate-value type arg (symbol-name field-name))
                  ;; Process the value
                  (cond
                   ;; List type with struct elements
                   ((and (listp type) (eq (car type) 'list))
                    (let ((element-type (cadr type)))
                      (if (and (symbolp element-type) (get element-type 'llm-struct-slots))
                          ;; The list elements are structs, recursively construct them
                          (instructor--map-sequence
                           (lambda (elem)
                             (funcall (instructor--struct-constructor element-type) elem))
                           arg)
                        ;; The list elements are basic types or complex types
                        arg)))
                   ;; Nested struct
                   ((and (symbolp type) (get type 'llm-struct-slots))
                    (funcall (instructor--struct-constructor type) arg))
                   ;; Basic or complex type
                   (t arg))))
              slots args))
            (keys (mapcar (lambda (slot)
                            (intern (concat ":" (symbol-name (nth 0 slot)))))
                          slots)))
        (apply constructor (cl-mapcan #'list keys values))))))


(defun instructor--map-sequence (function sequence)
  "Apply FUNCTION to each element of SEQUENCE, returning a sequence of the same type."
  (cond
   ((listp sequence)
    (mapcar function sequence))
   ((vectorp sequence)
    (cl-map 'vector function sequence))
   (t
    (error "Unsupported sequence type: %s" (type-of sequence)))))


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
;;; instructor.el ends here
