;;; -*- lexical-binding: t -*-
(require 'buttercup)
(require 'ellama)
(require 'instructor)

(setq llm (make-llm-ollama :chat-model "llama3.1"))

(describe
 "instructor-defstruct"
 (it
  "defines a struct and records metadata"

  (instructor-defstruct test-struct
                        (field1 :type string :description "A string field")
                        (field2 :type integer :description "An integer field"))

  ;; Check that the struct functions are defined
  (expect (functionp 'make-test-struct) :to-be-truthy)
  (expect (functionp 'test-struct-field1) :to-be-truthy)
  (expect (functionp 'test-struct-field2) :to-be-truthy)

  ;; Check that metadata is recorded
  (let ((metadata (get 'test-struct 'llm-struct-slots)))
    (expect metadata :to-equal
            '((field1 string "A string field")
              (field2 integer "An integer field"))))))

(describe
 "instructor-struct-to-function-args"
 (it
  "generates function args for primitive types"

  (instructor-defstruct primitive-struct
                        (string-field :type string :description "A string field")
                        (integer-field :type integer :description "An integer field")
                        (boolean-field :type boolean :description "A boolean field"))

  (let ((args (instructor-struct-to-function-args 'primitive-struct)))
    (expect (length args) :to-equal 3)
    (expect (mapcar #'llm-function-arg-name args)
            :to-equal '("string-field" "integer-field" "boolean-field"))
    (expect (mapcar #'llm-function-arg-type args)
            :to-equal '(string integer boolean))))

 (it
  "handles nested structs correctly"

  (instructor-defstruct child-struct
                        (child-field :type string :description "Child field"))

  (instructor-defstruct parent-struct
                        (parent-field :type child-struct :description "Nested child struct"))

  (let ((args (instructor-struct-to-function-args 'parent-struct)))
    (expect (length args) :to-equal 1)
    (let ((arg (car args)))
      (expect (llm-function-arg-name arg) :to-equal "parent-field")
      ;; The type should be an object with nested arguments
      (expect (llm-function-arg-type arg) :to-be-truthy)
      (let ((nested-args (cdr (llm-function-arg-type arg))))
        (expect (length nested-args) :to-equal 1)
        (let ((nested-arg (car nested-args)))
          (expect (llm-function-arg-name nested-arg) :to-equal "child-field")
          (expect (llm-function-arg-type nested-arg) :to-equal 'string)))))))

(describe
 "instructor--struct-constructor"
 (it
  "constructs instances from given arguments"

  (instructor-defstruct constructor-test
                        (name :type string)
                        (age :type integer))

  (let ((constructor (instructor--struct-constructor 'constructor-test))
        (args '("Alice" 30)))
    (let ((instance (apply constructor args)))
      (expect (constructor-test-p instance) :to-be-truthy)
      (expect (constructor-test-name instance) :to-equal "Alice")
      (expect (constructor-test-age instance) :to-equal 30)))))

(describe
 "instructor-call"
 (it
  "generates instances from the LLM"
  (instructor-defstruct person
                        (name :type string :description "Person's name")
                        (age :type integer :description "Person's age"))

  (let ((results (instructor-call
                  :llm llm
                  :type 'person
                  :prompt "Generate two people with a name and age.")))

    (expect (listp results) :to-be-truthy)
    (expect (length results) :to-be-greater-than 1)
    (dolist (person-instance results)
      (expect (person-p person-instance) :to-be-truthy)
      (expect (person-name person-instance) :to-be-truthy)
      (expect (person-age person-instance) :to-be-truthy)))))
