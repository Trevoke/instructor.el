;;; -*- lexical-binding: t -*-
(require 'buttercup)
(require 'ellama)
(require 'instructor)

(setq llm (make-llm-ollama :chat-model "llama3.1"))

(instructor-defstruct employee
                      (name :type string :description "The employee's name")
                      (position :type string :description "The employee's position")
                      (years-of-experience :type integer :description "Years of experience"))

(instructor-defstruct company
                      (name :type string :description "The company's name")
                      (employees :type (list employee) :description "A list of employees"))

(describe
 "Nested struct behavior with company and employees"
 (it
  "handles nested structs correctly in function arguments"
  ;; Generate function arguments from the company struct
  (let ((args (instructor-struct-to-function-args 'company)))
    ;; We expect two arguments: name and employees
    (expect (length args) :to-equal 2)
    (let ((name-arg (nth 0 args))
          (employees-arg (nth 1 args)))
      ;; Check the name argument
      (expect (llm-function-arg-name name-arg) :to-equal "name")
      (expect (llm-function-arg-type name-arg) :to-equal 'string)
      ;; Check the employees argument
      (expect (llm-function-arg-name employees-arg) :to-equal "employees")
      ;; The type should be a list of employee structs
      (expect (llm-function-arg-type employees-arg)
              :to-equal `(list (object ,@(instructor-struct-to-function-args 'employee)))))))

 (it
  "constructs nested instances from given arguments"

  ;; Get the constructor function for the company struct
  (let* ((constructor (instructor--struct-constructor 'company))
        (company-name "TechCorp")
        (employee1 '("Alice" "Developer" 5))
        (employee2 '("Bob" "Designer" 3))
        (employees-list (vector employee1 employee2)))
    ;; Construct the company instance
    (let ((instance (funcall constructor company-name employees-list)))
      (expect (company-p instance) :to-be-truthy)
      (expect (company-name instance) :to-equal "TechCorp")
      ;; Check employees
      (let ((employees (company-employees instance)))
        (expect (length employees) :to-equal 2)
        ;; Check first employee
        (let ((emp1 (aref employees 0)))
          (message "%S" employees)
          (message "%S" emp1)
          (message "%S" (employee-p emp1))
          (expect (employee-p emp1) :to-be-truthy)
          (expect (employee-name emp1) :to-equal "Alice")
          (expect (employee-position emp1) :to-equal "Developer")
          (expect (employee-years-of-experience emp1) :to-equal 5))
        ;; Check second employee
        (let ((emp2 (aref employees 1)))
          (expect (employee-p emp2) :to-be-truthy)
          (expect (employee-name emp2) :to-equal "Bob")
          (expect (employee-position emp2) :to-equal "Designer")
          (expect (employee-years-of-experience emp2) :to-equal 3))))))

 (it "expands nested structs inside list types"

     ;; Call instructor-struct-to-function-args
     (let ((args (instructor-struct-to-function-args 'company)))
       ;; We expect two arguments: name and employees
       (expect (length args) :to-equal 2)
       (let ((name-arg (nth 0 args))
             (employees-arg (nth 1 args)))
         ;; Check the name argument
         (expect (llm-function-arg-name name-arg) :to-equal "name")
         (expect (llm-function-arg-type name-arg) :to-equal 'string)
         ;; Check the employees argument
         (expect (llm-function-arg-name employees-arg) :to-equal "employees")
         ;; The type should be a list of objects with employee fields
         (let ((expected-employee-type
                `(list (object
                        ,@(list
                           (make-llm-function-arg :name "name" :description "The employee's name" :type 'string :required t)
                           (make-llm-function-arg :name "position" :description "The employee's position" :type 'string :required t)
                           (make-llm-function-arg :name "years-of-experience" :description "Years of experience" :type 'integer :required t))))))
           (expect (llm-function-arg-type employees-arg) :to-equal expected-employee-type)))))


 ;; (it
 ;;  "generates nested instances from the LLM"

 ;;  ;; Use instructor-call to generate a company instance
 ;;  (let ((results (instructor-call
 ;;                  :llm llm
 ;;                  :type 'company
 ;;                  :prompt "Generate a company with two employees.")))
 ;;    ;; Check that we get a list with one company instance
 ;;    (expect (listp results) :to-be-truthy)
 ;;    (expect (length results) :to-be-greater-than 1)
 ;;    (let ((company-instance (car results)))
 ;;      (expect (company-p company-instance) :to-be-truthy)
 ;;      ;; Check the company's name
 ;;      (expect (company-name company-instance) :to-be-truthy)
 ;;      ;; Check employees
 ;;      (let ((employees (company-employees company-instance)))
 ;;        (expect (length employees) :to-be >= 1)
 ;;        (dolist (emp employees)
 ;;          (expect (employee-p emp) :to-be-truthy)
 ;;          (expect (employee-name emp) :to-be-truthy)
 ;;          (expect (employee-position emp) :to-be-truthy)
 ;;          (expect (employee-years-of-experience emp) :to-be-truthy))))))
 )
