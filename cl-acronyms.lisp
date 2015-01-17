(defpackage :info.isoraqathedh.cl-acronyms
  (:use :cl)
  (:nicknames #:cl-acronyms)
  (:export #:expand
           #:*word-list-file-location*
           #:*master-word-list*
           #:*master-structures-list*
           #:total-entries
           #:total-structures
           #:refresh-list
           #:refresh-structures
           #:reset-list))

(in-package :cl-acronyms)

;;; Global variables

(defparameter *word-list-file-location* (asdf:system-relative-pathname 'cl-acronyms "mobyposi" :type "i")
  "Location of which the list of words with part of speech modifiers are located.")

(defparameter *structures-file-location* (asdf:system-relative-pathname 'cl-acronyms "structures" :type "lisp")
  "Location of which the list of structures is located.")

(defparameter *report-stream* *standard-output*
  "Certain functions will print a progress bar in reading files. This is where it is printed to.")

;;; Word list and associated functions maintaining it

(defun %reset-list ()
  "Blank list for *master-word-list*"
  (loop with ht = (make-hash-table)
        for part-of-speech
          in '(:nouns              ; Noun                            N
               :plurals            ; Plural                          p
               :noun-phrases       ; Noun Phrase                     h
               :verbs              ; Verb (usu participle)           V
               :transitive-verbs   ; Verb (transitive)               t
               :intransitive-verbs ; Verb (intransitive)             i
               :adjectives         ; Adjective                       A
               :adverbs            ; Adverb                          v
               :conjunctions       ; Conjunction                     C
               :prepositions       ; Preposition                     P
               :interjections      ; Interjection                    !
               :pronouns           ; Pronoun                         r
               :definite-articles  ; Definite Article                D
               :indef-articles     ; Indefinite Article              I
               :nominatives)       ; Nominative                      o
        do (setf (gethash part-of-speech ht) (make-array 1500 :adjustable t :fill-pointer 0))
        finally (return ht)))

(defun POS-associate-number (letter)
  "Turns the codes used by mobiposi.i into keywords corresponding to keys in the hash table."
  (loop for i-letter in '(#\N #\p #\h #\V #\t #\i #\A #\v #\C #\P #\! #\r #\D #\I #\o)
        for i-key in '(:nouns :plurals :noun-phrases :verbs :transitive-verbs
                       :intransitive-verbs :adjectives :adverbs :conjunctions :prepositions
                       :interjections :pronouns :definite-articles :indef-articles :nominatives)
        for j from 0
        when (etypecase letter
               (character (char= letter i-letter))
               (keyword (eql letter i-key)))
          return j
        finally (error "Not a recognized letter or keyform: ~s" letter)))

(defun letter-associate-number (letter)
  "Turns A = 0, B = 1, ..., Z = 25"
  (- (or (digit-char-p letter 36) (error "Not a recognized letter: ~@C" letter)) 10))

(defparameter *master-word-list* (%reset-list)
  "The word list holding all words and their parts of speech.")

(defparameter *word-count-array* (make-array '(15 26) :element-type 'fixnum)
  "An array counting the number of words satisfying a given first letter and a given part of speech.")

(defun get-dictionary-length (POS-letter start-letter)
  "Returns how many words there are in the word."
  (aref *word-count-array* (POS-associate-number POS-letter) (letter-associate-number start-letter)))

(defun total-entries ()
  "Returns the total length the dictionary."
  (loop for i being the hash-values of *master-word-list*
        sum (length i)))

(defun reset-list ()
  "Delete all entries from *master-word-list*. Returns t on success."
  (and (setf *master-word-list* (%reset-list)
             *word-count-array* (make-array '(15 26) :element-type 'fixnum))
       t))

(defun refresh-list ()
  "Reloads the list from mobiposi.i, replacing all entries orginally in *master-word-list*. 
Returns the number of items placed into the word bank.
Skips any entries that have spaces."
  (reset-list) ; clear the original list.
  (with-open-file (stream *word-list-file-location* :external-format :utf-8)
    (loop initially (format *report-stream* "~&Reading wordlist from file ~s: " *word-list-file-location*)
          with read = 0
          for j from 0
          for text              = (read-line stream nil) while text
          for word              = (subseq text 0 (position #\× text))
          for codes             = (subseq text (1+ (position #\× text)))
          for first-letter-then = #\Newline then first-letter-now ; #\Newline is guaranteed to never be part of a word.
          for first-letter-now  = (aref word 0)
          when (char-not-equal first-letter-then first-letter-now)
            do (princ (char-upcase first-letter-now))
          when (and (not (find #\Space word)) ; Forbid multi-word phrases to enter.
                    (or (char<= #\A (char word 0) #\Z)  ; Must start with an uppercase…
                        (char<= #\a (char word 0) #\z)));…or lowerccase letter.
            do (map nil
                    #'(lambda (p)
                        (vector-push-extend word (gethash (decode-POS-letter p) *master-word-list*))
                        (incf (aref *word-count-array* (POS-associate-number p) (letter-associate-number (char word 0)))))
                    codes)
               (incf read)
          finally
             (format *report-stream* "~%Read ~:d entries, out of ~:d total in file." read j)
             (return read))))

;;; Structures list and functions maintaining it

(defun %read-structures ()
  "Reads the structures from structures.lisp."
  (with-open-file (structures-file *structures-file-location* :external-format :utf-8)
    (let ((*read-eval* nil)) ;; Prevents #. from working; there's no need for it anyway.
      (format *report-stream* "~&Reading structures from file ~s." *structures-file-location*)
      (read structures-file))))

(defparameter *master-structures-list* (%read-structures)
  "The vector that holds all structures.")

(defun refresh-structures ()
  "Resets the structure list to the value seen at the structures.lisp file."
  (setf *master-structures-list* (%read-structures)))

(defun total-structures ()
  "Returns the total number of structures."
  (reduce #'+ *master-structures-list* :key #'length))

;;; Functions relating to building an acronym

(defun decode-POS-letter (letter)
  "Turns the codes used by mobiposi.i into keywords corresponding to keys in the hash table."
  (ccase letter
    (#\N :nouns             )
    (#\p :plurals           )
    (#\h :noun-phrases      )
    (#\V :verbs             )
    (#\t :transitive-verbs  )
    (#\i :intransitive-verbs)
    (#\A :adjectives        )
    (#\v :adverbs           )
    (#\C :conjunctions      )
    (#\P :prepositions      )
    (#\! :interjections     )
    (#\r :pronouns          )
    (#\D :definite-articles )
    (#\I :indef-articles    )
    (#\o :nominatives       )))

(defun generate-word (start-letter)
  "Generates a (nonsensical) word that starts with the given letter."
  ;; Should probably split this away into a separate package.
  (let ((phonemes (loop with ht = (make-hash-table)
                        for (i j) in '((:nasals "mn")
                                       (:plosives "pbtdkgc")
                                       (:fricatives "fvszh")
                                       (:liquids "lr")
                                       (:approximants "yw")
                                       (:vowels "aeiou")
                                       (:consonants "bcdfghjklmnpqrstvwxyz"))
                        do (setf (gethash i ht) j)
                        finally (return ht)))
        (syllable-onsets #((:consonants)
                           (:plosives :liquids)
                           (:plosives :fricatives)
                           (:fricatives :liquids)))
        (syllable-nuclei #((:vowels)))
        (syllable-codas #((:consonants)
                          (:plosives :fricatives)
                          ())))
    (with-output-to-string (out)
      (loop initially (princ start-letter out)
            for onset = (alexandria:random-elt syllable-onsets)
            for nucleus = (alexandria:random-elt syllable-nuclei)
            for coda = (alexandria:random-elt syllable-codas)
            for next-syllable-p = t then (< 3 (random 7))
            for first-syllable-p = t then nil
            while next-syllable-p
            when (and first-syllable-p
                      (not (find start-letter (gethash :consonants phonemes))))
              do (loop for i in onset
                       do (princ (alexandria:random-elt (gethash i phonemes)) out))
            do (loop for i in nucleus
                     do (princ (alexandria:random-elt (gethash i phonemes)) out))
               (loop for i in coda
                     do (princ (alexandria:random-elt (gethash i phonemes)) out))))))

(defun random-word (part-of-speech &optional letter)
  "Grabs a random word starting with the given part of speech starting with letter, if given.
If part-of-speech is supplied as a word, then that word is used, discarding letter.
[This is so to facilitate build-acronym].
If a letter is supplied, will only supply words that start with that letter;
if no such word exists, then a nonce word is generated instead."
  (let ((pos-hash-table (gethash part-of-speech *master-word-list*)))
    (cond ((stringp part-of-speech) part-of-speech)
          ((member letter '(:any nil t)) (alexandria:random-elt pos-hash-table))
          (t (if (zerop (get-dictionary-length part-of-speech letter))
                 (format nil "~@(~a~)" (generate-word letter))
                 (loop with threshold = (random
                                         (get-dictionary-length part-of-speech letter))
                       for i across pos-hash-table
                       when (char-equal letter (aref i 0))
                         do (decf threshold)
                       when (zerop threshold)
                         return (format nil "~@(~a~)" i)))))))

(defun get-POS-template (acronym)
  "Returns an appropriate part-of-speech template for a given acronym, ready for use in build-backronym."
  ;; MAYBE: Do common (and irregular) substitutions here, like 2 → to, 4 → for
  (if (find #\Space acronym)
      (loop for i = 0 then (1+ j)
            as j = (position #\Space acronym :start i)
            append (get-POS-template (subseq acronym i j))
            when j collect '("and")
            while j)
      (flet ((%get-POS-template (length)
               (alexandria:random-elt (aref *master-structures-list* (1- length)))))
        (if (<= (length acronym) (length *master-structures-list*))
            ;; Randomly pick a template to use.
            (%get-POS-template (length acronym))
            ;; If the acronym is too long for any precomposed templates,
            ;; Join up existing templates with prepositions into a final template.
            (loop with length-of-structures-list = (length *master-structures-list*)
                  with length-of-acronym         = (length acronym)
                  for i                          = (1+ (random length-of-structures-list))
                  and target                     = length-of-acronym then (- target i)
                  while (< length-of-structures-list target)
              append (%get-POS-template i) into final-list
                  collect (random-word :prepositions) into final-list
                  finally (return (append final-list
                                          (%get-POS-template target))))))))

(defun letterp (thing)
  (or (char<= #\a thing #\z) (char<= #\A thing #\Z)))

(defun %expand (acronym)
  "Main acronym expansion logic."
  (with-output-to-string (out-string)
    (loop for part-of-speech in (get-POS-template acronym)
          with pointer = 0
          for firstp = t then nil
          for letter = (aref acronym pointer)
          do (typecase part-of-speech
               (string (format out-string "~:[ ~;~]~a"
                               (or firstp (not (letterp (aref part-of-speech 0))))
                               part-of-speech))
               (list
                (format out-string "~:[ ~;~]~a"
                        firstp
                        (first part-of-speech))
                (incf pointer))
               (t
                (format out-string "~:[ ~;~]~a"
                        firstp (random-word part-of-speech letter))
                (incf pointer)))
          while (< pointer (length acronym)))))

(defun expand (acronym &optional (times 1 times-supplied-p))
  "Expands an acronym. If 'times' is provided, repeats expansion that many times and collects results into a list."
  ;; Remove all non-letter characters.
  ;(setf acronym (delete-if-not #'letterp acronym))
  (if times-supplied-p
      (loop repeat times collect (%expand acronym))
      (%expand acronym)))
  
;;; Autoload the list
(refresh-list)
