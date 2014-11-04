#(#(;; 1 letter
    (:nouns)
    (:plurals))
  #(;; 2 letters
    (:nouns :plurals)
    (:adjectives :plurals)
    (:adjectives :nouns)
    (:adjectives :noun-phrases))
  #(;; 3 letters
    (:adjectives :adjectives :nouns)
    (:adjectives :adjectives :noun-phrases)
    (:adjectives :adjectives :plurals)
    (:plurals :verbs :plurals))
  #(;; 4 letters
    (:adjectives :adjectives "and" :adjectives :nouns)
    (:adjectives :adjectives "and" :adjectives :plurals)
    (:adjectives :plurals "that" :transitive-verbs :plurals)
    (:nouns "that" :adverbs :transitive-verbs :plurals))
  #(;; 5 letters
    (:adverbs :transitive-verbs :adjectives :adjectives :plurals)
    (:adjectives :plurals :transitive-verbs :adjectives :plurals)
    (:adjectives :adjectives :plurals :adverbs :intransitive-verbs)))
