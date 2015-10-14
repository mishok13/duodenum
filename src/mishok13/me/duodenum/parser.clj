(ns mishok13.me.duodendum.parser)

(defn- token-kind
  "Given the state and token determine the kind of the token"
  [state token]
  )

(defn advance
  [parser state]
  (if-let [unprocessed (seq (:unprocessed state))]
    ;; XXX: This needs to switch context on certain conditions, and on
    ;; certain not so much. I wonder how to handle this without
    ;; creating a multi-page function?
    ;;
    ;; "-abc" -> "-a -b -c" in case all of these are options. If -a
    ;; takes argument, then identical to -a bc. Otherwise -- unknown?
    ;;
    ;; --foo=bar is identical to --foo bar
    ;;
    ;; -fbar identical to -f bar
    ;;
    ;; XXX: wtf happens if option is specified twice?
    ;; XXX: edge case -ab where a is collector and b is boolean
    ;;
    ;;
    (let [token (first unprocessed)
          state (assoc state :unprocessed (rest unprocessed))]
      (comment
        (case (token-kind state token)
          ;; there are N types of entry:
          ;; * "abcdef"
          :argument (depends on context)
          :option-value (baz)
          ;; * "-a"
          :short-option (check if exists, switch context)
          :unknown-short-option
          (-> state
              (update :))
          ;; * "-abc"
          :multiple-short-options (bar)
          :short-option-with-value
          (foo)
          ;; * "--foo"
          :long-option (long option?) (switch the context)
          ;; * "--foo=bar"
          :long-option-with-value (split-em-and-put-em-back)
          ;; * "--"
          :terminator (switch the context))))
    (assoc state :done? true)))
