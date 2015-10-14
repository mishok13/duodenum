(ns mishok13.me.duodenum.parser
  (:require [clojure.string :as str]))

(defn- token-kind
  "Given the state and token determine the kind of the token"
  [state token]
  :argument)

(defn advance
  [state]
  (if-let [tokens (seq (:tokens state))]
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
    (let [token (first tokens)
          tokens (rest tokens)
          state (assoc state :tokens tokens)]
      (case (token-kind state token)
        ;; there are N types of entry:
        ;; * "abcdef"
        :argument
        (let [argument (get-in state [:context :argument])
              current-value (get-in state [:processed :arguments (:name argument)])]
          (cond
            ;; When the value is nil we'd like to
            (and (nil? current-value) (= 1 (:count argument)))
            ;; Need to switch context to next argument!
            (-> state
                (assoc-in [:processed :arguments (:name argument)] token)
                (assoc :context {:processing :done})))
          ;; Need to update the context if argument has reached its count
          ;; (update-in state [:processed :arguments] conj {:argument argument :value token})
          )

        :option-value
        (let [option (:option (:context state))]
          (update state :options conj {:option option :value token}))

        ;; Token is "-a"
        :short-option
        (update state :context {:processing :option :option token})

        :unknown-short-option
        ;; We don't have to switch context here since the option has
        ;; been bogus
        (update state :errors conj {:kind :unknown-short-option :token token})

        ;; Token is "-abc"

        ;; "-abc" consists of 3 valid boolean options, we have to
        ;; process this token as if it was three separate tokens:
        ;; "-a", "-b", "-c"
        :multiple-short-options
        (assoc state :tokens (vec (concat (->> token rest (map str)) tokens)))

        :short-option-with-value
        (assoc state :tokens (vec (concat [(subs token 0 2) (subs token 2)] tokens)))
        ;; * "--foo"
        :long-option
        ;; FIXME: need to get the name of the option by token
        (assoc state :context {:option token :processing :option})
        ;; * "--foo=bar"
        :long-option-with-value
        (assoc state :tokens (vec (concat (str/split token #"=" 2) tokens)))
        ;; * "--"
        :terminator
        ;; FIXME: Which argument is it processing?
        (assoc state :context {:argument nil :processing :argument})
        (throw (ex-info "what" {:token token :state state}))))
    (assoc state :context {:processing :done})))
