(ns duodenum.parser
  (:require [clojure.string :as str]))

(defn token-kind
  "Given the state and token determine the kind of the token"
  [state token]
  (cond
    (= :done (get-in state [:context :processing]))
    :rest

    (empty? (:context state))
    :empty-context

    ;; Need to verify that we're not processing arguments here, I suppose
    (and (re-matches #"-\p{Alnum}" token)
         (->> state :options (map :short) (some #{token})))
    :short-option

    (re-matches #"-\p{Alnum}" token)
    :unknown-short-option

    :else
    :argument))

(defn init
  [parser args]
  (let [type (cond
               (and (:arguments parser) (:options parser)) :mixed
               (:arguments parser) :argument
               (:options parser) :option)]
    {;; Not-yet processed arguments
     :tokens (seq args)
     ;; Context should tell which argument or option we're collecting
     ;; for. For mixed content we have no context, thus we need to
     ;; make some additional work. For option only content the context
     ;; may be nil or current option being collected. For argument
     ;; content we need to have the argument name or else all unparsed
     ;; go to :unrecognized key
     :context nil
     ;; Processed but not understood
     :errors []
     :processed {}
     :arguments (->> (:arguments parser)
                     (map (fn [arg] {:count (:count arg) :name (:name arg)})))
     :options (->> (:options parser)
                   (map (fn [opt] [(:name opt) {:count (:count opt)
                                                :name (:name opt)
                                                :short-opt (:short-opt opt)
                                                :long-opt (:long-opt opt)
                                                :values []}]))
                   (into {}))}))

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
        :empty-context
        (cond
          (and (empty? (:options state))
               (not (empty? (:arguments state))))
          (-> state
              (update :tokens conj token)
              (assoc :context {:processing :argument :argument (first (:arguments state))})
              (update :arguments rest))

          (and (empty? (:options state))
               (empty? (:arguments state)))
          (-> state
              (update :tokens conj token)
              (assoc :context {:processing :done}))

          (and (not (empty? (:options state)))
               (empty? (:arguments state)))
          (-> state
              (assoc :context {:processing :unknown})
              (update :tokens conj token)))
        :rest
        (-> state
            ;; The token has not been processed, put it back in tokens list
            (update :tokens conj token)
            ;; Set the processing to done
            (assoc :context {:processing :done}))
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
                ;; Set to token to processed argument
                (assoc-in [:processed :arguments (:name argument)] token)
                ;; advance arguments; wonder if we need to handle
                ;; possible option popping up here?
                (assoc :context (if-let [argument (first (:arguments state))]
                                  {:processing :argument
                                   :argument argument}
                                  {:processing :done}))
                (update :arguments rest))

            (and (nil? current-value) (< 1 (:count argument)))
            ;; Append token to processed argument
            (assoc-in state [:processed :arguments (:name argument)] [token])

            (and (< (inc (count current-value)) (:count argument)))
            (update-in state [:processed :arguments (:name argument)] conj token)

            (and (= (inc (count current-value)) (:count argument)))
            (-> state
                ;; Append token to processed argument
                (update-in [:processed :arguments (:name argument)] conj token)
                ;; advance arguments
                (update :arguments rest))))

        :option-value
        (let [option (:option (:context state))]
          (update state :options conj {:option option :value token}))

        ;; Token is "-a"
        :short-option
        (do
          (prn "yes!")
          (update state :context {:processing :option :option token}))

        ;; Token is "-a" but we don't have similar token anywhere else
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
