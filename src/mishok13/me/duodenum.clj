(ns mishok13.me.duodenum)

(defn parser
  [& opts]
  (when opts
    {:min-count (reduce + (map (fn get-option-count
                                 [opt]
                                 (or (when-let [count (:count opt)]
                                       (when (and (number? count) (pos? (:count opt)))
                                         (:count opt)))
                                     0))
                               opts))
     :arguments (seq (filter (fn [opt] (= :argument (:kind opt))) opts))
     :options (seq (filter (fn [opt] (= :option (:kind opt))) opts))}))

(defn- initial
  [parser args]
  (let [type (cond
               (and (:arguments parser) (:options parser)) :mixed
               (:arguments parser) :arguments
               (:options parser) :options)
        context (when (= type :arguments)
                  (:name (first (:arguments parser))))]
    {;; Not-yet processed arguments
     :unprocessed (seq args)
     ;; Everything that matches goes here.
     :processed []
     ;; Context should tell which argument or option we're collecting
     ;; for. For mixed content we have no context, thus we need to
     ;; make some additional work. For option only content the context
     ;; may be nil or current option being collected. For argument
     ;; content we need to have the argument name or else all unparsed
     ;; go to :unrecognized key
     :context context
     ;; Processed but not understood
     :unrecognized []
     :arguments (->> (:arguments parser)
                     (map (fn [arg] [(:name arg) {:count (:count arg)
                                                  :name (:name arg)
                                                  :values []}]))
                     (into {}))
     :options (->> (:options parser)
                   (map (fn [opt] [(:name opt) {:count (:count opt)
                                                :name (:name opt)
                                                :short-opt (:short-opt opt)
                                                :long-opt (:long-opt opt)
                                                :values []}]))
                   (into {}))
     ;; Type denotes whether we're parsing options or
     ;; arguments currently
     :type type}))

(defn- terminator?
  "Determine whether the provided token is a terminator string \"--\""
  [state token]
  (and
   ;; A terminator is only valid if:
   ;; * parser expects mixed input
   (= :mixed (:content (:parser state)))
   ;; * context is still mixed
   (= :mixed (:content (:context state)))
   ;; * option is not still being parsed
   (or (not (:option (:context state)))
       ;; or if it's being parsed, but the number of arguments has
       ;; satisfied the requirement for minimum number of arguments
       (>= (count (:acc (:context state)))
           (:min-nargs (get (:options (:parser state)) (:option (:context state))))))))

(defn- advance-terminator
  [state _]
  ;; Token is ignored, since it's only used as a sort of "switch" in
  ;; this case
  ;;
  ;; We need to make sure the context is cleared and switched
  ;; "arguments-only" mode. Other than that we don't need much.
  (-> state
      ;; Clear context
      (dissoc :context)
      ;; Set the context to "arguments-only" with no current arguments
      (assoc :context {:content :arguments :argument nil})))

(defn- long-option?
  [state token]
  (comment
    (and
     ;; Parser has options
     (contains? #{:mixed :options} (:content (:parser state)))
     ;; State is either mixed or options-only
     (contains? #{:mixed :options} (:content (:context state)))
     ;; Looks like a long option
     (re-matches #"--[a-zA-Z0-9][a-zA-Z0-9-]*" token)
     ;; Exists in the list of options provided to parser
     (contains? (:long-options (:parser state)))
     ;; Is not being parsed
     (= token (as-token (current-option state))))))

(defn- short-option?
  [state token]
  (comment
    ;; Parser has options
    (contains? #{:mixed :options} (:content (:parser state)))
    ;; State is either mixed or options-only
    (contains? #{:mixed :options} (:content (:context state)))
    ))

(defn- argument?
  [state token]
  (or (= :arguments (:context state)) (not (.startsWith token "-"))))

(defn- advance
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
    ;;
    (let [current (first unprocessed)
          state (assoc state :unprocessed (rest unprocessed))]
      (comment
        (cond
          ;; there are N types of entry:
          ;; * "abcdef"
          (arg?) (depends on context)
          ;; * "-a"
          (short-opt?) (check if exists, switch context)
          ;; * "-abc"
          (many-short-opts?) (split-em, if they are all valid boolean options put em back "-a", "-b", "-c", otherwise put "-a" + "bc" back in)
          ;; * "--foo"
          (long option?) (switch the context)
          ;; * "--foo=bar"
          (long-option-with-arg?) (split-em-and-put-em-back)
          ;; * "--"
          (terminator?) (switch the context)
          ))
      (cond
        ;; We switch to argument-only processing here.
        (and (= :mixed (:type state)) (= "--" current))
        (assoc state :type :arguments)

        ;; For empty context arguments
        (and (= :arguments (:type state)) (nil? (:context state)))
        (update state :unrecognized conj current)

        ;; We have context, let's assign the value and update context
        (= :arguments (:type state))
        (let [name (:context state)
              current-arg-value (get-in state [:arguments name])
              updated-arg-value (update current-arg-value :values conj current)]
          (if (= (:count updated-arg-value) (count (:values updated-arg-value)))
            (-> state
                (assoc :context nil)
                (assoc-in [:arguments name] updated-arg-value))
            (assoc-in state [:arguments name] updated-arg-value)))))

    (assoc state :done? true)))

(defn- scalar?
  [arg]
  (= 1 (:count arg)))

(defn- render-result
  [parser state]
  {:arguments (->> parser
                   :arguments
                   (map (fn [arg]
                          (let [values (:values (get (:arguments state) (:name arg)))]
                            (cond
                              (and (= (count values) (:count arg)) (scalar? arg))
                              [(:name arg) (first values)]

                              (and (= (count values) (:count arg)) (not (scalar? arg)))
                              [(:name arg) values]))))
                   (into {})
                   not-empty)
   :options nil
   :unparsed (seq (:unrecognized state))})

(defn parse
  "Given a parser and list of arguments, parse them"
  [parser args]
  ;; Pre-parsing stage, where we split things into
  (when parser
    (loop [state (initial parser args)]
      (if (:done? state)
        (render-result parser state)
        (recur (advance parser state))))))

(defn group [])
(defn command
  [name]
  {:command "name"})

(defn option
  [name & {:keys [short long count kind] :or {count 0}}]
  ;; When count is 0, option is presumed to be a "boolean"
  (assert (or short long))
  (let [kind (or kind (if (not= 0 count) :boolean :counter))]
    {:name name
     :long long
     :short short
     :count count
     :kind kind}))

(defn argument
  [name & {:keys [count] :or {count 1} :as args}]
  (if (and count (zero? count))
    (throw (ex-info "Count can not be zero" {:name name}))
    {:name name
     :args args
     :count count
     :kind :argument}))
