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

(defn- advance
  [parser state]
  (if-let [unprocessed (seq (:unprocessed state))]
    (let [current (first unprocessed)
          state (assoc state :unprocessed (rest unprocessed))]
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
                   (into {}))
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

(defn option [])

(defn argument
  [name & {:keys [count] :or {count 1} :as args}]
  (if (and count (zero? count))
    (throw (ex-info "Count can not be zero" {:name name}))
    {:name name
     :args args
     :count count
     :kind :argument}))
