(ns simple-fsm.core)

(declare run-machine)

(defn- run-transition [states alphabet transition {:keys [value] :as mach} tf args]
  (println "in run-transition: " mach ", " tf ", " args)
  ;;FIXME: note: this currently ignores args for non-fn transition results.  maybe we should error or fail?
  ;(println "in run-transition: " new-s-or-fn ", " args)
  (let [fs (if (sequential? tf) tf (vector tf))
        [new-state new-value] (reduce (fn [[nst nv] f] (apply f nv args)) (vector nil value) fs)]
    (if new-state
      (if (some #{new-state} states)
        (if (transition new-state :enter)
          ; Transitioning to a new state...automatically fire the :enter transition
          (run-machine states alphabet transition (assoc mach :state-key new-state :value new-value) :enter args)
          ; But only if it exists, otherwise just update the machine
          (assoc mach :state-key new-state :value new-value))
        (assoc mach :error? :missing-state :error-value new-state)
        )
      (assoc mach :value new-value))
    )
  )

(defn- run-machine [states alphabet transition {:keys [state-key value snooze error?] :as mach} a args]
  (println "in fsm: " mach ", " a ", " args)
  (if error?
    ; If the machine is in error, it means there's something wrong on my side.  Don't let the machine proceed while in an error state
    (println "Machine is in error! Machine: " mach)

    (if-let [t (transition state-key a)]
      (run-transition states alphabet transition mach t args)
      (when-let [t (transition state-key :any)]
        (run-transition states alphabet transition mach t [])))))

(defn initial [s]
  s)

(defn final [s & ss]
  (let [alls (concat [s] ss)]
    {:final-states alls}))

(defn- one-on [m v]
  {:pre [(= 3 (count v))]}
  (let [[s a action] v
        entry (assoc {} s (assoc {} a action))]
    (merge-with merge m entry)))

(defn on [s a action & more]
  (->> (concat [s a action] more)
       (partition 3 3 nil)
       (reduce one-on {})))

(defn go-to [s]
  (fn [value & _]
    (vector s value)))

(defn call [f & args]
  (fn [value & iargs]
    (let [allargs (concat args [value] iargs)]
      (apply f allargs)
      )
    )
  )

(defn init-state [initial value]
  {:state-key initial :history [] :snooze 0 :error? nil :error-value nil :value value})

(def uninitialized (init-state nil nil))

(defn build-fsm [initial final t & ts]
  (let [allts (apply merge-with merge t ts)
        states (keys allts)
        alphabet (reduce (fn [a [_ v]] (concat v (keys v))) [] allts)
        tf (fn [s a] (get-in allts [s a]))]
    (println "in build-fsm: " initial final allts)
    (fn [s a & args]
      ;(println "in fsm: " s ", " a ", " args)
      (if (= s uninitialized)
        ;; Start new
        (apply init-state initial args)
        ;; Step
        (run-machine states alphabet tf s a (or args []))    
        )
      )))
