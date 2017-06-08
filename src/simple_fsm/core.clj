(ns simple-fsm.core
  (:require [clojure.tools.logging :as log]))

(declare run-machine)

(defn- call-one-transition-fn [[nst nv] f args]
  {:post [(and (vector? %)
               (= 2 (count %)))]}
  (log/debug "in call-one-transition-fn " nst ", " nv ", " f ", " args)
  (apply f nv args))

;; TODO: combine with run-machine
(defn- run-enter-if-available [states alphabet transition {:keys [state-key] :as mach} args]
  (log/debug "in run-enter-if-available: " states alphabet transition mach args)
  (if (transition state-key :enter)
    ; Transitioning to a new state...automatically fire the :enter transition
    (run-machine states alphabet transition mach :enter args)
    ; But only if it exists, otherwise just return the existing state
    mach))

(defn- run-transition [states alphabet transition {:keys [value] :as mach} tf args]
  (log/debug "in run-transition: " mach ", " tf ", " args)
  ;;FIXME: note: this currently ignores args for non-fn transition results.  maybe we should error or fail?
  (let [fs (if (sequential? tf) tf (vector tf))
        [new-state new-value] (reduce #(call-one-transition-fn %1 %2 args) (vector nil value) fs)]
    (if new-state
      (if (some #{new-state} states)
        (as-> mach mach
          (assoc mach :state-key new-state :value new-value)
          (run-enter-if-available states alphabet transition mach args)) 
        (assoc mach :error? :missing-state :error-value new-state))
      (assoc mach :value new-value))
    )
  )

(defn- run-machine [states alphabet transition {:keys [state-key value snooze error?] :as mach} a args]
  (log/debug "in run-machine: " states alphabet transition mach a args)
  (if error?
    ; If the machine is in error, it means there's something wrong on my side.  Don't let the machine proceed while in an error state
    (log/error "Machine is in error! Machine: " mach)

    (if-let [t (transition state-key a)]
      (run-transition states alphabet transition mach t args)
      (when-let [t (transition state-key :any)]
        (run-transition states alphabet transition mach t [])))))

(defn initial [s]
  s)

(defn final [s & ss]
  (let [alls (concat [s] ss)]
    alls))

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
    {:post [(and (vector? %)
                 (= 2 (count %)))]}
    (let [allargs (concat args [value] iargs)]
      (apply f allargs)
      )))

(defn init-state [initial value]
  {:state-key initial :history [] :snooze 0 :error? nil :error-value nil :value value})

(defprotocol IStateMachine
  (in-terminal-state? [this s])
  (start-new [this]
             [this value])
  (step [this s a]
        [this s a args])
  )

(defn build-fsm [initial final t & ts]
  (let [final-set (set final) 
        absorbing (map #(on %1 :any (go-to %1)) final-set)
        allts (apply merge-with merge t (concat ts absorbing))
        states (set (keys allts))
        alphabet (reduce (fn [a [_ v]] (concat v (keys v))) [] allts)
        tf (fn [s a] (get-in allts [s a]))]
    (log/debug "in build-fsm: " initial final allts)
    ;; FIXME: allows zero or 1 arg, but won't allow arbitrary args or error if more args are passed in
    (reify IStateMachine
      (in-terminal-state? [_ {:keys [state-key]}]
        (some? (final-set state-key)))
      (start-new [this]
        (start-new this nil))
      (start-new [_ value]
        (as-> (init-state initial value) mach
          (run-enter-if-available states alphabet tf mach [])))
      (step [this s a]
        (step this s a nil))
      (step [_ s a args]
        (run-machine states alphabet tf s a (or args [])))
      )))
