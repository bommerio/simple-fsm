(ns simple-fsm.core-test
  (:require [clojure.test :refer :all]
            [simple-fsm.core :refer :all]))

(deftest simple-fsm
  (testing "a simple fsm"
    (let [fsm (build-fsm (initial :begin)
                         (final :end)
                         (on :begin :go (go-to :end)))
          expected-first (init-state :begin nil)
          expected-last (init-state :end nil)
          ]
      ;; Initializing the machine
      (is (= expected-first (start-new fsm)))
      ;; An unrecognized transition returns nil
      (is (nil?             (step fsm expected-first :foo)))
      ;; Recognized transition
      (is (= expected-last  (step fsm expected-first :go)))
      ;; Absorbing state (:any action)
      (is (= expected-last  (step fsm expected-last :foo)))
      )))

(deftest fsm-with-initial-enter-fn
  (testing "fsm with enter function on initial state"
    (let [enter-called (atom false)
          fsm (build-fsm (initial :begin)
                         (final   :end)
                         (on :begin :enter (call (fn [v] (reset! enter-called true) (vector nil v)))
                             :begin :go (go-to :end)))]
      (start-new fsm)
      ;; Verify that the enter function is called
      (is (true? @enter-called))
      ))
  )

(deftest fsm-with-enter-fn
  (testing "fsm with enter function"
    (let [enter-called (atom false)
          fsm (build-fsm (initial :begin)
                         (final   :end)
                         (on :begin :go (go-to :end)
                             :end  :enter (call (fn [v] (reset! enter-called true) (vector nil v)))))
          expected-first (start-new fsm)
          expected-last (init-state :end nil)
          ]
      (is (false? @enter-called))
      (is (= expected-last  (step fsm expected-first :go)))
      ;; Verify that the enter function is called
      (is (true? @enter-called))
      ))

  )

(deftest fsm-with-initial-enter-fn-update-value
  (testing "fsm with enter function on initial state that updates value"
    (let [fsm (build-fsm (initial :begin)
                         (final   :end)
                         (on :begin :enter (call (fn [v] (vector nil (assoc v :enter-called true))))
                             :begin :go    (go-to :end)))
          expected-first (init-state :begin {:enter-called true})
          undertest      (start-new fsm {:enter-called false})
          ]
      ;; Verify that the enter function is called by testing the states with value
      (is (= expected-first undertest)))))

(deftest fsm-with-enter-fn-update-value
  (testing "fsm with enter function that updates value"
    (let [fsm (build-fsm (initial :begin)
                         (final   :end)
                         (on :begin :go (go-to :end)
                             :end  :enter (call (fn [v] (vector nil (assoc v :enter-called true))))))
          expected-first (init-state :begin {:enter-called false})
          expected-last  (init-state :end {:enter-called true})
          undertest      (start-new fsm {:enter-called false})
          ]
      ;; Verify that the enter function is called by testing the states with value
      (is (= expected-first undertest))
      (is (= expected-last  (step fsm undertest :go)))
      )))

(deftest on-definition
  (testing "on definition function"
    (let [go-to-1 (go-to :1)
          go-to-2 (go-to :2)
          go-to-3 (go-to :3)
          expected {:begin {:a go-to-1}
                    :1     {:b go-to-2
                            :c go-to-3}
                    :2     {:c go-to-3}
                    :3     {:any go-to-3}}
          actual  (on :begin :a go-to-1
                      :1 :b go-to-2
                      :1 :c go-to-3
                      :2 :c go-to-3
                      :3 :any go-to-3)]
      (is (= expected actual)))))
