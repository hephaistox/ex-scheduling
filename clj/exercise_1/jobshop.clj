(ns jobshop
  "101 Scheduling
  Exercise 1 - Jobshop schedule
  Variant - Idiomatic clojure example")

(def js-pb-example
  "Data from the exercise"
  {:J1 [{:p 9 :m :M0 :id :op1} {:p 9 :m :M11 :id :op2} {:p 4 :m :M2 :id :op3}]
   :J2 [{:p 16 :m :M0 :id :op4} {:p 4 :m :M1 :id :op5} {:p 15 :m :M2 :id :op6}]})

(defn rep-rep-init
  "Build the canonical repetition representation,
  All operations of first operation, then all operations of second operation, ..."
  [js-pb]
  (vec
   (apply concat
          (map (fn [[job-id job-ops]]
                 (repeat (count job-ops) job-id))
               js-pb))))
(comment
  (rep-rep-init js-pb-example)
  ;; => [:J1 :J1 :J1 :J2 :J2 :J2]
  )

(defn decode-rep
  "Turns the representation with repetition to a topological order"
  [js-pb rep]
  (loop [rep rep
         js-pb js-pb
         decoded-rep []]
    (if rep
      (let [job (first rep)
            job-sequence (job js-pb)]
        (recur (next rep)
               (assoc js-pb job
                      (next job-sequence))
               (conj decoded-rep
                     (assoc (first job-sequence)
                            :n job))))
      decoded-rep)))

(comment
  (decode-rep js-pb-example
              (rep-rep-init js-pb-example))
  ;; => [{:p 9, :m :M0, :id :op1, :n :J1}
  ;;     {:p 9, :m :M11, :id :op2, :n :J1}
  ;;     {:p 4, :m :M2, :id :op3, :n :J1}
  ;;     {:p 16, :m :M0, :id :op4, :n :J2}
  ;;     {:p 4, :m :M1, :id :op5, :n :J2}
  ;;     {:p 15, :m :M2, :id :op6, :n :J2}]
  )

(defn evaluate-schedule
  "Turn a topological order in a schedule"
  [topological-order]
  (loop [topological-order topological-order
         schedule {}
         last-op-by-machine {}
         last-op-by-job {}]
    (if topological-order
      (let [{:keys [p m n id] :as curr-op} (first topological-order)
            last-machine-op (get last-op-by-machine m)
            last-job-op (get last-op-by-job n)
            end-time-machine (get-in schedule [last-machine-op :end-time] 0)
            end-time-job (get-in schedule [last-job-op :end-time] 0)]
        (if (< end-time-machine end-time-job)
          (let [prev-op last-job-op
                start-time end-time-job
                end-time (+ start-time p)]
            (recur (next topological-order)
                   (assoc schedule
                          id (assoc curr-op
                                    :start-time start-time
                                    :end-time end-time
                                    :prev prev-op))
                   (assoc last-op-by-machine
                          m id)
                   (assoc last-op-by-job
                          n id)))
          (let [prev-op last-machine-op
                start-time end-time-machine
                end-time (+ start-time p)]
            (recur (next topological-order)
                   (assoc schedule
                          id (assoc curr-op
                                    :start-time start-time
                                    :end-time end-time
                                    :prev prev-op))
                   (assoc last-op-by-machine
                          m id)
                   (assoc last-op-by-job
                          n id)))))
      schedule)))

(comment
  (evaluate-schedule
   (decode-rep js-pb-example
               (rep-rep-init js-pb-example)))
  ;; => {:op1 {:p 9, :m :M0, :id :op1, :n :J1, :start-time 0, :end-time 9, :prev nil},
  ;;     :op2 {:p 9, :m :M11, :id :op2, :n :J1, :start-time 9, :end-time 18, :prev :op1},
  ;;     :op3 {:p 4, :m :M2, :id :op3, :n :J1, :start-time 18, :end-time 22, :prev :op2},
  ;;     :op4 {:p 16, :m :M0, :id :op4, :n :J2, :start-time 9, :end-time 25, :prev :op1},
  ;;     :op5 {:p 4, :m :M1, :id :op5, :n :J2, :start-time 25, :end-time 29, :prev :op4},
  ;;     :op6 {:p 15, :m :M2, :id :op6, :n :J2, :start-time 29, :end-time 44, :prev :op5}}
  )
