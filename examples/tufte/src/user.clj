(ns user
  "Fork of https://github.com/ptaoussanis/tufte#10-second-example for tap> and Portal"
  (:require [portal.api :as p]
            [taoensso.tufte :as tufte :refer (p profiled profile)]))

(def columns
  (-> [:min :p25 :p50 :p75 :p90 :p95 :p99 :max :mean :mad :sum]
      (zipmap (repeat :portal.viewer/duration-ns))
      (assoc :loc :portal.viewer/source-location)))

(defn format-data [stats]
  (-> stats
      (update-in [:loc :ns] symbol)
      (vary-meta update :portal.viewer/for merge columns)))

(defn format-pstats [pstats]
  (-> @pstats
      (:stats)
      (update-vals format-data)
      (with-meta
        {:portal.viewer/default :portal.viewer/table
         :portal.viewer/table
         {:columns [:n :min #_:p25 #_:p50 #_:p75 #_:p90 #_:p95 #_:p99 :max :mean #_:mad :sum :loc]}})))

(defn ->bar-chart [pstats]
  (let [stats (:stats @pstats)]
    ^{:portal.viewer/default :portal.viewer/vega-lite}
    {:$schema "https://vega.github.io/schema/vega-lite/v5.json",
     :mark {:type :bar :tooltip true}
     :encoding
     {:color {:field :pid}
      :xOffset {:field :pid}
      :x {:field :stats :sort [:min :max :mean]}
      :y {:field :time  :title "time (ms)" :type :quantitative}}
     :data
     {:values
      (mapcat
       (fn [[pid {:keys [min max mean]}]]
         (let [pid (pr-str pid)]
           [{:stats :min  :pid pid :time (/ min 1e6)}
            {:stats :max  :pid pid :time (/ max 1e6)}
            {:stats :mean :pid pid :time (/ mean 1e6)}]))
       stats)}}))

(comment (->bar-chart pstats))

(defn ->pie-chart [pstats]
  (let [stats (:stats @pstats)]
    ^{:portal.viewer/default :portal.viewer/vega-lite}
    {:$schema "https://vega.github.io/schema/vega-lite/v5.json",
     :mark {:type :arc :innerRadius 100 :tooltip true}
     :encoding
     {:theta  {:field :sum :type :quantitative :stack :normalize :title :time}
      :color  {:field :pid :type :nominal}}
     :data
     {:values
      (map (fn [[pid {:keys [n sum]}]]
             {:pid (pr-str pid) :calls n :sum sum}) stats)}}))

(comment (->pie-chart pstats))

(defn ->pie-stats [pstats]
  (let [stats (:stats @pstats)]
    ^{:portal.viewer/default :portal.viewer/vega-lite}
    {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
     :mark {:type :arc :innerRadius 40 :tooltip true}
     :encoding
     {:facet {:field :stats
              :columns 2
              :sort [:min :max :mean]}
      :theta  {:field :value :type :quantitative :stack :normalize}
      :color  {:field :pid :type :nominal}}
     :data
     {:values
      (mapcat (fn [[pid {:keys [min max mean n]}]]
                [{:pid pid :stats :min  :value min}
                 {:pid pid :stats :max  :value max}
                 {:pid pid :stats :mean :value mean}
                 {:pid pid :stats :n    :value n}])
              stats)}}))

(comment (->pie-stats pstats))

(defn add-tap-handler!
  "Adds a simple handler that logs `profile` stats output with `tap>`."
  [{:keys [ns-pattern handler-id]
    :or   {ns-pattern "*"
           handler-id :basic-tap}}]
  (tufte/add-handler!
   handler-id ns-pattern
   (fn [{:keys [?id ?data pstats]}]
     (tap> (vary-meta
            (format-pstats pstats)
            merge
            (cond-> {}
              ?id   (assoc :id ?id)
              ?data (assoc :data ?data)))))))

;;; Let's define a couple dummy fns to simulate doing some expensive work
(defn get-x [] (Thread/sleep 500)             "x val")
(defn get-y [] (Thread/sleep (rand-int 1000)) "y val")

(defn do-work []
  (dotimes [_ 5]
    (p :get-x (get-x)))
  (dotimes [_ 10]
    (p :get-y (get-y))))

(defn run []
  ;; CLI usage
  (println "Running profile...")
  (-> (profiled {} (do-work)) second format-pstats p/inspect))

(comment
  ;; REPL usage
  (p/open)
  (add-tap p/submit)
  (add-tap-handler! {})
  (profile {} (do-work))
  (remove-tap p/submit)

  (def pstats (second (profiled {} (do-work))))
  (format-pstats pstats)
  (->pie-chart pstats)
  (->bar-chart pstats))
