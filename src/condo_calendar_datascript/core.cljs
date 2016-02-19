(ns condo-calendar-datascript.core
  (:require [cljs-time.core :as t]
            [cljs-time.periodic :as p]
            [cljs-time.format :as cf]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]
            [goog.dom :as gdom]
            [datascript.core :as d]))

(enable-console-print!)






;; Here are the 7 possible month layouts - based on a 31 day month. Just stop at
;; 28, 29, or 30 for shorter months. All months can fit into 6 weeks
;
;  1  2   3   4   5   6   7
;  8  9  10  11  12  13  14
; 15 16  17  18  19  20  21
; 22 23  24  25  26  27  28
; 29 30  31
;
;     1   2   3   4   5   6
; 7   8   9  10  11  12  13
; 14 15  16  17  18  19  30
; 21 21  23  24  25  26  27
; 28 29  30  31
;
;         1   2   3   4   5
; 6   7   8   9  10  11  12
; 13  14 15  16  17  18  19
; 20  21 22  23  24  25  26
; 27  28 29  30  31
;
;              1   2   3   4
;  5   6   7   8   9  10  11
; 12  13  14  15  16  17  18
; 19  20  21  22  23  24  25
; 26  27  28  29  30  31
;
;                  1   2   3
;  4   5   6   7   8   9  10
; 11  12  13  14  15  16  17
; 18  19  20  21  22  23  24
; 25  26  27  28  29  30  31
;
;                      1   2
;  3   4   5   6   7   8   9
; 10  11  12  13  14  15  16
; 17  18  19  20  21  22  23
; 24  25  26  27  28  29  30
; 31
;
;                          1
;  2   3   4   5   6   7   8
;  9  10  11  12  13  14  15
; 16  17  18  19  20  21  22
; 23  24  25  26  27  28  29
; 30  31
;

(def schema {:days/by-date   {
                              :db/cardinality :db.cardinality/one
                              :db/unique      :db.unique/identity
                              :db/doc         "string represnetation of date, format yyyymmdd"
                              }
            :person/by-id   {
                              :db/cardinality :db.cardinality/one
                              :db/valueType   :db.type/ref
                              :db/doc         "Refenernce to the person holding a day"
                              }
             :person/name    {
                              :db/cardinality :db.cardinality/one
                              :db/doc         "A person's name"
                              }
             :person/color   {
                              :db/cardinality :db.cardinality/one
                              :db/doc         "The color to use to display a day held by a person"
                              }
             :month/month-id {
                              :db/cardinality :db.cardinality/one
                              :db/doc         "A person's name"
                              }
             :month/weeks    {
                              :db/cardinality :db.cardinality/one
                              :db/doc         "A month of weeks"
                              }
             :current-user   {
                              :db/cardinality :db.cardinality/one
                              :db/valueType   :db.type/ref
                              }
             }
  )

(def conn (d/create-conn schema))

(defn sunday-of-first-week-of-month [year month]
  "return the sunday starting the week containing the first of the month"
  (let [first-of-month (t/date-time year month 1)]
    (t/minus first-of-month (t/days (mod (t/day-of-week first-of-month) 7)))))

(defn date-key [day]
  (cf/unparse (cf/formatters :basic-date) day))

(defn date-to-date-entry [date]
  (let [date-string (date-key date)]
  (hash-map :date date :days/by-date date-string)))

(defn seq-of-days [year month]
  (map date-to-date-entry
       (p/periodic-seq (sunday-of-first-week-of-month year month) (t/days 1))))

(d/transact! conn (let [today (t/today)
                        month (t/month today)
                        year (t/year today)]
                    [{:db/id -1 :person/name "Jake" :person/color "green"}
                     {:db/id -2 :person/color "red" :person/name "Judy"}
                     {:db/id -3 :person/color "blue" :person/name "John"}
                     {:db/id -4 :person/color "yellow" :person/name "Susan"}
                     {:db/id -5 :current-user 1}
                     {:db/id -6 :month/month-id (t/date-time year month 1)}
                     {:db/id -7 :month/weeks (take 42 (seq-of-days year month))} ; six weeks x 7 days = 42
                     ]))

(defn date-to-assignment [state date]
  (get-in state (get-in state [:days/by-date date] [nil]) {:name "available" :color "white"}))

(defn denormalize-week [state week]
  (map #(merge % (date-to-assignment state (:days/by-date %))) week))

(defn calendar-without-assignments []
  (into (sorted-map) (map #(vector % (vector % "white" "unassigned")) (map :days/by-date (first (first (d/q '[:find ?m :where [_ :month/weeks ?m]] @conn)))))))

(defn calendar-assignments []
  (into (sorted-map) (map #(vector (first %) %) (d/q '[:find ?d ?c ?n :where [?e :days/by-date ?d] [?e :person/by-id ?p][?p :person/color ?c][?p :person/name ?n]] @conn))))

(defmulti read om/dispatch)

(defmethod read :month/month-id
  [{:keys [state selector] :as env} key {:keys [month]}]
  {:value (first (first (d/q '[:find ?m :where [_ :month/month-id ?m]] @conn)))})

(defmethod read :current-user
  [_ _ _]
  {:value (first (first (d/q '[:find ?cu :where [_ :current-user ?cu]] @conn)))})

(defmethod read :month/weeks
  [{:keys [state selector] :as env} key]
                 (d/q '[:find ?d ?c ?n :in $ [?d ...] :where [(get-else $ ?e :days/by-date ?d) ?d] [(get-else $ ?e :person/by-id -1) ?a]
                        [(get-else $ ?a :person/color "white") ?c] [(get-else $ ?a :person/name "unassigned") ?n]] @conn
                      (map :days/by-date (first (first (d/q '[:find ?m :where [_ :month/weeks ?m]] @conn)))))
  {:value (partition 7 (vals
                         (merge (calendar-without-assignments)
                                (calendar-assignments))))})

(defn next-month [current-month-start]
  "given a date-time, generate the date-time one month later"
  (t/plus current-month-start (t/months 1)))

(defn last-month [current-month-start]
  "given a date-time, generate the date-time one month earlier"
  (t/minus current-month-start (t/months 1)))


(defmulti mutate om/dispatch)

(defmethod mutate 'day/change-state
  [{:keys [state]} _ {:keys [date] :as params}]
  (let [current-user (first (first (d/q '[:find ?cu :where [_ :current-user ?cu]] @conn)))
        id (first (first (d/q '[:find ?p :in $ ?d :where [?day :person/by-id ?p] [?day :days/by-date ?d]] @conn (str date))))]
    (if id
      {:value  {:days/by-date date}
       :action (fn []
                 (d/transact! conn [[:db.fn/retractEntity [:days/by-date date]]]))}
      {:value  {:days/by-date date}                         ; the date is not assigned - so assign
       :action (fn []
                 (d/transact! conn [{:db/id -1 :person/by-id current-user :days/by-date date}]))})))

(defmethod mutate 'month/next
  [{:keys [state]} _ _]
  (let [current-month (first (first (d/q '[:find ?m :where [_ :month/month-id ?m]] @conn)))
        current-month-entity (first (first (d/q '[:find ?e :where [?e :month/month-id _]] @conn)))
        weeks-entity (first (first (d/q '[:find ?e :where [?e :month/weeks _]] @conn)))
        new-month (next-month current-month)
        month (t/month new-month)
        year (t/year new-month)]
    {:value  {:keys [:month/month-id :month/weeks]}
     :action (fn []
               (d/transact! conn [{:db/id current-month-entity :month/month-id new-month}
                                  {:db/id weeks-entity :month/weeks (take 42 (seq-of-days year month))}
                                  ]))}))

(defmethod mutate 'month/previous
  [{:keys [state]} _ _]
  (let [current-month (first (first (d/q '[:find ?m :where [_ :month/month-id ?m]] @conn)))
        current-month-entity (first (first (d/q '[:find ?e :where [?e :month/month-id _]] @conn)))
        weeks-entity (first (first (d/q '[:find ?e :where [?e :month/weeks _]] @conn)))
        new-month (last-month current-month)
        month (t/month new-month)
        year (t/year new-month)]
    {:value  {:keys [:month/month-id :month/weeks]}
     :action (fn []
               (d/transact! conn [{:db/id current-month-entity :month/month-id new-month}
                                  {:db/id weeks-entity :month/weeks (take 42 (seq-of-days year month))}
                                  ]))}))

(def reconciler
  (om/reconciler
    {:state  conn
     :parser (om/parser {:read read :mutate mutate})}))

(defui Day
  static om/Ident
  (ident [this {:keys [day/by-date]}]
    [:days/by-date by-date])
  Object
  (render [this]
    (let [day (first (om/props this))
          color (second (om/props this))
          name (nth (om/props this) 2)]
      (dom/div #js{:className (str "day " color)
                   :onMouseDown
                              (fn [e]
                                (om/transact! reconciler `[(day/change-state {:date ~day})]))}
               (dom/div #js {:className "day-no"} (subs day 6))
               (dom/span #js {:className "day-name"} name)
               ))))

(def day (om/factory Day {:keyfn first}))

(defui Week
  Object
  (render [this]
    (apply dom/div #js {:className "week"}
           (map day (om/props this)))))

(def week (om/factory Week {:keyfn #(first (first %))}))

(defui Month-header
  static om/IQuery
  (query [this]
    '[:month/month-id])
  static om/Ident
  (ident [this props]
    [:month/month-id props])
  Object
  (render [this]
    (let [month-id (om/props this)]
      (dom/div #js {:className "month-header"}
               (dom/button #js {:className "change-month"
                                :onClick
                                           (fn [e] (om/transact! reconciler '[(month/previous)]))}
                           (dom/i #js {:className "fa fa-chevron-left fa-2x"}))
               (dom/span #js {:className "month-year"}
                         (cf/unparse (cf/formatter "MMMM YYYY") month-id))
               (dom/button #js {:className "change-month"
                                :onClick
                                           (fn [e] (om/transact! reconciler '[(month/next)]))}
                           (dom/i #js {:className "fa fa-chevron-right fa-2x"}))))))


(def month-header (om/factory Month-header))

(defui Month-body
  static om/IQuery
  (query [this]
    '[:month/weeks])
  Object
  (render [this]
    (apply dom/div #js {:className "month-body"}
           (map week (:month/weeks (om/props this))))))


(def month-body (om/factory Month-body))

(defui Month
  static om/IQuery
  (query [this]
    [:month/weeks {:month/month-id (om/get-query Month-header)}])
  Object
  (render [this]
    (dom/div #js {:className "month"}
             (month-header (:month/month-id (om/props this)))
             (month-body (om/props this)))))

(om/add-root! reconciler
              Month (gdom/getElement "app"))
