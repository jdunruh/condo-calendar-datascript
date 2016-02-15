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
                              :db/doc         "string represnetation of date, format yyyymmdd"
                              }
             :days/person    {
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
             :week/days      {
                              :db/valueType   :db.type/ref
                              :db/cardinality :db.cardinality/one
                              :db/doc         "A week of days"
                              }
             :day/date       {
                              :db/cardinality :db.cardinality/one
                              :db/doc         "a day's date"
                              }
             :day/date-key   {
                              :db/cardinality :db.cardinality/one
                              :db/doc         "A string representing a date - formay yyyymmdd"
                              }
             :current-user   {
                              :db/cardinality :db.cardinality/one
                              :db/valueType   :db.type/ref
                              }
             }
  )

(def conn (d/create-conn schema))

; get name, color, and id for current user
;(d/q '[:find ?cu ?n ?c :where [_ :current-user ?cu] [?cu :person/name ?n] [?cu :person/color ?c]] @conn) ; get name and color for current user

; get name, color, date, and id for a date assignment
;(d/q '[:find ?i ?d ?n ?c :where [?d :days/by-date "20151217"] [?d :person/by-id ?i] [?i :person/name ?n] [?i :person/color ?c]] @conn)

; transact in a new date assignment
;(d/transact! conn [{:db/id -1 :person/by-id 3 :days/by-date "20151217"}])

(defn sunday-of-first-week-of-month [year month]
  "return the sunday starting the week containing the first of the month"
  (let [first-of-month (t/date-time year month 1)]
    (t/minus first-of-month (t/days (mod (t/day-of-week first-of-month) 7)))))

(defn date-key [day]
  (cf/unparse (cf/formatters :basic-date) day))

(defn seq-of-days [year month]
  (map #(hash-map :date % :days/by-date (date-key %))
       (p/periodic-seq (sunday-of-first-week-of-month year month) (t/days 1))))

(defn six-weeks-containing-month [year month]
  "return the six week period starting with the week of the first
  of the month and continuing for six weeks."
  (take 6 (partition 7 (seq-of-days year month))))

(println "transacting initial data")
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
(println "Initial data transaction completed")

; get current user
;(d/q '[:find ?e ?cu :where [?e :current-user ?cu]] @conn)

; update current user
;(d/transact! conn [{:db/id 5 :current-user 2}])

; get current user where the id is one of several values
;(d/q '[:find ?e :in $ [?u ...] :where [?e :current-user ?u]] @conn [1 2])

; get current user with name, using "unknown" for a default name if it the name isn't found
;(d/q '[:find ?e ?cu ?name :where [?e :current-user ?cu]
;[(get-else $ ?cu :person/name "unknown") ?name]] @conn)

;(def init-data
;  (let [today (t/today)
;        month (t/month today)
;        year (t/year today)]
;    {:person/by-id   {0 {:color "red" :name "Judy" :id 0}
;                      1 {:color "blue" :name "John" :id 1}
;                      2 {:color "green" :name "Jake" :id 2}
;                      3 {:color "yellow" :name "Susan" :id 3}}
;     :user           1
;     :month/month-id (t/date-time year month 1)
;     :month          (six-weeks-containing-month year month)
;     :days/by-date   {}
;     :current-user   2}))

; pulls a week of date IDs as a vector
; (vec (map :days/by-date (first (first (first (d/q '[:find ?m :where [_ :month/weeks ?m]] @conn))))))

; gets the days that match in the week - in this case the 3rd week
; (d/q '[:find ?e ?d ?o :in $ [?d ...] :where [?e :days/by-date ?d] [?e :person/by-id ?o]] @conn
;    (vec (map :days/by-date (nth (first (first (d/q '[:find ?m :where [_ :month/weeks ?m]] @conn))) 3))))

; gets day and associated person info
; (d/q '[:find  ?d  ?c ?n :in $ [?d ...] :where [?e :days/by-date ?d] [?e :person/by-id ?o] [?o :person/color ?c] [?o :person/name ?n]] @conn
;    (vec (map :days/by-date (nth (first (first (d/q '[:find ?m :where [_ :month/weeks ?m]] @conn))) 3))))

; get day and associated name and color with defaults for days not in :days/by-date
; (d/q '[:find ?d ?c ?n :in $ [?d ...] :where [(get-else $ ?e :days/by-date ?d) ?d][(get-else $ ?e :person/by-id  -1) ?a]
; [(get-else $ ?a :person/color "transparent" ) ?c] [(get-else $ ?a :person/name "unassigned") ?n]] @conn
; '(20151112 20151113 20151114))

; same as above, but get days of month from database
; condo-calendar-datascript.core=> (d/q '[:find ?d ?c ?n :in $ [?d ...] :where [(get-else $ ?e :days/by-date ?d) ?d][(get-else $ ?e :person/by-id  -1) ?a]
; [(get-else $ ?a :person/color "transparent" ) ?c] [(get-else $ ?a :person/name "unassigned") ?n]] @conn
; (vec (map :days/by-date (nth (first (first (d/q '[:find ?m :where [_ :month/weeks ?m]] @conn))) 3))))

; get everything out of datascript with current month
;
;(d/q '[:find ?d ?c ?n :in $ [?d ...] :where [(get-else $ ?e :days/by-date ?d) ?d] [(get-else $ ?e :person/by-id -1) ?a]
;       [(get-else $ ?a :person/color "transparent") ?c] [(get-else $ ?a :person/name "unassigned") ?n]] @conn
;     (map :days/by-date (first (first (d/q '[:find ?m :where [_ :month ?m]] @conn)))))

; same as above, but sorted by the date and organized in weeks
;(partition 7 (sort #(< (first %1) (first %2)) (d/q '[:find ?d ?c ?n :in $ [?d ...] :where [(get-else $ ?e :days/by-date ?d) ?d] [(get-else $ ?e :person/by-id -1) ?a]
;                                        [(get-else $ ?a :person/color "transparent") ?c] [(get-else $ ?a :person/name "unassigned") ?n]] @conn
;                                      (map :days/by-date (first (first (d/q '[:find ?m :where [_ :month ?m]] @conn)))))))

(defn date-to-assignment [state date]
  (get-in state (get-in state [:days/by-date date] [nil]) {:name "available" :color "white"}))

(defn denormalize-week [state week]
  (map #(merge % (date-to-assignment state (:days/by-date %))) week))


(defn denormalize-month [state]
  (map #(denormalize-week state %) (:month state)))

(defmulti read om/dispatch)

(defmethod read :month/month-id
  [{:keys [state selector] :as env} key {:keys [month]}]
  (println "reading :monnth/month-id")
  (println (first (first (d/q '[:find ?m :where [_ :month/month-id ?m]] @conn))))
  {:value (first (first (d/q '[:find ?m :where [_ :month/month-id ?m]] @conn)))})

(defmethod read :current-user
  [_ _ _]
  (println "reading :current-user")
  {:value (first (first (d/q '[:find ?cu :where [_ :current-user ?cu]] @conn)))})

; get the person holding a day
; (d/q '[:find ?p :in $ ?d :where [?day :person/by-id ?p] [?day :day/by-date ?d]] @conn "20141516")

(defmethod read :month/weeks
  [{:keys [state selector] :as env} key]
  (println "reading :month/weeks")
  (println (sort #(< (first %1) (first %2))     ; partition 7 separates data into 7 day weeks
                 (d/q '[:find ?d ?c ?n :in $ [?d ...] :where [(get-else $ ?e :days/by-date ?d) ?d] [(get-else $ ?e :person/by-id -1) ?a]
                        [(get-else $ ?a :person/color "white") ?c] [(get-else $ ?a :person/name "unassigned") ?n]] @conn
                      (map :days/by-date (first (first (d/q '[:find ?m :where [_ :month/weeks ?m]] @conn)))))))
  {:value (partition 7
       (sort #(< (first %1) (first %2))     ; partition 7 separates data into 7 day weeks
             (d/q '[:find ?d ?c ?n :in $ [?d ...] :where [(get-else $ ?e :days/by-date ?d) ?d] [(get-else $ ?e :person/by-id -1) ?a]
                   [(get-else $ ?a :person/color "white") ?c] [(get-else $ ?a :person/name "unassigned") ?n]] @conn
                    (map :days/by-date (first (first (d/q '[:find ?m :where [_ :month/weeks ?m]] @conn)))))))})


(defn add-assignment-to-calendar [state date assignee]
  (update state :days/by-date assoc date [:person/by-id assignee]))


(defn remove-date-from-days [days date]
  (dissoc days date))

(defn release-day [state date]
  (update state :days/by-date remove-date-from-days date))

(defn next-month [current-month-start]
  "given a date-time, generate the date-time one month later"
  (t/plus current-month-start (t/months 1)))

(defn last-month [current-month-start]
  "given a date-time, generate the date-time one month earlier"
  (t/minus current-month-start (t/months 1)))


(defmulti mutate om/dispatch)

;(defmethod mutate 'day/change-state
;  [{:keys [state]} _ {:keys [date] :as params}]
;  (let [st @state]
;    (if-let [day-assignee (get-in st [:days/by-date (str date)])]
;      (do
;          (if (= (second day-assignee) (:current-user st))  ; the date is assigned
;            {:value {:days/by-date date}                    ; it is assigned to the current user - so release
;             :action
;                    (fn []
;                      (swap! state release-day (str date)))}
;            {:value {:error "Cannot release someone else's day"}})) ; the date is assigned to someone else - user error
;      (do  {:value {:days/by-date date} ; the date is not assigned - so assign
;                                              :action
;                                                     (fn []
;                                                       (swap! state add-assignment-to-calendar (str date) (:current-user st)))}))))
(defmethod mutate 'day/change-state
  [{:keys [state]} _ {:keys [date] :as params}]
  (println "mutate day/change-state")
  (let [current-user (first (d/q '[:find ?cu ?n ?c :where [_ :current-user ?cu] [?cu :person/name ?n] [?cu :person/color ?c]] @conn))
        id (first (first (d/q '[:find ?p :in $ ?d :where [?day :person/by-id ?p] [?day :day/by-date ?d]] @conn (str date))))]
    (if id
      {:value  {:days/by-date date}
       :action (fn []
                 (d/transact! conn [[:db.fn/retractEntity [:days/by-date (str date)]]]))}
      {:value  {:days/by-date date}                         ; the date is not assigned - so assign
       :action (fn []
                 (d/transact! conn [{:db/id -1 :person/by-id id :days/by-date (str date)}]))})))

; get back the ID for the date
;(d/q '[:find ?e ?d ?o :in $ [?d ...] :where [?e :days/by-date ?d] [?e :person/by-id ?o]] @conn ["20151223"])

;(defmethod mutate 'month/next
;  [{:keys [state]} _ _]
;  (let [st @state
;        new-month (next-month (:month/month-id st))
;        month (t/month new-month)
;        year (t/year new-month)]
;    {:value  {:month/month-id (date-key new-month)}
;     :action (fn []
;               (swap! state assoc :month/month-id new-month :month (six-weeks-containing-month year month)))}))

(defmethod mutate 'month/next
  [{:keys [state]} _ _]
  (let [current-month (first (first (d/q '[:find ?m :where [_ :month/month-id ?m]] @conn)))
        current-month-entity (first (first (d/q '[:find ?e :where [?e :month/month-id _]] @conn)))
        weeks-entity (first (first (d/q '[:find ?e :where [?e :month/weeks _]] @conn)))
        new-month (next-month current-month)
        month (t/month new-month)
        year (t/year new-month)]
    (println (str "mutate month/next from " current-month " to " new-month))
    {:value  {:keys [:month/month-id :month/weeks]}
     :action (fn []
               (d/transact! conn [{:db/id current-month-entity :month/month-id new-month}
                                  {:db/id weeks-entity :month/weeks (take 42 (seq-of-days year month))}
                                  ]))}))


;(defmethod mutate 'month/previous
;  [{:keys [state]} _ _]
;  (let [st @state
;        new-month (last-month (:month/month-id st))
;        month (t/month new-month)
;        year (t/year new-month)]
;    {:value  {:month/month-id (date-key new-month)}
;     :action (fn []
;               (swap! state assoc :month/month-id new-month :month (six-weeks-containing-month year month)))}))

(defmethod mutate 'month/previous
  [{:keys [state]} _ _]
  (let [current-month (first (first (d/q '[:find ?m :where [_ :month/month-id ?m]] @conn)))
        current-month-entity (first (first (d/q '[:find ?e :where [?e :month/month-id _]] @conn)))
        weeks-entity (first (first (d/q '[:find ?e :where [?e :month/weeks _]] @conn)))
        new-month (last-month current-month)
        month (t/month new-month)
        year (t/year new-month)]
    (println (str "mutate month/next from " current-month " to " new-month))
    {:value  {:keys [:month/month-id :month/weeks]}
     :action (fn []
               (d/transact! conn [{:db/id current-month-entity :month/month-id new-month}
                                  {:db/id weeks-entity :month/weeks (take 42 (seq-of-days year month))}
                                  ]))}))

(def reconciler
  (om/reconciler
    {:state  conn
     :parser (om/parser {:read read :mutate mutate})}))




;; need to add click listeners, remove if owner by this user, none if owner by other user, assign if available
(defui Day
  static om/Ident
  (ident [this {:keys [day/by-date]}]
    [:days/by-date by-date])
  Object
  (render [this]
    (println "rendering day")
    (let [day (first (om/props this))
          color (second (om/props this))
          name (nth (om/props this) 2)]
      (dom/div #js{:className (str "day " color)
                   :onMouseDown
                              (fn [e]
                                (om/transact! reconciler `[(day/change-state {:date ~(date-key day)})]))}
               (dom/div #js {:className "day-no"} (subs day 6))
               (dom/span #js {:className "day-name"} name)
               ))))

(def day (om/factory Day {:keyfn first}))

(defui Week
  Object
  (render [this]
    (println "rendering week")
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
      (println (str "rendering month-header " month-id))
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
    (println "rendering month-body")
    (println (om/props this))
    (apply dom/div #js {:className "month-body"}
           (map week (:month/weeks (om/props this))))))


(def month-body (om/factory Month-body))

(defui Month
  static om/IQuery
  (query [this]
    [:month/weeks {:month/month-id (om/get-query Month-header)}])
  Object
  (render [this]
    (println "rendering month")
    (println (om/props this))
    (dom/div #js {:className "month"}
             (month-header (:month/month-id (om/props this)))
             (month-body (om/props this)))))

(om/add-root! reconciler
              Month (gdom/getElement "app"))
