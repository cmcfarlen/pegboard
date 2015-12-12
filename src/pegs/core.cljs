(ns pegs.core
  (:require [om.core :as om :include-macros true]
            [sablono.core :as html :refer-macros [html]]
            [cljs.core.async :as async :refer [<! >!]]
            [pegs.peg :as peg]
            [cljs.reader :as reader])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(enable-console-print!)

(defonce app-state (atom {:game {:board [1 1 1 1 0 1 1 1 1 1 1 1 1 1 1]
                                 :hover nil
                                 :selected nil
                                 :history []}
                          :preview nil
                          :last-move nil
                          :solutions []}))


(defn board-markup
  "td is a fn to generate the :td"
  [td]
  [:table
   [:tr [:td {:colSpan 4} ""] (td 0) [:td {:colSpan 4} ""]]
   [:tr [:td {:colSpan 3} ""] (td 1) [:td "-"] (td 2) [:td {:colSpan 3} ""]]
   [:tr [:td {:colSpan 2} ""] (td 3) [:td "-"] (td 4) [:td "-"] (td 5) [:td {:colSpan 2} ""]]
   [:tr [:td  ""] (td 6) [:td "-"] (td 7) [:td "-"] (td 8) [:td "-"] (td 9) [:td  ""] ]
   [:tr (td 10) [:td "-"] (td 11) [:td "-"] (td 12) [:td "-"] (td 13) [:td "-"] (td 14)]])


(defn render-board
  [data owner]
  (reify
    om/IRender
    (render [_]
      (let [bd (:board data)
            hover (:hover data)
            selected (:selected data)
            hl (into #{} (apply concat (peg/moves hover)))
            td (fn [i]
                 [:td {:className (cond
                                   (= selected i)
                                   "selected"
                                   (= hover i)
                                   "hover"
                                   (hl i)
                                   "highlight"
                                   :else
                                   "")
                       :on-click (fn [_]
                                   (if selected
                                     (if-let [mv (peg/valid-move? bd selected i)]
                                       (do
                                        (om/transact! data :history #(conj % bd))
                                        (om/transact! data :board #(peg/move % mv))
                                        (om/update! data :selected nil))
                                       (om/update! data :selected i))
                                     (om/update! data :selected i)))
                       :on-mouse-over (fn [_]
                                        (om/update! data :hover i))} (bd i)])]
        (html/html
         [:div
          (if (peg/game-over? bd) [:h3 "Game over"])
          [:div.board (board-markup td)]
          #_[:div.history [:ul
           (map (fn [b]
                  [:li (board-markup #(or [:td (b %)]))]
                  ) (:history data))]]])))))

(defn play-moves
  [cursor boards]
  (println boards)
  (go
    (loop [n (first boards)
           r (rest boards)]
      (when n
        (om/update! cursor :preview (second n))
        (om/update! cursor :last-move (first n))
        (<! (async/timeout 750))
        (recur (first r) (rest r))))))

(om/root
  (fn [data owner]
    (reify
      om/IRender
      (render [_]
        (html/html [:div
                    (om/build render-board (:game data))
                    [:div
                     [:select {:ref "placement"}
                      (map (fn [[bname bd]] [:option {:value (str bd)} bname])
                           {"Center" [1 1 1 1 0 1 1 1 1 1 1 1 1 1 1]
                            "Corner" [1 1 1 1 1 1 1 1 1 1 1 1 1 1 0]
                            "LeftOfCorner" [1 1 1 1 1 1 1 1 1 1 1 1 1 0 1]
                            "Bottom Center" [1 1 1 1 1 1 1 1 1 1 1 1 0 1 1]
                            "Easy" [1 1 0 1 1 1 0 0 0 0 0 0 0 0 0]})]
                     [:button {:on-click
                               (fn [_]
                                 (let [e (om/get-node owner "placement")
                                       bd (reader/read-string (.. e -value))]
                                   (om/update! data :game {:selected nil
                                                         :hover nil
                                                         :board bd
                                                         :history []})))} "Reset"]
                     [:button {:on-click
                               (fn [_]
                                 (om/transact! data :game #(let [hist (:history %)
                                                                 last-bd (last hist)]
                                                             (assoc %
                                                                    :board last-bd
                                                                    :history (into [] (butlast hist))))))} "Undo"]
                     [:button {:on-click
                               (fn [_]
                                 (om/update! data :solutions (into [] (take 10 (peg/find-solution (-> data :game :board)))))
                                 #_(let [s (peg/move-seq (-> data :game :board))
                                       ch (async/chan)
                                       on-ch (async/onto-chan ch s)]
                                   (go
                                     (loop []
                                       (when-let [r (<! ch)]
                                         (let [[bd moves] r]
                                           (when (= 1 (apply + bd))
                                             (om/transact! data :solutions #(conj % moves))))
                                         (om/transact! data :count (fnil inc 0))
                                         (<! (async/timeout 1))
                                         (recur))))))
                               } "Solutions"]
                     (if-let [cnt (:count data)]
                       [:span (str "Count: " cnt)])
                     [:hr]
                     [:div.board (board-markup #(or [:td (str %)]))]
                     (let [preview (:preview data)
                           last-move (into #{} (:last-move data))]
                        (if preview
                          [:div.board (board-markup #(or [:td
                                                          {:className (if (last-move %)
                                                                        "highlight"
                                                                        ""
                                                                        )}
                                                          (preview %)]))]))
                     (if-let [solutions (:solutions data)]
                       [:div
                        [:p (str "Solution count: " (count solutions))]
                        [:ul.solutions
                         (map (fn [soln]
                                (let [boards (reductions peg/move (-> data :game :board) soln)]
                                  [:li
                                   (map (fn [mv b]
                                          [:span {:on-mouse-over
                                                  (fn [_]
                                                    (om/update! data :preview b)
                                                    (om/update! data :last-move mv))
                                                  :on-mouse-out
                                                  (fn [_]
                                                    (om/update! data :preview (-> @data :game :board))
                                                    (om/update! data :last-move nil)
                                                    )} (pr-str mv)]
                                          ) soln boards)
                                   [:button {:on-click (fn [_] (play-moves data (map vector (conj soln nil) boards)))} ">"]])) solutions)]])]]))))
  app-state
  {:target (. js/document (getElementById "app"))})

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
