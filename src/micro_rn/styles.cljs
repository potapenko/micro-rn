(ns micro-rn.styles
  (:require [reagent.core :as r :refer [atom]]
            [reagent.impl.component :as ru]))

(set! js/React (js/require "react-native"))

(defn create-styles[s]
  (let [s1 (reduce #(assoc %1 (%2 0) (ru/camelify-map-keys (%2 1))) {} s)]
    (.create (.-StyleSheet js/React) (clj->js s1))))

(def styles (atom {}))
(def original-styles (atom {}))

(defn get-stylesheet[domain]
  (domain @styles))

(defn add-stylesheet [domain new-styles]
  (if-let[exists (get-stylesheet domain)]
    exists
    (do
      (reset! styles (assoc @styles domain (create-styles new-styles)))
      (get-stylesheet domain))))

(defn get-style [style-name]
  (when-let [sheet (get-stylesheet style-name)]
    (aget sheet (name style-name))))

(defn new-style [style-name style]
  (if-let [exists (get-style style-name)]
    exists
    (do
      (add-stylesheet style-name (assoc {} style-name style))
      (let [style-id (get-style style-name)]
        (reset! original-styles (assoc @original-styles style-id style))
        style-id
        ))))

(defn original [style-id]
  (get @original-styles style-id)
  )

; (print-style (shadow) bold (border) (margin))
(defn print-style [& style]
  (println)
  (println)
  (println)
  (let[style-map (apply merge (map original style))
       sorted-style (doall
                      (sort #(compare (name (:k %1)) (name (:k %2)))
                            (for [[k v] style-map] {:k k :v (if (string? v) (str "\"" v "\"") v)})))
       ]
    (println "(s/new-style :my-style-rename-this")
    (println "             {")
    (doseq [x sorted-style] (println "             " (:k x) (:v x)))
    (println "              })")

    )
  (println)
  (println)
  (println)
  (vec style)
  )

; colors

(def red            "#DB2828")
(def orange         "#F2711C")
(def yellow         "#FBBD08")
(def olive          "#B5CC18")
(def green          "#21BA45")
(def teal           "#00B5AD")
(def blue           "#2185D0")
(def violet         "#6435C9")
(def purple         "#A333C8")
(def pink           "#E03997")
(def brown          "#A5673F")
(def grey           "#767676")
(def black          "#1B1C1D")

(def light-red      "#ff695e")
(def light-orange   "#ff851b")
(def light-yellow   "#ffe21f")
(def light-olive    "#d9e778")
(def light-green    "#2ecc40")
(def light-teal     "#6dffff")
(def light-blue     "#54c8ff")
(def light-violet   "#a291fb")
(def light-purple   "#dc73ff")
(def light-pink     "#ff8edf")
(def light-brown    "#d67c1c")
(def light-grey     "#dcddde")
(def light-black    "#545454")

; layout

(defn flex
  ([] (flex 1))
  ([v]
  (new-style (keyword (str "flex-" v)) {:flex v})))

(defn align-self [v]
  (new-style (keyword (str "align-self-" v)) {:align-self v}))

(defn align-items [v]
  (new-style (keyword (str "align-items-" v)) {:align-items v}))

(defn flex-align [v]
  (new-style (keyword (str "flex-align-" v)) {:align-items v
                                              :justify-content v}))

(defn justify-content [v]
  (new-style (keyword (str "justify-content-" v)) {:justify-content v}))

(defn flex-wrap [v]
  (new-style (keyword (str "flex-wrap-" v)) {:flex-wrap v}))

(def row (new-style :row {:flex-direction "row"}))
(def column (new-style :column {:flex-direction "column"}))
(def align-center (align-items "center"))
(def align-right (align-items "flex-end"))
(def stretch (align-items "stretch"))
(def wrap (new-style :flex-wrap {:flex-wrap "wrap"}))
(def no-wrap (new-style :flex-wrap {:flex-wrap "nowrap"}))

; background color

(defn gray
  ([] (gray 2))
  ([opacity]
   (new-style
     (keyword (str "gray-" opacity))
     {:background-color (str "rgba(0,0,0,0." opacity ")")}))
  )

(defn white
  ([]  (white 999))
  ([opacity]
   (new-style
     (keyword  (str "white-" opacity))
     {:background-color (str "rgba(255,255,255,0." opacity ")")}))
  )

(defn background [v]
  (new-style (keyword (str "background-color-" v)) {:background-color v}))


;; text styles

(defn color [v]
  (new-style (keyword (str "color-" v)) {:color v}))

(defn font-style [v]
  (new-style (keyword (str "font-style" v)) {:font-style v}))

(defn font-family [v]
  (new-style (keyword (str "font-family" v)) {:font-family v}))

(defn font-weight [v]
  (new-style (keyword (str "font-weight" v)) {:font-weight v}))

(defn line-height [v]
  (new-style (keyword (str "line-height" v)) {:line-height v}))

(defn text-align-vertical [v] ; android
  (new-style (keyword (str "text-align-vertical" v)) {:text-align-vertical v}))

(defn leter-spacing [v] ; ios
  (new-style (keyword (str "leter-spacing" v)) {:leter-spacing v}))

(defn text-decoration-color [v] ; ios
  (new-style (keyword (str "text-decoration-color" v)) {:text-decoration-color v}))

(defn text-decoration-line [v] ; ios, enum('none', 'underline', 'line-through', 'underline line-through')
  (new-style (keyword (str "text-decoration-line" v)) {:text-decoration-line v}))

(defn text-decoration-style [v] ; ios, enum('solid', 'double', 'dotted', 'dashed')
  (new-style (keyword (str "text-decoration-style" v)) {:text-decoration-style v}))

(defn writing-direction [v] ; ios, enum('auto', 'ltr', 'rtl')
  (new-style (keyword (str "writing-direction" v)) {:writing-direction v}))

(def bold (font-weight "bold"))

(def italic (font-style "italic"))

(defn font-size [v]
  (new-style (keyword (str "font-size-" v)) {:font-size v}))

(defn text-align [v]
  (new-style (keyword (str "text-align-" v)) {:text-align v}))

(defn text-shadow
  ([] (text-shadow 0 2 5 2))
  ([w h r a]
  (new-style (keyword (str "text-shadow-" a "-" w "-" h "-" r))
             {
              :text-shadow-offset {:width w :height h}
              :text-shadow-radius r
              :text-shadow-color (str "rgba(0,0,0,0." a ")")
              })))

;; dimentions

(defn height [v]
  (new-style (keyword (str "height-" v)) {:height v}))

(defn width [v]
  (new-style (keyword (str "width-" v)) {:width v}))

;; borders

(defn rounded
  ([] (rounded 8))
  ([v]
  (new-style (keyword (str "rounded-" v))
             {:border-radius v}))
  ([tl bl] (rounded tl bl tl bl))
  ([tr br bl tl]
  (new-style (keyword (str "rounded-" tr "-" br "-" bl "-" tl))
             {:border-top-right-radius tr
              :border-bottom-right-radius br
              :border-bottom-left-radius bl
              :border-top-left-radius tl}))
  )

(defn border
  ([] (border 1))
  ([w] (border w "rgba(0,0,0,0.4)"))
  ([w c]
  (new-style (keyword (str "border-" w "-" c))
             {:border-width w
              :border-style "solid"
              :border-color c})))

(defn border-bottom
  ([] (border-bottom 1))
  ([w] (border-bottom w "rgba(0,0,0,0.4)"))
  ([w c]
  (new-style (keyword (str "border-bottom-" w "-" c))
             {:border-bottom-width w
              :border-bottom-color c})))

(defn border-top
  ([] (border-top 1))
  ([w] (border-top w "rgba(0,0,0,0.4)"))
  ([w c]
  (new-style (keyword (str "border-top-" w "-" c))
             {:border-top-width w
              :border-top-color c})))

(defn border-left
  ([] (border-left 1))
  ([w] (border-left w "rgba(0,0,0,0.4)"))
  ([w c]
  (new-style (keyword (str "border-left-" w "-" c))
             {:border-left-width w
              :border-left-style "solid"
              :border-left-color c})))

(defn border-right
  ([] (border-right 1))
  ([w] (border-right w "rgba(0,0,0,0.4)"))
  ([w c]
  (new-style (keyword (str "border-right-" w "-" c))
             {:border-right-width w
              :border-right-style "solid"
              :border-right-color c})))

(defn shadow
  ([] (shadow 4))
  ([h] (shadow 0 h))
  ([w h] (shadow w h h))
  ([w h r] (shadow w h r 4))
  ([w h r a]
  (new-style (keyword (str "text-shadow-" a "-" w "-" h "-" a))
             {:shadow-opacity 1
              :shadow-offset {:width w :height h}
              :shadow-radius r
              :shadow-color (str "rgba(0,0,0,0." a ")")
              :elevation h})))

;; box

(defn padding
  ([] (padding 8))
  ([v] (padding v v))
  ([t r] (padding t r t r))
  ([t r b l]
  (new-style (keyword (str "padding-" t "-" r "-" b "-" l))
             {:padding-top t
              :padding-right r
              :padding-bottom b
              :padding-left l})))

(defn padding-horizontal [v]
  (new-style (keyword (str "padding-horizontal-" v)) {:padding-horizontal v}))

(defn padding-vertical [v]
  (new-style (keyword (str "padding-vertical-" v)) {:padding-vertical v}))

(defn margin-horizontal [v]
  (new-style (keyword (str "margin-horizontal-" v)) {:margin-horizontal v}))

(defn margin-vertical [v]
  (new-style (keyword (str "margin-vertical-" v)) {:margin-vertical v}))

(defn margin
  ([] (margin 8))
  ([v] (margin v v))
  ([t r] (margin t r t r))
  ([t r b l]
  (new-style (keyword (str "margin-" t "-" r "-" b "-" l))
             {:margin-top t
              :margin-right r
              :margin-bottom b
              :margin-left l})))

(defn margin-top [v]
  (new-style (keyword (str "margin-top-" v)) {:margin-top v}))

(defn margin-left [v]
  (new-style (keyword (str "margin-left-" v)) {:margin-left v}))

(defn margin-bottom [v]
  (new-style (keyword (str "margin-bottom-" v)) {:margin-bottom v}))

(defn margin-right [v]
  (new-style (keyword (str "margin-right-" v)) {:margin-right v}))

(defn padding-top [v]
  (new-style (keyword (str "padding-top-" v)) {:padding-top v}))

(defn padding-left [v]
  (new-style (keyword (str "padding-left-" v)) {:padding-left v}))

(defn padding-bottom [v]
  (new-style (keyword (str "padding-bottom-" v)) {:padding-bottom v}))

(defn padding-right [v]
  (new-style (keyword (str "padding-right-" v)) {:padding-right v}))

;; position

(def position-absolute (new-style :position-absolute {:position "absolute"}))

(defn top [v]
  (new-style (keyword (str "top-" v)) {:top v}))

(defn left [v]
  (new-style (keyword (str "left-" v)) {:left v}))

(defn bottom [v]
  (new-style (keyword (str "bottom-" v)) {:bottom v}))

(defn right [v]
  (new-style (keyword (str "right-" v)) {:right v}))

;; transform

(defn transform [v]
  (new-style (keyword (str "transform-" v)) {:transform v}))

;; view

(defn overflow [v]
  (new-style (keyword (str "overflow-" v)) {:overflow v}))

(defn opacity [v]
  (new-style (keyword (str "opacity-" v)) {:opacity v}))

(defn border-width [v]; enum('visible', 'hidden')
  (new-style (keyword (str "border-width-" v)) {:border-width v}))

(defn border-style [v]
  (new-style (keyword (str "border-style-" v)) {:border-style v}))

(defn backface-visibility [v] ;enum('visible', 'hidden')
  (new-style (keyword (str "backface-visibility-" v)) {:backface-visibility v}))

(defn background-color [v]
  (new-style (keyword (str "background-color-" v)) {:background-color v}))

(defn border-top-color [v]
  (new-style (keyword (str "border-top-color-" v)) {:border-top-color v}))

(defn border-right-color [v]
  (new-style (keyword (str "border-right-color-" v)) {:border-right-color v}))

(defn border-bottom-color [v]
  (new-style (keyword (str "border-bottom-color-" v)) {:border-bottom-color v}))

(defn border-left-color [v]
  (new-style (keyword (str "border-left-color-" v)) {:border-left-color v}))

(defn border-top-width [v]
  (new-style (keyword (str "border-top-width-" v)) {:border-top-width v}))

(defn border-right-width [v]
  (new-style (keyword (str "border-right-width-" v)) {:border-right-width v}))

(defn border-bottom-width [v]
  (new-style (keyword (str "border-bottom-width-" v)) {:border-bottom-width v}))

(defn border-left-width [v]
  (new-style (keyword (str "border-left-width-" v)) {:border-left-width v}))

(defn border-bottom-left-radius [v]
  (new-style (keyword (str "border-bottom-left-radius-" v)) {:border-bottom-left-radius v}))

(defn border-bottom-right-radius [v]
  (new-style (keyword (str "border-bottom-right-radius-" v)) {:border-bottom-right-radius v}))

(defn border-top-left-radius [v]
  (new-style (keyword (str "border-top-left-radius-" v)) {:border-top-left-radius v}))

(defn border-top-right-radius [v]
  (new-style (keyword (str "border-top-right-radius-" v)) {:border-top-right-radius v}))

;; image

(defn border-color [v]
  (new-style (keyword (str "border-color-" v)) {:border-color v}))

(defn border-radius [v]
  (new-style (keyword (str "border-radius" v)) {:border-radius v}))


(defn resize-mode [v]
  (new-style (keyword (str "resize-mode" v)) {:resize-mode v}))

(defn overlay-color [v]
  (new-style (keyword (str "overlay-color" v)) {:overlay-color v}))

(defn tint-color [v]
  (new-style (keyword (str "tint-color" v)) {:tint-color v}))
