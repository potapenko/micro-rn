(ns micro-rn.components
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require
    [reagent.core :as r :refer [atom]]
    [reagent.impl.component :as ru]
    [micro-rn.styles :as s :refer [get-style new-style flex row column opacity background gray white red orange yellow olive green teal blue violet purple grey pink brown black rounded border height width align-center align-right padding margin padding-horizontal padding-vertical font-size bold color stretch position-absolute top left bottom right overflow shadow text-align text-shadow]]
    [micro-rn.utils :as utils]
    )
  )

(enable-console-print!)
(set! js/React (js/require "react-native"))

(def device (.get js/React.Dimensions "window"))

(defmacro create-components [names]
  (let [create-def (fn[prop-name component-name]
                     `(def ~(symbol prop-name)
                        (when-let [react-component (aget js/React ~component-name)]
                          (reagent.core/adapt-react-class react-component))))]
    `(do ~@(map create-def names))))

; build-in сomponents

(def image (r/adapt-react-class (.-Image js/React)))
(def list-view (r/adapt-react-class (.-ListView js/React)))
(def map-view (r/adapt-react-class (.-MapView js/React)))
(def modal (r/adapt-react-class (.-Modal js/React)))
(def refresh-contol (r/adapt-react-class (.-RefreshControl js/React)))
(def scroll-view (r/adapt-react-class (.-ScrollView js/React)))
(def switch (r/adapt-react-class (.-Switch js/React)))
(def text (r/adapt-react-class (.-Text js/React)))
(def text-input (r/adapt-react-class (.-TextInput js/React)))
(def touchable (r/adapt-react-class (.-TouchableWithoutFeedback js/React)))
(def touchable-highlight (r/adapt-react-class (.-TouchableHighlight js/React)))
(def touchable-native-feedback (r/adapt-react-class (.-TouchableNativeFeedback js/React)))
(def touchable-opacity (r/adapt-react-class (.-TouchableOpacity js/React)))
(def view (r/adapt-react-class (.-View js/React)))
(def animated-view (r/adapt-react-class (.-View (.-Animated js/React))))
(def web-view (r/adapt-react-class (.-WebView js/React)))
(def activity-indicator (r/adapt-react-class (.-ActivityIndicatorIOS js/React)))

; apis

(defn alert
  ([message] (.alert (.-Alert js/React) "Alert" message))
  ([title message] (.alert (.-Alert js/React) title message nil))
  ([title message buttons] (.alert (.-Alert js/React) title message (utils/prepare-to-js buttons))))

(def AppRegistry (.-AppRegistry js/React))
(def app-state (.-AppState js/React))
(def async-storage (.-AsyncStorage js/React))
(def camera-roll (.-CameraRoll js/React))
(def dimensions (.-Dimensions js/React))
(def interaction-manager (.-InteractionManager js/React))
(def Platform (.-Platform js/React))
(def os-android (= "android" (.-OS Platform)))
(def os-ios (= "ios" (.-OS Platform)))

(def layout-animation (.-LayoutAnimation js/React))
(def layout-animation-configure-next (.-configureNext layout-animation))
(def layout-animation-presets (.-Presets layout-animation))
(def layout-animation-presets-spring (.-spring layout-animation-presets))
(def layout-animation-presets-linear (.-linear layout-animation-presets))
(def layout-animation-presets-ease-in-ease-out (.-easeInEaseOut layout-animation-presets))

(defn animate-layout
   ([] (animate-layout "spring"))
   ([anim-type](let[type (case anim-type
                          "spring" layout-animation-presets-spring
                          "linear" layout-animation-presets-linear
                          "ease-in-ease-out" layout-animation-presets-ease-in-ease-out
                          layout-animation-presets-spring)]
                (layout-animation-configure-next type)))
  )

(def native-methods-mixin (.-NativeMethodsMixin js/React))
(def net-info (.-NetInfo js/React))
(def pan-responder (.-PanResponder js/React))
(def pixel-ratio (.-PixelRatio js/React))
(def style-sheet (.-StyleSheet js/React))

(defn animated-add [a b] (.add (.-Animated js/React) a b))
(defn animated-decay [value config] (.decay (.-Animated js/React) value config))
(defn animated-delay [time] (.delay (.-Animated js/React) time))
(defn animated-event [arg-mapping config] (.event (.-Animated js/React) arg-mapping config))
(defn animated-multiply [a b] (.multiply (.-Animated js/React) a b))
(defn animated-parallel [animations] (.parallel (.-Animated js/React) animations))
(defn animated-sequence [animations] (.sequence (.-Animated js/React) animations))
(defn animated-spring [value config] (.spring (.-Animated js/React) value config))
(defn animated-stagger [time animations] (.stagger (.-Animated js/React) time animations))
(defn animated-timing [value config] (.timing (.-Animated js/React) value config))
(defn animated-value [& args] (let[constructor (.-Value (.-Animated js/React))] (apply constructor. args)))
(defn animated-value-xy [& args] (let[constructor (.-ValueXY (.-Animated js/React))] (apply constructor. args)))

;; custom

(defn spacer [s]
  [view {:style [(width s) (height s)]}]
  )

(defn flexer []
  [view {:style [(flex)]}])

(def default-button-style [(overflow "hidden") (flex) (gray) (padding 4) (rounded) align-center (margin 1)])

(defn button
  ([label] (button {} label))
  ([props label]
   (let[{:keys [on-press ref background-color disabled tab-style underlay-color notification style]} props
        on-press (or on-press #(println "default on-press function"))
        button-style [default-button-style style
                      (when background-color (s/background background-color))
                      (case tab-style
                        "left" (rounded 0 0 8 8)
                        "center" (rounded 0 0 0 0)
                        "right" (rounded 8 8 0 0)
                        nil)]
      label (if (string? label) [view {:style [(flex) align-center]} [flexer][text {:style [(font-size 13) bold]} label][flexer]] label)
        button-component (if disabled view touchable-highlight)]
     [button-component
      {:ref ref
       :on-press (when-not disabled #(on-press %))
       :underlay-color (or underlay-color grey)
       :style button-style}

      [view
       label
        (when (and notification @notification)
          [view {:style [(overflow "hidden") (top -2) (right -14) (background red) align-center (rounded 10) s/position-absolute (width 16) (height 16)]}
           [text {:style [bold (font-size 9) (margin 2 0) (color "white")]} @notification]])
      ]
      ]))
  )

(defn toggle-button
  [props label]
  (let[{:keys [value state toggled]} props
       value (or value "toggled")
       state (or state (atom (if toggled value nil)))
       toggled (reaction (= @state value))
       ]
    (fn[props]
      (let[{:keys [on-press ref another-value disabled notification tab-style style selected-style background-color]} props
           style [default-button-style style]
           selected-style (or selected-style (gray 4))]
        [button
         {:on-press #(when-not disabled
                       (do
                         (reset! state (if (= @state value) another-value value))
                         (when on-press (on-press @state))))
          :notification notification
          :disabled disabled
          :ref ref
          :background-color background-color
          :tab-style tab-style
          :style (conj [] style (when @toggled selected-style))
          }
         label])))
  )

(println "reload core")
