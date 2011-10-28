;; Copyright (C) 2011, Eduardo Julián. All rights reserved.
;;
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0
;; (http://opensource.org/licenses/eclipse-1.0.php) which can be found
;; in the file epl-v10.html at the root of this distribution.
;;
;; By using this software in any fashion, you are agreeing to be bound
;; by the terms of this license.
;;
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Small lib for Context-Oriented Programming."
      :author "Eduardo Julián"}
  heimdallr
  (:require [clojure.set :as set]
            [clojure.walk :as walk]))

; Base Utilities
; Special thanks to Chris Houser for the original implementation in Contrib's Error-Kit
(defn- qualify-sym [sym]
  (if-let [v (resolve sym)]
    (apply symbol (map #(str (% (meta v))) [:ns :name]))
    (symbol (str (name (ns-name *ns*)) "/" (name sym)))))

; Heimdallr
(declare layer= fetch-layer active?)

(def ^{:doc "This dynamic var holds the current Context Stack for a thread."}
  ^:dynamic *stack* '())

(defrecord Layer [name requires involves implies excludes])

; Layer/Stack Utilities
(defn push-layer [cstack {:keys [name requires involves implies excludes], :as layer}]
  (if (or (vector? layer) (seq? layer)) ; If it's something seqable.
    (reduce push-layer cstack layer) ; Push all of its members.
    (if-not (active? layer cstack) ; If the layer is not active on the stack.
      (if (reduce #(and %1 %2) true (map (fn [r] (not (active? r cstack))) (map eval excludes))) ; Make sure there are no coliding layers active on the stack.
        (if (reduce #(and %1 %2) true (map (fn [r] (active? r cstack)) (map eval requires))) ; Check whether the requirements are in place.
          (let [cstack (map (fn [l] (if (some (fn [r] (= (:name l) (:name r))) (map eval requires)) ; If that's the case, make them know they have to deactive
                                      (update-in l [:deactivate] conj (:name layer))                ; this layer.
                                      l))
                            cstack)
                ; Also add layers involved (weak inclusion) and implied (strong inclusion), reference counting for when other layers involve/imply them.
                cstack (reduce (fn [s wi] (push-layer s wi)) cstack (map #(assoc % :ref-count 1) (map eval involves)))
                cstack (reduce (fn [s si] (push-layer s (update-in si [:deactivate] conj (:name layer)))) cstack (map #(assoc % :ref-count 1) (map eval implies)))]
            (conj cstack (update-in layer [:deactivate] concat (map (comp :name eval) involves) (map (comp :name eval) implies))))
          (throw (Exception. (str name " cannot be activated because the requirements are not in place: " (seq (map :name (map eval requires)))))))
        (throw (Exception. (str name " cannot be activated because there are contradictory layers: " (seq (map :name (map eval excludes)))))))
      (if (:ref-count layer) ; Reference counting is used for pushing and removing layers which are implied/involved by more than 1 layer.
        (map #(if (= (:name layer) (:name %))
                (if (:ref-count %) (update-in % [:ref-count] inc) (assoc % :ref-count 1))
                %)
             cstack)
        cstack))))

(defn remove-layer [cstack layer]
  (if (or (vector? layer) (seq? layer))
    (reduce remove-layer cstack layer)
    (if-let [layer (fetch-layer layer cstack)]
      (if (or (not (:ref-count layer)) (< (:ref-count layer) 2))
        (reduce (fn [s d] (remove-layer s (eval d)))
                (filter (fn [l] (not (layer= l layer))) cstack)
                (:deactivate layer))
        (map (fn [l] (if (layer= layer l) (update-in l [:ref-count] dec) l)) cstack))
      cstack)))

(defn push-layer!
  "Globally pushes a layer onto the Context Stack."
  [layer]
  (alter-var-root #'*stack* push-layer layer))

(defn pop-layer!
  "Globally pops the layer at the top of the Context Stack and removes any dependant layers. Returns the popped layer."
  []
  (let [popped (first *stack*)]
    (alter-var-root #'*stack* remove-layer popped)
    (dissoc popped :deactivate)))

(defn remove-layer!
  "Globally removes a layer from whatever place it is in the stack and removes any dependant layers."
  [layer]
  (alter-var-root #'*stack* remove-layer layer))

(defn layer? "" [l] (= heimdallr.Layer (class l)))

(defn layer= "Returns whether layers 1 and 2 are equal."
  [layer1 layer2]
  (= (:name layer1) (:name layer2)))

(defn- fetch-layer [layer cstack]
  (some (fn [l] (and (layer= l layer) l)) cstack))

(defn active? ""
  ([layer] (active? layer *stack*))
  ([layer cstack] (if (fetch-layer layer cstack) true false)))

; Layers/layered fns utilities.
(defmacro deflayer
  "Defines a layer.

The :requires param holds layers without which this layer cannot work. If the requirements are not met, an exception will be thrown.
The :involves param holds weak-inclusion layers. These layers will be activated when the source is pushed onto the stack,
  but can be deactivated without deactivating the source.
The :implies param holds strong-inclusion layers. These layers will be activated when the source is pushed onto the stack,
  and if deactivated, will also deactivate the source.
The :excludes param hold layers that will prevent this one from being activated. It is the opposite of :requires.
  If any of these layers is active on the stack, an exception will be thrown when trying to push this layer.

All parameters are optional."
  [sym & options]
  (let [[docs {:keys [requires, involves, implies, excludes]}] (if (string? (first options))
                                                                 [(first options) (apply hash-map (rest options))]
                                                                 [nil (apply hash-map options)])]
    `(def ~(vary-meta sym assoc :doc docs)
       (Layer. '~(qualify-sym sym)
               '~(map qualify-sym requires)
               '~(map qualify-sym involves)
               '~(map qualify-sym implies)
               '~(map qualify-sym excludes)))
    ))

(defmacro with-layer
  "Executes the given forms inside the given context(s).
In order to activate multiple layers at the same time, pass the layers as a vector (example: [layer-1, layer-2, ...])
The latter layers will be nested inside the former ones as in:
(layer-1
  (layer-2
    (layer-3 ...)))"
  [ctx & forms]
  `(binding [*stack* (push-layer *stack* ~ctx)] ~@forms))

(defmacro without-layer
  "Executes the given forms outside the given context(s).
In order to deactivate multiple layers at the same time, pass the layers as a vector (example: [layer-1, layer-2, ...])
If some layers depend on one or more of the layers to be deactivated, they will also be deactivated."
  [ctx & forms]
  `(binding [*stack* (remove-layer *stack* ~ctx)] ~@forms))

(defmacro deflayered
  "Defines a layered function. For the base definition, simply don't provide a layer.
For layer-dependant definitions, pass a layer before the args vector and sexps.
An optional doc-string can be provided for the base definition."
  [sym & extra]
  (let [[doc-str layer extra] (cond (string? (first extra)) [(first extra) nil (rest extra)]
                                    (symbol? (first extra)) [nil (first extra) (rest extra)]
                                    :else                   [nil nil extra])
        extra (if (vector? (first extra)) (list extra) extra)
        extra (map (fn [[args & forms]] ; Make 'proceed' available to the code using it by looking up the nearest implementation based on the stack.
                     (if (some (partial = 'proceed) (flatten forms))
                       (list args `(let [~'proceed (or (first (filter (partial not= (get-method ~sym :default))
                                                                      (map (partial get-method ~sym)
                                                                           (map :name
                                                                                (next (drop-while #(not (layer= ~layer %))
                                                                                                  *stack*))))))
                                                       (get-method ~sym nil))]
                                     ~@forms))
                       (cons args forms)))
                   extra)
        ]
    (if-not layer
      `(do (defmulti ~(vary-meta sym assoc :doc doc-str) (fn [& _#] (:name (first *stack*)))) ; Dispatch based on the name of the top layer's name.
         (defmethod ~sym :default [& args#]                                                   ; If no valid implementation is found for the top layer, find one for the others.
           (let [def# (get-method ~sym :default)]
             (apply (or (first (filter (partial not= def#)
                                       (map (partial get-method ~sym)
                                            (map :name *stack*))))
                        (get-method ~sym nil))
                    args#)))
         (defmethod ~sym nil ~@extra))                                                        ; Base implementation.
      (if (layer? (eval layer))
        `(defmethod ~sym (:name ~layer) ~@extra)                                              ; Layer-specific implementations.
        (throw (Exception. (str layer " is not a Layer.")))
        ))))
