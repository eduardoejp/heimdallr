;Version: 0.1.0
;Copyright: Eduardo Emilio Julián Pereyra, 2011
;Email: eduardoejp@gmail.com
;License: EPL 1.0 -> http://www.eclipse.org/legal/epl-v10.html

(ns heimdallr
  "Small lib for Context-Oriented Programming.")

(def #^{:doc "This dynamic var holds the current Context Stack for a thread."}
  *context* (with-meta '() {:type ::ContextStack}))
(def *layered-fn*)

(defn push-layer
  "Pushes a layer onto the Context Stack to be used globally by all layered functions."
  [layer] (def *context* (conj *context* layer)))

(defn pop-layer
"Pops the layer at the top of the Context Stack so it can't be used globally by all layered functions.
Returns the popped layer."
  []
  (let [popped (first *context*)]
    (def *context* (rest *context*))
    popped))

(defn _heim-dispatcher [& args]
  (if (-> args first type (= ::ContextStack))
    (ffirst args)
    (first *context*)))

(defmacro deflayer
"Defines a Layer.
The <requires> param must be a vector of layers that are required for this layer to work."
  ([sym] `(def ~sym (with-meta {:name '~sym, :requires []} {:type ::Layer})))
  ([sym requires] `(def ~sym (with-meta {:name '~sym, :requires '~requires} {:type ::Layer})))
  )

(defn layer? "" [l] (-> l type (= ::Layer)))

(defn proceed
"Given a layer currently on the stack and args for the partial definition, runs the code and returns the result.
Note #1: This fn can only be called inside a layered function.
Note #2: To use the base definition, pass the :base keyword as the layer.
Note #3: If no layer is given and the :base implementation is not requested,
         it will look for the first definition it can find for the layers in the stack
         by searching from the top to the bottom of the stack."
  [layer & args]
  (cond
    (layer? layer) (if (some #(= layer %) (rest *context*))
                     (apply (get-method *layered-fn* layer) args)
                     (throw (Exception. (str "The layer <" (:name layer) "> is not active in the Context Stack."))))
    (= :base layer) (apply (get-method *layered-fn* :default) args)
    :else (loop [ctx (rest *context*), impl (get-method *layered-fn* (first ctx))]
            (if impl (binding [*context* ctx] (apply impl layer args))
              (if (empty? ctx) (apply proceed :base args)
                (recur (rest ctx) (get-method *layered-fn* (fnext ctx))))
              ))
    ))

(defmacro with-context
"Executes the given forms inside the given context.
In order to activate multiple layers at the same time, pass the layers as a vector (example: [layer-1, layer-2, ...])
The latter layers will be nested inside the former ones as in:
(layer-1
  (layer-2
    (layer-3 ...)))"
  [ctx & forms]
  (if (vector? ctx)
    `(with-context ~(first ctx) (with-context ~(if (> (count (rest ctx)) 1) (vec (rest ctx)) (fnext ctx)) ~@forms))
    `(let [reqs# ~ctx]
       (if (or (empty? (:requires reqs#)) (->> (:requires reqs#) (map eval) (map #(some (fn [~'c] (= % ~'c)) *context*)) (every? true?)))
         (binding [*context* (if reqs# (conj *context* reqs#) *context*)] ~@forms)
         (throw (Exception. (str "The layer dependencies of <" (:name reqs#) ">, " (str (:requires reqs#)) " are not active in the Context Stack.")))))
    ))

(defmacro deflayered
"Defines a layered function. For the base definition, simply don't provide a layer.
For layer-dependant definitions, pass a layer before the args vector and sexps.
An optional doc-string can be provided for the base definition."
  [sym & extra]
  (let [ftoken (first extra)
        extra (if (or (string? ftoken) (symbol? ftoken)) (rest extra) extra)
        doc-str (if (string? ftoken) ftoken "")
        layer (when (symbol? ftoken) ftoken)]
    (if-not layer
      `(do (defmulti ~sym ~doc-str _heim-dispatcher) (defmethod ~sym :default ~@extra))
      (if (= ::Layer (type (eval layer)))
        (let [multi? (-> extra first vector? not)
              nxt (-> layer eval :next)
              nxt (if nxt`((proceed ~nxt ~@(first extra))) '())]
          (if multi?
            `(defmethod ~sym ~layer ~@(for [[args & forms] extra] `(~args (binding [*layered-fn* ~sym] ~@forms (proceed ~(-> layer eval :next) ~@args)))))
            `(defmethod ~sym ~layer ~(first extra) (binding [*layered-fn* ~sym] ~@(rest extra) ~@nxt))))
        (throw (Exception. (str layer " is not a Layer.")))
        ))))
