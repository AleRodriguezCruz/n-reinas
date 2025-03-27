(ns n-reinas.core
  (:gen-class))

(defn seguro? [tablero fila columna]
  "Verifica si es seguro colocar una reina en la posición (fila, columna)"
  (let [n (count tablero)]
    (loop [i 0]
      (cond
        (= i n) true
        (or (= (nth (nth tablero i) columna) 1)
            (some (fn [r] (and (< r fila) (= (nth (nth tablero r) columna) 1))) (range n))) false
        :else (recur (inc i))))))

(defn colocar-reina [tablero fila]
  "Coloca una reina en la fila 'fila' en la primera columna segura"
  (let [n (count tablero)]
    (loop [columna 0]
      (cond
        (= columna n) nil
        (seguro? tablero fila columna) (let [nuevo-tablero (assoc-in tablero [fila columna] 1)]
                                         (if (= fila (dec n))
                                           nuevo-tablero
                                           (colocar-reina nuevo-tablero (inc fila))))
        :else (recur (inc columna))))))

(defn resolver-n-reinas [n]
  "Resuelve el problema de las N reinas para un tablero de tamaño n x n"
  (let [tablero-inicial (vec (repeat n (vec (repeat n 0))))]
    (colocar-reina tablero-inicial 0)))

(defn -main [& args]
  (let [n 4]
    (println "Solución para N reinas con n=" n)
    (println (resolver-n-reinas n))))
