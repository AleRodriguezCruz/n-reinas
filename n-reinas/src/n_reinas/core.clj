"Alejandra Rodriguez de la Cruz No.Control:22760049"

(ns n-reinas.core
  (:gen-class))

(defn seguro? [tablero fila columna]
  "Verifica si es seguro colocar una reina en la posición (fila, columna)"
  (let [n (count tablero)]
    (every?
     (fn [i]
       (and
        (not= (get-in tablero [i columna] 0) 1)  ; Misma columna
        (not (and (>= fila i) (>= columna i) (= (get-in tablero [(- fila i) (- columna i)] 0) 1)))  ; Diagonal superior izquierda
        (not (and (>= fila i) (>= (- (dec n) columna) i) (= (get-in tablero [(- fila i) (+ columna i)] 0) 1))))) ; Diagonal superior derecha
     (range fila))))

(defn colocar-reina [tablero fila]
  "Coloca una reina en la fila 'fila' y explora todas las soluciones"
  (let [n (count tablero)]
    (if (= fila n)
      tablero ; Solución encontrada, devuelve el tablero
      (loop [columna 0]
        (if (= columna n)
          nil ; No se encontró una solución en esta rama
          (if (seguro? tablero fila columna)
            (let [nuevo-tablero (assoc-in tablero [fila columna] 1)]
              (or (colocar-reina nuevo-tablero (inc fila))  ; Intenta colocar en la siguiente fila
                  (recur (inc columna))))  ; Si falla, prueba la siguiente columna
            (recur (inc columna))))))))

(defn resolver-n-reinas [n]
  "Resuelve el problema de las N reinas para un tablero de tamaño n x n"
  (colocar-reina (vec (repeat n (vec (repeat n 0)))) 0))

(defn -main [& args]
  (let [n 3
        solucion (resolver-n-reinas n)]
    (if solucion
      (do
        (println "La matriz de ejemplo es:")
        (doseq [fila solucion]
          (println (str "{" (clojure.string/join ", " fila) "}")))
        (println "Solución encontrada N="n))
      (println "No hay solución para N =" n))))
