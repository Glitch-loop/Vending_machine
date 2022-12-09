; Fecha 06-04-22
; Alumno: Renet de Jesús Pérez Gómez
; Matricula: A01640555
(ns creador-pruebas.core (:gen-class))
(require '[clojure.java.io :as io])

;Crear pruebas
(defn crearTransaccion [ASIICMaximo vectorMonedas] 
  (list (if (<= ASIICMaximo 65) (str (char 65)) (str (char (+ 65 (rand-int (- ASIICMaximo 65)))))) (repeatedly (+ 1 (rand-int 9)) #(rand-nth vectorMonedas))))

(defn generarListaDeTransaccionesMaquina [idMaquina ASIICMaximo vectorMonedas noTransacciones] (list (repeatedly noTransacciones #(crearTransaccion ASIICMaximo vectorMonedas)) idMaquina))


(defn crearInventarioMonedas [listaMonedas]
  (map (fn [x] 
          (list x 
                (list (if (> x 2) (rand-int 10) ((fn [y] (if (<= y 10) (+ 10 y) y))(rand-int 50))) (if (> x 2) (rand-int 10) ((fn [y] (if (<= y 10) (+ 10 y) y))(rand-int 50)))) 
                (list (if (> x 2) (rand-int 20) ((fn [y] (if (<= y 10) (+ 20 y) y))(rand-int 100))) (Double/parseDouble (format "%.1f" (rand 0.30))) (Double/parseDouble  (format "%.1f" ((fn [y] (if (< y 0.30) (+ y 0.30) y))(rand)))))
          )) listaMonedas))

(defn inventarioProductos [listaProductos listaID]
  (pop listaProductos)
  (if (empty? (rest listaProductos))
    (list (list (str (char (first listaID))) (list (+ 1 (rand-int 49)) (first listaProductos)) (list (+ 1 (rand-int 19)) (+ 1 (rand-int 19))) (+ 1 (rand-int 19))))
    (concat (list (list (str (char (first listaID))) (list (+ 1 (rand-int 49)) (first listaProductos)) (list (+ 1 (rand-int 19)) (+ 1 (rand-int 19))) (+ 1 (rand-int 19))))
      (inventarioProductos (rest listaProductos) (rest listaID)))))

(defn crearInventarios [listaMonedas listaProductos listaID idMaquina] 
  (concat (cons (crearInventarioMonedas listaMonedas) nil) (cons (inventarioProductos listaProductos listaID) nil) (cons idMaquina nil))
  
  )

(defn -main
  [& args]

    (let [
            ;;Para transacciones
            numeroDeMaquinas 500
            idEnTransacciones 70 ;[65 = A, 90 = Z]
            ;Ingresa el codigo ASIC hasta el simbolo que quieras que "pueda" aparecer, siempre iniciara en 65 "A", el limite es 90 "Z" 
            monedasEnTransacciones (vector 1 2 5 10 20 50) 
            noTransacciones 20

            ;;Para inventarios
            denominacionMonedasPermitidas '(50 20 10 5 2 1)
            nombreProductos '(Coca_cola Sabritas Biscochos Pepsi)
          ]
        (spit (io/resource "creador_pruebas/transacciones.txt")
          (str (seq (map (fn [x] (generarListaDeTransaccionesMaquina x  idEnTransacciones monedasEnTransacciones noTransacciones)) (range 1 (+ numeroDeMaquinas 1) 1)))))
        (spit (io/resource "creador_pruebas/inventarios.txt")
          (str (seq (map (fn [x] (crearInventarios denominacionMonedasPermitidas nombreProductos (map #(+ % 65) (take (count nombreProductos) (range)))  x)) (range 1 (+ numeroDeMaquinas 1) 1))))))
        (println "Pruebas generadas")
          )
