; Fecha: 06-11-22
; Materia: Implementación de métodos computacionales (Gpo 850)

; Alumno: Renet de Jesús Pérez Gómez
; Matricula: A01640555

; Evidencia #3 Implementación de un simulador de una máquina expendedora (segunda parte)
(ns evidencia3-simulacion-maquinas-expendedoras.core (:gen-class))
(require '[clojure.java.io :as io])

; Función para replicadora de monedas
(defn replicarMoneda [moneda vecesRepetida] 
  (if (zero? vecesRepetida) '() (concat (cons moneda nil) (replicarMoneda moneda (- vecesRepetida 1)))))
  

;Función que genera la lista del cambio
(defn darCambio  [montoADarCambio inventarioMonedas]
    (cond 
        (empty? (rest inventarioMonedas))
            (cond
                (and 
                    (not (zero? (quot montoADarCambio (first (first inventarioMonedas)))))  ; Es diferente a "0" lo que tengo que dar de cambio de esta moneda
                    (not (zero? (first (first (rest (first inventarioMonedas))))))) ; El inventario de esta moneda es mayor a 0
                    (cond
                        (not (<= (first (first (rest (first inventarioMonedas)))) (quot montoADarCambio (first (first inventarioMonedas))))) ;¿El cambio que tenemos que dar de esta moneda es mayor a la cantidad que tenemos en inventario?
                            (replicarMoneda (first (first inventarioMonedas)) (quot montoADarCambio (first (first inventarioMonedas)))); Generamos una lista con la denominacion de la moneda actual del tamaño que se necesite                        
                        true (replicarMoneda (first (first inventarioMonedas)) (first (first (rest (first inventarioMonedas)))))); Generamos una lista con todas que tengamos en inventario de la moneda actual
                true '() ;Generamos una lista vacia (Esta moneda no paticipa en el cambio)    
        ) 
        true
            (cond
                (and 
                    (not (zero? (quot montoADarCambio (first (first inventarioMonedas))))) ;Es diferente a "0" lo que tengo que dar de cambio de esta moneda
                    (not (zero? (first (first (rest (first inventarioMonedas))))))) ; El inventario de esta moneda es mayor a 0
                        (concat 
                            (cond
                                (not (<= (first (first (rest (first inventarioMonedas)))) (quot montoADarCambio (first (first inventarioMonedas))))) ;¿El cambio que tenemos que dar de esta moneda es mayor a la cantidad que tenemos en inventario?
                                    (replicarMoneda (first (first inventarioMonedas)) (quot montoADarCambio (first (first inventarioMonedas)))); Generamos una lista con la denominacion de la moneda actual del tamaño que se necesite
                                true (replicarMoneda (first (first inventarioMonedas)) (first (first (rest (first inventarioMonedas)))))); Generamos una lista con todas que tengamos en inventario de la moneda actual                          
                            (darCambio (- montoADarCambio (* (first (first inventarioMonedas))
                            (if (not (<= (first (first (rest (first inventarioMonedas)))) (quot montoADarCambio (first (first inventarioMonedas)))))
                                (quot montoADarCambio (first (first inventarioMonedas))) (first (first (rest (first inventarioMonedas))))))); Si hay suficientes monedas restamos a monto el valor de las monedas necesarias, si no restamos lo que haya en inventario (vaciamos inventario)
                            (rest inventarioMonedas))); Actualizamos lista (quitamos "cambio"), actualizamos monto para dar cambio y seguimos con el resto de la lista
                true (concat '() (darCambio montoADarCambio (rest inventarioMonedas))); Generamos un alista vacia (esta moneda no participa en el cambio) y seguimos con el resto de la lista      
        )
))

;Actualiza los inventarios de sin dar cambio monedas "quitando para agregar nuevos cambios"
(defn actualizarInventarioSinCambio [montoADarCambio inventarioMonedas]
    (cond 
        (empty? (rest inventarioMonedas))
            (cond
                (and 
                    (not (zero? (quot montoADarCambio (first (first inventarioMonedas))))) ; Es diferente a "0" lo que tengo que dar de cambio de esta moneda
                    (not (zero? (first (first (rest (first inventarioMonedas))))))) ; El inventario de esta moneda es mayor a 0
                    (list (list 
                        (first (first inventarioMonedas))
                        ((fn [x]
                                (list
                                    (cond 
                                        (not (<= (first (first (rest (first inventarioMonedas)))) (quot montoADarCambio (first (first inventarioMonedas))))); ¿Hay mas monedas en el inventario que lo que tenemos que dar de cambio?
                                            (- (first x) (quot montoADarCambio (first (first inventarioMonedas)))); Quitamos del inventario las monedas que tenemos que dar de cambio
                                        true 0);Significa que voy a dar todo el inventario, tambien lo podriamos ver como (- (first x) (first x))
                                    (second x)
                                ))(first (rest (first inventarioMonedas))))
                        (first (rest (rest (first inventarioMonedas)))))); Actualizamos lista
                true (list (first inventarioMonedas)) ;Simplemente mandamos lista
        )
        true (cond
            (and 
                (not (zero? (quot montoADarCambio (first (first inventarioMonedas))))) ;Es diferente a "0" lo que tengo que dar de cambio de esta moneda
                (not (zero? (first (first (rest (first inventarioMonedas)))))))  ; El inventario de esta moneda es mayor a 0
            (concat 
                (list (list 
                    (first (first inventarioMonedas)) ;Denominacion moneda
                    ((fn [x]
                            (list
                                (cond 
                                    (not (<= (first (first (rest (first inventarioMonedas)))) (quot montoADarCambio (first (first inventarioMonedas)))))
                                        (- (first x) (quot montoADarCambio (first (first inventarioMonedas)))); Restamos las monedas que tenemos que dar de cambio (inventario actual)                                   
                                    true 0);Significa que voy a dar todo, tambien lo podriamos ver como (- (first x) (first x)) (inventario actual)
                                (second x) ;Inventario minimo
                            ))(first (rest (first inventarioMonedas))))
                    (first (rest (rest (first inventarioMonedas))))));Cantidad inical
                (actualizarInventarioSinCambio 
                (- montoADarCambio (* (first (first inventarioMonedas)) 
                    (if (not (<= (first (first (rest (first inventarioMonedas)))) (quot montoADarCambio (first (first inventarioMonedas)))));Si hay suficientes monedas, entonces restamos el valor de las monedas requeridas, sinó, el valos de lo que hay en el inventario (vaciamos inventario)
                                    (quot montoADarCambio (first (first inventarioMonedas))) (first (first (rest (first inventarioMonedas))))))) 
                (rest inventarioMonedas))); Actualizamos la lista de la moneda actual (quitandole el cambio a dar), actualizamos monto y continuamos con el resto de la lista 
            true (concat (list (first inventarioMonedas)) (actualizarInventarioSinCambio montoADarCambio (rest inventarioMonedas))) ; No hay necesidad de modificar la lista de la moneda actual, ni de actualizar el monto que sobra a dar de cambio, seguimos con el resto de la lista    
        )
))

; Actualiza inventario de monedas (basicamente agrega las monedas recibidas, recordar que las recibimos en formato lista; (10 5 5 1))
(defn actualizarMonedas [inventarioMonedas monedasRecibidas]
    (cond
        (empty? (rest monedasRecibidas)) 
          (map (fn [x]
                    (if (= (first monedasRecibidas) (first x)) ; La moneda recibida coincide con la moneda de la denominación actual
                        (list (first x) (list (+ 1 (first (second x))) (second (second x))) (second (rest x))) ; +1 Al inventario de la moneda actual 
                        x)) inventarioMonedas);La lista como esta actualmente
        true (actualizarMonedas
            (map (fn [x]
                    (if (= (first monedasRecibidas) (first x)) ; La moneda recibida coincide con la moneda de la denominación actual
                        (list (first x) (list (+ 1 (first (second x))) (second (second x))) (second (rest x))); +1 Al inventario de la moneda actual 
                        x)) inventarioMonedas) ;La lista como esta actualmente
            (rest monedasRecibidas)); Seguimos con el resto de la lista (de monedas recibidas por el usuario)
))

; Actualiza inventario de productos (basicamente quita en 1 el producto vendido)
(defn actualizarProducto [inventarioProducto idProducto]
    (map (fn [x] (if (= (first x) idProducto) 
                    (list (first x)(second x)((fn [y] (list (- (first y) 1) (second y))) (second (rest x))) (second (rest (rest x)))) ;Quitamos al inventario 1.
                    x)) inventarioProducto)); Devolvemos el inventaio tal cual


;Función para el caso en que la suma de las monedas del usuario, sea superior al precio 
(defn ventaCambio [monedasRecibidas precioProducto idProducto inventarioActual idMaquinaProcedimiento ticket]
    (cond 
        ; Si la suma total de monedas factibles para dar cambio es mayor al "monto de cambio"  
        (<= (- (apply + monedasRecibidas) precioProducto) (apply + (map (fn [x] (if (> (first x) (- (apply + monedasRecibidas) precioProducto)) 0 (* (first x) (first (second x))))) (first inventarioActual))));Obtener el monto que se dara de cambio  
            ((fn [x]
                (apply + (map (fn [x] (if (> (first x) (- (apply + monedasRecibidas) precioProducto)) 0 (* (first x) (first (second x))))) (first inventarioActual))) ;Sumar el total de monedas que pueden darse de cambio (osea si el cambio es $42, no puedes dar uno de $50)
                
                ; Comenzamos proceso para dar cambio
                ;Generamos la lista del cambio para el usuario
                (println (str ticket
                    "Precio del producto a comprar: $" precioProducto "\n"
                    "Gracias por su compra, dinero recibido: $" (apply + monedasRecibidas) "\n"
                    "Ten tu cambio: " (str (seq (darCambio (- (apply + monedasRecibidas) precioProducto) (first inventarioActual)))) 
                    "\n"))
                ;A el inventarios actualizados les quitamos el sobrante
                (concat 
                      (cons (actualizarInventarioSinCambio (- (apply + monedasRecibidas) precioProducto) (actualizarMonedas (first inventarioActual) monedasRecibidas)) nil) ;Inventario monedas sin cambio  
                      (cons (actualizarProducto (second inventarioActual) idProducto) nil) ;Inventario producto
                      (cons (second (rest inventarioActual)) nil)) ;ID de maquina
            )"")
        true ((fn [x] 
                (println (str ticket "Cambio no factible de entregar\n")) inventarioActual)"")))

;Función para el caso en que la suma de las monedas del usuario, sea igual al precio 
(defn ventaIgual [monedasRecibidas precioProducto idProducto inventarioActual idMaquinaProcedimiento ticket]
    (println (str ticket 
            "Precio del producto a comprar: $" precioProducto "\n"
            "Gracias por su compra, dinero recibido: $" (apply + monedasRecibidas) "\n"
            "\n"))
    ; Actualizamos ambos inventario (monedas y productos) y guardamos
      (concat (cons (actualizarMonedas (first inventarioActual) monedasRecibidas) nil) (cons (actualizarProducto (second inventarioActual) idProducto) nil) (cons (second (rest inventarioActual)) nil))
    )

;Función para el caso en que las monedas del usuario no ajuste para el producto
(defn noAjusta [monedasRecibidas precioProducto inventarioActual idMaquinaProcedimiento ticket]
    (println (str ticket
                "Precio del producto a comprar: $" precioProducto "\n"
                "El dinero recibido no ajusta para el producto: $" (apply + monedasRecibidas) "\n"))
    inventarioActual)

;Función para validar si una moneda es de denominación correcta (valida si es una transición valia para el "automata")
(defn validarMoneda [transitions userInput]
    (cond
        
        (empty? (rest transitions)) (if (= (first transitions) userInput) userInput false)
        true (if (= (first transitions) userInput) userInput (validarMoneda (rest transitions) userInput))))

;Funcion que usa al automata
(defn pago [automata listUserInput producto inventarioActual idMaquinaProcedimiento ticket]
  (cond 
      (= (apply + (first automata)) (second (rest automata))) (ventaIgual (first automata) (second (rest automata)) producto inventarioActual idMaquinaProcedimiento ticket); Si el estado actual es igual al estado aceptor (compra justa)
      (> (apply + (first automata)) (second (rest automata))) (ventaCambio (first automata) (second (rest automata)) producto inventarioActual idMaquinaProcedimiento ticket); Si el estado actual ya supero al estado aceptor (compra con cambio)
      (empty? (rest listUserInput)) 
          (cond 
              (false? (validarMoneda (second automata) (first listUserInput)))  
                (noAjusta (first automata) (second (rest automata)) inventarioActual idMaquinaProcedimiento  (str ticket "Moneda no permitida" "\n")); Es la ultima moneda y es una moneda no permitida
              (< (+ (apply + (first automata)) (validarMoneda (second automata) (first listUserInput))) (second (rest automata))) (noAjusta (concat (first automata) listUserInput) (second (rest automata)) inventarioActual idMaquinaProcedimiento ticket);Si es la ultima moneda, es permitida, pero no logra ajustar
              true (pago 
                        (list (concat (first automata)(cons (validarMoneda (second automata) (first listUserInput)) nil)) (second automata) (second (rest automata))) 
                        '() 
                        producto
                        inventarioActual
                        idMaquinaProcedimiento
                        ticket) ;Con la ultima moneda logra ajusta para el producto... Actualizamos automata y llamamos recursivamente (para su validación)
      );Es la ultima moneda
      true 
          (cond 
              (false? (validarMoneda (second automata) (first listUserInput))) 
                (pago automata (rest listUserInput) producto inventarioActual idMaquinaProcedimiento (str ticket "Moneda no permitida" "\n")); Seguimos con el resto de inputs de usuario  
              true
                  (pago 
                      (list (concat (first automata)(cons (validarMoneda (second automata) (first listUserInput)) nil)) (second automata) (second (rest automata))) ;Actualizamos automata
                      (rest listUserInput); Seguimos con el resto de inputs de usuario
                      producto
                      inventarioActual
                      idMaquinaProcedimiento
                      ticket); Mandamos producto
      )))

;Función que determina si existe un producto y de existir, si este tiene existencias
(defn existe-producto? [venta listaProductos inventarioActual idMaquinaProcedimiento]
    (cond 
        (empty? (rest listaProductos))  
            (if (= (first venta) (first (first listaProductos))) 
                ((fn [x]
                    (cond 
                        (> (first x) 0) 
                            (pago 
                                (concat (cons '(0) nil) (cons (map first (first inventarioActual)) nil) (cons (first (second (first listaProductos))) nil)) ;En esta linea es donde se construye elautomata
                                (second venta) 
                                (first venta)
                                inventarioActual
                                idMaquinaProcedimiento
                                (str "\nMaquina " idMaquinaProcedimiento "\nProducto a comprar: " (rest (second (first listaProductos))) "\n"))
                        true ((fn [y] (println (str "Maquina: " idMaquinaProcedimiento "\nNo hay existencias del producto solicitado. Producto: " (rest (second (first listaProductos))) "\n")) inventarioActual)""));Regresamos inventario actual
                )(second (rest (first listaProductos)))) 
                ((fn [y] (println (str "\nMaquina: " idMaquinaProcedimiento "\nEl producto con el ID: \"" (first venta) "\" no existe\n")) inventarioActual)"") ;Regresamos inventario actual
                ); Si estamos en la ultima posición
        (= (first venta) (first (first listaProductos))) 
            ((fn [x] 
                (cond 
                    (> (first x) 0)
                    (pago 
                        (concat (cons '(0) nil) (cons (map first (first inventarioActual)) nil) (cons (first (second (first listaProductos))) nil)) ;En esta linea es donde se construye elautomata
                        (second venta) 
                        (first venta)
                        inventarioActual
                        idMaquinaProcedimiento
                        (str "Maquina " idMaquinaProcedimiento "\nProducto a comprar: " (rest (second (first listaProductos))) "\n"))
                    true ((fn [y] (println (str "Maquina: " idMaquinaProcedimiento "\nNo hay existencias del producto solicitado. Producto: " (rest (second (first listaProductos))) "\n")) inventarioActual)"") ;Regresamos inventario actual
                ))(second (rest (first listaProductos)))); Si el producto actual es el que estamos buscando
        true (existe-producto? venta (rest listaProductos) inventarioActual idMaquinaProcedimiento)))

;Funcion para comenzar el proceso de una transaccion (el inventario resultante del proceso "anterior", sera el inventario que se usara en el proceso "actual")
(defn procesarTransacciones [transacciones inventarioActual idMaquinaProcedimiento] 
    (if (empty? (rest transacciones))
        (existe-producto? (first transacciones) (second inventarioActual) inventarioActual idMaquinaProcedimiento)
        (procesarTransacciones (rest transacciones) (existe-producto? (first transacciones) (second inventarioActual) inventarioActual idMaquinaProcedimiento) idMaquinaProcedimiento)
    ))

;;Funciones para analiticas
(defn ganaciaTotalObtenida [inventarios] 
    (apply + (map (fn [x] (apply + (map (fn [y] (* (first y) (first (second y)))) (first x)))) inventarios)))

(defn gananciaPorInventarios [inventarios obtenerTop] 
    (if (= 0.0 (- obtenerTop 1)) (str (first (first inventarios)) " - Ganancia de dicha maquina: $" (second (first inventarios))"\n") (str (first (first inventarios)) " - Ganancia de dicha maquina: $" (second (first inventarios))"\n" (gananciaPorInventarios (rest inventarios) (- obtenerTop 1)))))

(defn resurtidoProducto [inventarios]
    (apply str (map (fn [x] 
        (let [productosCasiVacios 
            (apply str (map 
                (fn [z] (str "\nID producto: " (first z) " - Nombre de producto: \"" (second (second z)) "\" - Cantidad actual: " (first (second (rest z))) " - inventario idoneo: " (second (second (rest z)))))
                (filter (fn [y] (< (first (second (rest y))) (second (second (rest y)))))(second x))))
            ] (if (= "" productosCasiVacios) "" (str "\nMaquina: " (second (rest x)) productosCasiVacios)))) inventarios)))

(defn resurtidoMonedas [inventarios]
    (apply str (map (fn [x] 
        (let [productosCasiVacios 
            (apply str (map 
                (fn [z] (str "\nLa moneda con la denominacion: $" (first z) " - Cantidad actual: " (first (second z)) " - Cantidad idonea: "(first(second (rest z)))))
                (filter (fn [y] (< (first (second y)) (* (first (second (rest y))) (second (second (rest y))))
                )) (first x))))
            ] (if (= "" productosCasiVacios) "" (str "\nMaquina: " (second (rest x)) productosCasiVacios)))) inventarios)))

;;Funcion para determinar cuando el inventario esta casi lleno
(defn vaciarMonedas [inventarios]
    (apply str (map (fn [x] 
        (let [productosCasiVacios 
            (apply str (map 
                (fn [z] (str "\nLa moneda con la denominacion: $" (first z) " - Cantidad actual: " (first (second z)) " - Cantidad idonea: "(first(second (rest z)))))
                (filter (fn [y] (>= (first (second y)) (* (first (second (rest y))) (second (rest (second (rest y)))))
                )) (first x))))
            ] (if (= "" productosCasiVacios) "" (str "\nMaquina: " (second (rest x)) productosCasiVacios)))) inventarios))
    )

(defn -main
  [& args]
  (println "Escribe el nombre del archivo de \"inventarios\" (no olvides poner la extension \".txt\")")
  (def nombreArchivoInventario (str "evidencia3_simulacion_maquinas_expendedoras/" (read-line)))
  
  (println "Escribe el nombre del archivo de \"transacciones\" (no olvides poner la extension \".txt\")")
  (def nombreArchivotransacciones (str "evidencia3_simulacion_maquinas_expendedoras/" (read-line)))
    (time
        ((fn [x]    
        (def inventario (clojure.edn/read-string (slurp (io/resource nombreArchivoInventario))))        
        
        (println (spit (io/resource nombreArchivoInventario)
        (str (seq (pmap (fn [idMaquinaProcedimiento] 
        (def transacciones (clojure.edn/read-string (slurp (io/resource nombreArchivotransacciones))))
            (procesarTransacciones 
                (first (first (filter (fn [transaccionesMaquinaActual] (= idMaquinaProcedimiento (second transaccionesMaquinaActual))) transacciones))) ;Pasamos las transacciones de la maquina actual
                    (first (filter (fn [inventarioMaquinaActual] (= idMaquinaProcedimiento (second (rest inventarioMaquinaActual)))) inventario)) ;Inventario del proceso actual
                        idMaquinaProcedimiento)
        ) (range 1 (+ (count inventario) 1) 1))))))    
        
        (def inventariosParaAnalisis (clojure.edn/read-string (slurp (io/resource nombreArchivoInventario))))    
        (println "--- RESULTADOS DEL DIA ---")
        (println "Ganancia obtenida: $"  (ganaciaTotalObtenida inventariosParaAnalisis))
        
        (println "\nTop 10% maquinas con mas ganancia: " )
        (println (gananciaPorInventarios (sort-by second > (map (fn [x] (list (str "ID maquina: "(second (rest x))) (apply + (map (fn [y] (* (first y) (first (second y)))) (first x))))) inventariosParaAnalisis)) (Math/ceil (* 0.10 (count inventariosParaAnalisis)))))
        
        (println "\nMaquinas que necesitan resurtido de producto" )
        (println (resurtidoProducto inventariosParaAnalisis))
        
        (println "\nMaquinas que necesitan resurtido de monedas" )
        (println (resurtidoMonedas inventariosParaAnalisis))
        
        (println "\nMaquinas con inventarios casi llenos")
        (println (vaciarMonedas inventariosParaAnalisis))
        (println "Proceso finalizado - PARALELO")
        ""
        )"")))
