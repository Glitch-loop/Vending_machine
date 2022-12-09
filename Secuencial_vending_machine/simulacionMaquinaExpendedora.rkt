#lang Racket
; Fecha: 05-07-22
; Materia: Implementación de métodos computacionales (Gpo 850)
; Evidencia #2 Implementación de un simulador de una máquina expendedora (primera parte)

; Alumno: Renet de Jesús Pérez Gómez
; Matricula: A01640555

; Entregable 2: Simulación de una maquina expendedora

;Consideraciones
; En las funciones "ventaCambio", "ventaIgual", "existe-producto?" y al comienzo de toda la simulación (final del documento)

; Función para replicadora de monedas
(define (replicarMoneda moneda vecesRepetida) 
    (if (zero? vecesRepetida) '() (append (cons moneda null) (replicarMoneda moneda (- vecesRepetida 1)))))

;Función que genera la lista del cambio
(define (darCambio  montoADarCambio inventarioMonedas)
    ; (writeln (car inventarioMonedas))
    (cond 
        ((null? (cdr inventarioMonedas))
            (cond
                ((and 
                    (not (zero? (quotient montoADarCambio (caar inventarioMonedas))))  ; Es diferente a "0" lo que tengo que dar de cambio de esta moneda
                    (not (zero? (caadar inventarioMonedas)))) ; El inventario de esta moneda es mayor a 0
                    (cond
                        ((not (<= (caadar inventarioMonedas) (quotient montoADarCambio (caar inventarioMonedas)))) ;¿El cambio que tenemos que dar de esta moneda es mayor a la cantidad que tenemos en inventario?
                            (replicarMoneda (caar inventarioMonedas) (quotient montoADarCambio (caar inventarioMonedas)))); Generamos una lista con la denominacion de la moneda actual del tamaño que se necesite                        
                        (else (replicarMoneda (caar inventarioMonedas) (caadar inventarioMonedas))))); Generamos una lista con todas que tengamos en inventario de la moneda actual
                (else '()) ;Generamos una lista vacia (Esta moneda no paticipa en el cambio)    
        )) 
        (else
            (cond
                ((and 
                    (not (zero? (quotient montoADarCambio (caar inventarioMonedas)))) ;Es diferente a "0" lo que tengo que dar de cambio de esta moneda
                    (not (zero? (caadar inventarioMonedas)))) ; El inventario de esta moneda es mayor a 0
                        (append 
                            (cond
                                ((not (<= (caadar inventarioMonedas) (quotient montoADarCambio (caar inventarioMonedas)))) ;¿El cambio que tenemos que dar de esta moneda es mayor a la cantidad que tenemos en inventario?
                                    (replicarMoneda (caar inventarioMonedas) (quotient montoADarCambio (caar inventarioMonedas)))); Generamos una lista con la denominacion de la moneda actual del tamaño que se necesite
                                (else (replicarMoneda (caar inventarioMonedas) (caadar inventarioMonedas)))); Generamos una lista con todas que tengamos en inventario de la moneda actual                          
                            (darCambio (- montoADarCambio (* (caar inventarioMonedas)
                            (if (not (<= (caadar inventarioMonedas) (quotient montoADarCambio (caar inventarioMonedas))))
                                (quotient montoADarCambio (caar inventarioMonedas)) (caadar inventarioMonedas)))); Si hay suficientes monedas restamos a monto el valor de las monedas necesarias, si no restamos lo que haya en inventario (vaciamos inventario)
                            (cdr inventarioMonedas)))); Actualizamos lista (quitamos "cambio"), actualizamos monto para dar cambio y seguimos con el resto de la lista
                (else (append '() (darCambio montoADarCambio (cdr inventarioMonedas)))); Generamos un alista vacia (esta moneda no participa en el cambio) y seguimos con el resto de la lista      
        ))
))

;Actualiza los inventarios de monedas "quitando para agregar nuevos cambios"
(define (actualizarInventarioSinCambio montoADarCambio inventarioMonedas)
    (cond 
        ((null? (cdr inventarioMonedas))
            (cond
                ((and 
                    (not (zero? (quotient montoADarCambio (caar inventarioMonedas)))) ; Es diferente a "0" lo que tengo que dar de cambio de esta moneda
                    (not (zero? (caadar inventarioMonedas)))) ; El inventario de esta moneda es mayor a 0
                    (list (list 
                        (caar inventarioMonedas)
                        ((lambda (x)
                                (list
                                    (cond 
                                        ((not (<= (caadar inventarioMonedas) (quotient montoADarCambio (caar inventarioMonedas)))); ¿Hay mas monedas en el inventario que lo que tenemos que dar de cambio?
                                            (- (car x) (quotient montoADarCambio (caar inventarioMonedas)))); Quitamos del inventario las monedas que tenemos que dar de cambio
                                        (else 0));Significa que voy a dar todo el inventario, tambien lo podriamos ver como (- (car x) (car x))
                                    (cadr x)
                                ))(cadar inventarioMonedas))
                        (caddar inventarioMonedas)))); Actualizamos lista
                (else (list (car inventarioMonedas))) ;Simplemente mandamos lista
        ))
        (else (cond
            ((and 
                (not (zero? (quotient montoADarCambio (caar inventarioMonedas)))) ;Es diferente a "0" lo que tengo que dar de cambio de esta moneda
                (not (zero? (caadar inventarioMonedas))))  ; El inventario de esta moneda es mayor a 0
            (append 
                (list (list 
                    (caar inventarioMonedas) ;Denominacion moneda
                    ((lambda (x)
                            (list
                                (cond 
                                    ((not (<= (caadar inventarioMonedas) (quotient montoADarCambio (caar inventarioMonedas))))
                                        (- (car x) (quotient montoADarCambio (caar inventarioMonedas)))); Restamos las monedas que tenemos que dar de cambio (inventario actual)                                   
                                    (else 0 ));Significa que voy a dar todo, tambien lo podriamos ver como (- (car x) (car x)) (inventario actual)
                                (cadr x) ;Inventario minimo
                            ))(cadar inventarioMonedas))
                    (caddar inventarioMonedas)));Cantidad inical
                (actualizarInventarioSinCambio 
                (- montoADarCambio (* (caar inventarioMonedas) 
                    (if (not (<= (caadar inventarioMonedas) (quotient montoADarCambio (caar inventarioMonedas))));Si hay suficientes monedas, entonces restamos el valor de las monedas requeridas, sinó, el valos de lo que hay en el inventario (vaciamos inventario)
                                    (quotient montoADarCambio (caar inventarioMonedas)) (caadar inventarioMonedas)))) 
                (cdr inventarioMonedas)))); Actualizamos la lista de la moneda actual (quitandole el cambio a dar), actualizamos monto y continuamos con el resto de la lista 
            (else (append (list (car inventarioMonedas)) (actualizarInventarioSinCambio montoADarCambio (cdr inventarioMonedas)))) ; No hay necesidad de modificar la lista de la moneda actual, ni de actualizar el monto que sobra a dar de cambio, seguimos con el resto de la lista    
        ))
))

; Actualiza inventario de monedas (basicamente agrega las monedas recibidas, recordar que las recibimos en formato lista; (10 5 5 1))
(define (actualizarMonedas inventarioMonedas monedasRecibidas)
    (cond
        ((null? (cdr monedasRecibidas)) 
                (map (lambda (x)
                    (if (= (car monedasRecibidas) (car x)) ; La moneda recibida coincide con la moneda de la denominación actual
                        (list (car x) (list (+ 1 (caadr x)) (cadadr x)) (caddr x)) ; +1 Al inventario de la moneda actual 
                        x)) inventarioMonedas))  ;La lista como esta actualmente
        (else (actualizarMonedas 
            (map (lambda (x)
                    (if (= (car monedasRecibidas) (car x)) ; La moneda recibida coincide con la moneda de la denominación actual
                        (list (car x) (list (+ 1 (caadr x)) (cadadr x)) (caddr x)); +1 Al inventario de la moneda actual 
                        x)) inventarioMonedas) ;La lista como esta actualmente
            (cdr monedasRecibidas))); Seguimos con el resto de la lista (de monedas recibidas por el usuario)
))

; Actualiza inventario de productos (basicamente quita en 1 el producto vendido)
(define (actualizarProducto inventarioProducto idProducto)
    (map (lambda (x) (if (equal? (car x) idProducto) 
                    (list (car x)(cadr x)((lambda (y)(list (- (car y) 1) (cadr y))) (caddr x)) (cadddr x)) ;Quitamos al inventario 1.
                    x)) inventarioProducto)); Devolvemos el inventaio tal cual

;Función para el caso en que la suma de las monedas del usuario, sea superior al precio 
(define (ventaCambio monedasRecibidas precioProducto idProducto)
    (writeln "")
    (define archInput (open-input-file "C:\\Users\\renet\\OneDrive\\Documentos\\Tec de monterrey\\Semestre 4\\Implementacion de metodos computacionales\\Periodo 2\\Ejercicio diario\\entregable2\\maquinaExpendedora\\inventarios.txt")) ; Obtenemos transacciones  
    (define inventario (read archInput))
    (close-input-port archInput)

    (cond  
        ((<= (- (apply + monedasRecibidas) precioProducto);Obtener el monto que se dara de cambio  
             (apply + (map (lambda (x) (if (> (car x) (- (apply + monedasRecibidas) precioProducto)) 0 (* (car x) (caadr x)))) (car inventario)))) ;Sumar el total de monedas que pueden darse de cambio (osea si el cambio es $42, no puedes dar uno de $50)
            (define inventarioNuevo (open-output-file #:exists 'replace "C:\\Users\\renet\\OneDrive\\Documentos\\Tec de monterrey\\Semestre 4\\Implementacion de metodos computacionales\\Periodo 2\\Ejercicio diario\\entregable2\\maquinaExpendedora\\inventarios.txt")) ; Obtenemos inventarios

            (write "Precio del producto a comprar: $")(writeln precioProducto)
            (write "Gracias por su compra, dinero recibido: $")(writeln (apply + monedasRecibidas))
            
            ; Actualizamos ambos inventario (monedas y productos) y guardamos
            (write (append (cons (actualizarMonedas (car inventario) monedasRecibidas) null) (cons (actualizarProducto (cadr inventario) idProducto) null)) inventarioNuevo)
            (close-output-port inventarioNuevo)

            ; Comenzamos proceso para dar cambio
            ;Obtenemos los inventarios ya actualizados 
            (define archInputSobreante (open-input-file "C:\\Users\\renet\\OneDrive\\Documentos\\Tec de monterrey\\Semestre 4\\Implementacion de metodos computacionales\\Periodo 2\\Ejercicio diario\\entregable2\\maquinaExpendedora\\inventarios.txt")) ; Obtenemos inventarios
            (define inventarioDevolverCambio (read archInputSobreante))
            (close-input-port archInputSobreante)

            ;A los inventarios actualizados les quitamos el sobrante
            (define inventarioNuevoSinSobrante (open-output-file #:exists 'replace "C:\\Users\\renet\\OneDrive\\Documentos\\Tec de monterrey\\Semestre 4\\Implementacion de metodos computacionales\\Periodo 2\\Ejercicio diario\\entregable2\\maquinaExpendedora\\inventarios.txt")) ; Creamos puerto para actualizar
            (write (append (cons (actualizarInventarioSinCambio (- (apply + monedasRecibidas) precioProducto) (car inventarioDevolverCambio)) null) (cons (cadr inventarioDevolverCambio) null)) inventarioNuevoSinSobrante) ;Actualizamos 
            (close-output-port inventarioNuevoSinSobrante) 

            ;Generamos la lista del cambio para el usuario
            (write "Ten tu cambio: ") (writeln (darCambio (- (apply + monedasRecibidas) precioProducto) (car inventarioDevolverCambio))); Entregamos cambio 
        ); Si la suma total de monedas factibles para dar cambio es mayor al "monto de cambio" 
        (else (writeln "Cambio no factible de entregar"))))

;Función para el caso en que la suma de las monedas del usuario, sea igual al precio 
(define (ventaIgual monedasRecibidas precioProducto idProducto)
    (writeln "")
    (define archInput (open-input-file "C:\\Users\\renet\\OneDrive\\Documentos\\Tec de monterrey\\Semestre 4\\Implementacion de metodos computacionales\\Periodo 2\\Ejercicio diario\\entregable2\\maquinaExpendedora\\inventarios.txt")) ; Obtenemos transacciones  
    (define inventario (read archInput))
    (close-input-port archInput)

    (define inventarioNuevo (open-output-file #:exists 'replace "C:\\Users\\renet\\OneDrive\\Documentos\\Tec de monterrey\\Semestre 4\\Implementacion de metodos computacionales\\Periodo 2\\Ejercicio diario\\entregable2\\maquinaExpendedora\\inventarios.txt")) ; Obtenemos transacciones  

    (write "Precio del producto a comprar: $")(writeln precioProducto)
    (write "Gracias por su compra, dinero recibido: $")(writeln (apply + monedasRecibidas))

    ; Actualizamos ambos inventario (monedas y productos) y guardamos
    (write (append (cons (actualizarMonedas (car inventario) monedasRecibidas) null) (cons (actualizarProducto (cadr inventario) idProducto) null)) inventarioNuevo)
    (close-output-port inventarioNuevo))

;Función para el caso en que las monedas del usuario no ajuste para el producto
(define (noAjusta monedasRecibidas precioProducto)
    (writeln "")
    (write "Precio del producto a comprar: $")(writeln precioProducto)
    (write "El dinero recibido no ajusta para el producto: $") (writeln (apply + monedasRecibidas)))

;Función para validar si una moneda es de denominación correcta (valida si es una transición valia para el "automata")
(define (validarMoneda transitions userInput)
    (cond 
        ((null? (cdr transitions)) (if (= (car transitions) userInput) userInput #f))
        (else (if (= (car transitions) userInput) userInput (validarMoneda (cdr transitions) userInput)))))

;Funcion que usa al automata
(define (pago automata listUserInput producto)
    (cond 
        ((= (apply + (car automata)) (caddr automata)) (ventaIgual (car automata) (caddr automata) producto)); Si el estado actual es igual al estado aceptor (compra justa)
        ((> (apply + (car automata)) (caddr automata)) (ventaCambio (car automata) (caddr automata) producto)); Si el estado actual ya supero al estado aceptor (compra con cambio)
        ((null? (cdr listUserInput))
            (cond
                ((false? (validarMoneda (cadr automata) (car listUserInput))) (writeln "Moneda de denominación no permitida") (noAjusta (car automata) (caddr automata))); Es la ultima moneda y es una moneda no permitida
                ((< (+ (apply + (car automata)) (validarMoneda (cadr automata) (car listUserInput))) (caddr automata)) (noAjusta (append (car automata) listUserInput) (caddr automata)));Si es la ultima moneda, es permitida, pero no logra ajustar
                (else (pago (list (append (car automata)(cons (validarMoneda (cadr automata) (car listUserInput)) null)) (cadr automata) (caddr automata)) '() producto)) ;Con la ultima moneda logra ajusta para el producto... Actualizamos automata y llamamos recursivamente (para su validación)
        ));Es la ultima moneda
        (else 
            (cond 
                ((false? (validarMoneda (cadr automata) (car listUserInput))) (writeln "Moneda de denominación no permitida") (pago automata (cdr listUserInput) producto)); Seguimos con el resto de inputs de usuario  
                (else
                    (pago 
                        (list (append (car automata)(cons (validarMoneda (cadr automata) (car listUserInput)) null)) (cadr automata) (caddr automata)) ;Actualizamos automata
                        (cdr listUserInput); Seguimos con el resto de inputs de usuario
                        producto)); Mandamos producto
        ))
))

;Función que determina si existe un producto y de existir, si este tiene existencias
(define (existe-producto? venta listaProductos)
    (define inventarios (open-input-file "C:\\Users\\renet\\OneDrive\\Documentos\\Tec de monterrey\\Semestre 4\\Implementacion de metodos computacionales\\Periodo 2\\Ejercicio diario\\entregable2\\maquinaExpendedora\\inventarios.txt")) ; Obtenemos transacciones
    (define inventarioMonedas (read inventarios))
    (close-input-port inventarios)
    
    (cond
        ((null? (cdr listaProductos)) 
            (if (equal? (car venta) (caar listaProductos)) 
                ((lambda (x)
                    (cond 
                        ((> (car x) 0) 
                            (write "Producto a comprar: ") (writeln (cdadar listaProductos)) (writeln "")
                            (pago (append (cons '(0) null) (cons (map append (map car (car  inventarioMonedas))) null) (cons (caadar listaProductos) null)) (cadr venta) (car venta)));Procedemos a validar pago
                        (else (writeln "No hay existencias")))
                )(caddar listaProductos))
                (writeln "El producto solicitado no existe")
                )); Si estamos en la ultima posición
        ((equal? (car venta) (caar listaProductos)) 
            ((lambda (x)
                (cond 
                    ((> (car x) 0)
                        (write "Producto a comprar: ")(writeln (cdadar listaProductos))
                        (writeln "")
                        (pago (append (cons '(0) null) (cons (map append (map car (car  inventarioMonedas))) null) (cons (caadar listaProductos) null)) (cadr venta) (car venta)));Procedemos a validar pago
                    (else (writeln "No hay existencias"))
                )) (caddar listaProductos))); Si el producto actual es el que estamos buscando
        (else (existe-producto? venta (cdr listaProductos)))
))

;Función que crea el reporte de resultados del día.
(define (resultados inventarios)
    (writeln "")
    (writeln "")
    (writeln "--- Resultados del día ---")
    
    (write "GANANCIA DEL DIA")
    (write "Cantidad total monedas final $")(writeln (apply + (map (lambda (x) (* (car x) (caadr x))) (car inventarios))))
    (write "Cantidad total monedas inicial $")(writeln (apply + (map (lambda (x) (* (car x) (cadadr x))) (car inventarios))))
    (write "Ganancia: $")(writeln (- (apply + (map (lambda (x) (* (car x) (caadr x))) (car inventarios))) (apply + (map (lambda (x) (* (car x) (cadadr x))) (car inventarios)))))
    (writeln "")
    (writeln "")

    (writeln "REPORTE INVENTARIO DE PRODUCTOS POCA O NULA EXISTENCIA")
    (map (lambda (x)
        (cond 
            ((zero? (caaddr x))
            (write "Producto: ") (write (cadadr x)) (write " ya NO hay EXISTENCIAS. Inventario actual: ") (writeln (caaddr x)))
            ((<= (caaddr x) (car (cdaddr x))) 
            (write "Producto: ") (write (cadadr x))
            (write " tiene poco inventario. Inventario actual: ") (writeln (caaddr x)))))
        (cadr inventarios))
    (writeln "")
    (writeln "")

    (writeln "REPORTE INVENTARIO DE MONEDAS CASI LLENO O LLENO")
    (map (lambda (x)
        (cond
            ((= (caadr x) (caaddr x))
            (write "El inventario con la denominacion: $") (write (car x)) (write " esta LLENO. Inventario actual: ") (write (caadr x))
            (writeln ""))
            ((>= (caadr x) ((lambda (y) (floor (* (car y) (caddr y))))(caddr x)))                    
                (write "El inventario con la denominacion: $") (write (car x)) (write " esta CASI LLENO. Inventario actual de esa moneda: ") (write (caadr x))
                (writeln "")))) (car inventarios))
    (writeln "")
    (writeln "")

    (writeln "REPORTE INVENTARIO DE MONEDAS CASI VACÍO O VACÍO")
    (map (lambda (x)
        (cond
            ((zero? (caadr x))
            (write "El inventario con la denominacion: $") (write (car x)) (write " esta VACÍO. Inventario actual: ") (write (caadr x))
            (writeln ""))
            ((<= (caadr x) ((lambda (y) (floor (* (car y) (cadr y))))(caddr x)))                    
                (write "El inventario con la denominacion: $")(write (car x)) (write " esta CASI VACÍO Inventario actual de esa moneda: ") (write (caadr x))
                (writeln "")))) (car inventarios))
    (writeln "")
    (writeln "")
    (writeln "Fin de programa")
)

;Comenzamos simulación
(define arch (open-input-file "C:\\Users\\renet\\OneDrive\\Documentos\\Tec de monterrey\\Semestre 4\\Implementacion de metodos computacionales\\Periodo 2\\Ejercicio diario\\entregable2\\maquinaExpendedora\\transacciones.txt")) ; Obtenemos transacciones
(define transacciones (read arch))
(close-input-port arch)
(map (lambda (transaccion) 
        (define arch (open-input-file "C:\\Users\\renet\\OneDrive\\Documentos\\Tec de monterrey\\Semestre 4\\Implementacion de metodos computacionales\\Periodo 2\\Ejercicio diario\\entregable2\\maquinaExpendedora\\inventarios.txt")) ; Obtenemos transacciones  
        (define inventario (read arch)) ;Definimos para poder cerrar el archivo
        (close-input-port arch) ; cierra el archivo 
        (writeln "----------------------------------------")
        (existe-producto? transaccion (cadr inventario))
        (writeln "----------------------------------------") '()) transacciones)

(define inventarios (open-input-file "C:\\Users\\renet\\OneDrive\\Documentos\\Tec de monterrey\\Semestre 4\\Implementacion de metodos computacionales\\Periodo 2\\Ejercicio diario\\entregable2\\maquinaExpendedora\\inventarios.txt")) ; Obtenemos transacciones
(resultados (read inventarios))
(close-input-port inventarios)