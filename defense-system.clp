

(deftemplate artifact

    ; position
    (slot x)
    (slot y)

    ; (slot z)

    ; angle
    ; (slot w)
    ; (slot s)

    (slot velocity)
    (slot material    (type STRING)) 
)


(deftemplate defense-system

    ; position
    (slot x)
    (slot y)

    ; 1 to 1
    (slot amount)

    ; time in seconds
    (slot colddown)

    ; 1 to Kilometers
    (slot range) 

    (slot material-counter    (type STRING))
    (slot name                (type STRING))

    (slot distance_to_artifact)
)



(deffacts main_facts

    ; ********************************

    ; defense-system

    ; ********************************

    (defense-system

        (x 8)
        (y 9)

        (amount              5)
        (colddown            32)
        (range               64) 
        (material-counter    "uranium") 
        (name                "AB_1")
    )

    (defense-system
        (x           -8)
        (y           -9)

        (amount              1)
        (colddown            160)
        (range               32) 
        (material-counter    "sodium-potasium") 
        (name                "AB_2")
    )

    (defense-system
        (x           8)
        (y           1)

        (amount              20)
        (colddown            5)
        (range               100) 
        (material-counter    "plastic") 
        (name                "CD_1")
    )

    (defense-system
        (x           8)
        (y           -9)

        (amount              2)
        (colddown            1)
        (range               30) 
        (material-counter    "plastic") 
        (name                "CD_2")
    )

    (defense-system
        (x          -8)
        (y           9)

        (amount              2)
        (colddown            100)
        (range               30) 
        (material-counter    "unknown") 
        (name                "CD_3")
    )
)


(deffunction get-artifact (?num)
    (do-for-all-facts ((?a artifact)) TRUE
        (return ?a)
    )
)

(defrule new_artifact

    (declare (salience 90))
    =>
    (printout t "x: y: velocity: material: ")
    (assert (artifact

            (x           (read))
            (y           (read))
            
            ; (printout t "z: ") (slot z           (read))
            ; (printout t "w: ") (slot w           (read))
            ; (printout t "s: ") (slot s           (read))
            
            (velocity    (read))
            (material    (read)) 
        )
    )
)



(defrule type_distance

    (declare (salience 80))
  =>

    (printout t "type_distance (euclidian, manhattan)" crlf)

    (bind ?distance (read))

    (if (eq ?distance "euclidian") then (assert (var_distance_euclidian)))
    (if (eq ?distance "manhattan") then (assert (var_distance_manhattan)))

)


(defrule manhattan_distance

    (var_distance_manhattan)

  =>

    (do-for-all-facts ((?c defense-system)) TRUE

        (modify ?c (distance_to_artifact 

            (+
                (abs (- ?c:x ?(get-artifact 9):x))
                (abs (- ?c:y ?(get-artifact 9):y))
            )
          )
        )
    )
    )
)


(defrule euclidian_distance

    (var_distance_euclidian)

  =>
    

    (do-for-all-facts ((?c defense-system)) TRUE

        (modify ?c (distance_to_artifact 
                (sqrt 
                    (+ 
                        (** (- ?c:x ?(get-artifact 9):x) 2)
                        (** (- ?c:y ?(get-artifact 9):y) 2)
                    )
                )
            )
        )

        (printout t ?c:distance_to_artifact crlf)

    )
    )
)



(defrule best-defense-system


    (declare (salience -10))
=>     

    (printout t "Defense system: " crlf)


    (do-for-all-facts ((?c defense-system))
    
        (and 

            (>  ?c:amount           0)

            (>= ?c:range            ?c:distance_to_artifact)

            (eq ?c:material-counter ?(get-artifact 9):material) 
        )

        (modify ?c (amount (- ?c:amount 1)))

        (printout t ?c:name crlf)
      )
    )
)
