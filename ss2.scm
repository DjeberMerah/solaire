#!r7rs
(import (scheme base) (scheme inexact) (scheme write)
	(only (racket base) require))

(require scheme/class)
(require scheme/gui/base)

(define NB_PLANETS 8)
;; fenêtre du système solaire
(define WIDTH 1000)
(define HEIGHT 1000)
(define WSUR2 (/ WIDTH  2))
(define HSUR2 (/ HEIGHT 2))

;; fenêtre des planètes
(define W 800)
(define H 800)
(define WS2 (/ W 2))
(define HS2 (/ H 2))

;; définition des planètes un vecteur de vecteurs (matrice)
;;                Nom       Dist  Ray    X0:position initiale  Y0:position initiale  teta x y rapport/vitesse nb lunes t
(define v-ss
  (vector (vector "Mercury"  60    8     (+ WSUR2  -4)  (+ HSUR2   -4)  0 0 0 4.147 0 0)
          (vector "Venus"    80   18     (+ WSUR2  -9)  (+ HSUR2   -9)  9.732 0 0 1.622 0 6)
          (vector "Earth"   110   20     (+ WSUR2 -10)  (+ HSUR2  -10)  30 0 0     1 1 30)
          (vector "Mars"    150   10     (+ WSUR2  -5)  (+ HSUR2   -5)  5.31 0 0 0.531 2 10)
          (vector "Jupiter" 200   60     (+ WSUR2 -30)  (+ HSUR2  -30)  23.52 0 0 0.084 4 280)
          (vector "Saturn"  300   60     (+ WSUR2 -30)  (+ HSUR2  -30)  3.400 0 0 0.034 5 100)
          (vector "Uranus"  380   30     (+ WSUR2 -15)  (+ HSUR2  -15)  0.600 0 0 0.012 6 50) 
          (vector "Neptune" 450   30     (+ WSUR2 -15)  (+ HSUR2  -15)  3 0 0 0.006 4 500) ))

;; fonction d'accès à un élément de la matrice v-ss
(define (element i j)
  (vector-ref (vector-ref v-ss i) j))

;; définition des lunes des planètes par des bases de données (listes) à la manière de Musique
;;                Nom       Dist  Ray    X0:position initiale  Y0:position initiale  rapport/vitesse nb lunes
(define (mk-moon name dist ray x0 y0 rapvit)
        (list name dist ray x0 y0 rapvit))

;; fonction d'accès à un élément d'une matrice v
(define (element1 v i j)
  (vector-ref (vector-ref v i) j))

;; fonction d'accès au nieme élément d'une liste
(define (nieme n l) (if (= n 1) (car l) (nieme (- n 1) (cdr l)))) 
(define (name moons-list)
  (car moons-list))
(define (dist moons-list)
  (car (cdr moons-list)))
(define (ray moons-list)
  (car (cddr moons-list)))
(define (x0 moons-list)
  (nieme 4 moons-list ))
(define (y0 moons-list)
  (nieme 5 moons-list))
(define (rapvit moons-list)
  (nieme 6 moons-list))

;; images réelles: fichiers jpeg pour le soleil et les planètes 
(define noir
  (make-object bitmap% "images/noir.jpeg"))
(define noir-uranus
  (make-object bitmap% "images/noir-uranus.jpeg"))
(define sun-bitmap
  (make-object bitmap% "images/sun.jpeg"))
(define mercury-bitmap
  (make-object bitmap% "images/mercury.jpeg"))
(define venus-bitmap
  (make-object bitmap% "images/venus.jpeg"))
(define earth-bitmap
  (make-object bitmap% "images/earth.jpeg"))
(define mars-bitmap
  (make-object bitmap% "images/mars.jpeg"))
(define jupiter-bitmap
  (make-object bitmap% "images/jupiter.jpeg"))
(define saturn-bitmap
  (make-object bitmap% "images/saturn.jpeg"))
(define uranus-bitmap
  (make-object bitmap% "images/uranus.jpeg"))
(define neptune-bitmap
  (make-object bitmap% "images/neptune.jpeg"))


;;la fenêtre principale
(define ss-frame
      (new frame% (label "Solar System") (width WIDTH) (height HEIGHT)))

;; fenêtre Sun
(define sun-frame
  (new frame% (label "Sun") (width W) (height H)))
(define sun1-bitmap
  (make-object bitmap% "images/sun1.jpeg"))
(define sun-vertical-panel (new vertical-panel% (parent sun-frame) ))
(define sun-infos1 (new message% (parent sun-vertical-panel)
                  (label "Diamètre: 1392684 km Masse: 1.98x10p30 kg Volume: 1.41x10p18 km3 Température: 15.1 mk Gravité: 273.95 ms-2")))
(define sun-canvas
  (new canvas% (parent sun-vertical-panel)
       (paint-callback
          (lambda (c dc)
              (send dc clear)
              (send dc draw-bitmap noir 0 0)
              (send dc draw-bitmap sun1-bitmap (- WS2 200) (- HS2 200))))))

;; fenêtre Mercury
(define mercury-frame
  (new frame% (label "Mercury planet") (width W) (height H)))
(define mercury1-bitmap
  (make-object bitmap% "images/mercury1.jpeg"))
(define mercury-vertical-panel (new vertical-panel% (parent mercury-frame) ))
(define mercury-infos1 (new message% (parent mercury-vertical-panel)
                  (label "Diamètre: 4875 km Masse: 0.1xterre Volume: 0.1xterre Température: -180/430°C Gravité: 0.4xterre")))
(define mercury-infos2 (new message% (parent mercury-vertical-panel)
                  (label "Satellites: 0   Distance au Soleil: 57.9 millions km   Période orbitale: 88 jours   Durée de rotation: 59 jours")))
(define mercury-canvas
  (new canvas% (parent mercury-vertical-panel)
       (paint-callback
          (lambda (c dc)
              (send dc clear)
              (send dc draw-bitmap noir 0 0)
              (send dc draw-bitmap mercury1-bitmap (- WS2 200) (- HS2 200))))))
              
;; fenêtre Venus
(define venus-frame
  (new frame% (label "Venus planet") (width W) (height H)))
(define venus1-bitmap
  (make-object bitmap% "images/venus1.jpeg"))
(define venus-vertical-panel (new vertical-panel% (parent venus-frame) ))
(define venus-infos1 (new message% (parent venus-vertical-panel)
                  (label "Diamètre: 12 104 km Masse: 0.8xterre Volume: 0.9xterre Température: 464°C Gravité: 0.9xterre")))
(define venus-infos2 (new message% (parent venus-vertical-panel)
                  (label "Satellites: 0   Distance au Soleil: 108.2 millions km   Période orbitale: 224,7 jours   Durée de rotation: 243 jours")))
(define venus-canvas
  (new canvas% (parent venus-vertical-panel)
       (paint-callback
          (lambda (c dc)
              (send dc clear)
              (send dc draw-bitmap noir 0 0)
              (send dc draw-bitmap venus1-bitmap (- WS2 200) (- HS2 200))))))
;; fenêtre Earth
(define earth-frame
  (new frame% (label "Earth planet") (width W) (height H)))
(define earth1-bitmap
  (make-object bitmap% "images/earth1.jpeg"))

;; vecteur pour les objets mutables (teta, coordonnées x et y, t)
(define v-earth-moons
  (vector (vector 0 0 0 0) ))  ;; teta  x   y   t

;; la Lune

(define earth-moon-bitmap
  (make-object bitmap% "images/earth-moon.jpeg"))
(define earth-moons
  (list (mk-moon "Lune" 300 25 (- WS2 25) (- HS2 25) 1)))

(define earth-vertical-panel (new vertical-panel% (parent earth-frame) ))
(define earth-infos1 (new message% (parent earth-vertical-panel)
                  (label "Diamètre: 12756km Masse: 5,97x10p24 kg Volume: 1,08x10p12 km3 Température: 15°C Gravité: 9,8 m/s2 ")))
(define earth-infos2 (new message% (parent earth-vertical-panel)
                  (label "Satellites: 1   Distance au Soleil: 149,6 millions km   Période orbitale: 365,3 jours   Durée de rotation: 23,9 heures ")))
(define earth-infos3 (new message% (parent earth-vertical-panel)
                  (label "Satellites: Lune")))
                                 
(define earth-canvas
  (new canvas% (parent earth-vertical-panel)
       (paint-callback
          (lambda (c dc)
              (send dc clear)
              (send dc draw-bitmap noir 0 0)
              (send dc draw-bitmap earth1-bitmap (- WS2 100) (- HS2 100))
              (send dc draw-bitmap earth-moon-bitmap (element1 v-earth-moons 0 1) (element1 v-earth-moons 0 2) ) ) )))

(define (lp-earth i)
  (when (< i (element 2 9)) ;; nombre de lunes
   (vector-set! (vector-ref v-earth-moons i) 1 (+ (x0 (nieme (+ i 1) earth-moons)) (* (dist (nieme (+ i 1) earth-moons)) (cos (element1 v-earth-moons i 0)))))
   (vector-set! (vector-ref v-earth-moons i) 2 (+ (y0 (nieme (+ i 1) earth-moons)) (* (dist (nieme (+ i 1) earth-moons)) (sin (element1 v-earth-moons i 0)))))
   (vector-set! (vector-ref v-earth-moons i) 0 (* (rapvit (nieme (+ i 1) earth-moons)) (element1 v-earth-moons i 3)))
   (vector-set! (vector-ref v-earth-moons i) 3  (+ (element1 v-earth-moons i 3) 0.004))
   (lp-earth (+ i 1))))

(define (loop-earth)
  (send earth-canvas on-paint)
  ;;(rotate 45 "images/earth.jpeg")
  (lp-earth 0)
  (sleep/yield 0.025)
  (if (send earth-frame is-shown?) (loop-earth) ))

;; fenêtre Mars
;; vecteur pour les objets mutables (teta, coordonnées x et y, t)
(define v-mars-moons
  (vector (vector 0 0 0 0)
          (vector 0 0 0 0)))  ;; teta  x   y   t
(define mars1-bitmap
  (make-object bitmap% "images/mars1.jpg"))
(define mars-phobos-bitmap
  (make-object bitmap% "images/mars-phobos.png"))
(define mars-deimos-bitmap
  (make-object bitmap% "images/mars-deimos.jpg"))

(define mars-moons
  (list (mk-moon "Phobos" 152 5 (- WS2 12) (- HS2 12) 1)
        (mk-moon "Deimos" 365 2.5 (- WS2 15) (- HS2 15) 1.37)))

(define mars-frame
  (new frame% (label "Mars planet") (width W) (height H)))

(define mars-vertical-panel (new vertical-panel% (parent mars-frame) ))
(define mars-infos1 (new message% (parent mars-vertical-panel)
                  (label "Diamètre: 6780 km Masse: 0.1xterre Volume: 0.2xterre Température: -125/25°C Gravité: 0.4xterre ")))
(define mars-infos2 (new message% (parent mars-vertical-panel)
                  (label "Satellites: 2   Distance au Soleil: 227,9 millions km   Période orbitale: 687 jours   Durée de rotation: 24,6 heures ")))
(define mars-infos3 (new message% (parent mars-vertical-panel)
                  (label "Satellites: Phobos, Deimos")))


(define mars-canvas
  (new canvas% (parent mars-vertical-panel)
       (paint-callback
          (lambda (c dc)
              (send dc clear)
              (send dc draw-bitmap noir 0 0)
              (send dc draw-bitmap mars1-bitmap (- WS2 100) (- HS2 100))
             (send dc draw-bitmap mars-phobos-bitmap (element1 v-mars-moons 0 1) (element1 v-mars-moons 0 2) )
              (send dc draw-bitmap mars-deimos-bitmap (element1 v-mars-moons 1 1) (element1 v-mars-moons 1 2) ) ) )))




(define (lp-mars i)
  (when (< i (element 3 9)) ;; nombre de lunes
   (vector-set! (vector-ref v-mars-moons i) 1 (+ (x0 (nieme (+ i 1) mars-moons)) (* (dist (nieme (+ i 1) mars-moons)) (cos (element1 v-mars-moons i 0)))))
   (vector-set! (vector-ref v-mars-moons i) 2 (+ (y0 (nieme (+ i 1) mars-moons)) (* (dist (nieme (+ i 1) mars-moons)) (sin (element1 v-mars-moons i 0)))))
   (vector-set! (vector-ref v-mars-moons i) 0 (* (rapvit (nieme (+ i 1) mars-moons)) (element1 v-mars-moons i 3)))
   (vector-set! (vector-ref v-mars-moons i) 3  (+ (element1 v-mars-moons i 3) 0.004))
   (lp-mars (+ i 1))))

(define (loop-mars)
   (send mars-canvas on-paint)
   (lp-mars 0)
   (sleep/yield 0.025) 
   (if (send mars-frame is-shown?) (loop-mars)))


;; fenêtre Jupiter
(define v-jupiter-moons
  (vector (vector 0 0 0 0)
          (vector 0 0 0 0)
          (vector 0 0 0 0)
          (vector 0 0 0 0)))  ;; teta  x   y   t


(define jupiter1-bitmap
  (make-object bitmap% "images/jupiter1.png"))
(define jupiter-callisto-bitmap
  (make-object bitmap% "images/jupiter-callisto.png"))
(define jupiter-ganymede-bitmap
  (make-object bitmap% "images/jupiter-ganymede.png"))
(define jupiter-europe-bitmap
  (make-object bitmap% "images/jupiter-europe.png"))
(define jupiter-io-bitmap
  (make-object bitmap% "images/jupiter-io.png"))


(define jupiter-moons
  (list (mk-moon "Io" 130 6 (- WS2 6) (- HS2 6) 1.13058225)
        (mk-moon "Europe" 150 5 (- WS2 5) (- HS2 5) 0.563221628)
        (mk-moon "Ganymede" 290 9 (- WS2 9) (- HS2 9) 0.279329609)
        (mk-moon "Callisto" 335 8 (- WS2 8) (- HS2 8) 0.117647059)))

(define jupiter-frame
  (new frame% (label "Jupiter planet") (width W) (height H)))

(define jupiter-vertical-panel (new vertical-panel% (parent jupiter-frame) ))
(define jupiter-infos1 (new message% (parent jupiter-vertical-panel)
                  (label "Diamètre: 142984 km Masse: 318xterre Volume: 1.321xterre Température: -110°C Gravité: 2.5xterre ")))
(define jupiter-infos2 (new message% (parent jupiter-vertical-panel)
                  (label "Satellites: 63   Distance au Soleil: 778,3 millions km   Période orbitale: 11,9 ans   Durée de rotation: 9,9 heures ")))

(define jupiter-infos3 (new message% (parent jupiter-vertical-panel) (label "Satellites: IO, Europe, Ganymède, Callisto")  )  )

;; le sous-canvas du canevas Jupiter
(define MY-CANVAS-JUPITER%    ;; pour gérer la souris
 (class canvas%       ;; une sous-classe de canvas%
    (define/override (on-event evt)
      (case (send evt get-event-type)
       ((motion) (let ((x (send evt get-x)) (y (send evt get-y)))
          (cond
            ((and (<= (abs (- (element1 v-jupiter-moons 0 1) x)) 6) (<= (abs (- (element1 v-jupiter-moons 0 2) y)) 6))   )
            
            )))
        (else (+ 0 0))))
    (super-new)))

(define jupiter-canvas (new MY-CANVAS-JUPITER% (parent jupiter-vertical-panel)
;;(new canvas% (parent jupiter-vertical-panel)
       (paint-callback
          (lambda (c dc)
              (send dc clear)
              (send dc draw-bitmap noir 0 0)
              (send dc draw-bitmap jupiter1-bitmap (- WS2 100) (- HS2 100))
              (send dc draw-bitmap jupiter-io-bitmap (element1 v-jupiter-moons 0 1) (element1 v-jupiter-moons 0 2) )
              (send dc draw-bitmap jupiter-europe-bitmap (element1 v-jupiter-moons 1 1) (element1 v-jupiter-moons 1 2) )
              (send dc draw-bitmap jupiter-ganymede-bitmap (element1 v-jupiter-moons 2 1) (element1 v-jupiter-moons 2 2) )
              (send dc draw-bitmap jupiter-callisto-bitmap (element1 v-jupiter-moons 3 1) (element1 v-jupiter-moons 3 2) )) )))




(define (lp-jupiter i)
  (when (< i (element 4 9)) ;; nombre de lunes
   (vector-set! (vector-ref v-jupiter-moons i) 1 (+ (x0 (nieme (+ i 1) jupiter-moons)) (* (dist (nieme (+ i 1) jupiter-moons)) (cos (element1 v-jupiter-moons i 0)))))
   (vector-set! (vector-ref v-jupiter-moons i) 2 (+ (y0 (nieme (+ i 1) jupiter-moons)) (* (dist (nieme (+ i 1) jupiter-moons)) (sin (element1 v-jupiter-moons i 0)))))
   (vector-set! (vector-ref v-jupiter-moons i) 0 (* (rapvit (nieme (+ i 1) jupiter-moons)) (element1 v-jupiter-moons i 3)))
   (vector-set! (vector-ref v-jupiter-moons i) 3  (+ (element1 v-jupiter-moons i 3) 0.004))
   (lp-jupiter (+ i 1))))

(define (loop-jupiter)
   (send jupiter-canvas on-paint)
   (lp-jupiter 0)
   (sleep/yield 0.025) 
   (if (send jupiter-frame is-shown?) (loop-jupiter)))

;; fenêtre Saturn

;; vecteur pour les objets mutables (teta, coordonnées x et y, t)
(define v-saturn-moons
  (vector (vector 0 0 0 0) ;; teta  x   y   t pour chaque lune
          (vector 0 0 0 0)
          (vector 0 0 0 0)
          (vector 0 0 0 0)
          (vector 0 0 0 0)
          (vector 0 0 0 0)
          (vector 0 0 0 0)
          (vector 0 0 0 0)))

;; les lunes de Saturne
(define saturn1-bitmap
  (make-object bitmap% "images/saturne.jpg"))
(define saturn-mimas-bitmap
  (make-object bitmap% "images/saturn-mimas.jpeg"))
(define saturn-encelade-bitmap
  (make-object bitmap% "images/saturn-encelade.jpeg"))
(define saturn-tethys-bitmap
  (make-object bitmap% "images/saturn-tethys.jpeg"))
(define saturn-dione-bitmap
  (make-object bitmap% "images/saturn-dione.jpeg"))
(define saturn-titan-bitmap
  (make-object bitmap% "images/saturn-titan.jpeg"))
(define saturn-moons
  (list (mk-moon "Mimas" 120 3 (- WS2 3) (- HS2 3) 1)
        (mk-moon "Encelade" 170 4 (- WS2 4) (- HS2 4) 0.729927007)
        (mk-moon "Téthys" 220 12 (- WS2 12) (- HS2 12) 0.531914894)
        (mk-moon "Dione" 280 14.8 (- WS2 14.8) (- HS2 14.8) 0.366300366)
        (mk-moon "Titan" 360 18 (- WS2 18) (- HS2 18) 0.062695925)))

(define saturn-frame
  (new frame% (label "Saturn planet") (width W) (height H)))
(define saturn-vertical-panel (new vertical-panel% (parent saturn-frame) ))
(define saturn-infos1 (new message% (parent saturn-vertical-panel)
                  (label "Diamètre: 120536 km Masse: 95xterre Volume: 763,6xterre Température: -140°C Gravité: 1,1xterre")))
(define saturn-infos2 (new message% (parent saturn-vertical-panel)
                  (label "Satellites: 61   Distance au Soleil: 1430 millions km   Période orbitale: 29,5 ans   Durée de rotation: 10,66 heures ")))
(define saturn-infos3 (new message% (parent saturn-vertical-panel)
                  (label "Satellites: Mimas, Encelade, Tethys, Dione, Titan")))

(define saturn-canvas
  (new canvas% (parent saturn-vertical-panel)
       (paint-callback
          (lambda (c dc)
              (send dc clear)
              (send dc draw-bitmap noir 0 0)
              (send dc draw-bitmap saturn1-bitmap (- WS2 100) (- HS2 100))
              (send dc draw-bitmap saturn-mimas-bitmap (element1 v-saturn-moons 0 1) (element1 v-saturn-moons 0 2) )
              (send dc draw-bitmap saturn-encelade-bitmap (element1 v-saturn-moons 1 1) (element1 v-saturn-moons 1 2) )
              (send dc draw-bitmap saturn-tethys-bitmap (element1 v-saturn-moons 2 1) (element1 v-saturn-moons 2 2) )
              (send dc draw-bitmap saturn-dione-bitmap (element1 v-saturn-moons 3 1) (element1 v-saturn-moons 3 2) )
              (send dc draw-bitmap saturn-titan-bitmap (element1 v-saturn-moons 4 1) (element1 v-saturn-moons 4 2) )))))

(define (lp-saturn i)
  (when (< i (element 5 9)) ;; nombre de lunes
   (vector-set! (vector-ref v-saturn-moons i) 1 (+ (x0 (nieme (+ i 1) saturn-moons)) (* (dist (nieme (+ i 1) saturn-moons)) (cos (element1 v-saturn-moons i 0)))))
   (vector-set! (vector-ref v-saturn-moons i) 2 (+ (y0 (nieme (+ i 1) saturn-moons)) (* (dist (nieme (+ i 1) saturn-moons)) (sin (element1 v-saturn-moons i 0)))))
   (vector-set! (vector-ref v-saturn-moons i) 0 (* (rapvit (nieme (+ i 1) saturn-moons)) (element1 v-saturn-moons i 3)))
   (vector-set! (vector-ref v-saturn-moons i) 3  (+ (element1 v-saturn-moons i 3) 0.004))
   (lp-saturn (+ i 1))))

(define (loop-saturn)
   (send saturn-canvas on-paint)
   (lp-saturn 0)
   (sleep/yield 0.025) 
   (if (send saturn-frame is-shown?) (loop-saturn)))

;; fenêtre Uranus
(define v-uranus-moons
  (vector (vector 0 0 0 0)
          (vector 0 0 0 0)
          (vector 0 0 0 0)
          (vector 0 0 0 0)
          (vector 0 0 0 0)
          (vector 0 0 0 0)))  ;; teta  x   y   t


(define uranus1-bitmap
  (make-object bitmap% "images/uranus1.png"))
(define uranus-oberon-bitmap
  (make-object bitmap% "images/uranus-oberon.png"))
(define uranus-titania-bitmap
  (make-object bitmap% "images/uranus-titania.png"))
(define uranus-umberiel-bitmap
  (make-object bitmap% "images/uranus-umberiel.png"))
(define uranus-ariel-bitmap
  (make-object bitmap% "images/uranus-ariel.png"))
(define uranus-miranda-bitmap
  (make-object bitmap% "images/uranus-miranda.png"))
(define uranus-puck-bitmap
  (make-object bitmap% "images/uranus-puck.png"))


(define uranus-moons
  (list (mk-moon "Puck" 130 1 (- 600 6) (- 600 6) 1.13058225)
        (mk-moon "Miranda" 196.360465116 2 (- 600 2) (- 600 2) 0.563221628)
        (mk-moon "Ariel" 288.569767442 5 (- 600 5) (- 600 5) 0.279329609)
        (mk-moon "Umbriel" 402.546511628 5.5 (- 600 5.5) (- 600 5.5) 0.117647059)
        (mk-moon "Titania" 542.523255814 6.5 (- 600 6.5) (- 600 6.5) 0.117647059)
        (mk-moon "Oberon"  580 6 (- 600 6) (- 600 6) 0.117647059)))

(define uranus-frame
  (new frame% (label "Uranus planet") (width 1200) (height 1200)))

(define uranus-vertical-panel (new vertical-panel% (parent uranus-frame) ))
(define uranus-infos1 (new message% (parent uranus-vertical-panel)
                  (label "Diamètre: 51118 km Masse: 14,5xterre Volume: 63,1xterre Température: -214°C Gravité: 0.9xterre ")))
(define uranus-infos2 (new message% (parent uranus-vertical-panel)
                  (label "Satellites: 27   Distance au Soleil: 2871 millions km   Période orbitale: 84 ans   Durée de rotation: 17,24 heures ")))
(define uranus-infos3 (new message% (parent uranus-vertical-panel)
                  (label "Satellites: Puck, Miranda, Ariel, Umbriel, Titania, Oberon")))


(define uranus-canvas
  (new canvas% (parent uranus-vertical-panel)
       (paint-callback
          (lambda (c dc)
              (send dc clear)
              (send dc draw-bitmap noir-uranus 0 0)
              (send dc draw-bitmap uranus1-bitmap (- 600 100) (- 600 100))
              (send dc draw-bitmap uranus-puck-bitmap (element1 v-uranus-moons 0 1) (element1 v-uranus-moons 0 2) )
              (send dc draw-bitmap uranus-miranda-bitmap (element1 v-uranus-moons 1 1) (element1 v-uranus-moons 1 2) )
              (send dc draw-bitmap uranus-ariel-bitmap (element1 v-uranus-moons 2 1) (element1 v-uranus-moons 2 2) )
              (send dc draw-bitmap uranus-umberiel-bitmap (element1 v-uranus-moons 3 1) (element1 v-uranus-moons 3 2) )
              (send dc draw-bitmap uranus-titania-bitmap (element1 v-uranus-moons 4 1) (element1 v-uranus-moons 4 2) )
              (send dc draw-bitmap uranus-oberon-bitmap (element1 v-uranus-moons 5 1) (element1 v-uranus-moons 5 2) )) )))




(define (lp-uranus i)
  (when (< i (element 6 9)) ;; nombre de lunes
   (vector-set! (vector-ref v-uranus-moons i) 1 (+ (x0 (nieme (+ i 1) uranus-moons)) (* (dist (nieme (+ i 1) uranus-moons)) (cos (element1 v-uranus-moons i 0)))))
   (vector-set! (vector-ref v-uranus-moons i) 2 (+ (y0 (nieme (+ i 1) uranus-moons)) (* (dist (nieme (+ i 1) uranus-moons)) (sin (element1 v-uranus-moons i 0)))))
   (vector-set! (vector-ref v-uranus-moons i) 0 (* (rapvit (nieme (+ i 1) uranus-moons)) (element1 v-uranus-moons i 3)))
   (vector-set! (vector-ref v-uranus-moons i) 3  (+ (element1 v-uranus-moons i 3) 0.004))
   (lp-uranus (+ i 1))))

(define (loop-uranus)
   (send uranus-canvas on-paint)
   (lp-uranus 0)
   (sleep/yield 0.025) 
   (if (send uranus-frame is-shown?) (loop-uranus)))
;; fenêtre Neptune
(define v-neptune-moons
  (vector (vector 0 0 0 0)
          (vector 0 0 0 0)
          (vector 0 0 0 0)
          (vector 0 0 0 0)))  ;; teta  x   y   t


(define neptune1-bitmap
  (make-object bitmap% "images/neptune1.png"))
(define neptune-proteus-bitmap
  (make-object bitmap% "images/neptune-proteus.png"))
(define neptune-larissa-bitmap
  (make-object bitmap% "images/neptune-larissa.png"))
(define neptune-galatea-bitmap
  (make-object bitmap% "images/neptune-galatea.png"))
(define neptune-despina-bitmap
  (make-object bitmap% "images/neptune-despina.png"))


(define neptune-moons
  (list (mk-moon "Despina" 128 3 (- WS2 3) (- HS2 3) 2.20)
        (mk-moon "Galatea" 144 8 (- WS2 8) (- HS2 8) 1.80)
        (mk-moon "Larissa" 190 14 (- WS2 14) (- HS2 14) 1.30)
        (mk-moon "Proteus" 238 16 (- WS2 16) (- HS2 16) 1)))

(define neptune-frame
  (new frame% (label "Neptune planet") (width W) (height H)))

(define neptune-vertical-panel (new vertical-panel% (parent neptune-frame) ))
(define neptune-infos1 (new message% (parent neptune-vertical-panel)
                  (label "Diamètre: 49532 km Masse: 17,1xterre Volume: 57,7xterre Température: -220°C Gravité: 1,1xterre ")))
(define neptune-infos2 (new message% (parent neptune-vertical-panel)
                  (label "Satellites: 13   Distance au Soleil: 4497 millions km   Période orbitale: 164,8 ans   Durée de rotation: 16,1 heures ")))
(define neptune-infos3 (new message% (parent neptune-vertical-panel)
                  (label "Satellites: Despina, Galatea, Larissa, Proteus")))


(define neptune-canvas
  (new canvas% (parent neptune-vertical-panel)
       (paint-callback
          (lambda (c dc)
              (send dc clear)
              (send dc draw-bitmap noir 0 0)
              (send dc draw-bitmap neptune1-bitmap (- WS2 100) (- HS2 100))
              (send dc draw-bitmap neptune-despina-bitmap (element1 v-neptune-moons 0 1) (element1 v-neptune-moons 0 2) )
              (send dc draw-bitmap neptune-galatea-bitmap (element1 v-neptune-moons 1 1) (element1 v-neptune-moons 1 2) )
              (send dc draw-bitmap neptune-larissa-bitmap (element1 v-neptune-moons 2 1) (element1 v-neptune-moons 2 2) )
              (send dc draw-bitmap neptune-proteus-bitmap (element1 v-neptune-moons 3 1) (element1 v-neptune-moons 3 2) )) )))




(define (lp-neptune i)
  (when (< i (element 7 9)) ;; nombre de lunes
   (vector-set! (vector-ref v-neptune-moons i) 1 (+ (x0 (nieme (+ i 1) neptune-moons)) (* (dist (nieme (+ i 1) neptune-moons)) (cos (element1 v-neptune-moons i 0)))))
   (vector-set! (vector-ref v-neptune-moons i) 2 (+ (y0 (nieme (+ i 1) neptune-moons)) (* (dist (nieme (+ i 1) neptune-moons)) (sin (element1 v-neptune-moons i 0)))))
   (vector-set! (vector-ref v-neptune-moons i) 0 (* (rapvit (nieme (+ i 1) neptune-moons)) (element1 v-neptune-moons i 3)))
   (vector-set! (vector-ref v-neptune-moons i) 3  (+ (element1 v-neptune-moons i 3) 0.004))
   (lp-neptune (+ i 1))))

(define (loop-neptune)
   (send neptune-canvas on-paint)
   (lp-neptune 0)
   (sleep/yield 0.025) 
   (if (send neptune-frame is-shown?) (loop-neptune)))


;; le sous-canvas du canevas principal
(define MY-CANVAS%    ;; pour gérer la souris
 (class canvas%       ;; une sous-classe de canvas%
    (define/override (on-event evt)
      (case (send evt get-event-type)
       ((left-down) (let ((x (send evt get-x)) (y (send evt get-y)))
          (cond
            ((and (and (<= x 550) (>= x 450)) (and (>= y 450) (<= y 550))) (send sun-frame show #t))
            ((and (<= (abs (- (element 0 6) x)) (element 0 2)) (<= (abs (- (element 0 7) y)) (element 0 2))) (send mercury-frame show #t))
            ((and (<= (abs (- (element 1 6) x)) (element 1 2)) (<= (abs (- (element 1 7) y)) (element 1 2))) (send venus-frame show #t))
            ((and (<= (abs (- (element 2 6) x)) (element 2 2)) (<= (abs (- (element 2 7) y)) (element 2 2))) (send earth-frame show #t) (loop-earth))
            ((and (<= (abs (- (element 3 6) x)) (element 3 2)) (<= (abs (- (element 3 7) y)) (element 3 2))) (send mars-frame show #t) (loop-mars))
            ((and (<= (abs (- (element 4 6) x)) (element 4 2)) (<= (abs (- (element 4 7) y)) (element 4 2))) (send jupiter-frame show #t) (loop-jupiter))
            ((and (<= (abs (- (element 5 6) x)) (element 5 2)) (<= (abs (- (element 5 7) y)) (element 5 2))) (send saturn-frame show #t) (loop-saturn))
            ((and (<= (abs (- (element 6 6) x)) (element 6 2)) (<= (abs (- (element 6 7) y)) (element 6 2))) (send uranus-frame show #t) (loop-uranus))
            ((and (<= (abs (- (element 7 6) x)) (element 7 2)) (<= (abs (- (element 7 7) y)) (element 7 2))) (send neptune-frame show #t) (loop-neptune)))))
        (else (+ 0 0))))
    (super-new)))

;; le canvas principal et les affichages
(define ss-canvas (new MY-CANVAS% (parent ss-frame)
                    (paint-callback
                     (lambda (c dc)
                       
                       (send dc clear)
                       (send dc draw-bitmap noir 0 0)
                     
                       ;; Sun
                       (send dc draw-bitmap sun-bitmap (- WSUR2 50) (- HSUR2 50))

                       ;; Mercury
                       (send dc draw-bitmap mercury-bitmap (element 0 6) (element 0 7))
                       
                       ;; Venus
                       (send dc draw-bitmap venus-bitmap   (element 1 6) (element 1 7))
                                                          
                       ;; Earth
                       (send dc draw-bitmap earth-bitmap   (element 2 6) (element 2 7))
                                                             
                       ;; Mars
                       (send dc draw-bitmap mars-bitmap    (element 3 6) (element 3 7))
                                                            
                       ;; Jupiter
                       (send dc draw-bitmap jupiter-bitmap (element 4 6) (element 4 7))
                                                            
                       ;; Saturn
                       (send dc draw-bitmap saturn-bitmap  (element 5 6) (element 5 7))
                                                            
                       ;; Uranus
                       (send dc draw-bitmap uranus-bitmap  (element 6 6) (element 6 7))
                                                             
                       ;; Neptune
                       (send dc draw-bitmap neptune-bitmap (element 7 6) (element 7 7))
                                                              
                       ))))

;; lecture clavier
;;(define (prompt/read prompt)
;;(display prompt)
;;(read-line))
;;(define orbital_speed (string->number (prompt/read "Enter an orbital period (0.4 to 0.2): ")))
(send ss-frame show #t)

;; Les orbites
;; coordonnées polaires
;; x = x0 + distance (dist) (planète par rapport au soleil)*cos(teta)
;; y = y0 + distance (dist) (planète par rapport au soleil)*sin(teta)
;; angle teta = omega * t     où teta et t sont initialisés à 0
;; omega = rapport vitesse planète par rapport à la terre. dans le cas de la terre omega=1, Vénus omega=1.622 ...
;; teta = omega*t

;; calcul des paramètres précédents
(define t 0)
(define (boucle1 i)
  (when (< i NB_PLANETS)
       (vector-set! (vector-ref v-ss i) 6 (+ (element i 3)   ;; x
         (* (element i 1) (cos (element i 5)))))
       (vector-set! (vector-ref v-ss i) 7 (+ (element i 4)   ;; y
         (* (element i 1) (sin (element i 5)))))
       (vector-set! (vector-ref v-ss i) 5 (* (element i 8)  (element i 10))) ;; teta = omega * t
       (vector-set! (vector-ref v-ss i) 10 (+ (element i 10) 0.04)) ;;t = t + 0.04
       (boucle1 (+ i 1))))

;; Mercure tourne 4.147 x terre (88 jours): 4.147 plus rapide que la terre
;; Vénus tourne 1.622 x terre (225 jours): 1.622 plus rapide que la terre
;; le Terre 1 fois (365 jours)
;; Mars tourne 0.531 x terre (687 jours): ou 687/365 (1.88) moins rapide que la terre
;; Jupiter tourne 0.084 x terre (4333 jours): ou 4333/365 (11.87) moins rapide que la terre
;; Saturne tourne 0.034 x terre (10759 jours): ou 10759/365 (29.5) moins rapide que la terre
;; Uranus tourne 0.012 x terre (30685 jours): ou 30685/365 (83.33) moins rapide que la terre
;; Neptune tourne 0.006 x terre (60266 jours): ou 60266/365 (166.66) moins rapide que la terre


(define (loop)
  (send ss-canvas on-paint)
  (boucle1 0)
  (sleep/yield 0.25)  ;; pour gérer la période orbitale des planètes
                               ;; une valeur de 0.025 permet un mouvement qu'on peut observer attentivement
  (loop) )

(loop)
