;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname visualizer) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(require rsound)
(require rsound/draw)
(require 2htdp/image)
(require 2htdp/universe)
(require scribble/base)
(require racket/base)
(require racket/async-channel)


(define SONG-LOCATION1 "songs/Derezzed.wav")
(define SONG-LOCATION2 "songs/The Intro.wav")
(define SONG-LOCATION3 "songs/rct2theme.wav")
(define SONG-LOCATION4 "songs/Looking Glass.wav")
(define SONG-LOCATION5 "songs/Luv Sick.wav")

(define SONG1 (rs-read SONG-LOCATION1))
(define SONG2 (rs-read SONG-LOCATION2))
(define SONG3 (rs-read SONG-LOCATION3))
(define SONG4 (rs-read SONG-LOCATION4))
(define SONG5 (rs-read SONG-LOCATION5))


(define SONGLEN1 (rs-frames SONG1))
(define SONGLEN2 (rs-frames SONG2))
(define SONGLEN3 (rs-frames SONG3))
(define SONGLEN4 (rs-frames SONG4))
(define SONGLEN5 (rs-frames SONG5))

(define SONG-LIST  (list SONG1 SONG2 SONG3 SONG4 SONG5))
(define SONG-NAME-LIST (list SONG-LOCATION1 SONG-LOCATION2 SONG-LOCATION3 SONG-LOCATION4 SONG-LOCATION5))

(define-struct world[t a c1now c1go slide-h drag? scene p cs cs-name song-drag?])
;; a world is (make-world Num Num Num Num X-coord Boolean Num Num Num Rsound String)



(define INITIAL-WORLD (make-world 0 0 300 300 900 false 0 0 SONG1 SONG-LOCATION1 false))

(define volume-song (box 1))
(define ctr (box 5))
(define play-speed (box (world-p INITIAL-WORLD)))
(define cur-frame (box 1))
(define cur-song (box (world-cs INITIAL-WORLD)))
(define time-ticks (box 0))
(define new-frame (box 1))

;; this channel will hold the events flowing from the big-bang side
(define events (make-async-channel))
(define song-events (make-async-channel))

;; only check for events every 1/100 of a second. Otherwise
;; we won't get any sound generated.
(define EVENT-CHECK-INTERVAL 441)


;; reset the value to zero if an event occurs
(define (maybe-reset-ctr ctr)
  ;; only check every EVENT-CHECK-INTERVAL frames
  (cond [(= (modulo ctr EVENT-CHECK-INTERVAL) 0)
         ;; checks if the posistion of the song has been changed
         (local [(define change-pos (async-channel-try-get song-events))]
           ;; change position of the song:
           (cond [change-pos (unbox new-frame)]
                 ;; try to get an event from the channel.
                 [else (local [(define maybe-event (async-channel-try-get events))]
                         ;; yep, there was an event:
                         (cond [maybe-event 0]
                               ;; no, no event:
                               [else ctr]))])) 
         ]
        ;; not time to check yet:
        [else ctr]))

(signal-play 
 (network ()
          [ctr = (maybe-reset-ctr (+ (prev ctr 0) (unbox play-speed)))]
          [out = (begin
                   (set-box! cur-frame ctr)
                   (* (unbox volume-song) ; volume
                      (rs-ith/left (unbox cur-song) ctr)))]  
          )) 

;; CREATES VISUALS
;; world -> world
;; on tick, take the sample of a song 
;; and use it create the radius of our visuals
(define (tock w)
  (begin
    (if (= (unbox time-ticks) 255)(set-box! time-ticks 0)(set-box! time-ticks (add1 (unbox time-ticks))))
    (if (> (unbox ctr) 0)
        (set-box! ctr (sub1 (unbox ctr)))
        (set-box! ctr 3)
        )
    (if (= (unbox ctr) 0)
        (make-world (world-t w) (world-a w) (abs (/ (+ (world-c1now w) (world-c1go w)) 2)) (+ 35 (* 150 (rs-ith/left (unbox cur-song) (unbox cur-frame))))
                    (world-slide-h w) (world-drag? w)(world-scene w)(world-p w) (world-cs w)(world-cs-name w)(world-song-drag? w))
        
        (make-world (world-t w) (world-a w) (abs (/ (+ (world-c1now w) (world-c1go w)) 2)) (world-c1go w)
                    (world-slide-h w) (world-drag? w) (world-scene w)(world-p w) (world-cs w)(world-cs-name w)(world-song-drag? w))
        )))

;; DRAWS SOME IMAGES

;; defines play and pause buttons

(define play-button (bitmap/file "img/play-button.png"))
(define pause-button (bitmap/file "img/pause-button.png"))

;; Visuals
(define R-LOGO (scale/xy 2 2 (bitmap/file "img/logo.png")))
(define BACKGROUND-IMG (scale/xy 1.5 1.5 (bitmap/file "img/bg1.png")))

;; Menu Buttons
(define STRT-BUTTON 
  (overlay
   (text "Visualizer" 24 "green")
   (bitmap/file "img/sample-button.png")))
(define AWESOME-BUTTON 
  (overlay
   (text "???" 24 "green")
   (bitmap/file "img/sample-button.png")))
(define return-button
  (overlay
   (text "return" 12 "white")
   (rectangle 30 20 "solid" "cyan")
   (rectangle 35 25 "solid" "blue")))
;; Next and Previous Song Buttons

(define NXT-SNG (bitmap/file "img/next-button.png"))
(define PREV-SNG (rotate 180 (bitmap/file "img/next-button.png")))

;; Displays what the current song is playing
(define (NOWPLAYING w)
  (place-image
   (text 
    (string-append 
     "Now Playing: " (substring (world-cs-name w) 6 (- (string-length (world-cs-name w)) 3))) 
    15 "white")
   150 20
   (rectangle 300 40 "solid" "orange"))) 

;; draws the visualizer parts of the scene
(define (draw-visuals w s)  
  ;; debug text
  (overlay
   (above/align "left"
    (text (string-append "cur-frame: " (number->string (unbox cur-frame))) 20 "black")
    (text (string-append "world-c1now:  " (number->string (world-c1now w))) 20 "black")
    (text (string-append "Color:  " (number->string (unbox time-ticks))) 20 "black")
    )
   
   (cond
     [(= s 1) 
      (place-image
       (star (world-c1now w) "solid" 
             (make-color (if (> 128 (unbox time-ticks))(+ 40 (unbox time-ticks))(- 295 (unbox time-ticks))) 
                         (if (> 128 (unbox time-ticks))(- 128 (unbox time-ticks))(+ (unbox time-ticks) -128))
                         (if (> 128 (unbox time-ticks))(+ 100 (unbox time-ticks)) (- 355 (unbox time-ticks)))))
       600 320
       (place-image
        (star (* (world-c1now w) 1.5) "solid" 
              (make-color 150 0 150))
       600 320
       (empty-scene 1200 720)))
      ]
     
     [(= s 3) 
      (place-image
       (circle (world-c1now w) "outline" "green")
       600 320
       (place-image
        (circle (* 1.3 (world-c1now w)) "outline" "green")
        600 320
        (place-image
         (circle (* 0.9 (world-c1now w)) "outline" "green")
         600 320
         (place-image
          (circle (* 0.5 (world-c1now w)) "outline" "green")
          600 320
          (empty-scene 1200 720)))))
      ]
     
     [(= s 4) 
      (place-image
       (circle (* 0.25 (world-c1now w)) "solid" "red")
       600 320
       (place-image
        (circle (* 0.50 (world-c1now w)) "solid" "blue")
        600 320
        (place-image
         (circle (* 0.75 (world-c1now w)) "solid" "green")
         600 320
         (place-image
          (circle (* 1.00 (world-c1now w)) "solid" "orange")
          600 320
          (empty-scene 1200 720)))))
      ]
     
     [(= s 5) 
      (local [(define square1 (square 50 "solid" "green"))]
       (local [(define square2 (square 50 "solid" "black"))]
          (place-image
           square1
           200 (+ 320 (* .5 (world-c1now w)))
           (place-image
            square2
            300 (+ 315 (* .75 (world-c1now w)))
            (place-image
             square1
             400 (+ 300 (* 1.5 (world-c1now w)))
             (place-image
              square2
              500 (+ 280 (* 2.5 (world-c1now w)))
              (place-image
               square1
               600 (+ 250 (* 3.5 (world-c1now w)))
               (place-image
                square2
                700 (+ 280 (* 2.5 (world-c1now w)))
                (place-image
                 square1
                 800 (+ 300 (* 1.5 (world-c1now w)))
                 (place-image
                  square2
                  900 (+ 315 (* .75 (world-c1now w)))
                  (place-image
                   square1
                   1000 (+ 320 (* .5 (world-c1now w)))
                   (empty-scene 1200 720))))))))))))
      ]
     
     [(= s 6) 
      (local [(define square1 (square 50 "solid" "green"))]
       (local [(define square2 (square 50 "solid" "black"))]
         (place-image 
          (circle (* 2 (world-c1now w)) "solid" "cyan")
          (+ 50 (random 1100)) (+ 70 (random 500))
          
          (place-image
           (circle (* .5 (world-c1now w)) "solid" "black")
           (+ 50 (random 1100)) (+ 70 (random 500))
           
           (place-image
            (circle (* .5 (world-c1now w)) "solid" "darkslategray")
            (+ 50 (random 1100)) (+ 70 (random 500))
            
            (place-image
             (circle (* 1.5 (world-c1now w)) "solid" "black")
             (+ 50 (random 1100)) (+ 70 (random 500))
             
             (place-image
              (line 200 (world-c1now w) "white")
              600 320
              
              (place-image
               (rectangle 1200 720 "solid" (make-color 
                                            (if (> 128 (unbox time-ticks))(+ 40 (unbox time-ticks))(- 295 (unbox time-ticks))) 
                                            (if (> 128 (unbox time-ticks))(- 128 (unbox time-ticks))(+ (unbox time-ticks) -128))
                                            (if (> 128 (unbox time-ticks))(+ 100 (unbox time-ticks)) (- 355 (unbox time-ticks)))))
               600 360
               (empty-scene 1200 720)))))))))
      ]
     )
   )
  )


;; Creates Song Position Slider
(define (draw-song-slider w)
  (place-image
   (overlay
    (circle 4 "solid" "black")
    (circle 7 "solid" "green"))
   (* 1000 (/ (+ 1 (unbox cur-frame)) (rs-frames (unbox cur-song)))) 6
   (rectangle 1000 12 "solid" "cyan")
   ))

;; Create bg for visualizer
(define background-visuals (bitmap/file "img/bg2-1.png"))

;; tab drawer
(define (tab-draw num)
  (overlay
   (text (number->string num) 12 "gray")
   (rectangle 50 20 "outline" "gray")))

;; volume draw
(define volume-dragger
  (overlay
   (square 15 "solid" "cyan")
   (square 20 "solid" "white")
   ))

(define volume-bar
  (rectangle 500 20 "solid" "black"))

(define volume-icon (bitmap/file "img/volume-icon.png"))



;; Draws the scene 
;; world -> world 
(define (draw w)
  (cond
    [(= (world-scene w) 0)
     (place-image STRT-BUTTON 600 350
                  (place-image AWESOME-BUTTON 600 450
                               (place-image R-LOGO 600 200
                                            (place-image BACKGROUND-IMG 600 360
                                                         (empty-scene 1200 720)))))]
    
    [(or (= (world-scene w) 1) (= (world-scene w) 3) (= (world-scene w) 4) (= (world-scene w) 5) (= (world-scene w) 6))
     (place-image
      (draw-song-slider w)
      600 550
      (place-image
       volume-dragger
       (world-slide-h w) 650
       (place-image
        play-button
        100 650
        (place-image
         pause-button
         200 650
         (place-image
          volume-icon
          600 650
          (place-image
           volume-bar
           900 650
           (place-image
            return-button
            40 30
            (place-image 
             NXT-SNG
             450 650
             (place-image
              PREV-SNG
              350 650
              (place-image
               (tab-draw 1)
               75 60
               (place-image
                (tab-draw 2)
                125 60
                (place-image
                 (tab-draw 3)
                 175 60
                 (place-image
                  (tab-draw 4)
                  225 60
                  (place-image
                   (tab-draw 5)
                   275 60
                   (place-image
                    (NOWPLAYING w)
                    600 500
                    (place-image
                     background-visuals
                     600 360
                     (place-image
                      (draw-visuals w (world-scene w))
                      600 360
                      (empty-scene 1200 720))))))))))))))))))
     ]
    
    [(= (world-scene w) 2)
     (place-image
      (above
       (text " E M O S E W A " 34 "red")
       (text " A W E S O M E " 34 "blue"))
      600 360
      (place-image
       (ellipse 500 150 200 "gray")
       600 360
       (place-image
        return-button
        40 30
        (empty-scene 1200 720))))]
    ))  


;bounds of buttons on menu screen
(define X_BOUNDARY1 500)
(define X_BOUNDARY2 700)
(define Y_BOUNDARY1 330)
(define Y_BOUNDARY2 380)

(define X_BOUNDARY3 500)
(define X_BOUNDARY4 700)
(define Y_BOUNDARY3 420)
(define Y_BOUNDARY4 480)

(define X_BOUNDARY5 25)
(define X_BOUNDARY6 55)
(define Y_BOUNDARY5 20)
(define Y_BOUNDARY6 30)

;; bounds of the tabs for selecting the visuals
(define X_BOUNDARY7 50)
(define X_BOUNDARY8 100)

(define X_BOUNDARY9 100)
(define X_BOUNDARY10 150)

(define X_BOUNDARY11 100)
(define X_BOUNDARY12 150)

(define X_BOUNDARY13 150)
(define X_BOUNDARY14 200)

(define X_BOUNDARY15 200)
(define X_BOUNDARY16 250)

(define X_BOUNDARY17 300)

;; y Bounds for tabs are the same
(define Y_BOUNDARY7 50)
(define Y_BOUNDARY8 70)

;; next and previous button boundaries (next and previous y boundaries are the same)
(define next-x-l (- 450 25)) ;left
(define next-x-r (+ 450 25)) ;right
(define prev-x-l (- 350 30))
(define prev-x-r (+ 350 30))
(define y-t (+ 650 10)) ;top
(define y-b (- 650 10)) ;bottom


;; function for changing songs

;; next song selector
;; current song, song list -> next song 
(define (next-song song-list cs)
  (cond
    [(rs-equal? cs (first song-list)) 
     (cond
       [(empty? (rest song-list)) (first SONG-LIST)]
       [else (first (rest song-list))])]    
    [else (next-song (rest song-list) cs)]))

;; previous song selector
;; current song, song list -> previous song
(define (prev-song song-list cs)
  (cond
    ;[(empty? (rest song-list)) (first (rest (reverse song-list)))]
    [(rs-equal? cs (first (rest song-list))) (first song-list)]
    [(rs-equal? cs (first song-list)) (first (reverse SONG-LIST))]
    [else (prev-song (rest song-list) cs)]))

;; functions for changing the name of the song

;; next song-name selector
;; current song, song list -> next song
(define (next-song-name song-list cs-name) 
  (cond
    [(string=? cs-name (first song-list))
     (cond
       [(empty? (rest song-list)) (first SONG-NAME-LIST)]
       [else (first (rest song-list))])]    
    [else (next-song-name (rest song-list) cs-name)]))

;; previous song-name selector
;; current song, song list -> previous song
(define (prev-song-name song-list cs-name)
  (cond
    ;[(empty? (rest song-list)) (first (rest (reverse song-list)))]
    [(string=? cs-name (first (rest song-list))) (first song-list)]
    [(string=? cs-name (first song-list)) (first (reverse SONG-NAME-LIST))]
    [else (prev-song-name (rest song-list) cs-name)]))

;; Mouse Events
;; World X-coord Y-coord Mouse-Event -> World 
(define (mouse-event w x y event)
  (cond
    ;; handles mouse clicks
    [(mouse=? event "button-down")
     ;; prev song
     (cond
       [(and (> y y-b) (< y y-t) (> x prev-x-l) (< x prev-x-r)(not (= (world-scene w) 0))) 
        (begin
          (async-channel-put events true)
          (set-box! cur-song (prev-song SONG-LIST (world-cs w)))
          (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) (world-drag? w) 
                      (world-scene w) (world-p w)
                      (prev-song SONG-LIST (world-cs w)) (prev-song-name SONG-NAME-LIST (world-cs-name w))(world-song-drag? w)))]  
       ;; next song
       [(and (> y y-b) (< y y-t) (> x next-x-l) (< x next-x-r)(not (= (world-scene w) 0))) 
        (begin 
          (async-channel-put events true) 
          (set-box! cur-song (next-song SONG-LIST (world-cs w)))  
          (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) (world-drag? w) 
                      (world-scene w) (world-p w) 
                      (next-song SONG-LIST (world-cs w)) (next-song-name SONG-NAME-LIST (world-cs-name w))(world-song-drag? w)))]
       ;; Menu Buttons
       ;; Music Player Button
       [(and (> y Y_BOUNDARY1) (< y Y_BOUNDARY2) (> x X_BOUNDARY1) (< x X_BOUNDARY2) (= 0 (world-scene w))) 
        (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) (world-drag? w)  1 (world-p w) (world-cs w)
                    (world-cs-name w)(world-song-drag? w))]
       ;; Secret Message
       [(and (> y Y_BOUNDARY3) (< y Y_BOUNDARY4) (> x X_BOUNDARY3) (< x X_BOUNDARY4))
        (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) (world-drag? w)  2 (world-p w) (world-cs w)
                    (world-cs-name w)(world-song-drag? w))]
       ;; Play Screen Buttons
       ;; Returns to Menu
       [(and (> y Y_BOUNDARY5) (< y Y_BOUNDARY6) (> x X_BOUNDARY5) (< x X_BOUNDARY6) (not (= 0 (world-scene w)))) 
        (begin 
          (set-box! play-speed 0) 
          (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) (world-drag? w)  0 0 (world-cs w)
                      (world-cs-name w)(world-song-drag? w)))] 
       ;; Goes to screen 1
       [(and (> y Y_BOUNDARY7) (< y Y_BOUNDARY8) (> x X_BOUNDARY7) (< x X_BOUNDARY8) (not (= 0 (world-scene w)))(not(= 1 (world-scene w)))) 
        (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) (world-drag? w) 1 (world-p w) (world-cs w)
                    (world-cs-name w)(world-song-drag? w))]
       ;; Goes to screen 3 which is the same as 1 but different visuals
       [(and (> y Y_BOUNDARY7) (< y Y_BOUNDARY8) (> x X_BOUNDARY9) (< x X_BOUNDARY10) (not (= 0 (world-scene w)))(not (= 3 (world-scene w))))
        (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) (world-drag? w) 3 (world-p w) (world-cs w)
                    (world-cs-name w)(world-song-drag? w))]
       ;; Goes to screen 4
       [(and (> y Y_BOUNDARY7) (< y Y_BOUNDARY8) (> x X_BOUNDARY13) (< x X_BOUNDARY14) (not (= 0 (world-scene w)))(not (= 4 (world-scene w))))
        (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) (world-drag? w) 4 (world-p w) (world-cs w)
                    (world-cs-name w)(world-song-drag? w))] 
       ;; Goes to screen 5
       [(and (> y Y_BOUNDARY7) (< y Y_BOUNDARY8) (> x X_BOUNDARY15) (< x X_BOUNDARY16) (not (= 0 (world-scene w)))(not (= 5 (world-scene w))))
        (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) (world-drag? w) 5 (world-p w) (world-cs w)
                    (world-cs-name w)(world-song-drag? w))]
       ;; Goes to screen 6
       [(and (> y Y_BOUNDARY7) (< y Y_BOUNDARY8) (> x X_BOUNDARY16) (< x X_BOUNDARY17) (not (= 0 (world-scene w)))(not (= 6 (world-scene w))))
        (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) (world-drag? w) 6 (world-p w) (world-cs w)
                    (world-cs-name w)(world-song-drag? w))]
       
       ;;pause
       [(and (> x (- 200 25)) (< x (+ 200 25)) (> y (- 650 25)) (< y (+ 650 25))(not (= (world-scene w) 0)))
        (begin 
          (set-box! play-speed 0)
          (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) (world-drag? w)(world-scene w) 0 (world-cs w)
                      (world-cs-name w)(world-song-drag? w)))]
       ;;plays  
       [(and (> x (- 100 25)) (< x (+ 100 25)) (> y (- 650 25)) (< y (+ 650 25))(not (= (world-scene w) 0)))
        (begin
          (set-box! play-speed 1);(define-struct world[t a c1now c1go slide-h drag? scene p cs cs-name song-drag?])
          (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) (world-drag? w)(world-scene w) 1 (world-cs w)
                      (world-cs-name w)(world-song-drag? w)))]
       [else w])]
    ;; makes drag? false when button is not held down
    [(mouse=? event "button-up")
     (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) false (world-scene w)(world-p w) (world-cs w)
                 (world-cs-name w) false)
     ] 
    [(mouse=? event "drag")
     (cond
       [(world-drag? w)
        (cond
          [(and (> x (- 910 250)) (< x (+ 890 250)) (not (= (world-scene w) 0)))
           (begin 
             (set-box! volume-song (/ (- (world-slide-h w) 660) 480))
             (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) x true (world-scene w) 
                         (world-p w) (world-cs w)
                         (world-cs-name w)(world-song-drag? w))) 
           ]
          [else 
           (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) true (world-scene w)(world-p w) (world-cs w)
                       (world-cs-name w)(world-song-drag? w))]
          )]
       [(world-song-drag? w)
        (cond
          [(and (> x 100) (< x 1100))
           (begin
             (set-box! new-frame (round (* (rs-frames (unbox cur-song)) (/ (- x 100) 1000))))
             (async-channel-put song-events true)
             (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) (world-drag? w) (world-scene w) 
                         (world-p w) (world-cs w)
                         (world-cs-name w) true))
           ]
          [else 
           (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) true (world-scene w)(world-p w) (world-cs w)
                       (world-cs-name w)(world-song-drag? w))])]
       ;; When mouse is within the boundaries of a slider, then drag? is true
       [else 
        (cond
          [(and (> x (- 910 250)) (< x (+ 890 250))(> y (- 650 20)) (< y (+ 650 20)) (not (= (world-scene w) 0)))
           (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w)
                       x true (world-scene w)(world-p w) (world-cs w)
                       (world-cs-name w)(world-song-drag? w))]
          [(and (> x 100) (< x 1100) (> y 544) (< y 556)) (not (= (world-scene w) 0))
                                                          (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w)
                                                                      (world-slide-h w) (world-drag? w) (world-scene w)(world-p w) (world-cs w)
                                                                      (world-cs-name w) true)]
          [else w])])
     ]
    [else w]))

(big-bang INITIAL-WORLD 
          [on-tick tock 1/60]
          [to-draw draw]
          [on-mouse mouse-event]
          )