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


;;


(define SONG-LOCATION1 "songs/Derezzed.wav")
(define SONG-LOCATION2 "songs/The Intro.wav")
(define SONG-LOCATION3 "songs/rct2theme.wav")


(define SONG1 (rs-read/clip SONG-LOCATION1 0 (rs-frames (rs-read SONG-LOCATION1)))) ;(* 44100 30)))
(define SONG2 (rs-read/clip SONG-LOCATION2 0 (rs-frames (rs-read SONG-LOCATION2))))
(define SONG3 (rs-read/clip SONG-LOCATION3 0 (rs-frames (rs-read SONG-LOCATION3))))


(define SONGLEN1 (rs-frames SONG1))
(define SONGLEN2 (rs-frames SONG2))
(define SONGLEN3 (rs-frames SONG3))

(define SONG-LIST  (list SONG1 SONG2 SONG3))

(define-struct world[t a c1now c1go slide-h drag? scene volume p cs])
;; a world is (make-world Num Num Num Num X-coord Boolean Num Num Num Rsound)

(define INITIAL-WORLD (make-world 0 0 300 300 900 false 0 1 0 SONG1))
(define volume-song (box (world-volume INITIAL-WORLD)))
(define ctr (box 5))
(define play-speed (box (world-p INITIAL-WORLD)))
(define cur-frame (box 1))
(define cur-song (box (world-cs INITIAL-WORLD)))

;; this channel will hold the events flowing from the big-bang side
(define events (make-async-channel))

;; only check for events every 1/100 of a second. Otherwise
;; we won't get any sound generated.
(define EVENT-CHECK-INTERVAL 441)


;; reset the value to zero if an event occurs
(define (maybe-reset-ctr ctr)
  ;; only check every EVENT-CHECK-INTERVAL frames
  (cond [(= (modulo ctr EVENT-CHECK-INTERVAL) 0)
         ;; try to get an event from the channel.
         (local [(define maybe-event (async-channel-try-get events))]
           ;; yep, there was an event:
           (cond [maybe-event 0]
                 ;; no, no event:
                 [else ctr]))]
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
    (if (> (unbox ctr) 0)
        (set-box! ctr (sub1 (unbox ctr)))
        (set-box! ctr 3)
        )
    (if (= (unbox ctr) 0)
        (make-world (world-t w) (world-a w) (abs (/ (+ (world-c1now w) (world-c1go w)) 2)) (+ 35 (* 150 (rs-ith/left (unbox cur-song) (unbox cur-frame))))
                    (world-slide-h w) (world-drag? w)(world-scene w)(world-volume w)(world-p w) SONG1)
        
        (make-world (world-t w) (world-a w) (abs (/ (+ (world-c1now w) (world-c1go w)) 2)) (world-c1go w)
                    (world-slide-h w) (world-drag? w) (world-scene w)(world-volume w)(world-p w) SONG1)
        )))

;; DRAWS SOME IMAGES
(define BACKGROUND-IMG (rectangle 1200 720 "solid" "white"))
(define R-LOGO (bitmap/file "img/rvisualizerlogo.jpg"))  
(define STRT-BUTTON (rectangle 200 60 "solid" "dodgerblue"))
(define AWESOME-BUTTON (rectangle 200 60 "solid" "hotpink"))
(define NXT-SNG 
  (place-image
   (text "next" 12 "white")
   25 10
   (rectangle 50 20 "solid" "blue")))
(define PREV-SNG 
  (place-image
   (text "previous" 12 "white")
   30 10
   (rectangle 60 20 "solid" "purple")))
(define NOWPLAYING 
  (place-image
   (text 
    (string-append 
     "Now Playing: " (substring SONG-LOCATION1 6 (- (string-length SONG-LOCATION1) 3)))
     15 "white")
    150 20
    (rectangle 300 40 "solid" "black")))

(define (draw-visuals w) 
  (overlay
   (above/align "left"
                (rectangle 1200 170 "solid" "blue")
                (beside
                 (rectangle 50 500 "solid" "red")
                 (rectangle 1100 500 "outline" "white")
                 (rectangle 50 500 "solid" "red")
                 )
                (rectangle 1200 150 "solid" "purple")
                )
   (above
    (text (string-append "cur-frame: " (number->string (unbox cur-frame))) 20 "black")
    (text (string-append "world-c1now:  " (number->string (world-c1now w))) 20 "black")
    )
   ;(circle (world-c1now w) "solid" "red")
   ))


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
    [(= (world-scene w) 1) 
     (place-image
      (circle (world-c1now w) "solid" "red")
      600 320
      (place-image
       (square 20 "solid" "green")
       (world-slide-h w) 650
       (place-image
        (rectangle 1100 500 "outline" "black")
        600 320
        (place-image
         (square 50 "solid" "green")
         100 650
         (place-image
          (square 50 "solid" "red")
          200 650
          (place-image
           (rectangle 500 20 "outline" "black")
           900 650
           (place-image
            (rectangle 30 20 "solid" "hotpink")
            40 30
            (place-image
             NXT-SNG
             450 650
             (place-image
              PREV-SNG
              350 650
              (place-image
               (rectangle 50 20 "outline" "black")
               75 60
               (place-image
                (rectangle 50 20 "outline" "black")
                125 60
                (place-image
                 (draw-visuals w)
                 600 310
                 (place-image
                  NOWPLAYING
                  600 500
                  (empty-scene 1200 720))))))))))))))]
    
    [(= (world-scene w) 2)
     (place-image (bitmap/file "img/emosewa.gif")
                  600 360
                  
                  (empty-scene 1200 720))]
    
    [(= (world-scene w) 3)
     (place-image
      (triangle (world-c1now w) "solid" "green")
      600 320
      (place-image
       (square 20 "solid" "green")
       (world-slide-h w) 650
       (place-image
        (rectangle 1100 500 "outline" "black")
        600 320
        (place-image
         (square 50 "solid" "green")
         100 650
         (place-image
          (square 50 "solid" "red")
          200 650
          (place-image
           (rectangle 500 20 "outline" "black")
           900 650
           (place-image
            (rectangle 30 20 "solid" "hotpink")
            40 30
            (place-image
             NXT-SNG
             450 650
             (place-image
              PREV-SNG
              350 650 
              (place-image
               (rectangle 50 20 "outline" "black")
               75 60
               (place-image
                (rectangle 50 20 "outline" "black")
                125 60
                (place-image
                 (draw-visuals w)
                 600 310
                 (empty-scene 1200 720)))))))))))))] 
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
(define Y_BOUNDARY7 50)
(define Y_BOUNDARY8 70)

(define X_BOUNDARY9 100)
(define X_BOUNDARY10 150)
(define Y_BOUNDARY9 50)
(define Y_BOUNDARY10 70)

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
                      (world-scene w) (world-volume w)(world-p w) SONG1))]
       ;; next song
       [(and (> y y-b) (< y y-t) (> x next-x-l) (< x next-x-r)(not (= (world-scene w) 0)))
        (begin 
          (async-channel-put events true)
          (set-box! cur-song (next-song SONG-LIST (world-cs w)))  
          (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) (world-drag? w) 
                      (world-scene w) (world-volume w)(world-p w) SONG2))]
       ;; Menu Buttons
       ;; Music Player Button
       [(and (> y Y_BOUNDARY1) (< y Y_BOUNDARY2) (> x X_BOUNDARY1) (< x X_BOUNDARY2) (= 0 (world-scene w))) 
        (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) (world-drag? w)   1 (world-volume w)(world-p w) 1)]
       ;; Place holder
       [(and (> y Y_BOUNDARY3) (< y Y_BOUNDARY4) (> x X_BOUNDARY3) (< x X_BOUNDARY4))
        (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) (world-drag? w)  2 (world-volume w)(world-p w) 1)]
       ;; Returns to Menu
       [(and (> y Y_BOUNDARY5) (< y Y_BOUNDARY6) (> x X_BOUNDARY5) (< x X_BOUNDARY6) (not (= 0 (world-scene w)))) 
        (begin 
          (set-box! play-speed 0) 
          (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) (world-drag? w)  0 (world-volume w) 0 1))] 
       ;; Goes to screen 1
       [(and (> y Y_BOUNDARY7) (< y Y_BOUNDARY8) (> x X_BOUNDARY7) (< x X_BOUNDARY8) (not (= 0 (world-scene w)))(not(= 1 (world-scene w)))) 
        (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) (world-drag? w)  1 (world-volume w)(world-p w) 1)]
       ;; Goes to screen 3 which is the same as 1 but different visuals
       [(and (> y Y_BOUNDARY9) (< y Y_BOUNDARY10) (> x X_BOUNDARY9) (< x X_BOUNDARY10) (not (= 0 (world-scene w)))(not (= 3 (world-scene w))))
        (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) (world-drag? w)  3 (world-volume w)(world-p w) 1)]
       
       ;;stops pstream
       [(and (> x (- 200 25)) (< x (+ 200 25)) (> y (- 650 25)) (< y (+ 650 25))(not (= (world-scene w) 0)))
        (begin 
          (set-box! play-speed 0)
          (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) (world-drag? w)(world-scene w) (world-volume w) 0 1)
          
          )]
       ;;play pstream  
       [(and (> x (- 100 25)) (< x (+ 100 25)) (> y (- 650 25)) (< y (+ 650 25))(not (= (world-scene w) 0)))
        (begin
          (set-box! play-speed 1)
          (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) (world-drag? w)(world-scene w)(world-volume w) 1 1))] 
       [else w])]
    ;; makes drag? false when button is not held down
    [(mouse=? event "button-up")
     (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) false (world-scene w)(world-volume w)(world-p w) 1)
     ] 
    [(mouse=? event "drag")
     (cond
       [(boolean=? true (world-drag? w))
        (cond
          [(and (> x (- 910 250)) (< x (+ 890 250))(not (= (world-scene w) 0)))
           ;(make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) x (world-slide-v w) true (round (* 255 (/ (- (world-slide-h w) 660) 480))) (world-c1g w)(world-c1b w)(world-scene w))]
           (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) x true (world-scene w) 
                       (begin 
                         (set-box! volume-song (/ (- (world-slide-h w) 660) 480))
                         (/ (- (world-slide-h w) 660) 480))
                       (world-p w) 1) 
           ]
          [else 
           (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) true (world-scene w)(world-volume w)(world-p w) 1)]
          )]
       ;; When mouse is within the boundaries of a slider, then drag? is true
       [else 
        (cond
          [(and (> x (- 910 250)) (< x (+ 890 250))(> y (- 650 20)) (< y (+ 650 20)) (not (= (world-scene w) 0)))
           (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w)
                       x true (world-scene w)(world-volume w)(world-p w) 1)]
          [else w])])
     ]
    [else w]) 
  )


(big-bang INITIAL-WORLD
          [on-tick tock 1/60]
          [to-draw draw]
          [on-mouse mouse-event]
          )

