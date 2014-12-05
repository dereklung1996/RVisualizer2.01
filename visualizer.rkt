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


;; song locations
(define SONG-LOCATION1 "songs/Derezzed - Daft Punk.wav")
(define SONG-LOCATION2 "songs/The Intro - The XX.wav")
(define SONG-LOCATION3 "songs/Rct2 Theme.wav")
(define SONG-LOCATION4 "songs/Looking Glass - Saturn.wav")
(define SONG-LOCATION5 "songs/Luv Sick - Saturn.wav")

;; converts a path into a rsound
(define SONG1 (rs-read SONG-LOCATION1))
(define SONG2 (rs-read SONG-LOCATION2))
(define SONG3 (rs-read SONG-LOCATION3))
(define SONG4 (rs-read SONG-LOCATION4))
(define SONG5 (rs-read SONG-LOCATION5))

;; defines the length of a song
(define SONGLEN1 (rs-frames SONG1))
(define SONGLEN2 (rs-frames SONG2))
(define SONGLEN3 (rs-frames SONG3))
(define SONGLEN4 (rs-frames SONG4))
(define SONGLEN5 (rs-frames SONG5))


;; a SONG-LIST is one of
;;-empty
;;-(cons SONG (rest SONG-LIST))
;; SONG is a rsound
(define SONG-LIST  (list SONG1 SONG2 SONG3 SONG4 SONG5))

;;a SONG-NAME-LIST is one of
;;-empty
;;(cons SONG-LOCATION (rest SONG-NAME-LIST)
;; SONG-LOCATION is a string
(define SONG-NAME-LIST (list SONG-LOCATION1 SONG-LOCATION2 SONG-LOCATION3 SONG-LOCATION4 SONG-LOCATION5))

;; a world is (make-world Num Num Num Num X-coord Boolean Num Num Num Rsound String)
(define-struct world[t a c1now c1go slide-h drag? scene p cs cs-name song-drag?])

;; initial world
(define INITIAL-WORLD (make-world 0 0 300 300 1100 false 0 0 SONG1 SONG-LOCATION1 false))

;; Visuals
(define R-LOGO (scale/xy 2 2 (bitmap/file "img/logo.png")))
(define BACKGROUND-IMG (scale/xy 1.5 1.5 (bitmap/file "img/bg1.png")))

;; defines play and pause buttons
(define play-button (bitmap/file "img/play-button.png"))
(define pause-button (bitmap/file "img/pause-button.png"))

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
   (text "return" 10 "red")
   (rectangle 30 20 "solid" "cyan")
   (rectangle 35 25 "solid" "blue")))

;; Next and Previous Song Buttons
(define NXT-SNG (bitmap/file "img/next-button.png"))
(define PREV-SNG (rotate 180 (bitmap/file "img/next-button.png")))

;; Create bg for visualizer
(define background-visuals (bitmap/file "img/bg2-1.png"))

;; bounds of buttons on menu screen
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

;; miscellaneous definitions
;; song volume
(define volume-song (box 1))
;; ctr used in network
(define ctr (box 5))
;; play-speed
(define play-speed (box (world-p INITIAL-WORLD)))
;; current frame
(define cur-frame (box 1))
;; current song
(define cur-song (box (world-cs INITIAL-WORLD)))
;; current song name
(define cur-song-name (box (world-cs-name INITIAL-WORLD)))
;; a ticker that ticks all the way to 255 starting from 0
;; used for make-color
(define time-ticks (box 0))
;; used to set the position slider
(define new-frame (box 1))
;; indicates if the end of the song has been reached
(define end-song? (box false))
;; counter used to delay next song (see next-song and prev-song)
(define next-song-ctr (box 10))
;; indicates which song change function is to be used when a song-change event occurs
(define next/prev (box true))
;; ???
(define CLEMENTS (bitmap/file "img/clements.png"))

;; volume elements
(define volume-dragger
  (overlay
   (square 15 "solid" "cyan")
   (square 20 "solid" "white")))
(define volume-bar
  (rectangle 500 20 "solid" "black"))
(define volume-icon (bitmap/file "img/volume-icon.png"))
;; volume button bounds
(define Volume-Icon-X1 575)
(define Volume-Icon-X2 625)
(define Volume-Icon-Y1 625)
(define Volume-Icon-Y2 675)



;; SOUND CODE

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
                      (rs-ith/left (unbox cur-song) ctr)))]))

;; VISUALS


;; Displays what the current song is playing
(define (NOWPLAYING w)
  (place-image
   (text 
    (string-append 
     "Now Playing: " (substring (world-cs-name w) 6 (- (string-length (world-cs-name w)) 4))) 
    15 "white")
   150 20
   (rectangle 300 40 "solid" "orange")))

#;(check-expect (NOWPLAYING INITIAL-WORLD)
              (place-image
               (text "Now Playing: Derezzed - Daft Punk" 15 "white")
               150 20
               (rectangle 300 40 "solid" "orange")))



;; draws the visualizer parts of the scene
(define (draw-visuals w s)  
  ;; debug text
  ;(overlay
  #;(above/align "left"
                 (text (string-append "cur-frame: " (number->string (unbox cur-frame))) 20 "white")
                 (text (string-append "world-c1now:  " (number->string (world-c1now w))) 20 "white")
                 (text (string-append "Color:  " (number->string (unbox time-ticks))) 20 "white"))
  (cond
    [(= s 1) 
     ;; draw the "star" visualizer
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
       (rectangle 1200 720 "solid" "black")))]
    [(= s 3) 
     ;; draw the "green circles" visualizer
     (place-image
      (circle (* 1.5 (world-c1now w)) "outline" "green")
      600 320
      (place-image
       (circle (* 1.25 (world-c1now w)) "outline" "green")
       600 320
       (place-image
        (circle (* 1 (world-c1now w)) "outline" "green") 
        600 320
        (place-image
         (circle (* 0.75 (world-c1now w)) "outline" "green")
         600 320
         (rectangle 1200 720 "solid" "black")))))]
    [(= s 4) 
     ;; draw the "flower" visualizer
     (local [(define colors (make-color 
                             0
                             252
                             (if (> 128 (unbox time-ticks))(+ 50 (unbox time-ticks)) (- 305 (unbox time-ticks)))))]
       (place-image 
        (underlay (ellipse (+ 10 (* 1.5 (world-c1now w))) (+ 60 (* 1.5 (world-c1now w))) 40 colors)
                  (ellipse (+ 20 (* 1.5 (world-c1now w))) (+ 50 (* 1.5 (world-c1now w))) 40 colors)
                  (ellipse (+ 30 (* 1.5 (world-c1now w))) (+ 40 (* 1.5 (world-c1now w))) 40 colors)
                  (ellipse (+ 40 (* 1.5 (world-c1now w))) (+ 30 (* 1.5 (world-c1now w))) 40 colors)
                  (ellipse (+ 50 (* 1.5 (world-c1now w))) (+ 20 (* 1.5 (world-c1now w))) 40 colors)
                  (ellipse (+ 60 (* 1.5 (world-c1now w))) (+ 10 (* 1.5 (world-c1now w))) 40 colors))
        600 320
        (rectangle 1200 720 "solid" "black")))]
    [(= s 5) 
     ;; draw the "radial star" visualizer
     (place-image
      (radial-star (+ (round (/ (if (> 128 (unbox time-ticks))(+ 2 (unbox time-ticks))(- 257 (unbox time-ticks))) 4)) 2 
                      (round (/ (world-c1now w) 3))) (+ 55 (* 0.5 (world-c1now w))) (+ 60 (* 0.7 (world-c1now w))) "outline" "red")
      600 320
      (rectangle 1200 720 "solid" "black"))]
    [(= s 6) 
     ;; draw the "random circles" visualizer
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
              (rectangle 1200 720 "solid" (make-color 
                                           (if (> 128 (unbox time-ticks))(+ 40 (unbox time-ticks))(- 295 (unbox time-ticks))) 
                                           (if (> 128 (unbox time-ticks))(- 128 (unbox time-ticks))(+ (unbox time-ticks) -128))
                                           (if (> 128 (unbox time-ticks))(+ 100 (unbox time-ticks)) (- 355 (unbox time-ticks)))))
              600 360
              (empty-scene 1200 720))))))))])
  ; )
  )

;; Creates Song Position Slider
;; world -> scene
;; Draws the position of the song
(define (draw-song-slider w)
  (place-image
   (overlay
    (circle 4 "solid" "blue")
    (circle 7 "solid" "green"))
   (* 1000 (/ (+ 1 (unbox cur-frame)) (rs-frames (unbox cur-song)))) 6
   (rectangle 1000 12 "solid" "cyan")))

;; tab drawer
;; number -> scene 
;; takes a number and draws a tab with the number inside
(define (tab-draw num)
  (overlay
   (text (number->string num) 12 "red")
   (rectangle 50 20 "outline" "gray")
   (rectangle 50 20 200 "white")))

#;(check-expect (tab-draw 3)
              (overlay
               (text "3" 12 "red")
               (rectangle 50 20 "outline" "gray")
               (rectangle 50 20 200 "white")))



;; next song selector
;; both next-song and prev-song are called exclusively in the on-tick function
;; next-song-ctr is needed to delay the current song from switching before the network ctr has reset
;; current song, song list, name-list -> next-song, 0
(define (next-song song-list name-list cs)
  (if (= (unbox next-song-ctr) 0) ;; if true, time for current song to change
  (cond ;; searches list for current song, and sets boxes to corresponding data from the next song
    [(rs-equal? cs (first song-list)) 
     (cond
       [(empty? (rest song-list)) 
        (begin
          (set-box! cur-song (first SONG-LIST))
          (set-box! cur-song-name (first SONG-NAME-LIST))
          (set-box! end-song? false))]
       [else
        (begin
          (set-box! cur-song (first (rest song-list)))
          (set-box! cur-song-name (first (rest name-list)))
          (set-box! end-song? false))])]    
    [else
     (next-song (rest song-list) (rest name-list) cs)])
  (begin
    (set-box! next-song-ctr (sub1 (unbox next-song-ctr))) ;; function is called until next-song-ctr is 0
    0)))

;(check-expect (next-song SONG-LIST SONG-NAME-LIST SONG1) 0)


;; previous song selector
;; current song, song list, name-list -> previous song, 0
;; operates the same way as next-song
(define (prev-song song-list name-list cs)
  (if (= (unbox next-song-ctr) 0)  
  (cond  
    [(rs-equal? cs (first (rest song-list)))
     (begin
       (set-box! cur-song (first song-list))
       (set-box! cur-song-name (first name-list))
       (set-box! end-song? false)
       (set-box! next-song-ctr 10))]
    [(rs-equal? cs (first song-list))
     (begin
       (set-box! cur-song (first (reverse SONG-LIST)))
       (set-box! cur-song-name (first (reverse SONG-NAME-LIST)))
       (set-box! end-song? false)
       (set-box! next-song-ctr 10))]
    [else
     (prev-song (rest song-list) (rest name-list) cs)])
  (begin
    (set-box! next-song-ctr (sub1 (unbox next-song-ctr))) 
    0)))

;(check-expect (prev-song SONG-LIST SONG-NAME-LIST SONG1) 0)


;; Draws the scene 
;; world -> world 
(define (draw w)
  (cond
    [(= (world-scene w) 0)
     ;; draw the main menu
     (place-image STRT-BUTTON 600 350
                  (place-image AWESOME-BUTTON 600 450
                               (place-image R-LOGO 600 200
                                            (place-image BACKGROUND-IMG 600 360
                                                         (empty-scene 1200 720)))))]
    [(or (= (world-scene w) 1) (= (world-scene w) 3) (= (world-scene w) 4) (= (world-scene w) 5) (= (world-scene w) 6))
     ;; draw the background elements and main interface
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
                      ;; draws the visuals depending on the scene
                      (draw-visuals w (world-scene w))
                      600 360
                      (empty-scene 1200 720))))))))))))))))))]
    [(= (world-scene w) 2)
     ;; draw the secret scene
     (place-image
      (overlay
       (rotate (* 3 (unbox time-ticks) (/ 360 255))
               (above
                (text " E M O S E W A " 100 (make-color 255 76 50 255))
                (text " A W E S O M E " 100 (make-color 50 76 255 255))))
       (rotate (- 1 (* 3 (unbox time-ticks)) (/ 360 255))
               (above
                (text " E M O S E W A " 100 (make-color 255 76 50 150))
                (text " A W E S O M E " 100 (make-color 50 76 255 150))))
       (rotate (- 2 (* 3 (unbox time-ticks)) (/ 360 255))
               (above
                (text " E M O S E W A " 100 (make-color 255 76 50 75))
                (text " A W E S O M E " 100 (make-color 50 76 255 75)))))
      600 360
      (place-image
       return-button
       40 30
       (place-image
        (scale (* 2 (random)) CLEMENTS)
        (random 1200)
        (random 720)
        (place-image
         (scale (* 0.6 (random)) CLEMENTS)
         (random 1200)
         (random 720)
         (place-image
          (scale (* 3 (random)) CLEMENTS)
          (random 1200)
          (random 720)
          (place-image
           (scale (/ (+ 1 (unbox time-ticks)) 80) CLEMENTS)
           600 360
           (place-image
            (scale (/ (+ 1 (unbox time-ticks)) 70) CLEMENTS)
            600 360
            (place-image
             (scale (/ (+ 1 (unbox time-ticks)) 60) CLEMENTS)
             600 360
             (place-image
              (scale (/ (+ 1 (unbox time-ticks)) 50) CLEMENTS)
              600 360
              (place-image
               (scale (/ (+ 1 (unbox time-ticks)) 40) CLEMENTS)
               600 360
               (place-image
                (scale (/ (+ 1 (unbox time-ticks)) 30) CLEMENTS)
                600 360
                (place-image
                 (scale (/ (+ 1 (unbox time-ticks)) 20) CLEMENTS)
                 600 360
                 (place-image
                  (scale (/ (+ 1 (unbox time-ticks)) 10) CLEMENTS)
                  600 360
                  (place-image
                   (scale (/ (+ 1 (unbox time-ticks)) 5) CLEMENTS)
                   600 360
                   (place-image
                    (scale (/ (+ 1 (unbox time-ticks)) 2) CLEMENTS)
                    600 360
                    (empty-scene 1200 720))))))))))))))))]))



;; CREATES VISUALS
;; world -> world
;; on tick, take the sample of a song 
;; and use it create the radius of our visuals
(define (tock w)
  (begin
    (if (= (unbox time-ticks) 255)(set-box! time-ticks 0)(set-box! time-ticks (add1 (unbox time-ticks))))
    (if (unbox end-song?)
        (if (unbox next/prev)  ;; if true, next song called.  if false, previous song called.
            (next-song SONG-LIST SONG-NAME-LIST (world-cs w))
            (prev-song SONG-LIST SONG-NAME-LIST (world-cs w)))
        0)
    (if (> (unbox ctr) 0)
        (set-box! ctr (sub1 (unbox ctr)))
        (set-box! ctr 3))
    (if (<= (- (rs-frames (world-cs w)) 735) (unbox cur-frame)) ;; end of song, tock function called every 735 frames
        (begin
          (async-channel-put events true)
          (set-box! next/prev true)
          (set-box! end-song? true)
          (set-box! cur-frame 1)
          (if (= (unbox ctr) 0)
              (make-world (add1 (world-t w)) (world-a w) (abs (/ (+ (world-c1now w) (world-c1go w)) 2)) (+ 35 (* 150 (rs-ith/left (unbox cur-song) (unbox cur-frame))))
                          (world-slide-h w) (world-drag? w)(world-scene w)(world-p w) (unbox cur-song) (unbox cur-song-name) (world-song-drag? w))
              (make-world (add1 (world-t w)) (world-a w) (abs (/ (+ (world-c1now w) (world-c1go w)) 2)) (world-c1go w)
                          (world-slide-h w) (world-drag? w) (world-scene w)(world-p w) (unbox cur-song) (unbox cur-song-name) (world-song-drag? w))))
        (if (= (unbox ctr) 0)
            (make-world (add1 (world-t w)) (world-a w) (abs (/ (+ (world-c1now w) (world-c1go w)) 2)) (+ 35 (* 150 (rs-ith/left (unbox cur-song) (unbox cur-frame))))
                        (world-slide-h w) (world-drag? w)(world-scene w)(world-p w) (unbox cur-song) (unbox cur-song-name) (world-song-drag? w))
            (make-world (add1 (world-t w)) (world-a w) (abs (/ (+ (world-c1now w) (world-c1go w)) 2)) (world-c1go w)
                        (world-slide-h w) (world-drag? w) (world-scene w)(world-p w) (unbox cur-song) (unbox cur-song-name) (world-song-drag? w))))))



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
          (set-box! next/prev false)
          (set-box! end-song? true)
          (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) (world-drag? w) (world-scene w) (world-p w)
                      (unbox cur-song) (unbox cur-song-name) (world-song-drag? w)))]
       ;; next song
       [(and (> y y-b) (< y y-t) (> x next-x-l) (< x next-x-r)(not (= (world-scene w) 0))) 
        (begin 
          (async-channel-put events true)
          (set-box! next/prev true)
          (set-box! end-song? true)
          (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) (world-drag? w) (world-scene w) (world-p w) (unbox cur-song) (unbox cur-song-name)
                      (world-song-drag? w)))]
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
       ;; Mute
       [(and (> y Volume-Icon-Y1) (< y Volume-Icon-Y2) (> x Volume-Icon-X1) (< x Volume-Icon-X2)(not (= (world-scene w) 0)))
        (begin 
          (set-box! volume-song 0)
          (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (- 910 250) (world-drag? w) (world-scene w) 
                      (world-p w) (world-cs w)
                      (world-cs-name w)(world-song-drag? w)))]
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
                 (world-cs-name w) false)] 
    [(mouse=? event "drag")
     (cond
       [(world-drag? w)
        (cond
          ;; Changes Volume
          [(and (> x (- 910 250)) (< x (+ 890 250)) (not (= (world-scene w) 0)))
           (begin 
             (set-box! volume-song (/ (- (world-slide-h w) 660) 480))
             (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) x true (world-scene w) 
                         (world-p w) (world-cs w)
                         (world-cs-name w)(world-song-drag? w)))]
          [else 
           (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) true (world-scene w)(world-p w) (world-cs w)
                       (world-cs-name w)(world-song-drag? w))])]
       ;; CHanges the position of the song
       [(world-song-drag? w)
        (cond
          [(and (> x 100) (< x 1100))
           (begin
             (set-box! new-frame (round (* (rs-frames (unbox cur-song)) (/ (- x 100) 1000))))
             (async-channel-put song-events true)
             (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) (world-drag? w) (world-scene w) 
                         (world-p w) (world-cs w)
                         (world-cs-name w) true))]
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
          [else w])])]
    [else w]))

(big-bang INITIAL-WORLD 
          [on-tick tock 1/60]
          [to-draw draw]
          [on-mouse mouse-event])