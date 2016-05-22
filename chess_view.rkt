#lang racket/gui
(require "chess_game.rkt")

(define status_text_height 30)
(define canvas_width 600)
(define canvas_height canvas_width)
(define cell_width (/ canvas_width 8))
(define cell_height (/ canvas_height 8))

(define main_frame (new frame% 
               [label "Chess"]
               [width canvas_width]
               [height (+ canvas_height status_text_height)]
               [style '(no-resize-border)]))
(define brush_black (new brush% [color (make-object color% 100 100 100)]))
(define brush_white (new brush% [color (make-object color% 255 255 255)]))

(define brush_ai_selected1 (new brush% [color (make-object color% 255 149 101)]))
(define brush_ai_selected2 (new brush% [color (make-object color% 255 99 71)]))

(define brush_user_selected1 (new brush% [color (make-object color% 190 119 220)]))
(define brush_user_selected2 (new brush% [color (make-object color% 200 169 220)]))

(define (bmp_from_file file_name) 
 (let ([bmp (make-object bitmap% (quotient canvas_width 8) (quotient canvas_height 8) #f #t)]
       [in_port (open-input-file file_name)])
  (send bmp load-file in_port 'png/alpha)
  (close-input-port in_port)
  bmp))

(define white_pawn_bmp (bmp_from_file  "images/white_pawn.png"))
(define black_pawn_bmp (bmp_from_file  "images/black_pawn.png"))
(define white_bishop_bmp (bmp_from_file  "images/white_bishop.png"))
(define black_bishop_bmp (bmp_from_file "images/black_bishop.png"))
(define white_rook_bmp (bmp_from_file  "images/white_rook.png"))
(define black_rook_bmp (bmp_from_file  "images/black_rook.png"))
(define white_king_bmp (bmp_from_file  "images/white_king.png"))
(define black_king_bmp (bmp_from_file  "images/black_king.png"))
(define white_queen_bmp (bmp_from_file  "images/white_queen.png"))
(define black_queen_bmp (bmp_from_file  "images/black_queen.png"))
(define white_knight_bmp (bmp_from_file  "images/white_knight.png"))
(define black_knight_bmp (bmp_from_file  "images/black_knight.png"))

(define (get_piece_bmp position)
 (let* ([piece (send (get_board) get_piece position)]
        [piece_type (send piece get_piece_type)]
        [piece_team (get-field piece_team piece)])
 (match (list piece_team piece_type)
  [`(white pawn) white_pawn_bmp]
  [`(black pawn) black_pawn_bmp]
  [`(white bishop) white_bishop_bmp]
  [`(black bishop) black_bishop_bmp]
  [`(white rook) white_rook_bmp]
  [`(black rook) black_rook_bmp]
  [`(white king) white_king_bmp]
  [`(black king) black_king_bmp]
  [`(white queen) white_queen_bmp]
  [`(black queen) black_queen_bmp]
  [`(white knight) white_knight_bmp]
  [`(black knight) black_knight_bmp])))

(define (canvasdc canvas dc) 
     (let draw_cells ([x 0]
                      [y 0]
                      [cell_num 0])
         (if (equal? (modulo (+ x y) 2) 0)
             (send dc set-brush brush_white)
             (send dc set-brush brush_black))

        (match (ai_selected_num (list x y))
         [1 (send dc set-brush brush_ai_selected1)]
         [2 (send dc set-brush brush_ai_selected2)]
         [_ #f])

        (match (this_user_pos_selected_num (list x y))
         [1 (send dc set-brush brush_user_selected1)]
         [2 (send dc set-brush brush_user_selected2)]
         [_ #f])

        (send dc draw-rectangle 
         (* cell_width x) 
         (* cell_height y) 
         cell_width 
         cell_height)
        (when (send (get_board) has_piece? (list x y))
         (send dc draw-bitmap (get_piece_bmp (list x y)) (* cell_width x) (* cell_height y) 'opaque))
        (if (and [>= (+ x 1) 8] [>= (+ y 1) 8])
             0
             (draw_cells (modulo (+ cell_num 1) 8) 
                         (quotient (+ cell_num 1) 8) 
                         (+ cell_num 1)))))

(define main_canvas%
 (class canvas%
  (init new_user_click_callback)
  (super-new [parent main_frame]
             [paint-callback canvasdc])
  (define user_click_callback new_user_click_callback)
  (define/override (on-event event) 
   (when (and (is-a? event mouse-event%) (send event get-left-down))
    (user_click_callback (list 
     (quotient (send event get-x) cell_width)
     (quotient (send event get-y) cell_height)))))))

(define status_text (new text-field%
                     [parent main_frame]
                     [label ""]
                     [min-height status_text_height]
                     [enabled #f]
                     [init-value ""]))

(define (on_ai_move_callback)
 (send main_canvas on-paint)
 (send status_text set-field-background (make-object color% 255 255 255))
 (send status_text set-value "")
 (displayln "on_ai_move_callback"))

(define (on_ai_check_callback)
 (displayln "on_ai_check_callback")
 (send status_text set-field-background (make-object color% 100 255 100))
 (send status_text set-value "Computer in CHECK!"))

(define (on_ai_check_mate_callback)
 (displayln "on_ai_check_mate_callback")
 (send status_text set-field-background (make-object color% 0 200 0))
 (send status_text set-value "COMPUTER IN CHECK MATE! YOU WON!"))

(define (on_user_check_callback)
 (send status_text set-field-background (make-object color% 255 100 100))
 (send status_text set-value "You're in CHECK!")
 (displayln "on_user_check_callback"))

(define (on_user_check_mate_callback)
 (send status_text set-field-background (make-object color% 200 0 0))
 (send status_text set-value "CHECK MATE! YOU LOSE!")
 (displayln "on_user_check_mate_callback"))

(define (on_user_select_callback)
 (send main_canvas on-paint)
 (displayln "on_user_select_callback"))

(define (on_stalemate_callback)
 (displayln "on_stalemate_callback")
 (send status_text set-field-background (make-object color% 200 0 0))
 (send status_text set-value "STALEMATE!"))

(register_callbacks
 (list
     `[on_ai_move ,on_ai_move_callback]
     `[on_ai_check ,on_ai_check_callback]
     `[on_ai_check_mate ,on_ai_check_mate_callback]
     `[on_user_check ,on_user_check_callback]
     `[on_user_check_mate ,on_user_check_mate_callback]
     `[on_user_move ,(lambda _ #f)]
     `[on_user_select ,on_user_select_callback]
     `[on_stalemate ,on_stalemate_callback]))

(send main_frame show #t)

(define main_canvas (new main_canvas% [new_user_click_callback user_select_pos]))
(pretty-write (get_pieces))
