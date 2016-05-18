#lang racket
(require "chess_model.rkt")
(provide get_pieces)
(provide get_board)
(provide user_pos_selected?)
(provide this_user_pos_selected?)
(provide user_select_pos)

(define game_board (new board_pieces%))
(define (get_board) game_board)
(define (get_pieces) (send game_board get_pieces))
(define user_selected_pos #f)
(define (user_pos_selected?) user_selected_pos)
(define (this_user_pos_selected? pos) (equal? pos user_selected_pos))

(define (try_move old_pos new_pos)
 (when (and 
      (send game_board has_piece? user_selected_pos)
      (not (equal? user_selected_pos new_pos))
      (send game_board is_legal_move? old_pos new_pos))
  (begin
   (send game_board move_and_update! user_selected_pos new_pos)))
   (set! user_selected_pos #f))

(define (user_select_pos pos) 
 (if (not (equal? user_selected_pos #f))
  (try_move user_selected_pos pos)
   (set! user_selected_pos pos)))
