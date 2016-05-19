#lang racket
(require "chess_model.rkt")
(provide get_pieces)
(provide get_board)
(provide user_pos_selected?)
(provide this_user_pos_selected?)
(provide user_select_pos)

(define game_board (new board_pieces%))
(define player_team 'white)
(define ai_team 'black)
(define team_turn 'white)
(define (get_board) game_board)
(define (get_pieces) (send game_board get_pieces))
(define user_selected_pos #f)
(define (user_pos_selected?) user_selected_pos)
(define (this_user_pos_selected? pos) (equal? pos user_selected_pos))

;try_move: returns true if move was successfully made
(define (try_move old_pos new_pos)
 (if [and 
      (send game_board has_piece? old_pos)
      (not (equal? old_pos new_pos))
      (send game_board is_legal_move? old_pos new_pos)]
   (let ([new_board (send game_board move_and_get_update old_pos new_pos)])
    (if [not (send new_board in_check? team_turn)]
     (begin (set! game_board new_board) #t)
     #f))
    #f))

(define (user_select_pos pos) 
 (if [not (equal? user_selected_pos #f)]
  (begin
   (when [try_move user_selected_pos pos] 
    (set! team_turn ai_team))
   (set! user_selected_pos #f))
  (set! user_selected_pos pos)))
