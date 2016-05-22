#lang racket
(require "chess_model.rkt")
(require "chess_agent.rkt")
(provide get_pieces)
(provide get_board)
(provide user_pos_selected?)
(provide this_user_pos_selected_num)
(provide ai_selected_num)
(provide user_select_pos)
(provide register_callbacks)

(define game_board (new board_pieces%))
(define player_team 'white)
(define ai_team 'black)
(define team_turn 'white)
(define chess_agent (new chess_agent% [team ai_team]))
(define (get_board) game_board)
(define (get_pieces) (send game_board get_pieces))
(define user_selected_pos '())
(define ai_move_pos '())
(define (user_pos_selected?) user_selected_pos)

(define (ai_selected_num pos)
 (cond
  [(equal? (length ai_move_pos) 0) 0]
  [(equal? (cadr ai_move_pos) pos) 1]
  [(equal? (car ai_move_pos) pos) 2]
  [else 0]))

(define (this_user_pos_selected_num pos) 
 (cond
  [(equal? (length user_selected_pos) 0) 0]
  [(equal? (length user_selected_pos) 1) 
   (if [equal? (car user_selected_pos) pos] 
     1 0)]
  [(equal? (length user_selected_pos) 2)
   (cond 
    [(equal? (car user_selected_pos) pos) 2] 
    [(equal? (cadr user_selected_pos) pos) 1] 
    [else 0])]))

(define registered_callbacks null)

;try_move: returns true if move was successfully made
(define (try_move old_pos new_pos)
 (if [and 
      (send game_board has_piece? old_pos)
      (not (equal? old_pos new_pos))
      (send game_board is_legal_move? old_pos new_pos)]
   (let ([new_board (send game_board move_and_get_update old_pos new_pos)])
    (if [not (send new_board in_check? team_turn)]
     (begin 
     (set! game_board new_board) #t)
     #f))
    #f))

(define (user_select_pos pos)
 (cond
  [(or (equal? (length user_selected_pos) 0) 
       (equal? (length user_selected_pos) 2))
    (begin
     (set! user_selected_pos (list pos))
     (try_callback 'on_user_select))]
  [(equal? (length user_selected_pos) 1)
         (let* ([did_move (try_move (car user_selected_pos) pos)])
          (if did_move 
           (begin 
            (set! team_turn ai_team)
            (set! user_selected_pos (cons pos user_selected_pos))
            (try_callback 'on_user_select)
            (try_callback 'on_user_move)
            (ai_move chess_agent game_board))
           (begin 
            (set! user_selected_pos '())
            (try_callback 'on_user_select))))]
  [else (error "too many positions in user_select_pos")]))

(define (ai_move chess_agent pieces)
 (let* ([minimax_res (send chess_agent minimax pieces)]
        [move (cdr minimax_res)])
    (displayln minimax_res)
    (set! ai_move_pos move)
    (set! game_board (send game_board move_and_get_update (car move) (cadr move)))
    (set! user_selected_pos '())
    (try_callback 'on_ai_move)
    (cond
     [(send game_board in_check_mate? player_team) (try_callback 'on_user_check_mate)]
     [(send game_board in_check?      player_team) (try_callback 'on_user_check)]
     [(send game_board in_stalemate?  player_team) (try_callback 'on_stalemate)]
     [else #f])
   (set! team_turn player_team)))

;register_callbacks
;callbacks should be of the form (list ['callback_name callback] ...)
(define (register_callbacks callbacks)
 (match callbacks
 [(list
     `[on_ai_move ,_]
     `[on_ai_check ,_]
     `[on_ai_check_mate ,_]
     `[on_user_check ,_]
     `[on_user_check_mate ,_]
     `[on_user_move ,_]
     `[on_user_select ,_]
     `[on_stalemate ,_])
        (set! registered_callbacks (make-immutable-hash callbacks))]
 [_ (error "Invalid callback format. 
     Refer to register_callbacks function for format")]))

(define (get_callback callback_name)
 (if (and 
      (not (equal? null registered_callbacks))
      (hash-has-key? registered_callbacks callback_name))
  (hash-ref registered_callbacks callback_name)
  #f))

(define (try_callback callback_name)
 (let ([callback (get_callback callback_name)])
     (if [not (equal? callback #f)]
      (begin ((car callback)) #t)
      (begin (displayln "Callback invocation failed") #f))))
