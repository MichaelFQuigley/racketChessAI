#lang racket
(provide board_pieces%)

(define board_pieces%
 (class object% 
  (super-new)
  (init [new_pieces (make-immutable-hash
                  (append (list
                   `((0 0) ,(new rook_piece%   [team 'black]))
                   `((1 0) ,(new knight_piece% [team 'black]))
                   `((2 0) ,(new bishop_piece% [team 'black]))
                   `((3 0) ,(new queen_piece%  [team 'black]))
                   `((4 0) ,(new king_piece%   [team 'black]))
                   `((5 0) ,(new bishop_piece% [team 'black]))
                   `((6 0) ,(new knight_piece% [team 'black]))
                   `((7 0) ,(new rook_piece%   [team 'black]))
                   `((0 7) ,(new rook_piece%   [team 'white]))
                   `((1 7) ,(new knight_piece% [team 'white]))
                   `((2 7) ,(new bishop_piece% [team 'white]))
                   `((3 7) ,(new queen_piece%  [team 'white]))
                   `((4 7) ,(new king_piece%   [team 'white]))
                   `((5 7) ,(new bishop_piece% [team 'white]))
                   `((6 7) ,(new knight_piece% [team 'white]))
                   `((7 7) ,(new rook_piece%   [team 'white])))
                   (let make_pawns ([x 0] [y 1])
                      (if [and (equal? x 7) (equal? y 6)]
                      `(((,x ,y) ,(new pawn_piece% [team (if (equal? y 1) 'black 'white)])))
                      (cons 
                      `((,x ,y) ,(new pawn_piece% [team (if (equal? y 1) 'black 'white)]))
                      (make_pawns (modulo (+ x 1) 8) (if [equal? x 7] 6 y)))))))]
           [new_white_king_pos '(4 7)]
           [new_black_king_pos '(4 0)])
  (field [white_king_pos new_white_king_pos])
  (field [black_king_pos new_black_king_pos])
  (define pieces new_pieces)
  (define (element_add listA listB)
   (map (lambda (n1 n2) (+ n1 n2)) listA listB))

  (define/public (move_piece orig_pos new_pos)
   (let* ([piece (get_piece orig_pos)]
          [is_king? (equal? (send piece get_piece_type) 'king)]
          [temp_white_king_pos (if (and is_king? (equal? (get-field piece_team piece) 'white))
            new_pos
            (get-field white_king_pos this))]
          [temp_black_king_pos (if (and is_king? (equal? (get-field piece_team piece) 'black))
            new_pos
            (get-field black_king_pos this))])
   `(,(hash-remove (hash-set pieces new_pos piece) orig_pos) ,temp_white_king_pos ,temp_black_king_pos)))

  (define/public (get_team_piece_score team)
    (foldl (lambda (pos acc)
            (let ([pb (get_piece pos)])
            (cond
             [(equal? team 'both) (+ acc (send pb get_piece_value))]
             [(equal? team (get-field piece_team pb)) (+ acc (send pb get_piece_value))]
             [else acc])))
     0 
     (hash-keys pieces)))

  (define/public (move_and_get_update old_pos new_pos)
   (match (move_piece old_pos new_pos)
    [(list pc w_pos b_pos) (new board_pieces% [new_pieces pc]
                                              [new_white_king_pos w_pos]
                                              [new_black_king_pos b_pos])]))

  (define/public (get_pieces) pieces)

  (define/public (get_piece position)
    (match (hash-ref pieces position)
     [(list a) a]
     [a a]))
;in_bounds?: #t if a position is within the bounds of the board
  (define/public (in_bounds? position) (and 
                               (< (car position)   8) 
                               (>= (car position)  0) 
                               (< (cadr position)  8) 
                               (>= (cadr position) 0)))
;position -> pair?
 (define/public (has_piece? position)
  (hash-has-key? pieces position))

 (define (get_king_pos team)
  (if (equal? team 'white) white_king_pos black_king_pos))

 (define/public (would_be_in_check? team old_pos new_pos)
  (send (move_and_get_update old_pos new_pos) in_check? team))

 (define/public (in_check_mate? team)
  (if (in_check? team)
   (let* ([king_pos (get_king_pos team)]
          [legal_moves (get_king_legal_moves king_pos)]
          [filtered_legal_moves (filter 
                                  (lambda (new_pos) (not (would_be_in_check? team king_pos new_pos)))
                                 legal_moves)])
        (if [empty? filtered_legal_moves] #t #f))
   #f))

 (define/public (in_check? team)
  (define (is_threat_piece? pos test_type) 
   (if (has_piece? pos)
    (let ([piece (get_piece pos)])
     (and (not (equal? (get-field piece_team piece) team)) 
      (equal? (send piece get_piece_type) test_type)))
    #f))

  (define (threat_from_type? king_pos test_type legal_moves)
     (cond
      [(empty? legal_moves) #f]
      [(is_threat_piece? (car legal_moves) test_type) #t]
      [else (threat_from_type? king_pos test_type (cdr legal_moves))]))
  (let* ([king_pos (get_king_pos team)]
         [vert_legals (get_vert_legal_moves king_pos)]
         [diag_legals (get_diag_legal_moves king_pos)]
         [pawn_legals (get_pawn_legal_moves king_pos)]
         [king_legals (get_king_legal_moves king_pos)]
         [knight_legals (get_knight_legal_moves king_pos)])
         (or
          (threat_from_type? king_pos 'queen vert_legals)
          (threat_from_type? king_pos 'queen diag_legals)
          (threat_from_type? king_pos 'pawn pawn_legals)
          (threat_from_type? king_pos 'king king_legals)
          (threat_from_type? king_pos 'rook vert_legals)
          (threat_from_type? king_pos 'bishop diag_legals)
          (threat_from_type? king_pos 'knight knight_legals))))

 (define/public (is_legal_move? old_pos new_pos)
  (let find_move ([legal_moves (get_legal_moves old_pos)])
   (if (not (empty? legal_moves))
    (if (equal? (car legal_moves) new_pos)
     #t
     (find_move (cdr legal_moves)))
     #f)))

 (define/public (get_all_piece_positions)
  (hash-keys pieces))

 (define/public (get_legal_moves position)
  (match (send (get_piece position) get_piece_type)
   ['pawn (get_pawn_legal_moves position)]
   ['bishop (get_diag_legal_moves position)]
   ['rook (get_vert_legal_moves position)]
   ['knight (get_knight_legal_moves position)]
   ['queen (append
             (get_vert_legal_moves position)
             (get_diag_legal_moves position))]
   ['king (get_king_legal_moves position)]
   [_ (error "invalid piece type")]))

 (define (get_pawn_legal_moves position) 
  (let* ([piece (get_piece position)]
         [y_pos (if [equal? (get-field piece_team piece) 'black] 
                 (add1 (cadr position)) 
                 (sub1 (cadr position)))]
        [y_pos2 (if [equal? (get-field piece_team piece) 'black] 
                 (add1 y_pos) 
                 (sub1 y_pos))]
         [diag_pos1 (list (add1 (car position)) y_pos)]
         [diag_pos2 (list (sub1 (car position)) y_pos)])
   (if [in_bounds? (list 0 y_pos)]
    (append 
     (if [not (has_piece? (list (car position) y_pos))]
      (if [and (not (has_piece? (list (car position) y_pos2)))
            (or (and (equal? 'white (get-field piece_team piece))
                     (equal? (cadr position) 6)) 
                (and (equal? 'black (get-field piece_team piece))
                     (equal? (cadr position) 1)))]
       `((,(car position) ,y_pos) (,(car position) ,y_pos2))
      `((,(car position) ,y_pos)))
      '())
     (if [and (has_piece? diag_pos1)
               (send piece not_my_team? (get_piece diag_pos1))]
     (list diag_pos1)
     '())
     (if [and (has_piece? diag_pos2)
               (send piece not_my_team? (get_piece diag_pos2))]
      (list diag_pos2)
     '()))
    '())))

  (define (get_legal_moves_helper position mod_fn orig_piece)
   (let ([pos (mod_fn position)])
   (let make_legal_moves ([curr_pos pos])
     (if [in_bounds? curr_pos]
      (if [has_piece? curr_pos]
       (if [send orig_piece not_my_team? (get_piece curr_pos)]
         (list curr_pos)
         '())
       (cons curr_pos (make_legal_moves [mod_fn curr_pos])))
      '()))))

  (define (get_diag_legal_moves pos)
   (append
    (get_legal_moves_helper pos (lambda(pos)(element_add pos '(1 1))) (get_piece pos))
    (get_legal_moves_helper pos (lambda(pos)(element_add pos '(-1 1))) (get_piece pos))
    (get_legal_moves_helper pos (lambda(pos)(element_add pos '(1 -1))) (get_piece pos))
    (get_legal_moves_helper pos (lambda(pos)(element_add pos '(-1 -1))) (get_piece pos))))

  (define (get_vert_legal_moves pos)
   (append
    (get_legal_moves_helper pos (lambda(pos)(element_add pos '(0 1))) (get_piece pos))
    (get_legal_moves_helper pos (lambda(pos)(element_add pos '(0 -1))) (get_piece pos))
    (get_legal_moves_helper pos (lambda(pos)(element_add pos '(1 0))) (get_piece pos))
    (get_legal_moves_helper pos (lambda(pos)(element_add pos '(-1 0))) (get_piece pos))))

  (define (get_knight_legal_moves position)
       (king_knight_legal_moves_helper position `((1 2) (-1 2) (1 -2) (-1 -2) (2 1) (2 -1) (-2 1) (-2 -1))))

  (define (get_king_legal_moves position)
   (king_knight_legal_moves_helper position `((1 1) (-1 1) (1 -1) (-1 -1) (0 1) (0 -1) (1 0) (-1 0))))

  (define (king_knight_legal_moves_helper position deltas)
   (foldl (lambda (x y)
           (let ([new_pos (element_add position x)])
           (if [and (in_bounds? new_pos) 
            (or (not(has_piece? new_pos)) 
             (and (has_piece? new_pos) (send (get_piece position) not_my_team? (get_piece new_pos))))]
            (cons new_pos y)
            y))) 
    '() 
    deltas))))

(define board_piece%
 (class object% 
  (init team)
  (super-new)
  (define/public (get_piece_type) '())
  (define/public (get_piece_value) 0)
  (define/public (not_my_team? other_piece) 
   (if (equal? (get-field piece_team other_piece) piece_team) #f #t))
  (field [piece_team team])))

(define pawn_piece%
 (class board_piece% 
  (super-new)
  (inherit not_my_team?)
  (define/override (get_piece_type) 'pawn)
  (define/override (get_piece_value) 20)))

(define bishop_piece%
 (class board_piece% 
  (super-new)
  (inherit not_my_team?)
  (define/override (get_piece_type) 'bishop)
  (define/override (get_piece_value) 30)))

(define knight_piece%
 (class board_piece% 
  (super-new)
  (define/override (get_piece_type) 'knight)
  (define/override (get_piece_value) 40)))

(define rook_piece%
 (class board_piece% 
  (super-new)
  (define/override (get_piece_type) 'rook)
  (define/override (get_piece_value) 50)))

(define king_piece%
 (class board_piece% 
  (super-new)
  (define/override (get_piece_type) 'king)
  (define/override (get_piece_value) 1000)))

(define queen_piece%
 (class board_piece% 
  (super-new)
  (define/override (get_piece_type) 'queen)
  (define/override (get_piece_value) 100)))

