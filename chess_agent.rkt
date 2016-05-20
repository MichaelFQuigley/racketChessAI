#lang racket
(provide chess_agent%)

(define chess_agent%
 (class object%
  (super-new)
  (init team)
  (field [agent_team team])
  (define minimax_depth 2)

  (define (piece_score_heuristic pieces team)
   (+ (send pieces get_team_piece_score team) (- 1042 (send pieces get_team_piece_score (opposite_team)))))

  (define (opposite_team) (if (equal? (get-field agent_team this) 'black) 'white 'black))
  ;get_all_legal_moves returns (list (list old_pos1 (list new_pos1 ...)) ...)
  (define (get_all_legal_moves pieces team)
    (map  
     (lambda (position)
      (list position (send pieces get_legal_moves position)))
     (filter (lambda (pos) (equal? (get-field piece_team (send pieces get_piece pos)) team))
      (send pieces get_all_piece_positions))))

  (define (max_arg0 listA listB)
   (if (> (car listA) (car listB))
    listA
    listB))

  (define (min_arg0 listA listB)
   (if (< (car listA) (car listB))
    listA
    listB))

  (define (max_helper pieces curr_depth alpha beta prev_pos next_pos)
    (if (equal? curr_depth minimax_depth)
     (list (piece_score_heuristic pieces (get-field agent_team this)) prev_pos next_pos)
     (let orig_pos_iter ([children (get_all_legal_moves pieces (get-field agent_team this))]
                         [v (list -1000000 prev_pos next_pos)]
                         [outer_alpha alpha])
      (if (empty? children)
       v
       ;(list (car v) prev_pos next_pos)
      (let ([children_tup (car children)])
      (if (empty? children_tup)
       v
       ;(list (car v) prev_pos next_pos)
      (let new_pos_iter ([curr_pos      (car children_tup)]
                         [new_pos_list  (cadr children_tup)]
                         [inner_alpha   outer_alpha]
                         [temp_v v])
        ;(list (car temp_v) prev_pos next_pos)
       (if [empty? new_pos_list]
        (orig_pos_iter (cdr children) temp_v inner_alpha)
        (let* ([min_helper_res (min_helper 
                           (send pieces move_and_get_update curr_pos (car new_pos_list))
                           ;(if (equal? curr_team 'white) 'black 'white)
                           curr_depth
                           inner_alpha
                           beta
                           curr_pos
                           (car new_pos_list))]
               [new_temp_v (max_arg0 temp_v (if (equal? curr_depth 0) (list (car min_helper_res) curr_pos (car new_pos_list)) min_helper_res))]
               [new_inner_alpha (max (car new_temp_v) inner_alpha)])
         (if (<= beta new_inner_alpha)
          new_temp_v
         (new_pos_iter curr_pos (cdr new_pos_list) new_inner_alpha new_temp_v)))))))))))

  (define (min_helper pieces curr_depth alpha beta prev_pos next_pos)
    (if (equal? curr_depth minimax_depth)
     (list (piece_score_heuristic pieces (get-field agent_team this))  prev_pos next_pos)
     (let orig_pos_iter ([children (get_all_legal_moves pieces (opposite_team))]
                         [v (list 1000000  prev_pos next_pos)]
                         [outer_beta beta])
      (if (empty? children)
       v
       ;(list (car v) prev_pos next_pos)
      (let ([children_tup (car children)])
      (if (empty? children_tup)
       v
       ;(list (car v) prev_pos next_pos)
      (let new_pos_iter ([curr_pos      (car children_tup)]
                         [new_pos_list  (cadr children_tup)]
                         [inner_beta   outer_beta]
                         [temp_v v])
        (list (car temp_v) prev_pos next_pos)
       (if (empty? new_pos_list)
        (orig_pos_iter (cdr children) temp_v inner_beta)
        (let* ([new_temp_v (min_arg0 temp_v (max_helper 
                           (send pieces move_and_get_update curr_pos (car new_pos_list))
                           ;(if (equal? curr_team 'white) 'black 'white)
                           (add1 curr_depth)
                           alpha
                           inner_beta
                           curr_pos
                           (car new_pos_list)))]
               [new_inner_beta (min (car new_temp_v) inner_beta)])
         (if (<= new_inner_beta alpha)
          new_temp_v
         (new_pos_iter curr_pos (cdr new_pos_list) new_inner_beta new_temp_v)))))))))))

  (define/public (minimax pieces)
    (max_helper pieces 0 -1000000 1000000 `(0 0) `(0 0)))))
