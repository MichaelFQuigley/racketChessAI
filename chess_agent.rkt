#lang racket
(provide chess_agent%)

(define chess_agent%
 (class object%
  (super-new)
  (init team)

  ;agent team is the team that the agent is on ('black or 'white)
  (field [agent_team team])

  ;minimax_depth is the ply-depth for minimax
  (define minimax_depth 2)

  ;epsilon is the probability of choosing the best action at 
  ;a certain level in the minimax tree
  ;(as opposed to choosing a suboptimal action)
  (define epsilon 0.999)

  ;max_v is the max possible value for a node in minimax
  ;this is useful for checking error conditions
  (define max_v 1000000)

  ;min_v is the min possible value for a node in minimax
  (define min_v -1000000)

  ;piece_score_heuristic is the heuristic used when minimax reaches its max search depth
  (define (piece_score_heuristic pieces team)
   (- (send pieces get_team_piece_score (get-field agent_team this)) (send pieces get_team_piece_score (opposite_team))))

  ;opposite_team returns opposing team with respect to chess_agent
  (define (opposite_team) (if (equal? (get-field agent_team this) 'black) 'white 'black))

  ;get_all_legal_moves returns (list (list old_pos1 (list new_pos1 ...)) ...)
  (define (get_all_legal_moves pieces team)
    (map  
     (lambda (position)
      (list position (send pieces get_legal_moves position)))
     (filter (lambda (pos) (equal? (get-field piece_team (send pieces get_piece pos)) team))
      (send pieces get_all_piece_positions))))

  ;flip_coin will return true with probability 'prob'
  (define (flip_coin prob)
   (let ([maxi 1000000])
   (if [>= (* prob maxi) (random maxi)]
    #t
    #f)))

  (define (max_arg0 listA listB)
   (if (and (>= (car listA) (car listB)) (flip_coin epsilon))
    listA
    listB))

  (define (min_arg0 listA listB)
   (if (and (<= (car listA) (car listB)) (flip_coin epsilon))
    listA
    listB))

  (define (max_helper pieces curr_depth alpha beta prev_pos next_pos)
    (if (equal? curr_depth minimax_depth)
     (list (piece_score_heuristic pieces (get-field agent_team this)) prev_pos next_pos)
     (let orig_pos_iter ([children (get_all_legal_moves pieces (get-field agent_team this))]
                         [v (list min_v prev_pos next_pos)]
                         [outer_alpha alpha])
      (if (empty? children)
       v
      (let ([children_tup (car children)])
      (if (empty? children_tup)
       v
      (let new_pos_iter ([curr_pos      (car children_tup)]
                         [new_pos_list  (shuffle (cadr children_tup))]
                         [inner_alpha   outer_alpha]
                         [temp_v v])
       (if [empty? new_pos_list]
        (orig_pos_iter (cdr children) temp_v inner_alpha)
        (let* ([min_helper_res (min_helper 
                           (send pieces move_and_get_update curr_pos (car new_pos_list))
                           curr_depth
                           inner_alpha
                           beta
                           prev_pos
                           next_pos)]
               [new_temp_v (max_arg0 
                             temp_v 
                             (if (equal? curr_depth 0) 
                              (list (car min_helper_res) curr_pos (car new_pos_list)) 
                              (list (car min_helper_res) prev_pos next_pos)))]
               [new_inner_alpha (max (car new_temp_v) inner_alpha)])
         (if (<= beta new_inner_alpha)
          new_temp_v
         (new_pos_iter curr_pos (cdr new_pos_list) new_inner_alpha new_temp_v)))))))))))

  (define (min_helper pieces curr_depth alpha beta prev_pos next_pos)
     (let orig_pos_iter ([children (shuffle (get_all_legal_moves pieces (opposite_team)))]
                         [v (list max_v prev_pos next_pos)]
                         [outer_beta beta])
      (if (empty? children)
       v
      (let ([children_tup (car children)])
      (if (empty? children_tup)
       v
      (let new_pos_iter ([curr_pos      (car children_tup)]
                         [new_pos_list  (cadr children_tup)]
                         [inner_beta   outer_beta]
                         [temp_v v])
       (if (empty? new_pos_list)
        (orig_pos_iter (cdr children) temp_v inner_beta)
        (let* ([new_temp_v (min_arg0 temp_v (max_helper 
                           (send pieces move_and_get_update curr_pos (car new_pos_list))
                           (add1 curr_depth)
                           alpha
                           inner_beta
                           prev_pos
                           next_pos))]
               [new_inner_beta (min (car new_temp_v) inner_beta)])
         (if (<= new_inner_beta alpha)
          new_temp_v
         (new_pos_iter curr_pos (cdr new_pos_list) new_inner_beta new_temp_v))))))))))

  (define/public (minimax pieces)
    (max_helper pieces 0 min_v max_v `(0 0) `(0 0)))))
