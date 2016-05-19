#lang racket
(provide chess_agent%)

(define chess_agent%
 (class object%
  (super-new)
  (init team)
  (field [agent_team team])

  (define (piece_score_heuristic pieces team)
   (send pieces get_team_piece_score team))

  ;get_all_legal_moves returns (list (list old_pos1 (list new_pos1 ...)) ...)
  (define (get_all_legal_moves pieces)
    (map  
     (lambda (position)
      (list position (send pieces get_legal_moves position)))
     (send pieces get_all_piece_positions)))
  ))
