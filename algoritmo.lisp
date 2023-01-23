;;; ####################################################################################################################################################################################################################################
;;; ALFA BETA ##########################################################################################################################################################################################################################
;;; ####################################################################################################################################################################################################################################


(defun alfa-beta (player &optional (node (no-teste)) (alfa (- 0 999)) (beta 999) (flag 0))
 (cond
  ((eq (no-profundidade node) (get-d)) (alfa-beta-eval node))
  (t
   (cond
    ((eq 0 flag) (alfa-beta-max player (ordenar-sucessores (sucessores node (operadores) player)) alfa beta))
    ((eq 1 flag) (alfa-beta-max player (ordenar-sucessores (sucessores node (operadores) (troca-jogador player))) alfa beta))
   )
  )
 )
)

(defun alfa-beta-max (player sucessores alfa beta)
 (let ((alfa (max alfa (alfa-beta player (car sucessores) alfa beta 1))))
  (cond
   ((>= alfa beta) beta)
   ((eq 1 (length sucessores)) (progn (setq *jogada* (car sucessores)) alfa))
   (t (progn (setq *jogada* (car sucessores)) (alfa-beta-max player (cdr sucessores) alfa beta)))
  )
 )
)

(defun alfa-beta-min (player sucessores alfa beta)
 (let ((beta (min beta (alfa-beta player (car sucessores) alfa beta 0))))
  (cond
   ((<= beta alfa) alfa)
   ((eq 1 (length sucessores)) (progn (setq *jogada* (car sucessores)) beta))
   (t (progn (setq *jogada* (car sucessores)) (alfa-beta-min player (cdr sucessores) alfa beta)))
  )
 )
)