;;; ####################################################################################################################################################################################################################################
;;; ALFA BETA ##########################################################################################################################################################################################################################
;;; ####################################################################################################################################################################################################################################


(defun alfa-beta (player &optional (node (no-teste)) (alfa (- 0 999)) (beta 999) (flag 0))
 (cond
  ((eq (no-profundidade node) (get-d)) (alfa-beta-eval node))
  (t
   (cond
    ((eq 0 flag) (alfa-beta-max player (ordenar-sucessores (sucessores node (operadores) player)) alfa beta))
    ((eq 1 flag) (alfa-beta-min player (ordenar-sucessores (sucessores node (operadores) (troca-jogador player))) alfa beta))
   )
  )
 )
)

(defun alfa-beta-max1 (player sucessores alfa beta)
 (let ((alfa (max alfa (alfa-beta player (car sucessores) alfa beta 1))))
  (cond
   ((>= alfa beta) beta)
   ((eq 1 (length sucessores)) (progn (setq *jogada* (car sucessores)) alfa))
   (t (progn (setq *jogada* (car sucessores)) (alfa-beta-max player (cdr sucessores) alfa beta)))
  )
 )
)

(defun alfa-beta-min1 (player sucessores alfa beta)
 (let ((beta (min beta (alfa-beta player (car sucessores) alfa beta 0))))
  (cond
   ((<= beta alfa) alfa)
   ((eq 1 (length sucessores)) (progn (setq *jogada* (car sucessores)) beta))
   (t (progn (setq *jogada* (car sucessores)) (alfa-beta-min player (cdr sucessores) alfa beta)))
  )
 )
)

;;;############################################################################################################################
(defun alfa-beta-max (player sucessores alfa beta)
  (cond 
   ((null sucessores) alfa)
   (t (let* ((valor (alfa-beta player (car sucessores) alfa beta 1))(novo-alfa (max alfa valor)))
        (if (>= novo-alfa beta)(progn beta)
        (progn (setf *jogada* (car sucessores)) 
            (max novo-alfa (alfa-beta-max player (cdr sucessores) novo-alfa beta))))))))

(defun alfa-beta-min (player sucessores alfa beta)
  (cond 
   ((null sucessores) beta)
   (t (let* ((valor (alfa-beta player (car sucessores) alfa beta 0))(novo-beta (min beta valor)))
        (if (<= novo-beta alfa)(progn alfa)
        (progn (setf *jogada* (car sucessores)) 
            (min novo-beta (alfa-beta-min player (cdr sucessores) alfa novo-beta))))))))

;;;############################################################################################################################


(defun alfa-beta-max1 (player sucessores alfa beta)
 (let ((novoalfa (max alfa (alfa-beta player (car sucessores) alfa beta 1))))
  (cond
   ((null sucessores) (progn (setq *jogada* (car sucessores)) novoalfa))
   ((>= novoalfa beta) beta) 
   (t (progn (setq *jogada* (car sucessores)) (alfa-beta-max player (cdr sucessores) novoalfa beta)))
  )
 )
)

(defun alfa-beta-min1 (player sucessores alfa beta)
 (let ((novobeta (min beta (alfa-beta player (car sucessores) alfa beta 0))))
  (cond
   ((null sucessores) (progn (setq *jogada* (car sucessores)) novobeta))
   ((<= novobeta alfa) alfa)
   (t (progn (setq *jogada* (car sucessores)) (alfa-beta-min player (cdr sucessores) alfa novobeta)))
  )
 )
)

;;;############################################################################################################################