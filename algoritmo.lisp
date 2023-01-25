;;; ####################################################################################################################################################################################################################################
;;; ALFA BETA ##########################################################################################################################################################################################################################
;;; ####################################################################################################################################################################################################################################


(defun alfa-beta (player &optional (node (no-teste)) (alfa (- 0 999)) (beta 999) (starttime (get-universal-time)))
 (cond
  ((> (- (get-universal-time) starttime) *timelimit*) (alfa-beta-eval node))
  ((or (null (sucessores node (operadores) player)) (eq (no-profundidade node) (get-d))) (alfa-beta-eval node))
  (t
   (cond
    ((evenp (no-profundidade node)) (alfa-beta-max player (ordenar-sucessores (sucessores node (operadores) player)) alfa beta))
    ((oddp (no-profundidade node)) (alfa-beta-min player (ordenar-sucessores (sucessores node (operadores) (troca-jogador player))) alfa beta))
   )
  )
 )
)

(defun alfa-beta-max (player sucessores alfa beta)
  (cond 
   ((null sucessores) alfa)
   (t (let* ((valor (alfa-beta player (car sucessores) alfa beta starttime))(novo-alfa (max alfa valor)))
        (if (>= novo-alfa beta)(progn beta)
        (progn (setf *jogada* (car sucessores)) 
            (max novo-alfa (alfa-beta-max player (cdr sucessores) novo-alfa beta))))))))

(defun alfa-beta-min (player sucessores alfa beta)
  (cond 
   ((null sucessores) beta)
   (t (let* ((valor (alfa-beta player (car sucessores) alfa beta starttime))(novo-beta (min beta valor)))
        (if (<= novo-beta alfa)(progn alfa)
        (progn (setf *jogada* (car sucessores)) 
            (min novo-beta (alfa-beta-min player (cdr sucessores) alfa novo-beta))))))))

;;;############################################################################################################################ TENTATIVA

;(defun alfa-beta-max (player sucessores alfa beta)(let ((alfa (max alfa (alfa-beta player (car sucessores) alfa beta))))(cond((>= alfa beta) beta)((eq 1 (length sucessores)) (progn (setq *jogada* (car sucessores)) alfa))(t (progn (setq *jogada* (car sucessores)) (alfa-beta-max1 player (cdr sucessores) alfa beta))))))

;(defun alfa-beta-min1 (player sucessores alfa beta)(let ((beta (min beta (alfa-beta player (car sucessores) alfa beta))))(cond((<= beta alfa) alfa)((eq 1 (length sucessores)) (progn (setq *jogada* (car sucessores)) beta))(t (progn (setq *jogada* (car sucessores)) (alfa-beta-min1 player (cdr sucessores) alfa beta))))))

;;;############################################################################################################################

;(defun alfa-beta-max3 (player sucessores alfa beta)(let ((novoalfa (max alfa (alfa-beta player (car sucessores) alfa beta))))(cond((null sucessores) (progn (setq *jogada* (car sucessores)) novoalfa))((>= novoalfa beta) beta) (t (progn (setq *jogada* (car sucessores)) (alfa-beta-max3 player (cdr sucessores) novoalfa beta))))))

;(defun alfa-beta-min3 (player sucessores alfa beta)(let ((novobeta (min beta (alfa-beta player (car sucessores) alfa beta))))(cond((null sucessores) (progn (setq *jogada* (car sucessores)) novobeta))((<= novobeta alfa) alfa)(t (progn (setq *jogada* (car sucessores)) (alfa-beta-min3 player (cdr sucessores) alfa novobeta))))))

;;;############################################################################################################################ TENTATIVA
