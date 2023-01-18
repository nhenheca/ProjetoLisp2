;;; ####################################################################################################################################################################################################################################
;;; ALFA BETA ##########################################################################################################################################################################################################################
;;; ####################################################################################################################################################################################################################################

(defun alfa-beta(depth operadores &optional (node (no-teste)) (alfa (- 0 999)) (beta 999) (player 0))
 (cond
  ((eq depth 0) (no-custo node))
  ((eq player 0) (progn (setq value (- 0 999)) (alfa-beta-max (sucessores node operadores 2) depth operadores alfa beta)))
  ((eq player 1) (progn (setq value 999) (alfa-beta-min (sucessores node operadores 2) depth operadores alfa beta)))
 )
)

(defun alfa-beta-max (childs depth operadores alfa beta)
 (cond
  ((null childs) value)
  (t (progn (setq value (max value (alfa-beta (- depth 1) operadores (car childs) alfa beta 1))) 
   (cond
    ((> value beta) value)
    (t (progn (setq alfa (max alfa value)) (alfa-beta-max (cdr childs) depth operadores alfa beta)))
   ))
  )
 )
)

(defun alfa-beta-min (childs depth operadores alfa beta)
 (cond
  ((null childs) value)
  (t (progn (setq value (min value (alfa-beta (- depth 1) operadores (car childs) alfa beta 0))) 
   (cond
    ((< value alfa) value)
    (t (progn (setq beta (min beta value)) (alfa-beta-min (cdr childs) depth operadores alfa beta)))
   ))
  )
 )
)