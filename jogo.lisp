(defun jogada-humano (node)
 (get-horizontal-vertical node)
)

(defun get-horizontal-vertical (node)
 (format t "Inserir peça na: ~%")
 (format t "1 - Horizontal~%")
 (format t "2 - Vertical~%")
 (let ((op (read)))
  (cond
   ((eq 1 op) (inserir-arco-horizontal node))
   ((eq 2 op) (inserir-arco-vertical node))
   (t nil)
  )
 )
)

(defun inserir-arco-horizontal (node)
 (format t "Digite a 'linha' e de seguida a 'coluna' pretendida. ~%")
 (jogada-computador (list (arco-horizontal node (read) (read) (get-arcos-verticais node) 1) (no-profundidade node) 0 (no-pai node)))
)

(defun inserir-arco-vertical (node)
 (format t "Digite a 'coluna' e de seguida a 'linha' pretendida. ~%")
 (jogada-computador (list (arco-vertical node (read) (read) (get-arcos-horizontais node) 1) (no-profundidade node) 0 (no-pai node)))
)

(defun jogada-computador (node)
 (jogada-humano (alfa-beta depth (operadores) node)) ;trocar para o no
)

;############################################################################

(defun imprimir-tabuleiro (node &optional (flag 0) (aux 0))
 (cond
  ((equal 0 flag) (progn (imprimir-linha-coluna-op1 (caar (no-estado node))) (imprimir-tabuleiro (list (list (cdr (car (no-estado node))) (car (cdr (no-estado node))))) 1 aux)))
  ((and (< aux 5) (equal 1 flag)) (progn (imprimir-linha-coluna-op2 (mapcar #'(lambda (x) (nth aux x)) (car (cdr (no-estado node))))) (imprimir-tabuleiro node 2 (+ 1 aux))))
  ((equal 2 flag) (imprimir-tabuleiro node 0 aux))
 )
)


(defun imprimir-linha-coluna-op1 (lista)
 (cond
  ((null lista)  (format t ".~%"))
  ((eq (car lista) 0) (progn (format t ".   ") (imprimir-linha-coluna-op1 (cdr lista))))
  ((not (eq (car lista) 0)) (progn (format t ".―――") (imprimir-linha-coluna-op1 (cdr lista))))
 )
)

(defun imprimir-linha-coluna-op2 (lista)
 (cond
  ((null lista)  (format t " ~%"))
  ((eq (car lista) 0) (progn (format t "    ") (imprimir-linha-coluna-op2 (cdr lista))))
  ((not (eq (car lista) 0)) (progn (format t "|   ") (imprimir-linha-coluna-op2 (cdr lista))))
 )
)