(defun jogada-humano (node player)
 (progn (imprimir-tabuleiro node)
 (cond 
  ((tabuleiro-preenchidop node) nil)
  (t (get-horizontal-vertical node player))
 ))
)

(defun get-horizontal-vertical (node player)
 (format t "Inserir peÃ§a na: ~%")
 (format t "1 - Horizontal~%")
 (format t "2 - Vertical~%")
 (let ((op (read)))
  (cond
   ((eq 1 op) (inserir-arco-horizontal node player))
   ((eq 2 op) (inserir-arco-vertical node player))
   (t nil)
  )
 )
)

(defun inserir-arco-horizontal (node player)
 (format t "Digite a 'linha' e de seguida a 'coluna' pretendida. ~%")
 (let ((tabuleiro (arco-horizontal node (read) (read) (get-arcos-verticais node) 1)))
  (cond
   ((null tabuleiro) (jogada-humano node player))
   (t (progn (imprimir-tabuleiro (list tabuleiro 0 0 0))
   (jogada-computador (list tabuleiro (+ 1 (no-profundidade node)) (heuristica (list tabuleiro 0 0 0)) (no-pai node)) player)))
  )
 )
)

(defun inserir-arco-vertical (node player)
 (format t "Digite a 'coluna' e de seguida a 'linha' pretendida. ~%")
 (let ((tabuleiro (arco-vertical node (read) (read) (get-arcos-horizontais node) 1)))
  (cond
   ((null tabuleiro) (jogada-humano node player))
   (t (progn (imprimir-tabuleiro (list tabuleiro 0 0 0))
   (jogada-computador (list tabuleiro (+ 1 (no-profundidade node)) (heuristica (list tabuleiro 0 0 0)) (no-pai node)) player)))
  )
 )
)

(defun jogada-computador (node player)
 (cond 
  ((tabuleiro-preenchidop node) nil)
  (t (jogada-humano (alfa-beta player node) player))
 )
)

;############################################################################

(defun start1 ()
 (progn (format t "1 - Jogador 1 Humano | 2 - Jogador 1 Computador. ~%")
 (let ((op (read)))
  (cond
   ((eq op 1) (jogada-humano (no-teste) 2))
   ((eq op 2) (jogada-computador (no-teste) 1))
   (t (start1))
  )
 ))
)


(defun start2 ()
 ;cpu vs cpu
)

;############################################################################ ESCREVER TABULEIRO

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
  ((not (eq (car lista) 0)) (progn (format t ".___") (imprimir-linha-coluna-op1 (cdr lista))))
 )
)

(defun imprimir-linha-coluna-op2 (lista)
 (cond
  ((null lista)  (format t " ~%"))
  ((eq (car lista) 0) (progn (format t "    ") (imprimir-linha-coluna-op2 (cdr lista))))
  ((not (eq (car lista) 0)) (progn (format t "|   ") (imprimir-linha-coluna-op2 (cdr lista))))
 )
)

;############################################################################ ESCREVER TABULEIRO