(defvar *jogada* nil)

;;;################################ iNTERACAO hUMANO-cOMPUTADOR ##############################################################

(defun jogada-humano (node pecaJ pecaC &optional (jp 0) (cp 0))
 (progn (imprimir-tabuleiro node)
 (cond 
  ((tabuleiro-preenchidop node) nil)
  (t (get-horizontal-vertical node pecaJ pecaC))
 ))
)

(defun get-horizontal-vertical (node pecaJ pecaC)
 (format t "Inserir peÃ§a na: ~%")
 (format t "1 - Horizontal~%")
 (format t "2 - Vertical~%")
 (let ((op (read)))
  (cond
   ((eq 1 op) (inserir-arco-horizontal node pecaJ pecaC))
   ((eq 2 op) (inserir-arco-vertical node pecaJ pecaC))
   (t nil)
  )
 )
)

(defun inserir-arco-horizontal (node pecaJ pecaC)
 (format t "Digite a 'linha' e de seguida a 'coluna' pretendida. ~%")
 (let ((tabuleiro (arco-horizontal node (read) (read) (get-arcos-verticais node) pecaJ)))
  (cond
   ((null tabuleiro) (jogada-humano node pecaJ pecaC))
   (t (progn (imprimir-tabuleiro (list tabuleiro 0 0 0))(format t "------------------------- ~%")(format t "------------------------- ~%")
   (jogada-computador (list tabuleiro 0 (heuristica (list tabuleiro 0 0 0)) (no-pai node)) pecaJ pecaC)))
  )
 )
)

(defun inserir-arco-vertical (node pecaJ pecaC)
 (format t "Digite a 'coluna' e de seguida a 'linha' pretendida. ~%")
 (let ((tabuleiro (arco-vertical node (read) (read) (get-arcos-horizontais node) pecaJ)))
  (cond
   ((null tabuleiro) (jogada-humano node pecaJ pecaC))
   (t (progn (imprimir-tabuleiro (list tabuleiro 0 0 0))(format t "------------------------- ~%")(format t "------------------------- ~%")
   (jogada-computador (list tabuleiro 0 (heuristica (list tabuleiro 0 0 0)) (no-pai node)) pecaJ pecaC)))
  )
 )
)

(defun jogada-computador (node pecaJ pecaC)
 (cond 
  ((tabuleiro-preenchidop node) nil)
  (t (progn (alfa-beta pecaC node) (jogada-humano *jogada* pecaJ pecaC)))
 )
)

;;;################################ iNTERACAO cOMPUTADOR-cOMPUTADOR ##############################################################

(defun jogada-computador1 (node &optional (c1p 0) (c2p 0))
(progn (imprimir-tabuleiro node)
 (cond 
  ((tabuleiro-preenchidop node) nil)
  (t (alfa-beta 1 node) (jogada-computador2 (list (car *jogada*) 0 (heuristica (list (car *jogada*) 0 0 0)) (no-pai (car *jogada*)))))
 ))
)

(defun jogada-computador2 (node &optional (c1p 0) (c2p 0))
(progn (imprimir-tabuleiro node)
 (cond 
  ((tabuleiro-preenchidop node) nil)
  (t (alfa-beta 2 node) (jogada-computador1 (list (car *jogada*) 0 (heuristica (list (car *jogada*) 0 0 0)) (no-pai (car *jogada*)))))
 ))
)

;;;######################################################################################## MENUS

(defun start ()
 (progn 
 (format t "|----------------------------------------------| ~%")
 (format t "|                                              | ~%")
 (format t "|          Selecione o modo de Jogo            | ~%")     
 (format t "|                                              | ~%")
 (format t "|           1 - Humano-Computador              | ~%")
 (format t "|         2 - Computador-Computador            | ~%")
 (format t "|                                              | ~%")
 (format t "|----------------------------------------------| ~%")
 (let ((op (read)))
  (cond
   ((eq op 1) (start1))
   ((eq op 2) (start2))
   (t (start1))
  )
 ))
)

(defun start1 ()
 (progn 
 (format t "|----------------------------------------------| ~%")
 (format t "|                                              | ~%")
 (format t "|        Selecione o primeiro Jogador          | ~%")     
 (format t "|                                              | ~%")
 (format t "|                 1 - Humano                   | ~%")
 (format t "|               2 - Computador                 | ~%")
 (format t "|                                              | ~%")
 (format t "|----------------------------------------------| ~%")
 (let ((op (read)))
  (cond
   ((eq op 1) (jogada-humano (no-teste) 1 2))
   ((eq op 2) (jogada-computador (no-teste) 2 1))
   (t (start1))
  )
 ))
)


(defun start2 ()
 (jogada-computador1 (no-teste) 1 2)
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
