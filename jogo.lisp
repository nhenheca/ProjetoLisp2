(defvar *jogada* nil)
(defvar *timelimit* nil)

;;;################################ iNTERACAO hUMANO-cOMPUTADOR ##############################################################

(defun jogada-computador (node pecaJ pecaC jp cp)
 (cond
  ((> (caixas-fechadas node) (+ jp cp)) (jogada-humano node pecaJ pecaC (+ 1 jp) cp))
  (t 
   (cond 
    ((tabuleiro-preenchidop node) (list jp cp))
    (t (progn (alfa-beta pecaC node) (jogada-humano *jogada* pecaJ pecaC jp cp)))
   )
  )
 )
)

(defun jogada-humano (node pecaJ pecaC jp cp)
 (cond
  ((> (caixas-fechadas node) (+ jp cp)) (jogada-computador node pecaJ pecaC jp (+ 1 cp)))
  (t 
   (progn (imprimir-tabuleiro node)
   (cond 
    ((tabuleiro-preenchidop node) (list jp cp))
    (t (get-horizontal-vertical node pecaJ pecaC jp cp))
   ))
  )
 )
)

(defun get-horizontal-vertical (node pecaJ pecaC jp cp)
 (format t "Inserir peÃ§a na: ~%")
 (format t "1 - Horizontal~%")
 (format t "2 - Vertical~%")
 (let ((op (read)))
  (cond
   ((eq 1 op) (inserir-arco-horizontal node pecaJ pecaC jp cp))
   ((eq 2 op) (inserir-arco-vertical node pecaJ pecaC jp cp))
   (t nil)
  )
 )
)

(defun inserir-arco-horizontal (node pecaJ pecaC jp cp)
 (format t "Digite a 'linha' e de seguida a 'coluna' pretendida. ~%")
 (let ((tabuleiro (arco-horizontal node (read) (read) (get-arcos-verticais node) pecaJ)))
  (cond
   ((null tabuleiro) (jogada-humano node pecaJ pecaC jp cp))
   (t (progn (imprimir-tabuleiro (list tabuleiro 0 0 0))(format t "------------------------- ~%")(format t "------------------------- ~%")
   (jogada-computador (list tabuleiro 0 (heuristica (list tabuleiro 0 0 0)) (no-pai node)) pecaJ pecaC jp cp)))
  )
 )
)

(defun inserir-arco-vertical (node pecaJ pecaC jp cp)
 (format t "Digite a 'coluna' e de seguida a 'linha' pretendida. ~%")
 (let ((tabuleiro (arco-vertical node (read) (read) (get-arcos-horizontais node) pecaJ)))
  (cond
   ((null tabuleiro) (jogada-humano node pecaJ pecaC jp cp))
   (t (progn (imprimir-tabuleiro (list tabuleiro 0 0 0))(format t "------------------------- ~%")(format t "------------------------- ~%")
   (jogada-computador (list tabuleiro 0 (heuristica (list tabuleiro 0 0 0)) (no-pai node)) pecaJ pecaC jp cp)))
  )
 )
)


;;;################################ iNTERACAO cOMPUTADOR-cOMPUTADOR ##############################################################

(defun jogada-computador1 (node &optional (c1p 0) (c2p 0))
 (cond
  ((> (caixas-fechadas node) (+ c1p c2p)) (jogada-computador2 (list (car node) 0 (heuristica (list (car node) 0 0 0)) (no-pai node)) c1p (+ 1 c2p)))
  (t 
   (progn (imprimir-tabuleiro node)
    (cond 
     ((tabuleiro-preenchidop node) (list c1p c2p))
     (t (progn (alfa-beta 1 node) (jogada-computador2 (list (car *jogada*) 0 (heuristica (list (car *jogada*) 0 0 0)) (no-pai *jogada*)) c1p c2p)))
   ))
  )
 )
)

(defun jogada-computador2 (node &optional (c1p 0) (c2p 0))
 (cond
  ((> (caixas-fechadas node) (+ c1p c2p)) (jogada-computador1 (list (car node) 0 (heuristica (list (car node) 0 0 0)) (no-pai node)) (+ 1 c1p) c2p))
  (t
   (progn (imprimir-tabuleiro node)
   (cond 
    ((tabuleiro-preenchidop node) (list c1p c2p))
    (t (progn (alfa-beta 2 node) (jogada-computador1 (list (car *jogada*) 0 (heuristica (list (car *jogada*) 0 0 0)) (no-pai *jogada*)) c1p c2p)))
   ))
  )
 )
)

;;;######################################################################################## MENUS
(defun main ()
  (progn 
 (format t "|----------------------------------------------| ~%")
 (format t "|                                              | ~%")
 (format t "|    Defina o tempo limite para o computador   | ~%")     
 (format t "|                                              | ~%")
 (format t "|                Entre 1 - 20                  | ~%")
 (format t "|                                              | ~%")
 (format t "|----------------------------------------------| ~%")
 (let ((op (read)))
  (progn (setf *timelimit* op)(start))
 ))
)

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
   ((eq op 1) (jogada-humano (no-teste) 1 2 0 0))
   ((eq op 2) (jogada-computador (no-teste) 2 1 0 0))
   (t (start1))
  )
 ))
)


(defun start2 ()
 (jogada-computador1 (no-teste))
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
