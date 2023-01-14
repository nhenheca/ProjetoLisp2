;;NOVO
(defun start ()
 (setq heuristicaop 1)
 (menu-escolher-tabuleiro)
)

(defun my-get-tab (i)
 (with-open-file (stream "C:/Users/Jessie/Documents/GitHub/ProjetoLisp/problem.dat" :direction :input) 
  (tab stream i) 
 )
)

;; Contagem a partir de 0
(defun tab (stream i)
 (let ((problema (read stream)))
  (cond
   ((zerop i) problema)
   (t (tab stream (- i 1)))
  )
 )
)

(defun get-number-of-lines ()
 (with-open-file (stream "C:/Users/Jessie/Documents/GitHub/ProjetoLisp/problem.dat" :direction :input) 
  (get-number-of-lines-aux stream) 
 )
)

;; Contagem a partir de 0
(defun get-number-of-lines-aux (stream &optional (i 0))
 (cond
  ((eq nil (read stream nil nil)) i)
  (t (get-number-of-lines-aux stream (+ i 1)))
 )   
)

;;;##################################################################################################################################################
;;;TABULEIRO ########################################################################################################################################
;;;##################################################################################################################################################

(defun menu-escolher-tabuleiro ()
 (format t "|-------------------------|~%")
 (format t "|                         |~%")
 (format t "|   ESCOLHA O TABULEIRO   |~%")
 (format t "|                         |~%")
 (menu-escolher-tabuleiro-content)
 (ler-op-tabuleiro)
 (menu-escolher-algoritmo)
)

(defun menu-escolher-tabuleiro-content (&optional (i (get-number-of-lines)))
 (cond
  ((zerop i) (format t "|                         |~%|-------------------------|~%"))
  (t (format t "|    ~d - Tabuleiro ~d      |~%" i i)(menu-escolher-tabuleiro-content (- i 1)))
 )
)

(defun ler-op-tabuleiro ()
 (format t "Digite o numero do tabuleiro indicados: ")
 (setq problema (my-get-tab (- (read) 1)))
)

(defun menu-escolher-algoritmo ()
 (format t "|-------------------------|~%")
 (format t "|                         |~%")
 (format t "|   ESCOLHA O ALGORITMO   |~%")
 (format t "|                         |~%")
 (format t "|       1 - BFS           |~%")
 (format t "|       2 - DLS           |~%")
 (format t "|       3 - A*            |~%")
 (format t "|                         |~%")
 (format t "|       0 - Voltar        |~%")
 (format t "|                         |~%")
 (format t "|-------------------------|~%")
 (ler-op-algoritmo)
)

(defun menu-escolher-heuristica ()
 (format t "|-------------------------|~%")
 (format t "|                         |~%")
 (format t "|  ESCOLHA A HEURISTICA   |~%")
 (format t "|                         |~%")
 (format t "|       1 - DADA          |~%")
 (format t "|       2 - GRUPO         |~%")
 (format t "|                         |~%")
 (format t "|       0 - Voltar        |~%")
 (format t "|                         |~%")
 (format t "|-------------------------|~%")
 (ler-op-heuristica)
)

(defun ler-op-algoritmo ()
 (format t "Digite o algoritmo do tabuleiro indicados: ")
 (let ((op (read)))
  (cond
   ((eq 2 op) (format t "~%Digite a profundidade: ")(write-to-file (dls (get-objective) (read) (operadores (get-cl)))))
   ((eq 1 op) (write-to-file (bfs (get-objective) (operadores (get-cl)))))
   ((eq 3 op) (menu-escolher-heuristica))
   ((eq 0 op) (menu-escolher-tabuleiro))
   (t (menu-escolher-algoritmo))
  )
 )
)

(defun ler-op-heuristica ()
 (format t "Digite a heuristica desejada: ")
 (let ((op (read)))
  (cond
   ((eq 1 op) (setq heuristicaop op)(write-to-file (a* (get-objective) (operadores (get-cl)))))
   ((eq 2 op) (setq heuristicaop op)(write-to-file (a* (get-objective) (operadores (get-cl)))))
   ((eq 0 op) (menu-escolher-algoritmo))
   (t (menu-escolher-heuristica))
  )
 )
)

(defun write-to-file (params)
 (with-open-file (str "C:/Users/Jessie/Documents/GitHub/ProjetoLisp/solucao.txt" :direction :output :if-exists :supersede :if-does-not-exist :create)
  (format str "~a" params))
)

(defun get-objective ()
 (second problema)
)

(defun get-tabuleiro ()
 (car problema)
)

(defun get-cl ()
 (length (caar problema))
)

(defun get-heuristicaop ()
 heuristicaop
)