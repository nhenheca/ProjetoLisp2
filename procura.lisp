;;; ####################################################################################################################################################################################################################################
;;; A* #################################################################################################################################################################################################################################
;;; ####################################################################################################################################################################################################################################

;;; Implementação do A*.
;;; (a* [operadores: Conjunto de Operadores] [abertos: Lista de abertos inicializada com o no incial] [fechados: Lista de fechados vazia] [pop-no: Atomo que guarda o no retirado da lista de abertos] [it: Atmo de controlo dentro da função recursiva] [bool: Atmo que impede que a função recursiva de devolver nil na primeira iteração]
(defun a* (objetivo operadores &optional (abertos (list (no-teste))) fechados pop-no (it 3)(bool 0)(starting-time (get-universal-time)))
 (cond
  ((and (not(eq bool 1))(null abertos)) nil) ;;; SE A LISTA DE ABERTOS FOR NULL ENTÃO NÃO EXSITE NO OBJETIVO
  ((eq it 3) (a* objetivo operadores (abertos-sem-no-menor-custo-a* abertos (pos-no-menor-custo-a* abertos)) fechados (no-menor-custo-a* abertos) 0 (+ bool 1) starting-time)) ;;; RETIRA O NO DE MENOR CUSTO DE ABERTOS, GUARDA REFERENCIA NO "NO-POP"
  ((no-objetivop pop-no objetivo) (list pop-no (+(length (sucessores pop-no operadores))(length abertos)(length fechados)) (length fechados) (/ (length fechados) (+ (length (sucessores pop-no operadores))(length fechados)(length abertos)))(- (get-universal-time) starting-time) )) ;;; SE ALGUM NO SUCESSOR FOR NO OBJETIVO DEVOLVE
  (t (a* objetivo operadores (append abertos (compile-sucessores-fechados-a* (compile-sucessores-abertos-a* (sucessores pop-no operadores) abertos) fechados)) (append (list pop-no) fechados) pop-no 3 1 starting-time)) ;;; SENAO ABERTOS RECEBE NOS SUCESSORES NAO REPETIDOS OU REPETIDOS COM MENOR CUSTO , FECHADOS RECEBE O "NO-POP"
 )
)


;;; Devolve a posição na lista de abertos do no de menor custo.
;;; (pos-no-menor-custo-a* [abertos: Lista de abertos] [no: Primeiro no da Lista de abertos] [i: Atmo auxiliar] [pos: Atmo devolve a posição do no] )
(defun pos-no-menor-custo-a* (abertos &optional (no (car abertos)) (i 0)(pos 0))
 (cond
  ((null abertos) pos)
  ((< (no-custo (car abertos)) (no-custo no)) (pos-no-menor-custo-a* (cdr abertos) (car abertos) (+ i 1) i))
  (t (pos-no-menor-custo-a* (cdr abertos) no (+ i 1) pos))
 )
)

;;; Devolve o no da lista de abertos de menor custo.
;;; (no-menor-custo-a* [abertos: Lista de abertos] [no: Primeiro no da Lista de abertos] [i: Atmo auxiliar] [pos: Atmo devolve a posição do no] )
(defun no-menor-custo-a* (abertos &optional (no (car abertos)) (i 0)(pos 0))
 (cond
  ((null abertos) no)
  ((< (no-custo (car abertos)) (no-custo no)) (no-menor-custo-a* (cdr abertos) (car abertos) (+ i 1) i))
  (t (no-menor-custo-a* (cdr abertos) no (+ i 1) pos))
 )
)

;;; Devolve a lista de abertos sem o no de menor custo
;;; (pos-no-menor-custo-a* [abertos: Lista de abertos] [pos: Posição do no na lista])
(defun abertos-sem-no-menor-custo-a* (abertos pos)
 (cond
  ((eq 1(length abertos)) (cdr abertos))
  (t (append (subseq abertos 0 pos)(subseq abertos (+ pos 1))))
 )
)

;;; Verifica se no é objetivo
;;; (sucessor-e-objetivo [sucessoresL: Lista de sucessores])
(defun sucessor-e-objetivo (sucessoresL objetivo)
 (cond
  ((null sucessoresL) nil)
  ((no-objetivop (car sucessoresL) objetivo) (car sucessoresL))
  (t (sucessor-e-objetivo (cdr sucessoresL) objetivo))
 )
)

;;; Verifica se o no já existe em abertos com maior custo
;;; (no-existe-abertos [no: no a comparar][abertos: lista de abertos])
(defun no-existe-abertos-a* (no abertos)
 (cond
  ((null abertos) no)
  ((and (eq (no-estado no)(no-estado (car abertos)))(< (no-custo (car abertos))(no-custo no))) nil)
  (t (no-existe-abertos-a* no (cdr abertos)))
 )
)

;;; Verifica se o no já existe em fechados com maior custo
;;; (no-existe-abertos [no: no a comparar][fechados: lista de fechados)
(defun no-existe-fechados-a* (no fechados)
 (cond
  ((null fechados) no)
  ((and (eq (no-estado no)(no-estado (car fechados)))(< (no-custo (car fechados))(no-custo no))) nil)
  (t (no-existe-fechados-a* no (cdr fechados)))
 )
)

;;;Compila todos os nos que não existem em abertos com menor custo
;;; (no-existe-abertos [sucessoresL: lista de sucessoresr][abertos: lista de abertos)
(defun compile-sucessores-abertos-a* (sucessoresL abertos)
 (remove nil (mapcar #'(lambda (x) (no-existe-abertos-a* x abertos)) sucessoresL))
)

;;;Compila todos os nos que não existem em fechados com menor custo
;;; (no-existe-abertos [sucessoresL: lista de sucessores][fechados: lista de fechados)
(defun compile-sucessores-fechados-a* (sucessoresL fechados)
 (remove nil (mapcar #'(lambda (x) (no-existe-fechados-a* x fechados)) sucessoresL))
)

;;; ####################################################################################################################################################################################################################################
;;; BFS ################################################################################################################################################################################################################################
;;; ####################################################################################################################################################################################################################################

;;; Implementação do BFS
;;; (bfs [operadores: Conjunto de Operadores] [abertos: Lista de abertos inicializada com o no incial] [fechados: Lista de fechados vazia] [pop-no: Atomo que guarda o no retirado da lista de abertos] [it: Atmo de controlo dentro da função recursiva] [bool: Atmo que impede que a função recursiva de devolver nil na primeira iteração]
(defun bfs (objetivo operadores &optional (abertos (list (no-teste))) fechados pop-no (it 1)(bool 0)(starting-time (get-universal-time)))
 (cond
  ((and (not(eq bool 1))(null abertos)) nil)
  ((eq it 1) (bfs objetivo operadores (cdr abertos) (append (list (car abertos))fechados) (car abertos) 2 (+ bool 1) starting-time))
  ((not (eq nil (sucessor-e-objetivo (sucessores pop-no operadores) objetivo))) (list (sucessor-e-objetivo (sucessores pop-no operadores) objetivo)(+(length (sucessores pop-no operadores))(length abertos)(length fechados))(- (length fechados) 1)(/ (length fechados) (+ (length (sucessores pop-no operadores))(length fechados)(length abertos))) (- (get-universal-time) starting-time)))
  ((eq it 2) (bfs objetivo operadores (append abertos (compile-sucessores-fechados (compile-sucessores-abertos (sucessores pop-no operadores) abertos) fechados)) fechados pop-no 1 1 starting-time))  
 )
)


;;; ####################################################################################################################################################################################################################################
;;; DFS ################################################################################################################################################################################################################################
;;; ####################################################################################################################################################################################################################################

;;; Implementação do DLS(Depth Limit Search) = DFS+DEPTH-LIMIT
;;; (dls [operadores: Conjunto de Operadores] [abertos: Lista de abertos inicializada com o no incial] [fechados: Lista de fechados vazia] [pop-no: Atomo que guarda o no retirado da lista de abertos] [it: Atmo de controlo dentro da função recursiva] [bool: Atmo que impede que a função recursiva de devolver nil na primeira iteração]
(defun dls (objetivo depth operadores &optional (abertos (list (no-teste))) fechados pop-no (it 1)(bool 0)(starting-time (get-universal-time)))
 (cond
  ((and (not(eq bool 1))(null abertos)) nil)
  ((eq it 1)(dls objetivo depth operadores (butlast abertos) (append (list (car (last abertos))) fechados) (car (last abertos)) 2 (+ bool 1) starting-time))
  ((and (eq it 2)(< (no-profundidade pop-no) depth))(dls objetivo depth operadores abertos fechados pop-no 3 1 starting-time))
  ((and (eq it 2)(>= (no-profundidade pop-no) depth))(dls objetivo depth operadores abertos fechados pop-no 1 1 starting-time))
  ((and (eq it 3)(sucessor-e-objetivo (sucessores pop-no operadores) objetivo)) (list (sucessor-e-objetivo (sucessores pop-no operadores) objetivo)(+(length (sucessores pop-no operadores))(length abertos)(length fechados))(- (length fechados) 1)(/ (length fechados) (+ (length (sucessores pop-no operadores))(length fechados)(length abertos)))(- (get-universal-time) starting-time) ))
  (t (dls objetivo depth operadores (append (compile-sucessores-fechados (compile-sucessores-abertos (sucessores pop-no operadores) abertos) fechados) abertos) fechados pop-no 1 1 starting-time))
 )
)

;;; ####################################################################################################################################################################################################################################
;;; FUNÇÕES AUXILIARES AO DLS/BFS ######################################################################################################################################################################################################
;;; ####################################################################################################################################################################################################################################

(defun no-existe-abertos (no abertos)
 (cond
  ((null abertos) no)
  ((eq (no-estado no)(no-estado (car abertos))) nil)
  (t (no-existe-abertos no (cdr abertos)))
 )
)

;;; Verifica se o no já existe em fechados com maior custo
;;; (no-existe-abertos [no: no a comparar][fechados: lista de fechados)
(defun no-existe-fechados (no fechados)
 (cond
  ((null fechados) no)
  ((eq (no-estado no)(no-estado (car fechados))) nil)
  (t (no-existe-fechados no (cdr fechados)))
 )
)

;;;Compila todos os nos que não existem em abertos com menor custo
;;; (no-existe-abertos [sucessoresL: lista de sucessoresr][abertos: lista de abertos)
(defun compile-sucessores-abertos (sucessoresL abertos)
 (mapcar #'(lambda (x) (no-existe-abertos x abertos)) sucessoresL)
)

;;;Compila todos os nos que não existem em fechados com menor custo
;;; (no-existe-abertos [sucessoresL: lista de sucessores][fechados: lista de fechados)
(defun compile-sucessores-fechados (sucessoresL fechados)
 (mapcar #'(lambda (x) (no-existe-fechados x fechados)) sucessoresL)
)