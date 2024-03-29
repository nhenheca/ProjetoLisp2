(defpackage :202001391-202000432-202002361 (:use :cl))
;;;MIGUEL GABRIEL MARQUES ########################################################

;;;Retorna um tabuleiro 3x3 (3 arcos na vertical por 3 arcos na horizontal) Profundidade Heuristica Pai"
(defun no-teste ()
 ;(list (tabuleiro-teste) 0 (heuristica (list (tabuleiro-teste) 0 0 nil) (get-objective)) nil)
 (list (tabuleiro-teste) 0 (heuristica (list (tabuleiro-teste) 0 0 nil)) nil)
)

;;;#########################################################################################################
;;;SELETORES ###############################################################################################
;;;#########################################################################################################

;;;Seleciona dentro do no o tabuleiro
(defun no-estado (no)
 (car no)
)

;;;Seleciona dentro do no o profundidade
(defun no-profundidade (no)
 (second no)
)

;;;Seleciona dentro do no o pai
(defun no-pai (no)
 (fourth no)
)

;;;Seleciona dentro do no o heuristica
(defun no-heuristica (no)
 (third no)
)

;;;Seleciona dentro do no o custo
(defun no-custo(no)
  (+ (no-profundidade no) (no-heuristica no))
)

;;;(get-arcos-horizontais (no-teste))
;;;((0 0 0) (0 0 1) (0 1 1) (0 0 1))
(defun get-arcos-horizontais(no)
 (car (no-estado no))
)

;;;(get-arcos-verticais (no-teste))
;;;((0 0 0) (0 1 1) (1 0 1) (0 1 1))
(defun get-arcos-verticais(no)
 (car (cdr (no-estado no)))
)

;;(get-arco-na-posicao 2 3 (get-arcos-horizontais (no-teste)))
;;1
(defun get-arco-na-posicao (x y l)
 (nth (- y 1)(nth (- x 1) l))
)

;;;#########################################################################################################
;;;SELETORES ###############################################################################################
;;;#########################################################################################################

;;;#########################################################################################################
;;;SUCESSORES ##############################################################################################
;;;#########################################################################################################

;;;Construtor
(defun cria-no (tabuleiro g h pai)
 (list tabuleiro g h pai)
)

(defun novo-sucessor (pai op peca)
 (cond
  ((equal nil (funcall (first op) pai (second op) (third op) (funcall (car (fourth op)) pai))) nil)
  (t (cria-no (funcall (first op) pai (second op) (third op) (funcall (car (fourth op)) pai) peca) (+ 1 (no-profundidade pai)) (heuristica (list (funcall (first op) pai (second op) (third op) (funcall (car (fourth op)) pai)))) pai))
 )
)

(defun sucessores (no opsList peca)
 (remove nil (mapcar #'(lambda (op) (novo-sucessor no op peca)) opsList))
)

;;;#########################################################################################################
;;;SUCESSORES ##############################################################################################
;;;#########################################################################################################

;;;#########################################################################################################
;;;AUXILIARES ##############################################################################################
;;;#########################################################################################################

;;; Tabuleiro
(defun tabuleiro-teste ()
'(((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)) ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)))
;'(((1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) ) ((1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1)))
)

;;; AUXILIARES ;;;
;;;(substituir 1 (car (get-arcos-horizontais (no-teste))))
;;;(1 0 0)
;;;(substituir 2 (car (get-arcos-verticais (no-teste))) 2)
;;;(0 2 0)
(defun substituir(x l &optional (y 1))
 (cond
  ((= x 1) (cons y (cdr l)))
  (t (cons (car l) (substituir (1- x) (cdr l) y)))
 )
)

;;;(arco-na-posicao 2 2 (get-arcos-horizontais (no-teste)))
;;;((0 0 0) (0 1 1) (0 1 1) (0 0 1))
;;;(arco-na-posicao 4 1 (get-arcos-verticais (no-teste)))
;;;((0 0 0) (0 1 1) (1 0 1) (1 1 1))
(defun arco-na-posicao (x y l &optional (z 1))
 (cond
  ((= x 1) (cons (substituir y (car l) z) (cdr l)))
  (t (cons (car l) (arco-na-posicao (1- x) y (cdr l) z)))
 )
)

;;;#########################################################################################################
;;;AUXILIARES ##############################################################################################
;;;#########################################################################################################

;;;#########################################################################################################
;;;OPERADORES ##############################################################################################
;;;#########################################################################################################

;;;(arco-horizontal 3 1 (get-arcos-verticais (no-teste)))
;;;((0 0 0) (0 0 1) (1 1 1) (0 0 1) (0 0 0) (0 1 1) (1 0 1) (0 1 1))
(defun arco-horizontal (no pos i l &optional (z 1))
 (cond
  ((equal nil (get-arco-na-posicao pos i (get-arcos-horizontais no))) nil)
  ((/= 0 (get-arco-na-posicao pos i (get-arcos-horizontais no))) nil)
  (t (list (arco-na-posicao pos i (get-arcos-horizontais no) z)  l))
 )
)

;;;(arco-vertical 1 2 (get-arcos-horizontais (no-teste)))
;;;(((0 0 0) (0 0 1) (0 1 1) (0 0 1))((0 1 0) (0 1 1) (1 0 1) (0 1 1)))
(defun arco-vertical (no pos i l &optional (z 1))
 (cond
  ((equal nil (get-arco-na-posicao pos i (get-arcos-verticais no))) nil)
  ((/= 0 (get-arco-na-posicao pos i (get-arcos-verticais no))) nil)
  (t (list l (arco-na-posicao pos i (get-arcos-verticais no) z)))
 )
)

;;;Devolve a lista de operadores
(defun operadoresC (&optional (c 7) (l 6) (cc 1) (ll 1))
 (cond
  ((and (equal cc c)(equal ll l)) nil)

  ((not (equal c cc)) (append (list(list 'arco-horizontal ll cc '(get-arcos-verticais ))) (operadoresC c l (+ cc 1) ll)))
  ((equal c cc) (operadoresC c l 1 (+ 1 ll)))
 )
)

(defun operadoresL (&optional (c 7) (l 6) (cc 1) (ll 1))
 (cond
  ((and (equal cc c)(equal ll l)) nil)

  ((not (equal l ll)) (append (list(list 'arco-vertical cc ll '(get-arcos-horizontais ))) (operadoresL c l cc (+ 1 ll))))
  ((equal l ll) (operadoresL c l (+ 1 cc) 1 ))
 )
)

(defun operadores ()
 (append (operadoresC) (operadoresL))
)

;;;#########################################################################################################
;;;OPERADORES ##############################################################################################
;;;#########################################################################################################

;;;#########################################################################################################
;;; HEURISTICA E AUXILIARES ################################################################################
;;;#########################################################################################################

(defun heuristica (node &optional (c 7) (l 5) (cc 1) (ll 1) (counter 0))
 (cond
  ((and (equal cc c)(equal ll l)) (- 30 counter))
  ((eq c cc) (heuristica node c l 1 (+ 1 ll) counter))
  ((eq 3 (length (remove 0 (list (get-arco-na-posicao ll cc (get-arcos-horizontais node))
                                 (get-arco-na-posicao (+ 1 ll) cc (get-arcos-horizontais node))
                                 (get-arco-na-posicao cc ll (get-arcos-verticais node))
                                 (get-arco-na-posicao (+ 1 cc) ll (get-arcos-verticais node)))))) 
  (heuristica node c l (+ 1 cc) ll (+ 1 counter)))
  (t (heuristica node c l (+ 1 cc) ll counter))
 )
)

;;;#########################################################################################################
;;; HEURISTICA E AUXILIARES ################################################################################
;;;#########################################################################################################


;;;#########################################################################################################
;;; FASE 2 AUXILIARES ######################################################################################
;;;#########################################################################################################

;;VERFICA SE O TABULEIRO ESTA CHEIO
(defun tabuleiro-preenchidop (node)
 (cond
  ((and (null (remove nil (mapcar #'(lambda (x) (member 0 x)) (car (no-estado node))))) (null (remove nil (mapcar #'(lambda (x) (member 0 x)) (car (cdr (no-estado node))))))) t)
  (t nil)
 )
)

(defun alfa-beta-eval (node &optional (c 7) (l 5) (cc 1) (ll 1) (counter3 0) (counter4 0))
 (cond
  ((and (equal cc c)(equal ll l)) (+ (- (* 10 counter4) (* 10 (caixas-fechadas (no-pai node)))) (* -10 counter3)))
  ((eq c cc) (alfa-beta-eval node c l 1 (+ 1 ll) counter3 counter4))
  ((eq 3 (length (remove 0 (list (get-arco-na-posicao ll cc (get-arcos-horizontais node))
                                 (get-arco-na-posicao (+ 1 ll) cc (get-arcos-horizontais node))
                                 (get-arco-na-posicao cc ll (get-arcos-verticais node))
                                 (get-arco-na-posicao (+ 1 cc) ll (get-arcos-verticais node)))))) 
  (alfa-beta-eval node c l (+ 1 cc) ll (+ 1 counter3) counter4))
  ((eq 4 (length (remove 0 (list (get-arco-na-posicao ll cc (get-arcos-horizontais node))
                                 (get-arco-na-posicao (+ 1 ll) cc (get-arcos-horizontais node))
                                 (get-arco-na-posicao cc ll (get-arcos-verticais node))
                                 (get-arco-na-posicao (+ 1 cc) ll (get-arcos-verticais node)))))) 
  (alfa-beta-eval node c l (+ 1 cc) ll counter3 (+ 1 counter4)))
  (t (alfa-beta-eval node c l (+ 1 cc) ll counter3 counter4))
 )
)

(defun troca-jogador (player)
 (cond
  ((eq player 1) 2)
  (t 1)
 )
)

(defun caixas-fechadas (node &optional (c 7) (l 5) (cc 1) (ll 1) (counter 0))
 (cond
  ((null node) 0)
  ((and (equal cc c)(equal ll l)) counter)
  ((eq c cc) (caixas-fechadas node c l 1 (+ 1 ll) counter))
  ((eq 4 (length (remove 0 (list (get-arco-na-posicao ll cc (get-arcos-horizontais node))
                                 (get-arco-na-posicao (+ 1 ll) cc (get-arcos-horizontais node))
                                 (get-arco-na-posicao cc ll (get-arcos-verticais node))
                                 (get-arco-na-posicao (+ 1 cc) ll (get-arcos-verticais node)))))) 
  (caixas-fechadas node c l (+ 1 cc) ll (+ 1 counter)))
  (t (caixas-fechadas node c l (+ 1 cc) ll counter))
 )
)


(defun ordenar-sucessores (sucessores)
 (sort sucessores #'< :key #'third)
)

;;;#########################################################################################################
;;; FASE 2 AUXILIARES ######################################################################################
;;;#########################################################################################################