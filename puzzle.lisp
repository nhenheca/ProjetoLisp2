;;;MIGUEL GABRIEL MARQUES ########################################################

;;;Retorna um tabuleiro 3x3 (3 arcos na vertical por 3 arcos na horizontal) Profundidade Heuristica Pai"
(defun no-teste ()
 ;(list (tabuleiro-teste) 0 (heuristica (list (tabuleiro-teste) 0 0 nil) (get-objective)) nil)
 (list (tabuleiro-teste) 0 0 nil)
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
  (t (cria-no (funcall (first op) pai (second op) (third op) (funcall (car (fourth op)) pai) peca) (+ 1 (no-profundidade pai)) 0 pai))
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
'(
 (;arcos horizontais
 (0 0 0 0 0 0) 
 (0 0 0 0 0 0) 
 (0 0 2 0 0 0) 
 (0 0 0 0 0 0) 
 (0 0 0 0 0 0) 
 (0 0 0 0 0 0) 
 ) 
 (;arcos verticais
 (0 0 0 0 0) 
 (0 0 1 0 0) 
 (0 0 1 0 0) 
 (0 0 0 0 0) 
 (0 0 0 0 0) 
 (0 0 0 0 0) 
 (0 0 0 0 0) 
 )
)
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
  ((= 1 (get-arco-na-posicao pos i (get-arcos-horizontais no))) nil)
  (t (list (arco-na-posicao pos i (get-arcos-horizontais no) z)  l))
 )
)

;;;(arco-vertical 1 2 (get-arcos-horizontais (no-teste)))
;;;(((0 0 0) (0 0 1) (0 1 1) (0 0 1))((0 1 0) (0 1 1) (1 0 1) (0 1 1)))
(defun arco-vertical (no pos i l &optional (z 1))
 (cond
  ((equal nil (get-arco-na-posicao pos i (get-arcos-verticais no))) nil)
  ((= 1 (get-arco-na-posicao pos i (get-arcos-verticais no))) nil)
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

;;(get-arco-na-posicao 2 3 (get-arcos-horizontais (no-teste)))
(defun nCaixasFechadas (no &optional (cl (length (car (no-estado no)))) (iL 1)(posL 1)(iC 1)(posC 1)(caixas 0)(itNumber 1))
 (cond
  ((equal itNumber cl) caixas)
  ((equal cl posC) (nCaixasFechadas no cl 1 (+ posL 1) (+ iC 1) 1 caixas (+ itNumber 1)))
  ((and (equal 1 (get-arco-na-posicao posL iL (get-arcos-horizontais no)))(equal 1 (get-arco-na-posicao (+ 1 posL) iL (get-arcos-horizontais no)))(equal 1 (get-arco-na-posicao posC iC (get-arcos-verticais no)))(equal 1 (get-arco-na-posicao (+ 1 posC) iC (get-arcos-verticais no)))) (nCaixasFechadas no cl (+ iL 1) posL iC (+ posC 1)(+ caixas 1) itNumber))
  ((not(and (equal 1 (get-arco-na-posicao posL iL (get-arcos-horizontais no)))(equal 1 (get-arco-na-posicao (+ 1 posL) iL (get-arcos-horizontais no)))(equal 1 (get-arco-na-posicao posC iC (get-arcos-verticais no)))(equal 1 (get-arco-na-posicao (+ 1 posC) iC (get-arcos-verticais no))))) (nCaixasFechadas no cl (+ iL 1) posL iC (+ posC 1) caixas itNumber))
 )
)

;;;#########################################################################################################
;;; HEURISTICA E AUXILIARES ################################################################################
;;;#########################################################################################################

;;;#########################################################################################################
;;; FASE 2 AUXILIARES ######################################################################################
;;;#########################################################################################################

(defun tabuleiro-preenchidop (node)
 (cond
  ((and (null (remove nil (mapcar #'(lambda (x) (member 0 x)) (car (no-estado node))))) (null (remove nil (mapcar #'(lambda (x) (member 0 x)) (car (cdr (no-estado node))))))) t)
  (t nil)
 )
)

(defun avaliar-no (tipo node &optional (l 1) (c 1) listaGlobal listaLocal paths)
 (cond
  ((and (equal nil (existe-em-listap l c listaLocal))(equal nil (existe-em-listap l c listaGlobal))(no-valido-para-dorap node l c)) 
  (cond
   ((equal 0 (get-arco-na-posicao (+ 1 l) c (get-arcos-horizontais node))) )
   ((equal 0 (get-arco-na-posicao (+ 1 l) c (get-arcos-horizontais node))) )
   ((equal 0 (get-arco-na-posicao (+ 1 l) c (get-arcos-horizontais node))) )
   ((equal 0 (get-arco-na-posicao (+ 1 l) c (get-arcos-horizontais node))) )
  ))
  (t (avaliar-no tipo node 1 1 listaGlobal nil (+ (is-path-big listaLocal) paths))
 )
)

(defun existe-em-listap (l c lista)
 (cond
  ((null lista) nil)
  ((and (equal l (first (car lista))) (equal c (second (car lista)))) t)
  (t (existe-em-listap l c (cdr lista)))
 )
)

(defun is-path-big (lista)
 (cond
  ((> (length lista) 2) 1)
  (t 0)
 )
)

(defun no-valido-para-dorap (node l c)
 (cond
  ((and (equal l 1)(equal c 1) (or (equal 0 (get-arco-na-posicao (+ 1 l) c (get-arcos-horizontais node)))(equal 0 (get-arco-na-posicao (+ 1 c) l (get-arcos-verticais node))))) t) 
  ((and (equal l 1)(equal c 6) (or (equal 0 (get-arco-na-posicao (+ 1 l) c (get-arcos-horizontais node)))(equal 0 (get-arco-na-posicao c l (get-arcos-verticais node))))) t) 
  ((and (equal l 5)(equal c 1) (or (equal 0 (get-arco-na-posicao l c (get-arcos-horizontais node)))(equal 0 (get-arco-na-posicao (+ 1 c) l (get-arcos-verticais node))))) t) 
  ((and (equal l 5)(equal c 6) (or (equal 0 (get-arco-na-posicao l c (get-arcos-horizontais node)))(equal 0 (get-arco-na-posicao c l (get-arcos-verticais node))))) t) 
  ((equal l 1) t)
  ((equal l 5) t)
  ((equal c 1) t)
  ((equal c 6) t)
  (t )
 )
)


;;;#########################################################################################################
;;; FASE 2 AUXILIARES ######################################################################################
;;;#########################################################################################################