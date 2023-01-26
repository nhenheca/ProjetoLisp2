# Projeto Nº2: Época Normal - Fase nº 2 

## Inteligência Artificial 22/23
Prof. Joaquim Filipe

Eng. Filipe Mariano
# Jogo Dots and Boxes

## Manual Técnico

Realizado por:

Miguel Rodrigues - 202001391

João Marques - 202000432

Gabriel Garcia - 202002361

# Indice 

1. Introdução
2. Arquitetura do Sistema
3. Entidades e sua implementação
4. Algoritmos e sua implementação
5. Limitações técnicas
6. Resultados

# Introdução

Neste documento iremos abordar uma versão simplificada do problema "Jogo Dots and Boxes".

A sua implementação com AlfaBeta FailHard.


# Arquitetura do Sistema


A estrutura do projeto está organizada em 4 ficheiros:

- puzzle.lisp - Código relacionado com o problema.
- jogo.lisp - Carrega os outros ficheiros de código, escreve e lê de ficheiros e trata da interação com o utilizador.
- algoritmo.lisp - Implementação do algoritmo AlfaBeta FailHard.
- log.dat - Ficheiro com os resultados do jogo.

# Entidades e sua implementação


  ## Tabuleiro  

O tabuleiro é uma lista de listas, em que a primeira lista são as arestas horizontais por linha e a segunda lista são as arestas verticais por coluna.

```Lisp
    (
        ((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)) 
        ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0))
    )
```
 ---

## Operadores

Existem 2 tipos de operadores, que podem ter um numero variavel de filhos dependendo do tamanho do tabuleiro.

Os operadores são os seguintes:

```Lisp
(arco-horizontal no posicao indice lista (valor-default 1))
```
```Lisp
(arco-vertical no posicao indice lista (valor-default 1))
```

Os operadores preenchem uma posição aresta do tabuleiro, retornam nil se esta já existir.

Existe uma função operadores que gera todos os operadores possiveis para um dado nó dos 2 tipos:

```Lisp
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
```
## Nó

### Composição do Nó

A composição do nó é uma lista composta por:

(Tabuleiro | Profundidade | Heuristica | Pai)

```Lisp
(defun cria-no (tabuleiro g h pai)
 (list tabuleiro g h pai)
)
```


## Seletores do Nó
Os seletores do Nó são seletores que retornam os atributos que o compõem.

Nomeadamente:

- *no-estado*
- *no-profundidade*
- *no-pai* 
- *no-heuristica* 
- *no-custo* 
- *get-arcos-horizontais*
- *get-arcos-verticais*
- *get-arco-na-posicao*
  

## Sucessões

A Sucessão de um determinado nó, é um conjunto de movimentos permitidos apara colocar uma nova aresta no tabuleiro.


```Lisp
(defun novo-sucessor (pai op peca)
 (cond
  ((equal nil (funcall (first op) pai (second op) (third op) (funcall (car (fourth op)) pai))) nil)
  (t (cria-no (funcall (first op) pai (second op) (third op) (funcall (car (fourth op)) pai) peca) (+ 1 (no-profundidade pai)) (heuristica (list (funcall (first op) pai (second op) (third op) (funcall (car (fourth op)) pai)))) pai))
 )
)

(defun sucessores (no opsList peca)
 (remove nil (mapcar #'(lambda (op) (novo-sucessor no op peca)) opsList))
)
```

# Algoritmos e sua implementação

Nesta segunda parte do projeto foi implementado apenas um algoritmo, sendo este o AlfaBeta FailHard.

```lisp
(defun alfa-beta (player &optional (node (no-teste)) (alfa (- 0 999)) (beta 999) (starttime (get-universal-time)))
 (cond
  ((> (- (get-universal-time) starttime) (timelimit)) (progn (setspendtime (- (get-universal-time) starttime)) (alfa-beta-eval node)))
  ((or (null (sucessores node (operadores) player)) (eq (no-profundidade node) (maxdepth))) (progn (setspendtime (- (get-universal-time) starttime)) (alfa-beta-eval node)))
  (t
   (cond
    ((evenp (no-profundidade node)) (alfa-beta-max player (ordenar-sucessores (sucessores node (operadores) player)) alfa beta starttime))
    ((oddp (no-profundidade node)) (alfa-beta-min player (ordenar-sucessores (sucessores node (operadores) (troca-jogador player))) alfa beta starttime))
   )
  )
 )
)

(defun alfa-beta-max (player sucessores alfa beta starttime)
  (cond 
   ((null sucessores) alfa)
   (t (let* ((valor (alfa-beta player (car sucessores) alfa beta starttime))(novo-alfa (max alfa valor)))
        (if (>= novo-alfa beta)(progn (setncortesa (+ (ncortesa) 1))  beta)
        (progn (setJogada (car sucessores))(setnosanalisados (+ (nosanalisados) 1)) 
            (max novo-alfa (alfa-beta-max player (cdr sucessores) novo-alfa beta starttime))))))))

(defun alfa-beta-min (player sucessores alfa beta starttime)
  (cond 
   ((null sucessores) beta)
   (t (let* ((valor (alfa-beta player (car sucessores) alfa beta starttime))(novo-beta (min beta valor)))
        (if (<= novo-beta alfa)(progn (setncortesb (+ (ncortesb) 1)) alfa)
        (progn (setJogada (car sucessores))(setnosanalisados (+ (nosanalisados) 1)) 
            (min novo-beta (alfa-beta-min player (cdr sucessores) alfa novo-beta starttime))))))))
```


## Ordenação dos nós
Foi implementada uma função de ordenação dos nós sucessores com o objetivo de melhorar o número de cortes realizados por o algoritmo AlfaBeta. Para esse efeito foi aplicada uma heurística de avaliação.

```lisp
(defun ordenar-sucessores (sucessores)
 (sort sucessores #'< :key #'third)
)
```
O símbolo 'third' retorna o valor da heurística do nó.

---
## Heuristica

```lisp
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
```
Esta é a heurística implementada para a ordenação dos nós.

# Limitações técnicas e ideias para desenvolvimento futuro


Ao longo do desenvolvimento do projeto, tivemos muitas dificuldades em implementar o algoritimo AlfaBeta. Devido a necessitarmos de saber qual a peça de cada jogador iria utilizar, não nos foi possível implementar o AlfaBeta puro, ou seja, sem nenhum input adicional. 

# Resultados

## AlfaBeta
*(Usando heuristica criada)*

Para este problema foi utilizado uma profundidade máxima de 4 com um limite de tempo de 20 segundos.
```lisp
JOGADOR1 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)
            (0 0 0 0 0) (0 0 0 0 1)))
          37550 442 4578 20) 
JOGADOR2 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)
            (0 0 0 0 0) (0 0 0 2 1)))
          37415 444 4715 20)
```
<p>Como podemos observar, o primeiro jogador analisou 37550 nós, efetuou 442 cortes alfa, 4578 cortes beta e demorou 20 segundos.
Isto significa que o AlfaBeta não conseguiu explorar a árvore inteira e finalizou prematuramente, podendo não ter devolvido a melhor jogada possível.</p>
<p>Também podemos concluir que a partir da profundidade 4 a nossa implementação do AlfaBeta não é eficiente o suficiente. A profundidade 4 tem um total de 23319240 nós, enquanto a 3 tem 342930 nós.</p>
<p>Concluimos que a nossa implementação do AlfaBeta é capaz de explorar a árvore inteira se tiver abaixo da profundidade 4.</p>

<p>O exemplo abaixo foi utilizado uma profundidade máxima de 3 com um limite de tempo de 20 segundos.

```lisp
JOGADOR1 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)
            (0 0 0 0 0) (0 0 0 0 1)))
          11733 152 547 3) 
JOGADOR2 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)
            (0 0 0 0 0) (0 0 0 2 1)))
          5322 71 69 3)
```
Neste problema o AlfaBeta foi capaz de analisar a árvore completa.

---
<br>

## Análise contra Humano



| Problema         | Nós Gerados   | Nós Expandidos | *g(x)*   Profundidade  |Penetrância|Execução em segundos
| --------- | :-------:|:--------:|:-------:| :---------:| :---------:| 
| A |   77  |  4    |  2    | 5/77      |0 
| B |   16   |  0   |   1   | 1/16        |0 
| C |   Memory Bound   |  Memory Bound    |  Memory Bound    | Memory Bound          |Memory Bound
| D |   Memory Bound   |  Memory Bound    |  Memory Bound    | Memory Bound          |Memory Bound
| E |   Memory Bound   |  Memory Bound    |  Memory Bound    | Memory Bound          |Memory Bound






<br>
<br>
<br>

# Projeto Nº1: Época Normal 


## Inteligência Artificial 22/23
Prof. Joaquim Filipe

Eng. Filipe Mariano


# Jogo Dots and Boxes


## Manual de Utilizador


Realizado por:

Miguel Rodrigues - 202001391

João Marques - 202000432

Gabriel Garcia - 202002361

# Indice

1. Introdução
2. Instalação
3. Configuração
4. Interface da Aplicação
5. Output


# Introdução

Este documento servirá de manual de utilizador, guiará a instalação de todo o software necessario e de como utiliza-lo.



# Instalação

É necessario instalar um compilador de LISP e o respetivo editor, recomendamos o LispWorks.

Lispworks é uma Integrated cross-platform development tool for Common Lisp  [aqui](http://www.lispworks.com/products/lispworks.html)

A estrutura do projeto está organizada em 5 ficheiros:

- projeto.lisp - Interação com o utilizador, escrita e leitura de ficheiros.
- puzzle.lisp - Implementação da resolução do problema incluindo seletores, operadores heuristicas e outras funcõess auxiliares.
- procura.lisp - Implementação dos algoritmos de procura BFS, DFS e A*.
- problemas.dat - Tabuleiros representantes do problema.
- solucao.txt - A solução ao problema dado mais os seus dados estatisticos.


# Configuração

É necessario alterar a diretoria do projeto nas seguintes funções contidas em projeto.lisp:

```Lisp
(defun get-number-of-lines ()
 (with-open-file (stream "C:/Users/nhenhecas/Documents/GitHub/ProjetoLisp/problem.dat" :direction :input) 
  (get-number-of-lines-aux stream) 
 )
)

(defun my-get-tab (i)
 (with-open-file (stream "C:/Users/nhenhecas/Documents/GitHub/ProjetoLisp/problem.dat" :direction :input) 
  (tab stream i) 
 )
)

(defun write-to-file (params)
 (with-open-file (str "C:/Users/nhenhecas/Documents/GitHub/ProjetoLisp/solucao.txt" :direction :output :if-exists :supersede :if-does-not-exist :create)
  (format str "~a" params))
)
```

Após as funções alteradas deverá abrir os ficheiros puzzle.lisp, projeto.lisp, procura.lisp e compilar cada um deles.


# Interface da Aplicação

## Escolher tabuleiro

No menu princial terá a hipotese de escolher um dos tabuleiros contidos em problema.dat
```
|-------------------------|
|                         |
|   ESCOLHA O TABULEIRO   |
|                         |
|    6 - Tabuleiro 6      |
|    5 - Tabuleiro 5      |
|    4 - Tabuleiro 4      |
|    3 - Tabuleiro 3      |
|    2 - Tabuleiro 2      |
|    1 - Tabuleiro 1      |
|                         |
|-------------------------|
Digite o numero do tabuleiro indicados:
```
## Escolher algoritmo 
Neste menu terá a hipotese de escolher o algotitmo a aplicar a um dos tabuleiros contidos em problema.dat escolhido anteriormente.

```
|-------------------------|
|                         |
|   ESCOLHA O ALGORITMO   |
|                         |
|       1 - BFS           |
|       2 - DLS           |
|       3 - A*            |
|                         |
|       0 - Voltar        |
|                         |
|-------------------------|
Digite o algoritmo do tabuleiro indicados: 
```

### Escolher algoritmo DLS

```
|-------------------------|
|                         |
|   ESCOLHA O ALGORITMO   |
|                         |
|       1 - BFS           |
|       2 - DLS           |
|       3 - A*            |
|                         |
|       0 - Voltar        |
|                         |
|-------------------------|
Digite o algoritmo do tabuleiro indicados: 2

Digite a profundidade:
```
## Escolher heuristica
Neste menu terá a hipotese de escolher a heuristica a aplicar a um dos tabuleiros contidos em problema.dat escolhido anteriormente.
```
|-------------------------|
|                         |
|  ESCOLHA A HEURISTICA   |
|                         |
|       1 - DADA          |
|       2 - GRUPO         |
|                         |
|       0 - Voltar        |
|                         |
|-------------------------|
Digite a heuristica desejada:
```
# Output

## CONSOLE OUTPUT
O output da consela é nil, informa que o algoritmo correu com sucesso até ao nó objetivo. 


## FILE OUTPUT
 O output do ficheiro será uma lista constituida pelo caminhio até ao nó objetivo, profundidade, numero total de nós gerados, numero de nós expandidos, penetração e tempo de execução.

Exemplo problema E executado pelo A* com a heuristica criada. 
```
(((((0 0 0 1 0 0) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 0) (0 0 1 1 1 1) (0 0 1 1 1 1)) ((0 0 0 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (0 1 1 1 0 0) (0 1 1 1 1 1))) 24 23 ((((0 0 0 1 0 0) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 0) (0 0 1 1 1 1) (0 0 1 1 1 1)) ((0 0 0 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (0 0 1 1 0 0) (0 1 1 1 1 1))) 23 25 ((((0 0 0 1 0 0) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 0) (0 0 1 1 1 1) (0 0 1 1 1 1)) ((0 0 0 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 0) (0 0 1 1 0 0) (0 1 1 1 1 1))) 22 27 ((((0 0 0 1 0 0) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 0) (0 0 1 1 1 1) (0 0 1 1 1 1)) ((0 0 0 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 0 1 0) (0 0 1 1 0 0) (0 1 1 1 1 1))) 21 29 ((((0 0 0 1 0 0) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 0 1 1 0) (0 0 1 1 1 1) (0 0 1 1 1 1)) ((0 0 0 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 0 1 0) (0 0 1 1 0 0) (0 1 1 1 1 1))) 20 31 ((((0 0 0 1 0 0) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 0 0 1 1 0) (0 0 1 1 1 1) (0 0 1 1 1 1)) ((0 0 0 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 0 1 0) (0 0 1 1 0 0) (0 1 1 1 1 1))) 19 33 ((((0 0 0 1 0 0) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 0 0 1 1 0) (0 0 1 1 1 1) (0 0 1 1 1 1)) ((0 0 0 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 0 1 0 1 0) (0 0 1 1 0 0) (0 1 1 1 1 1))) 18 35 ((((0 0 0 1 0 0) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (0 0 0 1 1 0) (0 0 1 1 1 1) (0 0 1 1 1 1)) ((0 0 0 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 0 1 0 1 0) (0 0 1 1 0 0) (0 1 1 1 1 1))) 17 37 ((((0 0 0 1 0 0) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 0) (0 0 0 1 1 0) (0 0 1 1 1 1) (0 0 1 1 1 1)) ((0 0 0 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 0 1 0 1 0) (0 0 1 1 0 0) (0 1 1 1 1 1))) 16 39 ((((0 0 0 1 0 0) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 0) (0 0 0 1 1 0) (0 0 1 1 1 1) (0 0 1 1 1 1)) ((0 0 0 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 0) (1 0 1 0 1 0) (0 0 1 1 0 0) (0 1 1 1 1 1))) 15 41 ((((0 0 0 1 0 0) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 0) (0 0 0 1 1 0) (0 0 1 1 1 1) (0 0 1 1 1 1)) ((0 0 0 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 0 0) (1 0 1 0 1 0) (0 0 1 1 0 0) (0 1 1 1 1 1))) 14 43 ((((0 0 0 1 0 0) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 0 1 1 0) (0 0 0 1 1 0) (0 0 1 1 1 1) (0 0 1 1 1 1)) ((0 0 0 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 0 0) (1 0 1 0 1 0) (0 0 1 1 0 0) (0 1 1 1 1 1))) 13 45 ((((0 0 0 1 0 0) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 0 0 1 1 0) (0 0 0 1 1 0) (0 0 1 1 1 1) (0 0 1 1 1 1)) ((0 0 0 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 0 0) (1 0 1 0 1 0) (0 0 1 1 0 0) (0 1 1 1 1 1))) 12 47 ((((0 0 0 1 0 0) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 0 0 1 1 0) (0 0 0 1 1 0) (0 0 1 1 1 1) (0 0 1 1 1 1)) ((0 0 0 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 0 1 1 0 0) (1 0 1 0 1 0) (0 0 1 1 0 0) (0 1 1 1 1 1))) 11 49 ((((0 0 0 1 0 0) (1 1 1 1 1 1) (1 1 1 1 1 1) (0 0 0 1 1 0) (0 0 0 1 1 0) (0 0 1 1 1 1) (0 0 1 1 1 1)) ((0 0 0 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 0 1 1 0 0) (1 0 1 0 1 0) (0 0 1 1 0 0) (0 1 1 1 1 1))) 10 51 ((((0 0 0 1 0 0) (1 1 1 1 1 1) (1 1 1 1 1 1) (0 0 0 1 1 0) (0 0 0 1 1 0) (0 0 1 1 1 1) (0 0 1 1 1 1)) ((0 0 0 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (0 0 1 1 0 0) (1 0 1 0 1 0) (0 0 1 1 0 0) (0 1 1 1 1 1))) 9 53 ((((0 0 0 1 0 0) (1 1 1 1 1 1) (1 1 1 1 1 0) (0 0 0 1 1 0) (0 0 0 1 1 0) (0 0 1 1 1 1) (0 0 1 1 1 1)) ((0 0 0 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (0 0 1 1 0 0) (1 0 1 0 1 0) (0 0 1 1 0 0) (0 1 1 1 1 1))) 8 55 ((((0 0 0 1 0 0) (1 1 1 1 1 1) (1 1 1 1 1 0) (0 0 0 1 1 0) (0 0 0 1 1 0) (0 0 1 1 1 1) (0 0 1 1 1 1)) ((0 0 0 1 1 1) (1 1 1 1 1 1) (1 1 1 0 1 1) (0 0 1 1 0 0) (1 0 1 0 1 0) (0 0 1 1 0 0) (0 1 1 1 1 1))) 7 57 ((((0 0 0 1 0 0) (1 1 1 1 1 1) (1 1 1 1 1 0) (0 0 0 1 1 0) (0 0 0 1 1 0) (0 0 1 1 1 1) (0 0 1 1 1 1)) ((0 0 0 1 1 1) (1 1 1 1 1 1) (0 1 1 0 1 1) (0 0 1 1 0 0) (1 0 1 0 1 0) (0 0 1 1 0 0) (0 1 1 1 1 1))) 6 59 ((((0 0 0 1 0 0) (1 1 1 1 1 1) (1 1 1 1 1 0) (0 0 0 1 1 0) (0 0 0 1 1 0) (0 0 1 1 1 1) (0 0 1 1 1 1)) ((0 0 0 1 1 1) (1 1 1 0 1 1) (0 1 1 0 1 1) (0 0 1 1 0 0) (1 0 1 0 1 0) (0 0 1 1 0 0) (0 1 1 1 1 1))) 5 61 ((((0 0 0 1 0 0) (1 1 0 1 1 1) (1 1 1 1 1 0) (0 0 0 1 1 0) (0 0 0 1 1 0) (0 0 1 1 1 1) (0 0 1 1 1 1)) ((0 0 0 1 1 1) (1 1 1 0 1 1) (0 1 1 0 1 1) (0 0 1 1 0 0) (1 0 1 0 1 0) (0 0 1 1 0 0) (0 1 1 1 1 1))) 4 63 ((((0 0 0 1 0 0) (1 1 0 1 1 1) (1 1 1 1 1 0) (0 0 0 1 1 0) (0 0 0 1 1 0) (0 0 1 1 1 1) (0 0 1 1 1 1)) ((0 0 0 1 1 1) (1 1 0 0 1 1) (0 1 1 0 1 1) (0 0 1 1 0 0) (1 0 1 0 1 0) (0 0 1 1 0 0) (0 1 1 1 1 1))) 3 65 ((((0 0 0 1 0 0) (1 0 0 1 1 1) (1 1 1 1 1 0) (0 0 0 1 1 0) (0 0 0 1 1 0) (0 0 1 1 1 1) (0 0 1 1 1 1)) ((0 0 0 1 1 1) (1 1 0 0 1 1) (0 1 1 0 1 1) (0 0 1 1 0 0) (1 0 1 0 1 0) (0 0 1 1 0 0) (0 1 1 1 1 1))) 2 67 ((((0 0 0 1 0 0) (0 0 0 1 1 1) (1 1 1 1 1 0) (0 0 0 1 1 0) (0 0 0 1 1 0) (0 0 1 1 1 1) (0 0 1 1 1 1)) ((0 0 0 1 1 1) (1 1 0 0 1 1) (0 1 1 0 1 1) (0 0 1 1 0 0) (1 0 1 0 1 0) (0 0 1 1 0 0) (0 1 1 1 1 1))) 1 69 ((((0 0 0 1 0 0) (0 0 0 1 1 1) (1 1 1 1 1 0) (0 0 0 1 1 0) (0 0 0 1 1 0) (0 0 1 1 1 1) (0 0 1 1 1 1)) ((0 0 0 1 1 1) (0 1 0 0 1 1) (0 1 1 0 1 1) (0 0 1 1 0 0) (1 0 1 0 1 0) (0 0 1 1 0 0) (0 1 1 1 1 1))) 0 71 NIL))))))))))))))))))))))))) 690 24 24/691 0)
```