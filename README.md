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

### AlfaBeta
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
## Função de avaliação AlfaBeta

<p> A função de avaliação alfa-beta verifica o numero de caixas no tabuleiro com 3 arestas e o numero de caixas no tabuleiro com 4 arestas.</p>
<p>Cada caixa com 4 arestas vale +10 e cada caixa com 3 arestas vale -10.</p>
<p>Isto significa se na sua jogada colocar uma terceira aresta numa caixa é mau pois oferece uma caixa por fechar ao oponente mas se colocar uma quarta aresta numa caixa significa que a fechou, isto é, ganhou 1 ponto.</p>
<p>A funcão devolve o somatorio de ocorrencias caixas 3 vezes -10 menos o somatorio de ocorrencias caixas 4 vezes 10.</p>

```lisp
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

## Limitações técnicas e ideias para desenvolvimento futuro


Ao longo do desenvolvimento do projeto, tivemos muitas dificuldades em implementar o algoritimo AlfaBeta. Devido a necessitarmos de saber qual a peça de cada jogador iria utilizar, não nos foi possível implementar o AlfaBeta puro, ou seja, sem nenhum input adicional. 

# Resultados

## AlfaBeta
*(Usando heuristica criada)*

## Análise contra Cpu-Cpu

###Problema 1

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

### Problema 2

Para este problema foi utilizado uma profundidade máxima de 3 com um limite de tempo de 20 segundos.

```lisp
Jogada Humana (H - 1 1)
.___.   .   .   .   .   .

.   .   .   .   .   .   .

.   .   .   .   .   .   .

.   .   .   .   .   .   .

.   .   .   .   .   .   .

.   .   .   .   .   .   .
```
```lisp
Jogada CPU
.___.   .   .   .   .   .

.   .   .   .   .   .   .

.   .   .   .   .   .   .

.   .   .   .   .   .   .

.   .   .   .   .   .   .
                        |
.   .   .   .   .   .   .
```
```lisp
Jogada Humana (H - 2 1)
.___.   .   .   .   .   .

.___.   .   .   .   .   .

.   .   .   .   .   .   .

.   .   .   .   .   .   .

.   .   .   .   .   .   .
                        |
.   .   .   .   .   .   .
```
```lisp
Jogada CPU
.___.   .   .   .   .   .

.___.   .   .   .   .   .

.   .   .   .   .   .   .

.   .   .   .   .   .   .
                        |
.   .   .   .   .   .   .
                        |
.   .   .   .   .   .   .
```
```lisp
Jogada Humana (V - 1 1)
.___.   .   .   .   .   .
|
.___.   .   .   .   .   .

.   .   .   .   .   .   .

.   .   .   .   .   .   .
                        |
.   .   .   .   .   .   .
                        |
.   .   .   .   .   .   .
```
```lisp
Jogada CPU (Fechou uma caixa e colocou mais uma aresta)
.___.   .   .   .   .   .
|   |
.___.   .   .   .   .   .

.   .   .   .   .   .   .
                        |
.   .   .   .   .   .   .
                        |
.   .   .   .   .   .   .
                        |
.   .   .   .   .   .   .
```

| Jogadas  | Nós Analisados | Cortes Alfa | Cortes Beta | Execução em segundos | Caixa Fechada |
| :-: | :-: | :-: | :-: | :-: | :-: |
| 1 |   5322  |  71    |  69    | 3      | ❌
| 2 |   22841  |  208    |  67    | 10      | ❌
| 3 |   50233  |  758    |  14    | 20      | ✔️
| 4 |   151  |  64    |  0    | 0      | ❌

<p>Podemos analisar que o computador demora bastante tempo e analisa varios nós com o objetivo de fechar uma caixa. Após fechar a caixa ele coloca a próxima aresta instataneamente.</p>

<br>
<br>
<br>

# Projeto Nº2: Época Normal 


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
4. Interface da Aplicação
5. Output


# Introdução

Este documento servirá de manual de utilizador, guiará a instalação de todo o software necessario e de como utiliza-lo.



# Instalação

É necessario instalar um compilador de LISP e o respetivo editor, recomendamos o LispWorks.

Lispworks é uma Integrated cross-platform development tool for Common Lisp  [aqui](http://www.lispworks.com/products/lispworks.html)

A estrutura do projeto está organizada em 5 ficheiros:

- jogo.lisp - Interação com o utilizador, escrita e leitura de ficheiros.
- puzzle.lisp - Implementação da resolução do problema incluindo seletores, operadores heuristicas e outras funcõess auxiliares.
- algoritmo.lisp - Implementação do algoritmo AlfaBeta FailHard.
- log.dat - Ficheiro com os resultados do jogo.

# Interface da Aplicação

## Escolher profundidade

No menu princial terá a hipotese de a profundidade máxima do AlfaBeta.
```
|----------------------------------------------|
|                                              |
|         Defina a profundidade máxima         |
|                                              |
|                Entre 2 - 10                  |
|                                              |
|----------------------------------------------|
```

## Escolher tempo limite

Neste menu deverá escolher o tempo maximo que o CPU tem pra efetuar a sua jogada.
```
|----------------------------------------------| 
|                                              |
|    Defina o tempo limite para o computador   |
|                                              |
|                Entre 1 - 20                  |
|                                              |
|----------------------------------------------|
```

## Escolher modo de Jogo

Neste menu deverá escolher o modo de jogo.
```
|----------------------------------------------| 
|                                              |
|          Selecione o modo de Jogo            |
|                                              |
|           1 - Humano-Computador              |
|         2 - Computador-Computador            |
|                                              |
|----------------------------------------------|
```

## Modo de Jogo Humano-Computador

Neste menu deverá escolher qual será o primeiro jogador a colocar uma aresta.
```
|----------------------------------------------| 
|                                              |
|        Selecione o primeiro Jogador          |
|                                              |
|                 1 - Humano                   |
|               2 - Computador                 |
|                                              |
|----------------------------------------------|
```

# Output

## CONSOLE OUTPUT
O resultado é mostrado como:

```
.___.___.___.___.___.___.
|   |   |   |   |   |   |
.___.___.___.___.___.___.
|   |   |   |   |   |   |
.___.___.___.___.___.___.
|   |   |   |   |   |   |
.___.___.___.___.___.___.
|   |   |   |   |   |   |
.___.___.___.___.___.___.
|   |   |   |   |   |   |
.___.___.___.___.___.___.
(15 15)
```
15 Pontos para o Jogador 1, 15 Pontos para o Jogador 2


## FILE OUTPUT
 O output do ficheiro será uma lista constituida pelo nó estado, nós analisados, nnumero de cortes alfa, numero de cortes beta e tempo de execução.

Exemplo problema de ouput CPU-CPU
```
JOGADOR1 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)
            (0 0 0 0 0) (0 0 0 0 1)))
          141 0 70 0) 
JOGADOR2 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)
            (0 0 0 0 0) (0 0 0 2 1)))
          139 0 69 0) 
JOGADOR1 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)
            (0 0 0 0 0) (0 0 1 2 1)))
          137 0 68 0) 
JOGADOR2 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)
            (0 0 0 0 0) (0 2 1 2 1)))
          135 0 67 1) 
JOGADOR1 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)
            (0 0 0 0 0) (1 2 1 2 1)))
          133 0 66 0) 
JOGADOR2 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)
            (0 0 0 0 2) (1 2 1 2 1)))
          131 0 65 0) 
JOGADOR1 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)
            (0 0 0 1 2) (1 2 1 2 1)))
          193 0 63 0) 
JOGADOR2 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)
            (0 0 2 1 2) (1 2 1 2 1)))
          127 0 63 0) 
JOGADOR1 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)
            (0 1 2 1 2) (1 2 1 2 1)))
          187 0 61 0) 
JOGADOR2 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)
            (2 1 2 1 2) (1 2 1 2 1)))
          245 0 59 0) 
JOGADOR1 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          241 0 58 1) 
JOGADOR2 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          237 0 57 0) 
JOGADOR1 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          233 0 56 0) 
JOGADOR2 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          229 0 55 0) 
JOGADOR1 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          225 0 54 0) 
JOGADOR2 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          221 0 53 0) 
JOGADOR1 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          217 0 52 0) 
JOGADOR2 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          213 0 51 1) 
JOGADOR1 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          209 0 50 0) 
JOGADOR2 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          205 0 49 0) 
JOGADOR1 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          201 0 48 0) 
JOGADOR2 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          197 0 47 0) 
JOGADOR1 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 0 0) (0 0 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          193 0 46 0) 
JOGADOR2 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 0 0) (0 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          189 0 45 0) 
JOGADOR1 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 0 0) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          185 0 44 0) 
JOGADOR2 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 0 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          181 0 43 0) 
JOGADOR1 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 0 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          177 0 42 0) 
JOGADOR2 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 0 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          173 0 41 0) 
JOGADOR1 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (0 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          169 0 40 0) 
JOGADOR2 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 0) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          165 0 39 0) 
JOGADOR1 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 0 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          161 0 38 0) 
JOGADOR2 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 0 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          157 0 37 0) 
JOGADOR1 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 0 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          153 0 36 0) 
JOGADOR2 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((0 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          149 0 35 0) 
JOGADOR1 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 0))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          145 0 34 0) 
JOGADOR2 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 0) (0 0 0 0 0 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          106 0 34 0) 
JOGADOR1 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
            (0 0 0 0 0 1) (0 0 0 0 0 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          137 0 32 0) 
JOGADOR1 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 1)
            (0 0 0 0 0 1) (0 0 0 0 0 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          133 0 31 0) 
JOGADOR1 ((((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 1) (0 0 0 0 0 1)
            (0 0 0 0 0 1) (0 0 0 0 0 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          129 0 30 0) 
JOGADOR1 ((((0 0 0 0 0 0) (0 0 0 0 0 1) (0 0 0 0 0 1) (0 0 0 0 0 1)
            (0 0 0 0 0 1) (0 0 0 0 0 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          125 0 29 0) 
JOGADOR1 ((((0 0 0 0 0 1) (0 0 0 0 0 1) (0 0 0 0 0 1) (0 0 0 0 0 1)
            (0 0 0 0 0 1) (0 0 0 0 0 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          121 0 28 0) 
JOGADOR1 ((((0 0 0 0 0 1) (0 0 0 0 0 1) (0 0 0 0 0 1) (0 0 0 0 0 1)
            (0 0 0 0 0 1) (0 0 0 0 1 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          88 0 28 0) 
JOGADOR2 ((((0 0 0 0 0 1) (0 0 0 0 0 1) (0 0 0 0 0 1) (0 0 0 0 0 1)
            (0 0 0 0 2 1) (0 0 0 0 1 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          113 0 26 0) 
JOGADOR2 ((((0 0 0 0 0 1) (0 0 0 0 0 1) (0 0 0 0 0 1) (0 0 0 0 2 1)
            (0 0 0 0 2 1) (0 0 0 0 1 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          109 0 25 0) 
JOGADOR2 ((((0 0 0 0 0 1) (0 0 0 0 0 1) (0 0 0 0 2 1) (0 0 0 0 2 1)
            (0 0 0 0 2 1) (0 0 0 0 1 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          105 0 24 0) 
JOGADOR2 ((((0 0 0 0 0 1) (0 0 0 0 2 1) (0 0 0 0 2 1) (0 0 0 0 2 1)
            (0 0 0 0 2 1) (0 0 0 0 1 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          101 0 23 0) 
JOGADOR2 ((((0 0 0 0 2 1) (0 0 0 0 2 1) (0 0 0 0 2 1) (0 0 0 0 2 1)
            (0 0 0 0 2 1) (0 0 0 0 1 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          97 0 22 0) 
JOGADOR2 ((((0 0 0 0 2 1) (0 0 0 0 2 1) (0 0 0 0 2 1) (0 0 0 0 2 1)
            (0 0 0 0 2 1) (0 0 0 2 1 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          70 0 22 0) 
JOGADOR1 ((((0 0 0 0 2 1) (0 0 0 0 2 1) (0 0 0 0 2 1) (0 0 0 0 2 1)
            (0 0 0 1 2 1) (0 0 0 2 1 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          89 0 20 0) 
JOGADOR1 ((((0 0 0 0 2 1) (0 0 0 0 2 1) (0 0 0 0 2 1) (0 0 0 1 2 1)
            (0 0 0 1 2 1) (0 0 0 2 1 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          85 0 19 0) 
JOGADOR1 ((((0 0 0 0 2 1) (0 0 0 0 2 1) (0 0 0 1 2 1) (0 0 0 1 2 1)
            (0 0 0 1 2 1) (0 0 0 2 1 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          81 0 18 0) 
JOGADOR1 ((((0 0 0 0 2 1) (0 0 0 1 2 1) (0 0 0 1 2 1) (0 0 0 1 2 1)
            (0 0 0 1 2 1) (0 0 0 2 1 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          77 0 17 0) 
JOGADOR1 ((((0 0 0 1 2 1) (0 0 0 1 2 1) (0 0 0 1 2 1) (0 0 0 1 2 1)
            (0 0 0 1 2 1) (0 0 0 2 1 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          73 0 16 0) 
JOGADOR1 ((((0 0 0 1 2 1) (0 0 0 1 2 1) (0 0 0 1 2 1) (0 0 0 1 2 1)
            (0 0 0 1 2 1) (0 0 1 2 1 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          52 0 16 0) 
JOGADOR2 ((((0 0 0 1 2 1) (0 0 0 1 2 1) (0 0 0 1 2 1) (0 0 0 1 2 1)
            (0 0 2 1 2 1) (0 0 1 2 1 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          65 0 14 0) 
JOGADOR2 ((((0 0 0 1 2 1) (0 0 0 1 2 1) (0 0 0 1 2 1) (0 0 2 1 2 1)
            (0 0 2 1 2 1) (0 0 1 2 1 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          61 0 13 0) 
JOGADOR2 ((((0 0 0 1 2 1) (0 0 0 1 2 1) (0 0 2 1 2 1) (0 0 2 1 2 1)
            (0 0 2 1 2 1) (0 0 1 2 1 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          57 0 12 0) 
JOGADOR2 ((((0 0 0 1 2 1) (0 0 2 1 2 1) (0 0 2 1 2 1) (0 0 2 1 2 1)
            (0 0 2 1 2 1) (0 0 1 2 1 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          53 0 11 0) 
JOGADOR2 ((((0 0 2 1 2 1) (0 0 2 1 2 1) (0 0 2 1 2 1) (0 0 2 1 2 1)
            (0 0 2 1 2 1) (0 0 1 2 1 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          49 0 10 0) 
JOGADOR2 ((((0 0 2 1 2 1) (0 0 2 1 2 1) (0 0 2 1 2 1) (0 0 2 1 2 1)
            (0 0 2 1 2 1) (0 2 1 2 1 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          34 0 10 0) 
JOGADOR1 ((((0 0 2 1 2 1) (0 0 2 1 2 1) (0 0 2 1 2 1) (0 0 2 1 2 1)
            (0 1 2 1 2 1) (0 2 1 2 1 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          41 0 8 0) 
JOGADOR1 ((((0 0 2 1 2 1) (0 0 2 1 2 1) (0 0 2 1 2 1) (0 1 2 1 2 1)
            (0 1 2 1 2 1) (0 2 1 2 1 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          37 0 7 0) 
JOGADOR1 ((((0 0 2 1 2 1) (0 0 2 1 2 1) (0 1 2 1 2 1) (0 1 2 1 2 1)
            (0 1 2 1 2 1) (0 2 1 2 1 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          33 0 6 0) 
JOGADOR1 ((((0 0 2 1 2 1) (0 1 2 1 2 1) (0 1 2 1 2 1) (0 1 2 1 2 1)
            (0 1 2 1 2 1) (0 2 1 2 1 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          29 0 5 0) 
JOGADOR1 ((((0 1 2 1 2 1) (0 1 2 1 2 1) (0 1 2 1 2 1) (0 1 2 1 2 1)
            (0 1 2 1 2 1) (0 2 1 2 1 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          25 0 4 0) 
JOGADOR1 ((((0 1 2 1 2 1) (0 1 2 1 2 1) (0 1 2 1 2 1) (0 1 2 1 2 1)
            (0 1 2 1 2 1) (1 2 1 2 1 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          16 0 4 0) 
JOGADOR2 ((((0 1 2 1 2 1) (0 1 2 1 2 1) (0 1 2 1 2 1) (0 1 2 1 2 1)
            (2 1 2 1 2 1) (1 2 1 2 1 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          17 0 2 0) 
JOGADOR2 ((((0 1 2 1 2 1) (0 1 2 1 2 1) (0 1 2 1 2 1) (2 1 2 1 2 1)
            (2 1 2 1 2 1) (1 2 1 2 1 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          7 0 3 0) 
JOGADOR2 ((((0 1 2 1 2 1) (0 1 2 1 2 1) (2 1 2 1 2 1) (2 1 2 1 2 1)
            (2 1 2 1 2 1) (1 2 1 2 1 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          5 0 2 0) 
JOGADOR2 ((((0 1 2 1 2 1) (2 1 2 1 2 1) (2 1 2 1 2 1) (2 1 2 1 2 1)
            (2 1 2 1 2 1) (1 2 1 2 1 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          3 0 1 0) 
JOGADOR2 ((((2 1 2 1 2 1) (2 1 2 1 2 1) (2 1 2 1 2 1) (2 1 2 1 2 1)
            (2 1 2 1 2 1) (1 2 1 2 1 2))
           ((1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1) (2 1 2 1 2) (1 2 1 2 1)
            (2 1 2 1 2) (1 2 1 2 1)))
          1 0 0 0) 
```