;; ===============================================================================================================================
;; ========================================= CÓDIGO REFERENTE AO JOGO DO CAVALO ================================================== 
;; ===============================================================================================================================
;;;; jogo.lisp
;;;; Disciplina de IA - 2019 / 2020
;;;; Projeto 2
;;;; Autores: Sara Batista [nº 170221054] e Carolina Castilho [nº 180221078]


;;======================================================================================
;;                                 ESTRUTURA DO NÓ 
;;======================================================================================        
;;           ESTADO (TABULEIRO + PONTUAÇÃO)
;;           F 
;;    NÓ =>  G 
;;           H
;;           NÓ-PAI




;;; ======================
;;;    FUNÇÃO DE TESTE
;;; ======================

(defun no-teste (tabuleiro)
"Define um no teste do projeto em que T=tabuleiro e P=pontos, profundidade=0 e pai=NIL"
 (list '((funcall tabuleiro) 0) 0 nil)
 ; chamar a funcao que retorna um tabuleiro  
 ; tem 0 pontos no inicio
) 

;;; ======================
;;;    CONSTRUTOR
;;; ======================
(defun cria-no (estado &optional (g 0) (pai nil))
  (list estado g pai)
)

(defun criar-no-inicial (tabuleiro)
  (list (list tabuleiro 0) 0 nil)
)

(defun operadores ()
 "Cria uma lista com todos os operadores do problema das vasilhas."
 (list 'operador-1 'operador-2 'operador-3 'operador-4 'operador-5 'operador-6 'operador-7 'operador-8))



;;; ========================================================
;;;                    MÉTODOS SELETORES
;;; ========================================================


;;; ======================
;;;   Vocabulário LISP 
;;; ======================

(defun no-estado(no)
  (first no)
)

(defun no-tabuleiro(estado)
  (first estado)
)

(defun no-pontos(estado)
  (second estado)
)

(defun no-profundidade(no)
  (second no)
)

(defun no-pai(no)
  (third no)
)

(defun no-f(no)
  (second no)
)

(defun no-g(no)
  (third no)
)

(defun no-h(no)
  (fourth no)
)

(defun no-profundidade-a*(no)
  (fifth no)
)

(defun no-pai-a*(no)
  (sixth no)
)


;;; ===============================================
;;;    FUNÇÕES AUXILIARES DA PROCURA
;;; ===============================================

;;; ===========================
;;;    Posições no tabuleiro
;;; ===========================

; Função que devolve a linha correspondente a um determinado indice
(defun linha (indice tabuleiro)
"Retorna uma lista que representa a linha no tabuleiro recebido"  
  (cond 
   ((equal indice 0) (car tabuleiro))
   (T (linha (- indice 1)  (cdr tabuleiro)))))


; Função que devolve a celula correspondente a um determinado indice de linha e coluna
(defun celula (n x tabuleiro)
"Retorna o valor presente na celula no tabuleiro, representada pelos dois indices"
(cond
 ((or (null n) (null x)) nil)
 ((or (< n 0) (< x 0)) nil)
 (t (nth x (nth n tabuleiro)))))


; Função que 
(defun procura-celula (valor tabuleiro &optional (indiceL 0) (indiceC 0))
  (cond
   ((null tabuleiro) nil)
   ((equal (celula 0 0 tabuleiro) (+ (* (mod valor 10) 10) (floor valor 10))) (list indiceL indiceC))
   (t 
      (cond
       ((null (car tabuleiro)) (procura-celula valor (cdr tabuleiro) (+ indiceL 1) 0))
       (t (procura-celula valor (cons (cdr (car tabuleiro)) (cdr tabuleiro)) indiceL (+ indiceC 1)))))))



(defun procura-duplo (tabuleiro &optional (indiceL 0) (indiceC 0) (indiceL-duplo (- 1)) (indiceC-duplo (- 1)) (duplo (- 1)))
  (cond
   ((null tabuleiro) (list indiceL-duplo  indiceC-duplo))
   ((null (car tabuleiro)) (procura-duplo (cdr tabuleiro) (+ indiceL 1) 0 indiceL-duplo indiceC-duplo duplo))
   ((null (car (car tabuleiro))) (procura-duplo (cons (cdr (car tabuleiro)) (cdr tabuleiro)) indiceL (+ indiceC 1) indiceL-duplo indiceC-duplo duplo))
   (t (let* ((valor-celula (car (car tabuleiro)))
             (valor-duplo (verifica-duplo valor-celula)))
         (cond
          ((and valor-duplo (> valor-celula duplo) (> valor-celula 0)) (procura-duplo (cons (cdr (car tabuleiro)) (cdr tabuleiro)) indiceL (+ indiceC 1) indiceL indiceC valor-celula)) 
          (t (procura-duplo (cons (cdr (car tabuleiro)) (cdr tabuleiro)) indiceL (+ indiceC 1) indiceL-duplo indiceC-duplo duplo)))))))
      


(defun verifica-duplo (numero)                                           
  (cond 
 ((or (equal numero nil) (equal numero T)) nil)
 ((< numero 0) nil)
 (t (equal (mod numero 10) (floor numero 10)))))


; Função que
(defun substituir-posicao (indice lista &optional (valor nil))
  (cond
   ((equal indice 0) (cons valor (rest lista)))
   (t (cons (car lista) (substituir-posicao  (- indice 1) (cdr lista) valor)))))


; Função que
(defun substituir (indiceL indiceC tabuleiro &optional (valor nil))
  (cond 
   ((equal indiceL 0) (cons (substituir-posicao indiceC (car tabuleiro) valor) (rest tabuleiro)))
   (t (cons (car tabuleiro) (substituir (- indiceL 1) indiceC (rest tabuleiro) valor)))))


; Função que
(defun posicao-cavalo (tabuleiro &optional (indiceL 0) (indiceC 0))
    (cond
     ((null tabuleiro) nil)
     ((equal (celula 0 0 tabuleiro) t) (list indiceL indiceC))
     (t 
      (cond
       ((null (car tabuleiro)) (posicao-cavalo (cdr tabuleiro) (+ indiceL 1) 0))
       (t (posicao-cavalo (cons (cdr (car tabuleiro)) (cdr tabuleiro)) indiceL (+ indiceC 1)))))))



;;; =====================================
;;;     Gerar um tabuleiro aleatório
;;; =====================================

;; Lista dos números do tabuleiro
(defun lista-numeros (&optional (n 100))
"cria uma lista com todos os nÃºmeros entre 0 (inclusive) e n (exclusive)"
  (cond
   ((equal n 1) (list 0))
   (T (cons (- n 1) (lista-numeros (- n 1))))))


;; Remover um valor que já exista no tabuleiro
(defun remover-se (pred lista)
  (cond 
   ((null lista) nil)
   ((funcall pred (car lista)) (remover-se pred (cdr lista)))
   (t (cons (car lista) (remover-se pred (cdr lista))))))


;; Baralhar a lista
(defun baralhar (lista)
  (cond
   ((null lista) nil)
   (t (let ((numero-aleatorio  (nth (random (length lista)) lista)))
              (cons numero-aleatorio (baralhar(remover-se  (lambda (x) (= x numero-aleatorio))  lista)))))))


;; Função que gera um tabuleiro aleatório
(defun tabuleiro-aleatorio (lista n)  
  (cond    
   ((null lista) nil)    
   (t (cons (subseq lista 0 n) (tabuleiro-aleatorio (subseq lista n) n)))))



;;; ======================
;;;     Funções solução
;;; ======================

(defun no-solucaop-a*(no pontuacao)
  (cond 
    ((or (>= (no-pontos no) pontuacao) (equal (sucessores (no operadores algoritmo &optional (profundidade 4))) nil)) t)
    (t nil)
  )
)

(defun no-solucaop (no algoritmo)
  (cond 
    ((equal (sucessores no (operadores) algoritmo) nil) t)
    (t nil)))

; SÓ BFS
(defun solucao-p (sucessores algoritmo)
  (cond
   ((null sucessores) nil)
   ((no-solucaop (first sucessores) algoritmo) (first sucessores))
   (t (solucao-p (rest sucessores) algoritmo))
 )
)

(defun escreve-solucao (sucessores algoritmo)
  (cond
   ((null sucessores) nil)
   ((no-solucaop (first sucessores) algoritmo) (escreve-no (first sucessores) algoritmo))
   (t (solucao-p (rest sucessores) algoritmo))
 )
)

(defun escreve-no (no)
 "Permite escrever no ecra um no do problema."
  (progn
     ;(format t "| A: ~a | B: ~a | G: ~a |~%" (vasilha-a-conteudo (no-estado no)) (vasilha-b-conteudo (no-estado no)) (no-profundidade no))   
     (format t " ~a ~%" no)
  )
)

;;; =========================
;;;     Funções sucessores
;;; =========================

;; SÓ PARA BFS E DFS

(defun novo-sucessor(no operador)
  (cond 
   ((null operador) nil)
   (t (let ((estado (funcall operador (no-estado no))))
        (cond
         ((equal estado nil) nil)
         (t (list estado (1+ (no-profundidade no)) no)))))))


(defun sucessores(no operadores algoritmo &optional (profundidade 4))
  (let ((sucessor (novo-sucessor no (car operadores))))
   (cond
     ((null operadores) nil)
     ((and (equal algoritmo "dfs") (equal (no-profundidade no) profundidade)) nil)

     ((equal sucessor nil) (sucessores no (rest operadores) algoritmo profundidade))

     (t (cons (novo-sucessor no (car operadores)) (sucessores no (rest operadores) algoritmo profundidade)))
    )
))

;;SÓ PARA BFS
(defun no-existep-bfs (no lista-nos)
  (cond
    ((null lista-nos) nil)
    ((equal (no-estado no) (no-estado (car lista-nos))) t)
    (t (no-existep-bfs no (rest lista-nos)))
  )
)

(defun sucessores-validos-bfs (sucessores fechados abertos)
  (cond
   ((null sucessores) nil)
   ((not (no-existep-bfs (first sucessores) (append abertos fechados))) (cons (first sucessores) (sucessores-validos-bfs (rest sucessores) fechados abertos)))
   (t (sucessores-validos-bfs (rest sucessores) fechados abertos))))


;;SÓ PARA DFS
(defun no-existep-dfs (no lista-nos)
  (cond
    ((null lista-nos) nil)
    ((and (equal (no-estado no) (no-estado (car lista-nos))) (> (no-profundidade no)(no-profundidade (first lista-nos)))) t)
    (t (no-existep-dfs no (rest lista-nos)))
  )
)

(defun sucessores-validos-dfs (sucessores fechados abertos)
  (cond
   ((null sucessores) nil)
   ((not (no-existep-dfs (first sucessores) (append abertos fechados))) (cons (first sucessores) (sucessores-validos-dfs (rest sucessores) fechados abertos)))
   (t (sucessores-validos-dfs (rest sucessores) fechados abertos))))


;;SÓ PARA A*
;; (....completar....)



;;; ========================
;;;  OPERADORES DO PROBLEMA
;;; ========================

(defun operador-default (estado L C)
  (let* ((tabuleiro (first estado))
        (valor-cavalo (celula (+ (first (posicao-cavalo tabuleiro)) L) (+ (second (posicao-cavalo tabuleiro)) C) tabuleiro)))
    (cond 
     ((null valor-cavalo) nil)
     (t 
      (let* (
             (posicao (posicao-cavalo tabuleiro))
             (simetrico (procura-celula valor-cavalo tabuleiro))
             (linha-celula-simetrico (first simetrico))
             (coluna-celula-simetrico (second simetrico))
             (duplo (procura-duplo tabuleiro))
             (linha-celula-duplo (first duplo))
             (coluna-celula-duplo (second duplo))
             (tabuleiro-nil (substituir (first posicao) (second posicao) tabuleiro))
             (tabuleiro-cavalo (substituir (+ (first posicao) L) (+ (second posicao) C) tabuleiro-nil T))
             )
  (cond
   ((> (+ (first posicao) L) (length tabuleiro)) nil)
   ((> (+ (second posicao) C) (length (car tabuleiro))) nil)
   ((verifica-duplo valor-cavalo) (list (substituir linha-celula-duplo coluna-celula-duplo tabuleiro-cavalo) (+ (second estado) valor-cavalo)))
   ((or (null linha-celula-simetrico) (null coluna-celula-simetrico)) (list (substituir (+ (first posicao) L) (+ (second posicao) C) tabuleiro-nil T) (+ (second estado) valor-cavalo)))
   (t (list (substituir linha-celula-simetrico coluna-celula-simetrico tabuleiro-cavalo) (+ (second estado) valor-cavalo)))))))))


(defun operador-1 (estado)
  (operador-default estado 2 (- 1)))

(defun operador-2 (estado)
  (operador-default estado 2 1))

(defun operador-3 (estado)
  (operador-default estado 1 2))

(defun operador-4 (estado)
  (operador-default estado (- 1) 2))

(defun operador-5 (estado)
  (operador-default estado (- 2) 1))

(defun operador-6 (estado)
  (operador-default estado (- 2) (- 1)))

(defun operador-7 (estado)
  (operador-default estado (- 1) (- 2)))

(defun operador-8 (estado)
  (operador-default estado 1 (- 2)))


(defun percorrer-linha0 (lista estado &optional (c 0))
  (cond
   ((null lista) nil)
   ((numberp (car lista)) (cons (list (substituir 0 c (first estado) T) (+ (second estado) (car lista))) (percorrer-linha0 (cdr lista) estado (+ c 1))))))


(defun primeiros-sucessores (lista no-inicial)
  (cond
   ((null lista) nil)
   (t (cons (list (cria-no (car lista) 1 no-inicial)) (primeiros-sucessores (cdr lista) no-inicial)))))


;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;; NOVAS FUNÇÕES

(defun colocar-cavalo-branco (linha1)
  (cond
   ((= (length linha1) 1) (first linha1))
   (t (cons (max (first linha1)) (colocar-cavalo-branco (rest linha1))))
   )
  ;(max (first tabuleiro))
)

(defun colocar-cavalo-preto (linha10)
)

(defun criar-no-inicial ()
)

