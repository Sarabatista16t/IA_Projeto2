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

(defun no-f(no)
  (second no)
)

(defun no-g(no)
  (third no)
)

(defun no-h(no)
  (fourth no)
)

(defun no-pai(no)
  (fifth no)
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


(defun no-solucaop (no algoritmo)
  (cond 
    ((equal (sucessores no (operadores) algoritmo) nil) t)
    (t nil)))


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

(defun encontrar-maximo-lista (lista &optional (maximo (first lista)))
  "Função que devolve o valor máximo da lista"
  (cond
      ((null lista) maximo)
      ((> (first lista) maximo) (encontrar-maximo-lista (rest lista) (first lista)))
      (t (encontrar-maximo-lista (rest lista) maximo))
  )
)

(defun obter-indice-do-valor-maximo (lista &optional (count 0))
  (cond
     ((null lista) nil)
     ((equal (first lista) (encontrar-maximo-lista lista)) count)
     (t (obter-indice-do-valor-maximo (rest lista) (1+ count)))
  )
)

(defun colocar-cavalo-branco (tabuleiro)
  "Função que coloca o cavalo branco na posição com maior valor da linha 1"
  (substituir 0 (obter-indice-do-valor-maximo (linha 1 tabuleiro)) tabuleiro T)
)

(defun colocar-cavalo-preto (tabuleiro)
  "Função que coloca o cavalo preto na posição com maior valor da linha 10"
  (substituir 9 (obter-indice-do-valor-maximo (linha 9 tabuleiro)) tabuleiro T)
)

(defun tabuleiro-teste ()
  '(
    (02 20 44 1 4 5 66 88 99 2 )
    (02 20 44 1 4 5 66 88 99 2 )
    (02 20 44 1 4 5 66 88 99 2 )
    (02 20 44 1 4 5 66 88 99 2 )
    (02 20 44 1 4 5 66 88 99 2 )
    (02 20 44 1 4 5 66 88 99 2 )
    (02 20 44 1 4 5 66 88 99 2 )
    (02 20 44 1 4 5 66 88 99 2 )
    (02 20 44 1 4 5 66 88 99 2 )
    (02 20 44 1 4 5 66 88 99 2 )
    )
)

(defun criar-no-inicial ()
  
)


(defun jogada-humano ()
; 1) pede a linha e coluna onde quer colocar a peça
; 2) Verificação se é possivel aplicar algum operador à peça para a colocar no local indicado
; 3) Se der, atualizar o estado (posicionar o cavalo, somar os pontos) e chamar a jogada do computador
; 4) Se não der, devolver o estado sem alterações e chamar a jogada do computador
  (cond 
   () ;escrever esta dica no ecrã, antes de pedir a jogada: <letra> <numero de 1 a 10>
; escrever o tabuleiro antes 
   )
)

(defun jogada-computador ()
; 1) aplica o Negamax e escolhe a melhor posição para colocar o cavalo
; 2) Se der, atualiza o estado (posicionar o cavalo, somar os pontos), devolve-o e chama a jogada do humano
; 4) Se não der para mover o cavalo, devolve o estado sem alterações e chama a jogada do computador
  (cond 
   ()
   )
)

(defun jogar (estado tempo) ; Pedido no enunciado
  ; alternar entre os jogadores
)

(defun funcao-avaliacao-nos-folha (pontos-jogador1 pontos-jogador2) ; (jogada => estado (tabuleiro + pontuação) + )
  (cond
   ((> pontos-jogador1 pontos-jogador2) 1) ; ganha jogador 1 / perde jogador 2
   ((equal pontos-jogador1 pontos-jogador2) 0) ; empate
   (t -1) ; perde jogador 1 / ganha jogador 2
   )
)

(defun funcao-avaliacao-pseudo-nos-folha (pontos-jogador1 pontos-jogador2)
  ; max pontos = 5050
  (/ (* 0.99 (- pontos-jogador1 pontos-jogador2)) 5050)
)


