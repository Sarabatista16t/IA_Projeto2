;; ===============================================================================================================================
;; ========================================= CÓDIGO REFERENTE AO JOGO DO CAVALO ================================================== 
;; ===============================================================================================================================
;;;; jogo.lisp
;;;; Disciplina de IA - 2019 / 2020
;;;; Projeto 2
;;;; Autores: Sara Batista [nº 170221054] e Carolina Castilho [nº 180221071]


;;======================================================================================
;;                                 ESTRUTURA DO NÓ 
;;======================================================================================        
;;           ESTADO (TABULEIRO + PONTUAÇÃO_CAVALO_BRANCO + PONTUAÇÃO_CAVALO_PRETO + JOGADOR + profundidade)
;;           F 
;;    NÓ =>  G 
;;           H
;;           NÓ-PAI





;;; ======================
;;;    CONSTRUTOR
;;; ======================
(defun cria-no (estado &optional (g 0))
  (list estado g)
)

(defun operadores ()
 "Cria uma lista com todos os operadores do problema"
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

(defun no-pontos-cavalo-branco(estado)
  (second estado)
)

(defun no-pontos-cavalo-preto(estado)
  (third estado)
)

(defun no-jogador(estado)
  (fourth estado)
)

(defun no-g(no)
  (second no)
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
 ((equal numero nil) nil)
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
(defun posicao-cavalo (tabuleiro jogador &optional (indiceL 0) (indiceC 0))
    (cond
     ((null tabuleiro) nil)
     ((equal (celula 0 0 tabuleiro) jogador) (list indiceL indiceC))
     (t 
      (cond
       ((null (car tabuleiro)) (posicao-cavalo (cdr tabuleiro) jogador (+ indiceL 1) 0))
       (t (posicao-cavalo (cons (cdr (car tabuleiro)) (cdr tabuleiro)) jogador indiceL (+ indiceC 1)))))))



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

(defun novo-sucessor (no operador)
  (cond 
   ((null operador) nil)
   (t (let ((estado-no (funcall operador (no-estado no))))
        (cond
         ((equal estado-no nil) nil)
         (t (cria-no estado-no (+ (no-g no) 1)))))
   )
))


(defun sucessores (no operadores)
  (let ((sucessor (novo-sucessor no (car operadores))))
   (cond
     ((null operadores) nil)

     ((equal sucessor nil) (sucessores no (rest operadores)))

     (t (cons (novo-sucessor no (car operadores)) (sucessores no (rest operadores))))
    )
   )
)


;;; ========================
;;;  OPERADORES DO PROBLEMA
;;; ========================

(defparameter cavalo-branco -1)
(defparameter cavalo-preto -2)

(defun jogador-oposto (jogador)
  (cond
   ((equal jogador -2) -1)
   ((equal jogador -1) -2)))

(defun operador-default (estado L C)
  (cond 
   ((and (equal (+ (first (posicao-cavalo (first estado) (fourth estado))) L) nil) (equal (+ (second (posicao-cavalo (first estado) (fourth estado))) C) nil)) nil)

   (t
    (let* ((tabuleiro (first estado))
           (valor-cavalo (celula (+ (first (posicao-cavalo tabuleiro (fourth estado))) L) (+ (second (posicao-cavalo tabuleiro (fourth estado))) C) tabuleiro)))
      (cond 
       ((null valor-cavalo) nil)
       ((equal valor-cavalo (jogador-oposto (fourth estado))) nil)
       (t 
        (let* (
               (posicao (posicao-cavalo tabuleiro (fourth estado)))
               (simetrico (procura-celula valor-cavalo tabuleiro))
               (linha-celula-simetrico (first simetrico))
               (coluna-celula-simetrico (second simetrico))
               (tabuleiro-nil (substituir (first posicao) (second posicao) tabuleiro))
               (tabuleiro-cavalo (substituir (+ (first posicao) L) (+ (second posicao) C) tabuleiro-nil (fourth estado)))
               (duplo (procura-duplo tabuleiro-cavalo))
               (linha-celula-duplo (first duplo))
               (coluna-celula-duplo (second duplo))
               (tabuleiro-nil (substituir (first posicao) (second posicao) tabuleiro))
               )
          (cond
           ((equal (fourth estado) -1)
            (cond
             ((> (+ (first posicao) L) (length tabuleiro)) nil)
             ((> (+ (second posicao) C) (length (car tabuleiro))) nil)
             ((verifica-duplo valor-cavalo) (list (substituir linha-celula-duplo coluna-celula-duplo tabuleiro-cavalo) (+ (second estado) valor-cavalo) (third estado) -2))
             ((or (null linha-celula-simetrico) (null coluna-celula-simetrico)) (list (substituir (+ (first posicao) L) (+ (second posicao) C) tabuleiro-nil (fourth estado)) (+ (second estado) valor-cavalo) (third estado) -2))
             (t (list (substituir linha-celula-simetrico coluna-celula-simetrico tabuleiro-cavalo) (+ (second estado) valor-cavalo) (third estado) -2))))
           ((equal (fourth estado) -2) 
            (cond
             ((> (+ (first posicao) L) (length tabuleiro)) nil)
             ((> (+ (second posicao) C) (length (car tabuleiro))) nil)
             ((verifica-duplo valor-cavalo) (list (substituir linha-celula-duplo coluna-celula-duplo tabuleiro-cavalo) (second estado) (+ (third estado) valor-cavalo) -1))
             ((or (null linha-celula-simetrico) (null coluna-celula-simetrico)) (list (substituir (+ (first posicao) L) (+ (second posicao) C) tabuleiro-nil (fourth estado)) (second estado) (+ (third estado) valor-cavalo) -1))
             (t (list (substituir linha-celula-simetrico coluna-celula-simetrico tabuleiro-cavalo) (second estado) (+ (third estado) valor-cavalo) -1))))
           (T nil)))))))))


(defun operador-1 (estado)
  (operador-default estado 2 (- 1)))

(defun operador-2 (estado)
  (operador-default estado 2 1 ))

(defun operador-3 (estado)
  (operador-default estado 1 2 ))

(defun operador-4 (estado)
  (operador-default estado (- 1) 2))

(defun operador-5 (estado)
  (operador-default estado (- 2) 1 ))

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


;;======================================================================================
;;                "OPERADOR 0" (posicionar os cavalos no inicio do jogo)
;;======================================================================================        

(defun encontrar-maximo-lista (lista &optional (maximo 0))
  "Função que devolve o valor máximo da lista"
  (cond
      ((null lista) maximo)
      ((equal (first lista) nil) (encontrar-maximo-lista (rest lista) maximo))
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



; NOVA ABORDAGEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEM

(defun colocar-cavalo-branco (tabuleiro)
  "Função que coloca o cavalo branco na posição com maior valor da linha 1"
  (substituir 0 (obter-indice-do-valor-maximo (linha 0 tabuleiro)) tabuleiro -1)
)

(defun pontuação-inicial-cavalo-branco (tabuleiro)
  (celula 0 (obter-indice-do-valor-maximo (linha 0 tabuleiro)) tabuleiro)
)

(defun colocar-cavalo-preto (tabuleiro)
  "Função que coloca o cavalo preto na posição com maior valor da linha 10"
  (substituir 9 (obter-indice-do-valor-maximo (linha 9 tabuleiro)) tabuleiro -2)
)

(defun pontuação-inicial-cavalo-preto (tabuleiro)
  (celula 9 (obter-indice-do-valor-maximo (linha 9 tabuleiro)) tabuleiro)
)

(defun posicionar-jogadores-no-tabuleiro (tabuleiro jogador)
  "Função que cria o nó inicial com o tabuleiro que é lido do ficheiro e aplica as funções para colocar os cavalos no inicio do jogo"
  
  ; jogador 1  =>  -1  =>   cavalo branco
  ; jogador 2  =>  -2  =>   cavalo preto
 
  (list (colocar-cavalo-preto (colocar-cavalo-branco tabuleiro)) (pontuação-inicial-cavalo-branco tabuleiro) (pontuação-inicial-cavalo-preto tabuleiro) jogador)
  )


;;======================================================================================
;;                                 JOGAR 
;;======================================================================================        

; Constantes

(let ((*profundidade* 5))) ; tempo máximo para o computador jogar (milisegundos)


(defun jogar (no tempo primeiro-jogador) 
  (calcular-profundidade tempo) ; Calcula a profundidade máxima com base no tempo limite para a jogada do computador
  (cond 
     ; alternar entre os jogadores
   ((equal primeiro-jogador -1) (jogada-humano no -1))
   (t (jogada-computador no -1))
   )
  )


;;======================================================================================
;;                                 JOGADA HUMANO
;;======================================================================================        


(defun jogada-humano (no jogador)
  (cond
   ((equal (tabuleiro-final no) T) (vencedor no) nil)
   (t 
    (escrever-estado t (no-estado no)) ; Escreve o tabuleiro e as pontuações
    (let* ((posicoes (posicoes-validas no))
           (estado (no-estado no)))
      (escreve-posicoes-validas posicoes)
    
      (cond 
       ((and (equal jogador -1) (equal posicoes nil)) (jogada-computador (list (list (no-tabuleiro estado) (no-pontos-cavalo-branco estado) (no-pontos-cavalo-preto estado) (jogador-oposto jogador)) 0) -2))
       ((and (equal jogador -2) (equal posicoes nil)) (jogada-computador (list (list (no-tabuleiro estado) (no-pontos-cavalo-branco estado) (no-pontos-cavalo-preto estado) (jogador-oposto jogador)) 0) -1))
       (t 
        (format t "~%~%Introduza a linha e a coluna onde pretende colocar a peça: ~%")
        (format t "<letra> <numero de 1 a 10> ~%")
        (let* ((numero (read))
               (linha (floor numero 10))
               (coluna (mod numero 10))
               )
         
          (cond
          
           ((and (equal jogador -1) (movimento-válido linha coluna posicoes (no-tabuleiro estado)) (let ((jogada_aux1 (list (colocar-peca linha coluna estado) 0))) 
                                                                                                     (jogada-computador jogada_aux1 -2) ; manda a jogada ao adversário
                                                                                                     )))
           ((and (equal jogador -2) (movimento-válido linha coluna posicoes (no-tabuleiro estado)) (let ((jogada_aux2 (list (colocar-peca linha coluna estado) 0)))
                                                                                                     (jogada-computador jogada_aux2 -1) ; manda a jogada ao adversário
                                                                                                     )))
           (t  (format t "Movimento inválido! Escolha outra posição. ~%") (jogada-humano no jogador))
           )
          )
        ))))))


(defun colocar-peca (linha coluna estado) 
    "mover a peça, alterar as pontuaçoes"
    (let* ((posicao-atual (posicao-cavalo (no-tabuleiro estado) (no-jogador estado)))
           (linha-atual (first posicao-atual))
           (coluna-atual (second posicao-atual))
           (indiceL (- linha linha-atual))
           (indiceC (- coluna coluna-atual)))
      (operador-default estado indiceL indiceC)))
    

(defun posicoes-validas (no) 
  "Devolve todas as posições válidas para mover o cavalo do utilizador"
 (obter-posicoes-validas (sucessores no (operadores))))

(defun escreve-posicoes-validas (posicoes-validas)
  (cond 
   ((null posicoes-validas) (format  t "~%Ups... Não há jogadas possíveis...:( ~%"))
   (t (format t "~%Jogadas Possiveis: ~%~A" posicoes-validas))))

(defun obter-posicoes-validas (possiveis-sucessores)
  "Devolve uma lista com os indices das posições possíveis"
  (let* ((first (first possiveis-sucessores))
         (estado (no-estado first)))
  (cond
   ((null possiveis-sucessores) nil)
   (t (cons (posicao-cavalo (no-tabuleiro estado) (jogador-oposto (no-jogador estado))) (obter-posicoes-validas (rest possiveis-sucessores))))
   )
  ))

(defun tabuleiro-final (no)
  (let* ((estado (no-estado no))
        (sucessores-um (obter-posicoes-validas (sucessores no (operadores))))
        (sucessores-dois (obter-posicoes-validas (sucessores (list (list (no-tabuleiro estado) (no-pontos-cavalo-branco estado) (no-pontos-cavalo-preto estado) (jogador-oposto (no-jogador estado))) 0) (operadores)))))
    (cond
     ((and (null sucessores-um) (null sucessores-dois)) T)
     (t nil))))

(defun vencedor (no)
  (let ((estado (no-estado no)))
    (cond
     ((> (no-pontos-cavalo-branco estado) (no-pontos-cavalo-preto estado)) (format t "Vencedor: Cavalo Branco ~%"))
     ((< (no-pontos-cavalo-branco estado) (no-pontos-cavalo-preto estado)) (format t "Vencedor: Cavalo Preto ~%"))
     (t (format t "Empate! ~%")))))

(defun verificar-operador-possivel (posicoes-validas linha coluna)
  (cond
   ((null posicoes-validas) nil)
   ((and (equal linha (first (first posicoes-validas))) (equal coluna (second (first posicoes-validas)))) t)
   (t (verificar-operador-possivel (rest posicoes-validas) linha coluna))))


(defun movimento-válido (linha coluna posicoes-validas tabuleiro)
  "Verifica se é possível aplicar algum operador de modo a colocar o cavalo na prosição que o utilizador pretende"

  (cond
   ((or (< coluna 0) (< linha 0) (>= linha (length tabuleiro)) (>= coluna (length (car tabuleiro)))) nil) ; verifica se está dentro dos limites do tabuleiro
   ((verificar-operador-possivel posicoes-validas linha coluna) t) 
   (t nil)
   )
  )


;;======================================================================================
;;                                 JOGADA COMPUTADOR
;;======================================================================================        


(defun jogada-computador (no jogador)
  (cond
   ((equal (tabuleiro-final no) T) (vencedor no) nil)
   (t 
  (let ((estado (no-estado no)))
    (cond
     ((equal jogador -1) (let ((jogada_aux (negamax-inicial no 2 1)))
                           (cond 
                            ((null jogada_aux) (jogada-humano (list (list (no-tabuleiro estado) (no-pontos-cavalo-branco estado) (no-pontos-cavalo-preto estado) (jogador-oposto jogador)) 0) -2))
                            (t (jogada-humano jogada_aux -2) ; manda a jogada ao adversário
                               ))))
         
     ((equal jogador -2) (let* ((jogada_aux2 (negamax-inicial no 2 1)))
                           (cond 
                            ((null jogada_aux2) (jogada-humano (list (list (no-tabuleiro estado) (no-pontos-cavalo-branco estado) (no-pontos-cavalo-preto estado) (jogador-oposto jogador)) 0) -1))
                            (t (jogada-humano jogada_aux2 -1)) ; manda a jogada ao adversário
                            )))
     (t nil)
     )
    ))))

(defun calcular-profundidade (tempo)
  ;(alterar-profundidade (/ (* (/ tempo 2) 10) "tempo"))
)


;;======================================================================================
;;                                 FUNÇÕES DE AVALIAÇÃO
;;======================================================================================        

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



;;======================================================================================
;;                                 ESCRITA DOS DADOS
;;======================================================================================        


(defun escrever-tabuleiro (f tabuleiro)
  (format f "~%Tabuleiro: ~%~A" tabuleiro)
  )


(defun escrever-pontuacao-cavalo-branco (f pontos)
  (format f "~%Pontuação cavalo branco: ~%~A" pontos)
  )

(defun escrever-pontuacao-cavalo-preto (f pontos)
  (format f "~%Pontuação cavalo preto: ~%~A" pontos)
  )

(defun escrever-estado (f estado)
  (escrever-tabuleiro f (no-tabuleiro estado))
  (escrever-pontuacao-cavalo-branco f (no-pontos-cavalo-branco estado))
  (escrever-pontuacao-cavalo-preto f (no-pontos-cavalo-preto estado))
  )

(defun no-teste ()
 '((((42 65 63 -1 73 78 5 32 68 61) (86 30 82 80 28 75 45 50 81 90) (59 24 47 9 67 22 56 77 60 99) (41 62 18 85 76 27 13 94 93 70) (14 54 0 71 35 39 92 38 49 52) (83 11 46 6 48 15 23 88 96 8) (25 84 2 89 7 53 66 31 20 12) (37 19 3 36 10 44 64 16 97 33) (40 69 95 29 4 51 34 1 26 21) (72 79 -2 43 74 55 58 57 91 17)) 87 98 -1) 0))


