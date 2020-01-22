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
;;           ESTADO (TABULEIRO + PONTUAÇÃO_CAVALO_BRANCO + PONTUAÇÃO_CAVALO_PRETO)
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

(defun no-pontos-cavalo-branco(estado)
  (second estado)
)

(defun no-pontos-cavalo-preto(estado)
  (third estado)
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

(defun novo-sucessor (estado operador)
  (cond 
   ((null operador) nil)
   (t (let ((estado-no (funcall operador estado)))
        (cond
         ((equal estado-no nil) nil)
         (t estado-no))))
   )
)


(defun sucessores (estado operadores)
  (let ((sucessor (novo-sucessor estado (car operadores))))
   (cond
     ((null operadores) nil)

     ((equal sucessor nil) (sucessores estado (rest operadores)))

     (t (cons (novo-sucessor estado (car operadores)) (sucessores estado (rest operadores))))
    )
   )
)

(defun pre-sucessores (estado operadores)
  (cond
   ((equal (fourth estado) -1) (sucessores (list (first estado) (second estado) (third estado) -2) operadores))
   (t (sucessores (list (first estado) (second estado) (third estado) -1) operadores))
   )
)

;;; ========================
;;;  OPERADORES DO PROBLEMA
;;; ========================

(defun operador-default (estado L C)
  (cond 
   ((and (equal (+ (first (posicao-cavalo (first estado) (fourth estado))) L) nil) (equal (+ (second (posicao-cavalo (first estado) (fourth estado))) C) nil)) nil)
    
   (t
    (let* ((tabuleiro (first estado))
           (valor-cavalo (celula (+ (first (posicao-cavalo tabuleiro (fourth estado))) L) (+ (second (posicao-cavalo tabuleiro (fourth estado))) C) tabuleiro)))
      (cond 
       ((equal valor-cavalo nil) nil)
       (t 
        (let* (
               (posicao (posicao-cavalo tabuleiro (fourth estado)))
               (simetrico (procura-celula valor-cavalo tabuleiro))
               (linha-celula-simetrico (first simetrico))
               (coluna-celula-simetrico (second simetrico))
               (duplo (procura-duplo tabuleiro))
               (linha-celula-duplo (first duplo))
               (coluna-celula-duplo (second duplo))
               (tabuleiro-nil (substituir (first posicao) (second posicao) tabuleiro))
               (tabuleiro-cavalo (substituir (+ (first posicao) L) (+ (second posicao) C) tabuleiro-nil (fourth estado)))
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
  (substituir 0 (obter-indice-do-valor-maximo (linha 1 tabuleiro)) tabuleiro -1)
)

(defun colocar-cavalo-preto (tabuleiro)
  "Função que coloca o cavalo preto na posição com maior valor da linha 10"
  (substituir 9 (obter-indice-do-valor-maximo (linha 9 tabuleiro)) tabuleiro -2)
)


(defun posicionar-jogadores-no-tabuleiro (tabuleiro)
  "Função que cria o nó inicial com o tabuleiro que é lido do ficheiro e aplica as funções para colocar os cavalos no inicio do jogo"
  (list (colocar-cavalo-preto (colocar-cavalo-branco tabuleiro)) 0 0))
  ; jogador 1  =>  -1  =>   cavalo branco
  ; jogador 2  =>  -2  =>   cavalo preto


;;======================================================================================
;;                                 JOGAR 
;;======================================================================================        

; Constantes

(let ((*jogada* nil) ; tabuleiro a ser passado ao adversário, pontos cavalo-branco e pontos cavalo-preto
      (*tempo* 1)) ; tempo máximo para o computador jogar (milisegundos)

     ; *jogada*
     (defun alterar-jogada (jogada)
       (setf *jogada* jogada))

     (defun alterar-tabuleiro (tabuleiro)
       (setf *jogada* (list tabuleiro (second *jogada*) (third *jogada*))))

     (defun alterar-pontos-cavalo-branco (pontos)
       (setf *jogada* (list (first *jogada*) pontos (third *jogada*))))

     (defun alterar-pontos-cavalo-preto (pontos)
       (setf *jogada* (list (first *jogada*) (second *jogada*) pontos)))


     (defun obter-jogada ()
       *jogada*)

     (defun obter-tabuleiro ()
       (first *jogada*))

     (defun obter-pontos-cavalo-branco ()
       (second *jogada*))

     (defun obter-pontos-cavalo-preto ()
       (third *jogada*))


     ; *tempo*
     (defun alterar-tempo (tempo)
       (setf *tempo* tempo))

     (defun obter-tempo ()
       *tempo*)
)


; ATENÇÃO!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! JOGADOR 1 = -1 & jOGADOR 2 = -2 !!!!!!!!!!!!!!!!!!!!!!!!!!!




(defun jogada-humano (tabuleiro jogador)

; NOTA: escrever o tabuleiro antes 

  (format t "Introduza a linha e a coluna onde pretende colocar a peça: ~%")
  (format t "<letra> <numero de 1 a 10> ~%")
  (let* ((numero (read))
        (linha (floor numero 10))
        (coluna (mod numero 10)))
    (cond
     ((and (equal jogador -1) (movimento-válido linha coluna tabuleiro)) (let ((jogada_aux (alterar-jogada (colocar-peca linha coluna tabuleiro -1)))) ; atualiza a variável *jogada*
                                                                        (jogada-computador (obter-jogada) -2) ; manda o tabuleiro ao adversário
                                                                        ))
     ((and (equal jogador -2) (movimento-válido linha coluna tabuleiro)) (let ((jogada_aux2 (alterar-jogada (colocar-peca linha coluna tabuleiro -2)))) ; atualiza a variável *jogada*
                                                                        (jogada-computador (obter-jogada) -1) ; manda o tabuleiro ao adversário
                                                                        ))
     (t nil))
    )
)

(defun colocar-peca (linha coluna tabuleiro &optional (valor 0))
  (substituir-posicao linha (substituir-posicao coluna tabuleiro valor) valor)
)


(defun posicoes-validas (tabuleiro) ; ????????????????? NÃO ESTA A FUNCIONAR
  "Devolve todas as posições válidas para mover o cavalo do utilizador"
  (sucessores (list tabuleiro 0) (operadores) jogador)
)

(defun movimento-válido (linha coluna tabuleiro)
  "Verifica se é possível aplicar algum operador de modo a colocar o cavalo na prosição que o utilizador pretende"

  (cond
   ((and (< coluna (length (car tabuleiro))) (< linha (length tabuleiro))) t) ; verifica se está dentro dos limites do tabuleiro
   ;() ; verifica se pudemos aplicar algum operador que dê aquela casa do tabuleiro
   (t nil)
   )
)

(defun jogada-computador (tabuleiro jogador)
; 1) aplica o Negamax
; 2) atualiza a variável global *jogada*
; 3) chama a jogada do humano

  ;(let (()  ; NOTA: calcular a profundidade consoante o tempo
        (cond
         ((equal jogador -1) (let ((jogada_aux (alterar-jogada (negamax (obter-jogada) 6 1)))) ; atualiza a variável *jogada*
                                      (jogada-humano (obter-tabuleiro) -2) ; manda o tabuleiro ao adversário
                                      ))
         
         ((equal jogador -2) (let ((jogada_aux2 (alterar-jogada (negamax (obter-jogada) 6 1)))) ; atualiza a variável *jogada*
                                      (jogada-humano (obter-tabuleiro) -1) ; manda o tabuleiro ao adversário
                                      ))
         (t nil)
         )
  ;)
)

(defun terminou-jogo ()
  ; o jogo termina se não der para mover mais o cavalo (sucessores NIL)
)

(defun jogar (tabuleiro tempo primeiro-jogador) ; Por default começa o Computador
  ; alternar entre os jogadores
  (cond 
   ((equal primeiro-jogador 'humano) (jogada-humano tabuleiro -1))
   (t (jogada-computador tabuleiro -1))
   )
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


