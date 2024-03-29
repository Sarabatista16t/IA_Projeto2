;; ===============================================================================================================================
;; ========================================= C�DIGO REFERENTE AO JOGO DO CAVALO ================================================== 
;; ===============================================================================================================================
;;;; jogo.lisp
;;;; Disciplina de IA - 2019 / 2020
;;;; Projeto 2
;;;; Autores: Sara Batista [n� 170221054] e Carolina Castilho [n� 180221071]


;;======================================================================================
;;                                 ESTRUTURA DO N� 
;;======================================================================================        
;;          ESTADO (TABULEIRO + PONTUA��O_CAVALO_BRANCO + PONTUA��O_CAVALO_PRETO + JOGADOR)
;;    N� =>  
;;          PROFUNDIDADE
;;           



;;; ======================
;;;    CONSTANTES
;;; ======================
(defparameter *Jogador1* -1)  ; Cavalo branco
(defparameter *Jogador2* -2)  ; Cavalo preto


;;; ======================
;;;    CONSTRUTOR
;;; ======================
(defun cria-no (estado &optional (g 0))
  (list estado g)
)


;;; ========================================================
;;;                    M�TODOS SELETORES
;;; ========================================================


;;; ======================
;;;   Vocabul�rio LISP 
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

(defun no-profundidade(no)
  (second no)
)



;;; ===============================================
;;;       FUN��ES AUXILIARES DA PROCURA
;;; ===============================================

;;; ===========================
;;;    Posi��es no tabuleiro
;;; ===========================

(defun linha (indice tabuleiro)
"Retorna uma lista que representa a linha no tabuleiro recebido"  
  (cond 
   ((equal indice 0) (car tabuleiro))
   (T (linha (- indice 1)  (cdr tabuleiro)))))


(defun celula (n x tabuleiro)
"Retorna o valor presente na celula no tabuleiro, representada pelos dois indices"
(cond
 ((or (null n) (null x)) nil)
 ((or (< n 0) (< x 0)) nil)
 (t (nth x (nth n tabuleiro)))))


(defun procura-celula (valor tabuleiro &optional (indiceL 0) (indiceC 0))
  "Retorna a celula com o valor sim�trico do valor recebido"
  (cond
   ((null tabuleiro) nil)
   ((equal (celula 0 0 tabuleiro) (+ (* (mod valor 10) 10) (floor valor 10))) (list indiceL indiceC))
   (t 
      (cond
       ((null (car tabuleiro)) (procura-celula valor (cdr tabuleiro) (+ indiceL 1) 0))
       (t (procura-celula valor (cons (cdr (car tabuleiro)) (cdr tabuleiro)) indiceL (+ indiceC 1)))))))



(defun procura-duplo (tabuleiro &optional (indiceL 0) (indiceC 0) (indiceL-duplo (- 1)) (indiceC-duplo (- 1)) (duplo (- 1)))
  "Fun��o que procura a casa com o maior valor duplo no tabuleiro recebido"
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
  "Fun��o que verifica se um numero passado por par�metro � duplo"                                          
  (cond 
   ((equal numero nil) nil)
   ((< numero 0) nil)
   (t (equal (mod numero 10) (floor numero 10)))
   )
  )


(defun substituir-posicao (indice lista &optional (valor nil))
  "Fun��o auxiliar para substituir uma posi��o da lista por um dado valor (por default � nil) "
  (cond
   ((equal indice 0) (cons valor (rest lista)))
   (t (cons (car lista) (substituir-posicao  (- indice 1) (cdr lista) valor)))
   )
  )


(defun substituir (indiceL indiceC tabuleiro &optional (valor nil))
  "Fun��o para substituir uma posi��o do tabuleiro por um valor (por default � nil)"
  (cond 
   ((equal indiceL 0) (cons (substituir-posicao indiceC (car tabuleiro) valor) (rest tabuleiro)))
   (t (cons (car tabuleiro) (substituir (- indiceL 1) indiceC (rest tabuleiro) valor)))
   )
  )


(defun posicao-cavalo (tabuleiro jogador &optional (indiceL 0) (indiceC 0))
  "Fun��o que devolve a posi��o do cavalo, dependendo de qual o jogador que est� a jogar"
  (cond
   ((null tabuleiro) nil)
   ((equal (celula 0 0 tabuleiro) jogador) (list indiceL indiceC))
   (t 
    (cond
     ((null (car tabuleiro)) (posicao-cavalo (cdr tabuleiro) jogador (+ indiceL 1) 0))
     (t (posicao-cavalo (cons (cdr (car tabuleiro)) (cdr tabuleiro)) jogador indiceL (+ indiceC 1))))
    )
   )
  )



;;; =====================================
;;;     Gerar um tabuleiro aleat�rio
;;; =====================================

(defun lista-numeros (&optional (n 100))
  "Cria uma lista com todos os n�meros entre 0 (inclusive) e n (exclusive)"
  (cond
   ((equal n 1) (list 0))
   (T (cons (- n 1) (lista-numeros (- n 1))))
   )
  )


(defun remover-se (pred lista)
  "Remove um valor que j� exista no tabuleiro"
  (cond 
   ((null lista) nil)
   ((funcall pred (car lista)) (remover-se pred (cdr lista)))
   (t (cons (car lista) (remover-se pred (cdr lista))))
   )
  )


(defun baralhar (lista)
  "Baralha a lista"
  (cond
   ((null lista) nil)
   (t (let ((numero-aleatorio  (nth (random (length lista)) lista)))
              (cons numero-aleatorio (baralhar(remover-se  (lambda (x) (= x numero-aleatorio))  lista)))))
   )
  )


(defun tabuleiro-aleatorio (lista n) 
  "Fun��o que gera um tabuleiro aleat�rio"
  (cond    
   ((null lista) nil)    
   (t (cons (subseq lista 0 n) (tabuleiro-aleatorio (subseq lista n) n)))
   )
  )



;;; =========================
;;;     Fun��es sucessores
;;; =========================

(defun novo-sucessor (no operador)
  "Fun��o auxiliar: aplica um operador e gera o respetivo sucessor"
  (cond 
   ((null operador) nil)
   (t (let ((estado-no (funcall operador (no-estado no))))
        (cond
         ((equal estado-no nil) nil)
         (t (cria-no estado-no (+ (no-profundidade no) 1)))))
      )
   )
  )


(defun sucessores (no operadores)
  "Fun��o para gerar os sucessores de um n�, aplicando os operadores"
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

(defun operadores ()
 "Cria uma lista com todos os operadores do problema"
 (list 'operador-1 'operador-2 'operador-3 'operador-4 'operador-5 'operador-6 'operador-7 'operador-8)
 )


(defun operador-1 (estado)
  "Fun��o que aplica o operador 1"
  (operador-default estado 2 (- 1))
  )

(defun operador-2 (estado)
 "Fun��o que aplica o operador 2"
  (operador-default estado 2 1 )
  )

(defun operador-3 (estado)
 "Fun��o que aplica o operador 3"
  (operador-default estado 1 2 )
  )

(defun operador-4 (estado)
 "Fun��o que aplica o operador 4"
  (operador-default estado (- 1) 2)
  )

(defun operador-5 (estado)
 "Fun��o que aplica o operador 5"
  (operador-default estado (- 2) 1 )
  )

(defun operador-6 (estado)
 "Fun��o que aplica o operador 6"
  (operador-default estado (- 2) (- 1))
  )

(defun operador-7 (estado)
 "Fun��o que aplica o operador 7"
  (operador-default estado (- 1) (- 2))
  )

(defun operador-8 (estado)
 "Fun��o que aplica o operador 8"
  (operador-default estado 1 (- 2))
  )


(defun operador-default (estado L C)
  "Fun��o que aplica os operadores ao estado do n�"
  (cond 
   ((and (equal (+ (first (posicao-cavalo (no-tabuleiro estado) (no-jogador estado))) L) nil) (equal (+ (second (posicao-cavalo (no-tabuleiro estado) (no-jogador estado))) C) nil)) nil)

   (t
    (let* ((tabuleiro (no-tabuleiro estado))
           (valor-cavalo (celula (+ (first (posicao-cavalo tabuleiro (no-jogador estado))) L) (+ (second (posicao-cavalo tabuleiro (no-jogador estado))) C) tabuleiro)))
      (cond 
       ((null valor-cavalo) nil)
       ((equal valor-cavalo (jogador-oposto (no-jogador estado))) nil)
       (t 
        (let* (
               (posicao (posicao-cavalo tabuleiro (no-jogador estado)))
               (simetrico (procura-celula valor-cavalo tabuleiro))
               (linha-celula-simetrico (first simetrico))
               (coluna-celula-simetrico (second simetrico))
               (tabuleiro-nil (substituir (first posicao) (second posicao) tabuleiro))
               (tabuleiro-cavalo (substituir (+ (first posicao) L) (+ (second posicao) C) tabuleiro-nil (no-jogador estado)))
               (duplo (procura-duplo tabuleiro-cavalo))
               (linha-celula-duplo (first duplo))
               (coluna-celula-duplo (second duplo))
               (tabuleiro-nil (substituir (first posicao) (second posicao) tabuleiro))
               )
          (cond
           
           ((equal (no-jogador estado) *Jogador1*)
            (cond
             ((> (+ (first posicao) L) (length tabuleiro)) nil)
             ((> (+ (second posicao) C) (length (car tabuleiro))) nil)
             ((verifica-duplo valor-cavalo) (list (substituir linha-celula-duplo coluna-celula-duplo tabuleiro-cavalo) (+ (no-pontos-cavalo-branco estado) valor-cavalo) (no-pontos-cavalo-preto estado) *Jogador2*))
             ((or (null linha-celula-simetrico) (null coluna-celula-simetrico)) (list (substituir (+ (first posicao) L) (+ (second posicao) C) tabuleiro-nil (no-jogador estado)) (+ (no-pontos-cavalo-branco estado) valor-cavalo) (no-pontos-cavalo-preto estado) *Jogador2*))
             (t (list (substituir linha-celula-simetrico coluna-celula-simetrico tabuleiro-cavalo) (+ (no-pontos-cavalo-branco estado) valor-cavalo) (no-pontos-cavalo-preto estado) *Jogador2*))))
           
           ((equal (no-jogador estado) *Jogador2*) 
            (cond
             ((> (+ (first posicao) L) (length tabuleiro)) nil)
             ((> (+ (second posicao) C) (length (car tabuleiro))) nil)
             ((verifica-duplo valor-cavalo) (list (substituir linha-celula-duplo coluna-celula-duplo tabuleiro-cavalo) (no-pontos-cavalo-branco estado) (+ (no-pontos-cavalo-preto estado) valor-cavalo) *Jogador1*))
             ((or (null linha-celula-simetrico) (null coluna-celula-simetrico)) (list (substituir (+ (first posicao) L) (+ (second posicao) C) tabuleiro-nil (no-jogador estado)) (no-pontos-cavalo-branco estado) (+ (no-pontos-cavalo-preto estado) valor-cavalo) *Jogador1*))
             (t (list (substituir linha-celula-simetrico coluna-celula-simetrico tabuleiro-cavalo) (no-pontos-cavalo-branco estado) (+ (no-pontos-cavalo-preto estado) valor-cavalo) *Jogador1*))))
           (T nil))))))))
  )






;;======================================================================================
;;                "OPERADOR 0" (Posicionar os cavalos no inicio do jogo)
;;======================================================================================        

(defun encontrar-maximo-lista (lista &optional (maximo 0))
  "Fun��o que devolve o valor m�ximo da lista"
  (cond
      ((null lista) maximo)
      ((equal (first lista) nil) (encontrar-maximo-lista (rest lista) maximo))
      ((> (first lista) maximo) (encontrar-maximo-lista (rest lista) (first lista)))
      (t (encontrar-maximo-lista (rest lista) maximo))
      )
  )

(defun obter-indice-do-valor-maximo (lista &optional (count 0))
  "Fun��o que devolve o indice da coluna onde est� o valor m�ximo"
  (cond
     ((null lista) nil)
     ((equal (first lista) (encontrar-maximo-lista lista)) count)
     (t (obter-indice-do-valor-maximo (rest lista) (1+ count)))
  )
)

(defun colocar-cavalo-branco (tabuleiro)
  "Fun��o que devolve o tabuleiro com o cavalo branco na posi��o com maior valor da linha 1"
  (substituir 0 (obter-indice-do-valor-maximo (linha 0 tabuleiro)) tabuleiro *Jogador1*)
)

(defun pontuacao-inicial-cavalo-branco (tabuleiro)
  "Fun��o que devolve a pontua��o inicial do cavalo branco (valor da maior casa da 1� linha)"
  (celula 0 (obter-indice-do-valor-maximo (linha 0 tabuleiro)) tabuleiro)
)

(defun colocar-cavalo-preto (tabuleiro)
  "Fun��o que devolve o tabuleiro com o cavalo preto na posi��o com maior valor da linha 10"
  (substituir 9 (obter-indice-do-valor-maximo (linha 9 tabuleiro)) tabuleiro *Jogador2*)
)

(defun pontuacao-inicial-cavalo-preto (tabuleiro)
  "Fun��o que devolve a pontua��o inicial do cavalo preto (valor da maior casa da 10� linha)"
  (celula 9 (obter-indice-do-valor-maximo (linha 9 tabuleiro)) tabuleiro)
)


(defun posicionar-jogadores-no-tabuleiro (tabuleiro jogador)
  "Fun��o que cria o n� inicial com o tabuleiro e aplica as fun��es para colocar os cavalos no inicio do jogo"
  (list (colocar-cavalo-preto (colocar-cavalo-branco tabuleiro)) (pontuacao-inicial-cavalo-branco tabuleiro) (pontuacao-inicial-cavalo-preto tabuleiro) jogador)
    ; jogador 1  =>  -1  =>   cavalo branco
    ; jogador 2  =>  -2  =>   cavalo preto
  )

(defun substituir-casa-cavalo-branco (tabuleiro)
  "Fun��o que aplica as regras do jogo ao colocar o cavalo branco no tabuleiro inicial"
  (let* ((valor-cavalo (pontuacao-inicial-cavalo-branco tabuleiro))
         (tabuleiro-inicial (colocar-cavalo-branco tabuleiro))
         (duplo (procura-duplo tabuleiro-inicial))
         (linha-celula-duplo (first duplo))
         (coluna-celula-duplo (second duplo))
         (simetrico (procura-celula valor-cavalo tabuleiro-inicial))
         (linha-celula-simetrico (first simetrico))
         (coluna-celula-simetrico (second simetrico)))
         
  (cond
   ((verifica-duplo valor-cavalo) (substituir linha-celula-duplo coluna-celula-duplo tabuleiro-inicial))
   ((null (verifica-duplo valor-cavalo)) (substituir linha-celula-simetrico coluna-celula-simetrico tabuleiro-inicial))
   (t tabuleiro-inicial))))

(defun substituir-casa-cavalo-preto (tabuleiro)
  "Fun��o que aplica as regras do jogo ao colocar o cavalo preto no tabuleiro inicial"
  (let* ((valor-cavalo (pontuacao-inicial-cavalo-preto tabuleiro))
         (tabuleiro-inicial (colocar-cavalo-preto tabuleiro))
         (duplo (procura-duplo tabuleiro-inicial))
         (linha-celula-duplo (first duplo))
         (coluna-celula-duplo (second duplo))
         (simetrico (procura-celula valor-cavalo tabuleiro-inicial))
         (linha-celula-simetrico (first simetrico))
         (coluna-celula-simetrico (second simetrico)))
         
  (cond
   ((verifica-duplo valor-cavalo) (substituir linha-celula-duplo coluna-celula-duplo tabuleiro-inicial))
   ((null (verifica-duplo valor-cavalo)) (substituir linha-celula-simetrico coluna-celula-simetrico tabuleiro-inicial))
   (t tabuleiro-inicial))))

(defun tabuleiro-inicial (tabuleiro jogador)
  "Fun��o que retorna o tabuleiro inicial, com os cavalos na casa de maior valor da primeira e ultima linha, com as regras do jogo aplicadas (sim�trico e duplo)"
  (list (substituir-casa-cavalo-preto (substituir-casa-cavalo-branco tabuleiro)) (pontuacao-inicial-cavalo-branco tabuleiro) (pontuacao-inicial-cavalo-preto tabuleiro) jogador))




;;======================================================================================
;;                                 JOGAR 
;;======================================================================================        

(defun jogar (ficheiro no tempo primeiro-jogador) 
  "Fun��o que inicia o jogo consoante o modo escolhido pelo utilizador"
  ;(set-tempo-permitido (- tempo 100))
  (cond 
   ((equal primeiro-jogador -1) (jogada-humano ficheiro no *Jogador1*))
   (t (jogada-computador ficheiro no *Jogador1*))
   )
  )


;;======================================================================================
;;                                 JOGADA HUMANO
;;======================================================================================        


(defun jogada-humano (ficheiro no jogador)
  "Fun��o que permite o humano inserir posi��es e efetuar a jogada no tabuleiro"
  (cond
   ((equal (terminou-jogo no) T)  nil (vencedor ficheiro no))
   (T
    (let* ((estado (no-estado no))
           (posicoes (posicoes-validas no))
           (posicoes-jogador-oposto (posicoes-validas (list (list (no-tabuleiro estado) (no-pontos-cavalo-branco estado) (no-pontos-cavalo-preto estado) (jogador-oposto (no-jogador estado))) (no-profundidade no))))
           )
      (escrever-estado ficheiro (no-estado no))
      (escreve-posicoes-validas posicoes)
    
      (cond 
       ((and (equal jogador *Jogador1*) (equal posicoes nil)) (jogada-computador ficheiro (list (list (no-tabuleiro estado) (no-pontos-cavalo-branco estado) (no-pontos-cavalo-preto estado) (jogador-oposto jogador)) 0) *Jogador2*))
       ((and (equal jogador *Jogador2*) (equal posicoes nil)) (jogada-computador ficheiro (list (list (no-tabuleiro estado) (no-pontos-cavalo-branco estado) (no-pontos-cavalo-preto estado) (jogador-oposto jogador)) 0) *Jogador1*))
       (t 
        (format t "~%~%Introduza a linha e a coluna onde pretende colocar a pe�a: ~%")
        (format t "<numero de 0 a 9> <numero de 0 a 9> ~%")
        (let* ((numero (read))
               (linha (floor numero 10))
               (coluna (mod numero 10))
               )
         
          (cond
          
           ((and (equal jogador *Jogador1*) (movimento-v�lido linha coluna posicoes posicoes-jogador-oposto (no-tabuleiro estado)) (let ((jogada_aux1 (list (colocar-peca linha coluna estado) 0))) 
                                                                                                             (jogada-computador ficheiro jogada_aux1 *Jogador2*) ; manda a jogada ao advers�rio
                                                                                                             )))
           ((and (equal jogador *Jogador2*) (movimento-v�lido linha coluna posicoes posicoes-jogador-oposto (no-tabuleiro estado)) (let ((jogada_aux2 (list (colocar-peca linha coluna estado) 0)))
                                                                                                             (jogada-computador ficheiro jogada_aux2 *Jogador1*) ; manda a jogada ao advers�rio
                                                                                                             )))
           ((equal (verifica-posicoes-inseridas linha coluna posicoes) nil) (format t "Movimento inv�lido! Tente outra posi��o.~%") (jogada-humano ficheiro no jogador))
           (t nil)
           )
          )
        )
       )
      )
    )
   )
  )



;;=================================================
;;     FUN��ES AUXILIARES � JOGADA HUMANO
;;=================================================

(defun jogador-oposto (jogador)
 "Fun��o para se obter o jogador oposto"
  (cond
   ((equal jogador *Jogador2*) *Jogador1*)
   ((equal jogador *Jogador1*) *Jogador2*)
   )
  )


(defun colocar-peca (linha coluna estado) 
  "Fun��o que move a pe�a para a casa pretendida e altera a respetiva pontua��o"
  (let* ((posicao-atual (posicao-cavalo (no-tabuleiro estado) (no-jogador estado)))
         (linha-atual (first posicao-atual))
         (coluna-atual (second posicao-atual))
         (indiceL (- linha linha-atual))
         (indiceC (- coluna coluna-atual)))
    (operador-default estado indiceL indiceC)
    )
  )
    

;;============================================
;;  Fun��es de verifica��o do estado do jogo
;;============================================

(defun terminou-jogo (no)
  "Fun��o para verificar se o jogo j� terminou"
  (let* ((estado (no-estado no))
        (sucessores-um (obter-posicoes-validas (sucessores no (operadores))))
        (sucessores-dois (obter-posicoes-validas (sucessores (list (list (no-tabuleiro estado) (no-pontos-cavalo-branco estado) (no-pontos-cavalo-preto estado) (jogador-oposto (no-jogador estado))) 0) (operadores)))))
    (cond
     ((and (null sucessores-um) (null sucessores-dois)) T)
     (t nil)
     )
    )
  )


(defun vencedor (ficheiro no)
  "Fun��o para determinar quem � o vencedor"
  (let* ((estado (no-estado no)))
    (cond
     ((> (no-pontos-cavalo-branco estado) (no-pontos-cavalo-preto estado)) (escrever-vencedor ficheiro "Cavalo branco" estado))
     ((< (no-pontos-cavalo-branco estado) (no-pontos-cavalo-preto estado)) (escrever-vencedor ficheiro "Cavalo preto" estado))
     (t (format ficheiro "Empate! ~%") (escrever-tabuleiro t estado))
     )
    )
  )

(defun escrever-vencedor (ficheiro cavalo estado)
  "Fun��o para escrever no ecr� e no ficheiro quem � o vencedor, as pontua��es finais dos jogadores e o tabuleiro final"
  (format ficheiro "~%------------------------------------- FIM DO JOGO -------------------------------------")
  (format ficheiro "~%Vencedor: ~A~%" cavalo)
  (format ficheiro"~%Pontos do cavalo branco: ~A~%" (no-pontos-cavalo-branco estado)) 
  (format ficheiro "~%Pontos do cavalo preto: ~A~%~%" (no-pontos-cavalo-preto estado)) 
  (escrever-tabuleiro ficheiro (no-tabuleiro estado))

  (format t "~%------------------------------------- FIM DO JOGO -------------------------------------")
  (format t "~%Vencedor: ~A~%" cavalo)
  (format t"~%Pontos do cavalo branco: ~A~%" (no-pontos-cavalo-branco estado)) 
  (format t "~%Pontos do cavalo preto: ~A~%~%" (no-pontos-cavalo-preto estado)) 
  (format t"~%------------------------------------------~%")
  (escrever-tabuleiro t (no-tabuleiro estado))
  )


;;==============================================================
;;  Fun��es de valida��o do movimento do cavalo do utilizador
;;==============================================================

(defun movimento-v�lido (linha coluna posicoes-validas posicoes-validas-oposto tabuleiro)
  "Verifica se � poss�vel aplicar algum operador de modo a colocar o cavalo na posi��o que o utilizador pretende"
  (cond
   ((or (< coluna 0) (< linha 0) (>= linha (length tabuleiro)) (>= coluna (length (car tabuleiro)))) nil) ; verifica se est� dentro dos limites do tabuleiro
   ((and (null (casa-ameacada linha coluna posicoes-validas-oposto)) (verificar-operador-possivel posicoes-validas linha coluna)) t) ; verifica se se podem aplicar operadores
   (t nil)
   )
  )

(defun verificar-operador-possivel (posicoes-validas linha coluna)
  "Fun��o para verificar se � poss�vel aplicar o operador"
  (cond
   ((null posicoes-validas) nil)
   ((and (equal linha (first (first posicoes-validas))) (equal coluna (second (first posicoes-validas)))) t)
   (t (verificar-operador-possivel (rest posicoes-validas) linha coluna))))


(defun posicoes-validas (no) 
  "Devolve todas as posi��es v�lidas para mover o cavalo"
  (obter-posicoes-validas (sucessores no (operadores)))
 
 )


(defun obter-posicoes-validas (possiveis-sucessores)
  "Devolve uma lista com os indices das posi��es poss�veis"
  (let* ((first (first possiveis-sucessores))
         (estado (no-estado first)))
    (cond
     ((null possiveis-sucessores) nil)
     (t (cons (posicao-cavalo (no-tabuleiro estado) (jogador-oposto (no-jogador estado))) (obter-posicoes-validas (rest possiveis-sucessores))))
     )
    )
  )


(defun escreve-posicoes-validas (posicoes-validas)
  "Escreve a lista com os indices das posi��es v�lidas"
  (cond 
   ((null posicoes-validas) (format  t "~%Ups... N�o h� jogadas poss�veis...:( ~%"))
   (t (format t "~%Jogadas Possiveis: ~%~A~%" posicoes-validas))))


(defun verifica-posicoes-inseridas (linha coluna lista)
  "Fun��o que verifica se a linha e a coluna inserida pelo utilizador faz parte das posi��es poss�veis"
    (cond 
     ((null lista) nil)
     ((and (equal linha (first (first lista))) (equal coluna (second (first lista)))) t)
     (t (verifica-posicoes-inseridas linha coluna (rest lista)))))

(defun casa-ameacada (linha coluna posicoes-validas-jogador-oposto)
  (cond
   ((null posicoes-validas-jogador-oposto) nil)
   ((and (equal linha (first (first posicoes-validas-jogador-oposto))) (equal coluna (second (first posicoes-validas-jogador-oposto)))) t)
   (t (casa-ameacada linha coluna (rest posicoes-validas-jogador-oposto)))))



;;======================================================================================
;;                                 JOGADA COMPUTADOR
;;======================================================================================        
(defun metricas (ficheiro)
  "Fun��o que escreve no ecr� e no ficheiro as m�tricas da jogada do computador"
  (format ficheiro "~%---M�tricas---~%")
  (format ficheiro "~%N�s Analisados: ~%~A~%" (get-nos-analisados))
  (format ficheiro "~%Cortes Beta: ~%~A~%" (get-cortes-beta))
  (format ficheiro "~%Cortes Alfa: ~%~A~%" (get-cortes-alfa))
  (format ficheiro "~%Tempo da jogada: ~%~A~%" (get-tempo-jogada))
  (format ficheiro "~%--------------~%")
  
  (format t "~%---M�tricas---~%")
  (format t "~%N�s Analisados: ~%~A~%" (get-nos-analisados))
  (format t "~%Cortes Beta: ~%~A~%" (get-cortes-beta))
  (format t "~%Cortes Alfa: ~%~A~%" (get-cortes-alfa))
  (format t "~%Tempo da jogada: ~%~A~%" (get-tempo-jogada))
  (format t "~%--------------~%")
)


(defun jogada-computador (ficheiro no jogador)
  "Fun��on que permite o computador jogar, atrav�s do algoritmo negamax"
  (cond
   ((equal (terminou-jogo no) T) nil (vencedor ficheiro no))
   (t 
  (let ((estado (no-estado no)))
    (cond
     ((equal jogador -1) (let ((jogada_aux (negamax-inicial no 1 1)))
                           (cond 
                            ((null jogada_aux) (jogada-humano ficheiro (list (list (no-tabuleiro estado) (no-pontos-cavalo-branco estado) (no-pontos-cavalo-preto estado) (jogador-oposto jogador)) 0) *Jogador2*))
                            (t (metricas ficheiro) (jogada-humano ficheiro jogada_aux *Jogador2*)  ; manda a jogada ao advers�rio
                               ))))
         
     ((equal jogador -2) (let* ((jogada_aux2 (negamax-inicial no 1 1)))
                           (cond 
                            ((null jogada_aux2) (jogada-humano ficheiro (list (list (no-tabuleiro estado) (no-pontos-cavalo-branco estado) (no-pontos-cavalo-preto estado) (jogador-oposto jogador)) 0) *Jogador1*))
                            (t (metricas ficheiro) (jogada-humano ficheiro jogada_aux2 *Jogador1*)  ; manda a jogada ao advers�rio
                            ))))
     (t nil)
     )
    ))))


(defun jogada-computador-computador (ficheiro no jogador)
  "Fun��o que permite um computador jogar contra outro computador"
  (cond
   ((equal (terminou-jogo no) T) nil (vencedor ficheiro no))
   (t 
    (let ((estado (no-estado no)))
    
      (cond
       ((equal jogador -1) (let ((jogada_aux (negamax-inicial no 6 1)))

                             (format t"~%------------------------------------------~%")
                             (escrever-estado ficheiro (no-estado no))
                             (cond 
                              ((null jogada_aux) (jogada-computador-computador ficheiro (list (list (no-tabuleiro estado) (no-pontos-cavalo-branco estado) (no-pontos-cavalo-preto estado) (jogador-oposto jogador)) 0) *Jogador2*))
                              (t (metricas ficheiro) (jogada-computador-computador ficheiro jogada_aux *Jogador2*) ; manda a jogada ao advers�rio
                                 ))))
         
       ((equal jogador -2) (let* ((jogada_aux2 (negamax-inicial no 5 1)))

                             (cond 
                              ((null jogada_aux2) (jogada-computador-computador ficheiro (list (list (no-tabuleiro estado) (no-pontos-cavalo-branco estado) (no-pontos-cavalo-preto estado) (jogador-oposto jogador)) 0) *Jogador1*))
                              (t (metricas ficheiro) (jogada-computador-computador ficheiro jogada_aux2 *Jogador1*)) ; manda a jogada ao advers�rio

                              )))
       (t nil)
       )
      ))))



;;======================================================================================
;;                                 FUN��ES DE AVALIA��O
;;======================================================================================        

(defun funcao-avaliacao-nos-folha (pontos-jogador1 pontos-jogador2) ; (jogada => estado (tabuleiro + pontua��o))
  "Fun��o de avalia��o para os n�s folha do grafo"
  (cond
   ((> pontos-jogador1 pontos-jogador2) 1) ; ganha jogador 1 / perde jogador 2
   ((equal pontos-jogador1 pontos-jogador2) 0) ; empate
   (t -1) ; perde jogador 1 / ganha jogador 2
   )
  )

(defun funcao-avaliacao-pseudo-nos-folha (pontos-jogador1 pontos-jogador2)
  "Fun��o de avalia��o para os n�s pseudo-folha do grafo"
  ; max pontos = 4950
  (/ (* 0.99 (- pontos-jogador2 pontos-jogador1)) 4950)
  )




;;======================================================================================
;;                                 ESCRITA DOS DADOS
;;======================================================================================        


(defun escrever-tabuleiro (f tabuleiro)
  "Fun��o que escreve o tabuleiro"
  (cond
   ((null tabuleiro) (format t "~%------------------------------------------~%"))
   (t (escrever-linha-tabuleiro f (car tabuleiro)) (escrever-tabuleiro f (cdr tabuleiro))))
   )
  


(defun escrever-linha-tabuleiro (f linha)
  (cond
   ((null linha) (format f "~%"))
   (t (format f "~3A|" (car linha)) (escrever-linha-tabuleiro f (cdr linha)))
  ))



(defun escrever-pontuacao-cavalo-branco (f pontos)
  (format f "~%~%Pontua��o cavalo branco: ~%~A~%" pontos)
  (format t "~%~%Pontua��o cavalo branco: ~%~A~%" pontos)
  )

(defun escrever-pontuacao-cavalo-preto (f pontos)
  (format f "~%Pontua��o cavalo preto: ~%~A~%" pontos)
  (format t "~%Pontua��o cavalo preto: ~%~A~%" pontos)
  )

(defun escrever-estado (f estado)
  (escrever-tabuleiro f (no-tabuleiro estado))
  (escrever-tabuleiro t (no-tabuleiro estado))
  (escrever-pontuacao-cavalo-branco f (no-pontos-cavalo-branco estado))
  (escrever-pontuacao-cavalo-preto f (no-pontos-cavalo-preto estado))
  )

(defun no-teste ()
 '((((42 65 63 -1 73 78 5 32 68 61) (86 30 82 80 28 75 45 50 81 90) (59 24 47 9 67 22 56 77 60 99) (41 62 18 85 76 27 13 94 93 70) (14 54 0 71 35 39 92 38 49 52) (83 11 46 6 48 15 23 88 96 8) (25 84 2 89 7 53 66 31 20 12) (37 19 3 36 10 44 64 16 97 33) (40 69 95 29 4 51 34 1 26 21) (72 79 -2 43 74 55 58 57 91 17)) 87 98 -1) 0))


