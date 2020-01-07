;; ===============================================================================================================================
;;                                             INTERFACE COM UTILIZADOR  
;; ===============================================================================================================================
;;;; interact.lisp
;;;; Disciplina de IA - 2019 / 2020
;;;; Projeto 2
;;;; Autores: Sara Batista [nº 170221054] e Carolina Castilho [nº 180221078]


;; CARREGAR FICHEIROS

(load "C:\\Users\\Carolina\\Desktop\\Projeto\\puzzle2.LISP")
(load "C:\\Users\\Carolina\\Desktop\\Projeto\\procura2.LISP")
;(load "C:\\Users\\Carolina\\OneDrive\\3ºAno\\IA\\Projeto\\puzzle.LISP")
;(load "C:\\Users\\Carolina\\OneDrive\\3ºAno\\IA\\Projeto\\procura.LISP")

;;=========================================
;;      INPUT E LEITURA DE FICHEIROS  
;;=========================================

;;; Inicialização do programa
(defun iniciar ()
  
  ; Pergunta inicial
    (format t "Introduza o número do problema que pretende testar [1 até 6]: ~%") ;; ~% -> mudar de linha  
  ; Ler ficheiro
   (with-open-file (f1 "C:\\Users\\Carolina\\OneDrive\\3ºAno\\IA\\Projeto\\problemas.dat" :direction :input) ;; ficheiro f1 -> problemas.dat 
      ;;(with-open-file (f2 "C:\\Users\\sarab\\Documents\\Faculdade\\CURSO EI\\5º Semestre\\Inteligência Artificial\\Projeto1\\resultados.dat" :direction :output) ; ficheiro f2 -> onde vai ser escrito o resultado do jogo
        (jogar (criar-no-inicial (ler-tabuleiro f1 (read))))
         ; (escrever-tabuleiro (jogar (criar-no-inicial (ler-tabuleiro f1 (read))) sucessores operadores solucao) f2)
         ; escrever-penetrancia
         ; escrever-fator-de-ramificacao
         ;)

      )
)


;; Função para ler o tabuleiro do ficheiro
(defun ler-tabuleiro (f1 n)
  "Devolve uma lista que representa o tabuleiro de jogo"
  (cond 
   ((= n 1) (ler-tabuleiro1 f1))
   ((= n 2) (ler-tabuleiro2 f1))
   ((= n 3) (ler-tabuleiro3 f1))
   ((= n 4) (ler-tabuleiro4 f1))
   ((= n 5) (ler-tabuleiro5 f1))
   ((= n 6) (tabuleiro-aleatorio (baralhar (lista-numeros)) 10))
   )

  ;(cond 
   ; ((= n 1) (read f1))
    ;((= n 6) (tabuleiro-aleatorio (baralhar (lista-numeros)) 100))
    ;(t (cons (read f1) (ler-tabuleiro f1 (1- n))))
  ;)
)

(defun ler-tabuleiro1 (f1)
  "Devolve uma lista que representa o tabuleiro de jogo"
  (read f1)
)


(defun ler-tabuleiro2 (f1)
  "Devolve uma lista que representa o tabuleiro de jogo"
  (read f1)
  (read f1)
)

(defun ler-tabuleiro3 (f1)
  "Devolve uma lista que representa o tabuleiro de jogo"
  (read f1)
  (read f1)
  (read f1)
)

(defun ler-tabuleiro4 (f1)
  "Devolve uma lista que representa o tabuleiro de jogo"
  (read f1)
  (read f1)
  (read f1)
  (read f1)
)

(defun ler-tabuleiro5 (f1)
  "Devolve uma lista que representa o tabuleiro de jogo"
  (read f1)
  (read f1)
  (read f1)
  (read f1)
  (read f1)
)


(defun validar-input-utilizador (n)
  (cond 
   ((> n 6) (format t "Número inválido ~% Introduza o número do problema que pretende testar [1 até 6]:"))
   (t nil)
  )
)


;; Função para ler o algoritmo
(defun ler-algoritmo ()
"Permite fazer a leitura do algoritmo a utilizar."
  (progn
    (format t "Que algoritmo quer usar para procurar? ~%")
    (format t "1- Procura na largura ~%")
    (format t "2- Procura na profundidade ~%")
    (format t "3- Algoritmo A* ~%")
    (let ((resposta (read)))
      (cond 
       ((= resposta 1) 'breadth-first)
       ((= resposta 2) 'depth-first)
       (t 'a*))
    )
  )
)


;; Função para ler a profundidade
(defun ler-profundidade()
"Permite fazer a leitura da profundidade limite para o algoritmo dfs."
    (progn
    (format t "Qual a profundidade limite? ~%")
    (read)
    ))


;;=========================================
;;            INICIAR O JOGO  
;;=========================================

;; Fun��o para iniciar o jogo
(defun jogar (estado tempo) ; Pedido no enunciado
"Permite iniciar o programa, fazendo a leitura do teclado do estado inicial e do algoritmo a utilizar para procurar a solução (neste caso a procura na profundidade ou na largura)"
  (let* ((algoritmo (ler-algoritmo))
         (profundidade (cond ((eql algoritmo 'depth-first) (ler-profundidade)) (T 9999))) )
	(cond
		((equal algoritmo 'breadth-first) (breadth-first no-inicial 'sucessores 'no-solucaop 'solucao-p 'operadores))
		((equal algoritmo 'depth-first) (depth-first no-inicial profundidade 'sucessores 'no-solucaop 'operadores))
	)
  )
)


;;=========================================
;;                OUTPUT  
;;=========================================

;; Função para escrever o tabuleiro no final do jogo num ficheiro
(defun escrever-tabuleiro-final-jogo (tabuleiro f2)
  "Escreve no ficheiro a lista que representa o tabuleiro atualizado no final do jogo"
  (format f2 "~A" tabuleiro)
)

;; Funções para escrever as estatísticas num ficheiro

;; Escrever penetrância
(defun escrever-penetrancia (tabuleiro f2)
  "Escreve no ficheiro o valor da penetrância"
  (format f2 "~A" tabuleiro)
;(...completar...)
)

;; Escrever fator de ramificação
(defun escrever-fator-de-ramificacao (tabuleiro f2)
  "Escreve no ficheiro o valor do fator de ramificação"
  (format f2 "~A" tabuleiro)
;(...completar...)
)


