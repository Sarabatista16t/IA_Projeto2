;; ===============================================================================================================================
;;                                             INTERFACE COM UTILIZADOR  
;; ===============================================================================================================================
;;;; interact.lisp
;;;; Disciplina de IA - 2019 / 2020
;;;; Projeto 2
;;;; Autores: Sara Batista [nÂº 170221054] e Carolina Castilho [nÂº 180221078]


;; CARREGAR FICHEIROS

;(load "C:\\Users\\carol\\OneDrive\\3ºAno\\IA\\Projeto 2\\IA_Projeto2\\IA_Projeto2\\algoritmo.LISP") 
;(load "C:\\Users\\carol\\OneDrive\\3ºAno\\IA\\Projeto 2\\IA_Projeto2\\IA_Projeto2\\jogo.LISP")

(load "C:\\Users\\sarab\\Documents\\Faculdade\\CURSO EI\\5º Semestre\\Inteligência Artificial\\Projeto2\\Scripts\\IA_Projeto2\\algoritmo.LISP")
(load "C:\\Users\\sarab\\Documents\\Faculdade\\CURSO EI\\5º Semestre\\Inteligência Artificial\\Projeto2\\Scripts\\IA_Projeto2\\jogo.LISP")

;;=========================================
;;      INPUT E LEITURA DE FICHEIROS  
;;=========================================

;;; Inicialização do programa
(defun iniciar ()
  (let ((parametros (menu)))
    ;(with-open-file (f2 "C:\\Users\\sarab\\Documents\\Faculdade\\CURSO EI\\5º Semestre\\Inteligência Artificial\\Projeto2\\Scripts\\IA_Projeto2\\log.dat" :direction :output) 
      (jogar (posicionar-jogadores-no-tabuleiro (tabuleiro-aleatorio (baralhar (lista-numeros)) 10)) (first parametros) (second parametros))
     ; )
    )
  )



;; Função para ler a profundidade
(defun ler-profundidade()
  "Permite fazer a leitura da profundidade limite para o algoritmo dfs."
  (progn
    (format t "Qual a profundidade limite? ~%")
    (read)
    )
  )


;;=========================================
;;            INICIAR O JOGO  
;;=========================================

(defun escrever-cabecalho (f)
  (format f "~%=============================================================================~%")
  (format f "UC Inteligencia Artificial                               Ano letivo 2019/2020~%~%")
  (format f "Projeto realizado por:~%")
  (format f "   * Sara Batista, numero 170221054~%")
  (format f "   * Carolina Castilho, numero 180221078~%")
  (format f "~%=============================================================================~%~%")
  )

(defun perguntar-modo-jogo ()
  "Função que devolve o modo de jogo"
  (progn
    (escrever-cabecalho t)
    (format t "Selecione o modo de jogo: ~%")
    (format t "1 -> Humano vs Computador ~%")
    (format t "2 -> Computador vs Computador ~%")

    (read)
    )
  )

(defun perguntar-primeiro-jogador()
  (progn
    (format t "~%Quem joga primeiro? ~%")
    (format t "1 -> Humano ~%")
    (format t "2 -> Computador ~%")
    (read)
    )
  )

(defun perguntar-tempo ()
  (format t "~%Indique o tempo limite para a jogada do computador: [milissegundos] ~%")
  (read) ; 1000 e 5000 milissegundos
  )

(defun menu ()
  "Devolve uma lista com os operadores necessários ao jogo: primeiro-jogador, tempo"
  (let* ((modo-jogo (perguntar-modo-jogo))
         (tempo (perguntar-tempo)))
    (cond 
     ((= modo-jogo 2) (list tempo 'computador))
     (t (let ((primeiro-jogador (perguntar-primeiro-jogador)))
          (cond
           ((= primeiro-jogador 1) (list tempo 'humano))
           (t (list tempo 'humano))
           )
          )
        )
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

;; Funções para escrever as estatisticas num ficheiro




