;; ================================================================================================================================
;;                                            IMPLEMENTAÇÃO DO NEGAMAX 
;; ================================================================================================================================
;;;; procura.lisp
;;;; Disciplina de IA - 2019 / 2020
;;;; Projeto 2
;;;; Autores: Sara Batista [nÂº 170221054] e Carolina Castilho [nÂº 180221078]


;; =====================================================================
;;                         NEGAMAX
;; =====================================================================
;;; argumentos: nó n, profundidade d, cor c
;;; b = ramificação (número de sucessores)
 function negamax (n, d, c &aux bestValue)
     if d = 0 ou n é terminal
         return c * valor heuristico de n
     bestValue := -8
     para cada sucessor de n (n1, ..., nk,..., nb)
        bestValue := max(bestValue, -negamax(nk, d-1, -c) )
     return bestValue

;;; Valor do nó inicial
rootNegamaxValue := negamax( rootNode, d, -1) 



;; NEGAMAX COM CORTES ALFA-BETA

;;; argumentos: nó n, profundidade d, cor c
;;; b = ramificação (número de sucessores)
function negamax (n, d, a, ß, c)
     se d = 0 ou n é terminal
         return c * valor heuristico de n
     sucessores := OrderMoves(GenerateMoves(n))
     bestValue := -8
     para cada sucessor nk em sucessores
         bestValue := max (bestValue, -negamax (nk, d-1, -ß, - a, -c))
         a := max (a, bestValue)
         se a = ß
             break
     return bestValue
