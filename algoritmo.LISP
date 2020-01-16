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

(defun negamax-sucessores (estado d jogador sucessores alfa beta &optional (bestValue most-negative-fixnum))
  (cond
   ((null sucessores) alfa)
   (t
    (let* ((valorSucessor (- (negamax (first sucessores) d jogador alfa beta)))
           (bestSucessor (max bestValue valorSucessor))
           (newAlfa (max alfa bestSucessor)))
      (cond
       ((>= newAlfa beta) bestSucessor)
       (t (negamax-sucessores estado d  jogador (rest sucessores) newAlfa beta bestSucessor)))))))


(defun negamax (estado d jogador &optional (alfa most-negative-fixnum) (beta most-positive-fixnum)) 
  (let ((sucessores (sucessores estado (operadores) d jogador)))
    (cond
     ((or (equal d 0) (null sucessores)) (funcao-avaliacao estado jogador)) ;;acrescentar a multiplicação com a heuristica no projeto (?)
;;gero o 1o sucessor do no
     (t (negamax-sucessores estado (- d 1) (- jogador) sucessores (- alfa) (- beta))))))
