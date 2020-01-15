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

(defun negamax-sucessores (no d jogador sucessores alfa beta &optional (bestValue most-negative-fixnum))
  (cond
   ((null sucessores) alfa)
   (t
    (let* ((valorSucessor (- (negamax (first sucessores) d jogador alfa beta)))
           (bestSucessor (max bestValue valorSucessor))
           (newAlfa (max alfa bestSucessor)))
      (cond
       ((>= newAlfa beta) bestSucessor)
       (t (negamax-sucessores no d  jogador (rest sucessores) newAlfa beta bestSucessor)))))))


(defun negamax (no d jogador &optional (alfa most-negative-fixnum) (beta most-positive-fixnum)) 
  (let ((sucessores (sucessores no (operadores) d jogador)))
    (cond
     ((or (equal d 0) (null sucessores)) (funcao-avaliacao (no-estado no) jogador)) ;;acrescentar a multiplicação com a heuristica no projeto (?)
;;gero o 1o sucessor do no
     (t (negamax-sucessores no (- d 1) (- jogador) sucessores (- alfa) (- beta))))))
