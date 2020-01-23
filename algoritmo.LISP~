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

(defun negamax-sucessores (estado d cor sucessores alfa beta &optional (bestValue most-negative-fixnum))
  (cond
   ((null sucessores) alfa)
   (t
    (let* ((valorSucessor (- (negamax (first sucessores) d cor alfa beta)))
           (bestSucessor (max bestValue valorSucessor))
           (newAlfa (max alfa bestSucessor)))
      (cond
       ((>= newAlfa beta) bestSucessor)
       (t (negamax-sucessores estado d cor (rest sucessores) newAlfa beta bestSucessor)))))))


(defun negamax (estado d cor &optional (alfa most-negative-fixnum) (beta most-positive-fixnum)) 
  (let ((sucessores (sucessores estado (operadores))))
    (cond
     ((null sucessores) (* cor (funcao-avaliacao-nos-folha (second estado) (third estado)))) 
     ((equal d 0) (* cor (funcao-avaliacao-pseudo-nos-folha (second estado) (third estado)))) 
     (t (negamax-sucessores estado (- d 1) (- cor) sucessores (- alfa) (- beta))))))


(defun tabuleiro-teste ()
'(((-1 NIL NIL NIL NIL NIL NIL NIL NIL NIL)
 (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
 (NIL 05 NIL NIL NIL NIL NIL NIL NIL NIL)
 (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
 (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
 (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
 (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
 (NIL NIL NIL NIL NIL NIL NIL NIL 03 NIL)
 (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
 (NIL NIL NIL NIL NIL NIL NIL NIL NIL -2)) 0 0 -1))
