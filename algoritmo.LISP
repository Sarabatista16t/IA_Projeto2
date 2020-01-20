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
  (let ((sucessores (sucessores estado (operadores) jogador)))
    (cond
     ((null sucessores) (* (cor jogador) (funcao-avaliacao-nos-folha (second estado) (third estado)))) 
     ((equal d 0) (* (cor jogador) (funcao-avaliacao-pseudo-nos-folha (second estado) (third estado)))) 
     (t (negamax-sucessores estado (- d 1) (- jogador) sucessores (- alfa) (- beta))))))

(defun cor (jogador)
  (cond
   ((equal jogador -1) 1)
   ((equal jogador -2) -1)))
