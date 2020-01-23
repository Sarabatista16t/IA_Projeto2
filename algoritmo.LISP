;; ================================================================================================================================
;;                                            IMPLEMENTAÇÃO DO NEGAMAX 
;; ================================================================================================================================
;;;; procura.lisp
;;;; Disciplina de IA - 2019 / 2020
;;;; Projeto 2
;;;; Autores: Sara Batista [nÂº 170221054] e Carolina Castilho [nÂº 180221071]


;; =====================================================================
;;                         NEGAMAX
;; =====================================================================

(defun negamax-sucessores (estado d cor sucessores alfa beta &optional (bestValue most-negative-fixnum))
  (cond
   ((null sucessores) alfa)
   (t
    (let* ((valorSucessor (- (negamax (first sucessores) d (- cor) (- alfa) (- beta))))
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
     (t (negamax-sucessores estado (- d 1) cor sucessores alfa beta)))))


(defun tabuleiro-teste ()
'(((-1 nil 44 01 51 39 43 21 70 34)
 (77 71 61 23 69 62 94 83 33 57)
 (60 03 30 78 74 38 15 93 28 67)
 (48 76 52 95 97 42 92 99 47 09)
 (31 96 00 22 73 37 16 27 91 82)
 (11 24 68 04 89 49 81 98 56 14)
 (18 88 nil 05 55 64 85 13 41 86)
 (75 44 87 79 36 26 90 80 29 66)
 (12 58 40 72 84 59 06 08 50 46)
 (32 10 25 17 63 45 54 19 -2 65)) 2 35 -1))
