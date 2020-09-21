;;; alfabeta.lisp

(in-package :140221017-140221002)

;; alfabeta
  (defun alfabeta (no prof-max peca &optional (obj 49) (tempo-limite (+ (get-universal-time) 4)) (alpha -1000) (beta 1000))
	"Funcao que da inicio ao algoritmo alfabeta, recebendo o no inicial, profundidade maxima, a peça a jogar, e
	opcionalmente o objetivo, tempo limite, limite alfa e limite beta."
  (cond
   ((or (<= tempo-limite (get-universal-time)) (solucaop no) (= prof-max (get-profundidade no))) (avaliar-no no peca))
   ((eq (get-profundidade no) 0)
    (jogadorMax (ordena-sucessores (sucessores-no no peca (third no) (fourth no) obj tempo-limite) peca) prof-max peca obj tempo-limite alpha beta))
   (t
    (let* ((nova-peca (peca-jogada-atual peca))
           (sucessores (sucessores-no no (peca-jogada-atual peca) (third no) (fourth no) obj tempo-limite)))
      (cond
       ((eq 'MAX (jogador-jogada-atual no))
        (jogadorMax sucessores prof-max nova-peca obj (- tempo-limite (* (- tempo-limite (get-universal-time)) 0.1)) alpha beta))
       (t
        (jogadorMin sucessores prof-max nova-peca obj (- tempo-limite (* (- tempo-limite (get-universal-time)) 0.1)) alpha beta)))))
   )
  )
  
;; jogadorMax
  (defun jogadorMax (sucessores prof-max peca obj tempo-limite alpha beta)
  "Funcao que representa uma jogada Max."
  (let ((novo-max (max alpha (alfabeta (car sucessores) prof-max peca obj tempo-limite alpha beta))))
    
    (progn
      (cond
       ((and (not (= novo-max alpha)) (= (get-profundidade (car sucessores)) 1))
        (setf *jogada* (get-tabuleiro-no (car sucessores)))))
      (setf *nos-explorados* (1+ *nos-explorados*))
      (cond
       ((>= novo-max beta)
        (progn (setf *cortes-beta* (1+ *cortes-beta*)) beta)
        )
       ((null (cdr sucessores)) novo-max)
	   ((<= tempo-limite (get-universal-time)) novo-max)
       (t (jogadorMax (cdr sucessores) prof-max peca obj (- tempo-limite (* (- tempo-limite (get-universal-time)) 0.1)) novo-max beta)))
      )
    )
  )
  
;; jogadorMin
  (defun jogadorMin (sucessores prof-max peca obj tempo-limite alpha beta)
  "Funcao que representa uma jogada Min."
  (let ((novo-min (min beta (alfabeta (car sucessores) prof-max peca obj tempo-limite alpha beta))))
    (cond
     ((<= novo-min alpha) 
      (progn (setf *cortes-alfa* (1+ *cortes-alfa*)) alpha)
      )
     ((null (cdr sucessores)) novo-min)
	 ((<= tempo-limite (get-universal-time)) novo-min)
     (t (jogadorMin (cdr sucessores) prof-max peca obj (- tempo-limite (* (- tempo-limite (get-universal-time)) 0.1)) alpha novo-min)))
    )
  )