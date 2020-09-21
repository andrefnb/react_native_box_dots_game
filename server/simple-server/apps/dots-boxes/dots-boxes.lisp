;;; dots-boxes.lisp
;;; Definicao de um serviço web para o jogo dos pontos e das caixas, comum com a disciplina de computação móvel. O serviço web é baseado no servidor Aserve e na framework Simple Server.  Permite criar uma aplicação web baseada em JSON.
;;; Adapted from https://www.cs.northwestern.edu/academics/courses/325/readings/web-services.php#simple-server
;;; JSON server built on Simple Server
;;;
;;; Defines STOP-SERVER and PARAM-VALUE
;;; Needs server-specific implementations of DEFROUTE and START-SERVER

(defpackage #:140221017-140221002
 (:use #:common-lisp #:json #:simple-server))

(in-package :140221017-140221002)

;;;
;;; Key parts:
;;;   - static HTML5 files (HTML, CSS, JavaScript, images, ...)
;;;   - JavaScript in browser sends and gets JSON from the server, using AJAX
;;;   - Lisp code on server gets data from request parameters and JSON, and returns JSON
;;;
;;;
;;; One form lets a user store a key-value pair on the server, and get back a JSON
;;; object with all stored key value pairs. This example illustrates sending data in JSON form.
;;;



;;; Publishing routes to JSON endpoints
;;; -----------------------------------


;;; POST /save-data?name=... + JSON data
;;;   Push the list equivalent of the JSON data in the global Lisp
;;;   variable name
;;;   Return the new list of stored values.

(defroute :post "/save-data" 'save-data)

(defvar *alist* nil)

;;Variavel global que guarda o valor da jogada a escolher enquanto o alfabeta decorre
(defvar *jogada* nil)

;;Variavel que guarda a peca da maquina
(defvar *peca-maquina* nil)

;;Variavel que guarda o nr de cortes alfa feitos
(defvar *cortes-alfa* 0)

;;Variavel que guarda o nr de cortes beta feitos
(defvar *cortes-beta* 0)

;;Variavel que guarda os nos explorados
(defvar *nos-explorados* 0)

(defun save-data (data params)
  (encode-json-alist-to-string (update-alist (get-json-from-string data))))

(defun get-json-from-string (str)
  (and str (stringp str) (> (length str) 0)
       (decode-json-from-string str)))

(defun update-alist (alist)
  "Atualiza a lista com a jogada da maquina"
  (setq *alist*  (formatar-saida (jogar alist))))


(defun formatar-saida (tabuleiro)
  "Permite formatar o tabuleiro com a jogada do PC antes de ser transformado numa string JSON"
    (list (list ':tabuleiro (get-arcos-horizontais tabuleiro) (get-arcos-verticais tabuleiro)))
  )


;;; ------------------------------
;;; Jogada da maquina
;;; O código a seguir deverá ser alterado para permitir jogadas com o algoritmo AlfaBeta
;;; A funcao jogar é o ponto de entrada para a execução da jogada da maquina
;;; ------------------------------

;;; faz jogada nos arcos horizontais
(defun jogar (jogada-anterior)
    "Efetua uma jogada aleatoria nos arcos horizontais para o jogador maquina com base nos dados do jogo recebidos: tabuleiro, peca da maquina,
     numero de caixas do jogador com as pecas 1 e numero de caixas do jogador com as pecas 2."
    (let*
      (
	  (peca (get-peca-maquina jogada-anterior))
      (tabuleiro (get-tabuleiro jogada-anterior))
	  (caixasJ1 (get-numero-caixas-jogador1 jogada-anterior))
	  (caixasJ2 (get-numero-caixas-jogador2 jogada-anterior))
	  (no (cria-no tabuleiro caixasJ1 caixasJ2))
	  (profundidade-max (ver-prof-max tabuleiro)))
	  (progn
            (reset-cortes)
            (setf *peca-maquina* peca)
            (alfabeta no profundidade-max peca (limite-numero-caixas-fechadas tabuleiro))
            (escreve)
            *jogada*
	  )
    )
)
  
  ;; ver-prof-max
(defun ver-prof-max (tabuleiro)
  "Funcao que recebe um tabuleiro e devolve uma profundidade apropriada para o tabuleiro recebido"
	(let ((nr-nils (ver-nr-nils tabuleiro)))
		(cond	
		((> (numero-caixas-quase-fechadas tabuleiro) 0) 1)
		((> nr-nils (/ (limite-numero-caixas-fechadas tabuleiro) 2)) 2)
		((> nr-nils 10) 3)
		(t nr-nils)
		)
	)
)
  
  ;; ver-nr-nils
(defun ver-nr-nils (tabuleiro)
  "Funcao que recebe um tabuleiro e devolve o numero de nils existentes"
  (apply '+ (mapcar #'(lambda (arco) (ver-nr-nils-arcos arco)) tabuleiro))
)
  
  ;; ver-nr-nils-arcos
(defun ver-nr-nils-arcos (arco)
  "Funcao que recebe um arco (vertical ou horizontal) e devolve o numero de nils existentes"
  (apply '+ (mapcar #'(lambda (lista) (ver-nr-nils-lista lista)) arco))
)
  
  ;; ver-nr-nils-lista
(defun ver-nr-nils-lista (lista)
  "Funcao que recebe uma lista e devolve o numero de nils existentes na mesma"
  (apply '+ (mapcar #'(lambda (elem) (cond ((null elem) 1) (t 0))) lista))
)
  
;; cria-no
(defun cria-no (tabuleiro caixasJ1 caixasJ2)
"Funcao que cria o no inicial que e mandado para o alfabeta"
  (list tabuleiro 0 caixasJ1 caixasJ2)
)

;; reset-cortes
(defun reset-cortes ()
"Funcao que mete os valores das variaveis que guardam os cortes a 0"
(progn
  (setf *cortes-alfa* 0)
  (setf *cortes-beta* 0)
  (setf *nos-explorados* 0)
  )
)

;; escreve
;Para alterar o diretorio da criação do ficheiro basta alterar /andre/ para o nome do user do seu computador 
(defun escreve ()
"Funcao que recebe o necessario para escrever na consola e no ficheiro as estatisticas da simulacao e cria uma string com o necessario para chamar a funcao escreve-ecra-ficheiro"
  (let (
        (resultados (concatenate 'string
                                 (format nil "Chamada ao alfabeta ~%" )
                                 (format nil "Cortes Alfa: ~a~%" *cortes-alfa*)
                                 (format nil "Cortes Beta: ~a~%" *cortes-beta*)
								 (format nil "Nos explorados: ~a~%~%" *nos-explorados*)
                                 )))
    (escreve-ecra-ficheiro "C:/Users/luism/Desktop/" resultados))
)

;; escreve-ecra-ficheiro
(defun escreve-ecra-ficheiro (diretorio string)
"Recebe a string a escrever na consola e no ficheiro"
  (with-open-file (stream (concatenate 'string diretorio "resultados.dat")
                          :direction :output :if-exists :append :if-does-not-exist :create)
      (princ string stream))
)
  
;; peca-jogada-atual
(defun peca-jogada-atual (peca)
"Funcao que recebe a peca atual e devolve a peca a jogar"
(cond
 ((= peca 2) 1)
 (t 2))
)

;; jogador-jogada-atual
(defun jogador-jogada-atual (no)
"Funcao que recebe um no devolve qual tipo de jogador vai jogar, entre Max e Min"
  (cond
   ((evenp (get-profundidade no)) 'MAX)
   (T 'MIN))
)

;; ordena-sucessores
(defun ordena-sucessores(sucessores peca)
"Fucao que recebe uma lista de sucessores e a peca jogar (1 e 2) e ordena os sucessores de maneira a dar vantagem ࡰe衠recebida"
  (sort sucessores #'(lambda (no1 no2)
                       (cond
                        ((and (eq peca 1) (>= (- (third no1) (fourth no1)) (- (third no2) (fourth no2)))) t)
                        ((and (eq peca 2) (>= (- (fourth no1) (third no1)) (- (fourth no2) (third no2)))) t)
                        (t nil))) )
  )
  
;; numero-caixas-fechadas
(defun numero-caixas-fechadas (tabuleiro)
"Recebe um tabuleiro e devolve o numero de caixas ja fechadas"
  (let ((linhas (car tabuleiro)) (colunas (cadr tabuleiro)))
    (cond
      ((or (eq (length linhas) 1) (eq (length colunas) 1)) 0)
      (t (+ (numero-caixas-fechadas-horizontal tabuleiro) 
        (numero-caixas-fechadas (list (cdr linhas) (tira-primeiras colunas)))))
    )
  )
)

;; limite-numero-caixas-fechadas
(defun limite-numero-caixas-fechadas (tabuleiro)
"Metodo que calcula o limite de numero de caixas fechadas no tabuleiro que o utilizador quer usar"
  (let ((horizontais (1- (length (get-arcos-horizontais tabuleiro))))
        (verticais (1- (length (get-arcos-verticais tabuleiro)))))
    (* horizontais verticais))
)

;; tabuleiro-preenchido-p
(defun tabuleiro-preenchido-p  (tabuleiro)
"Metodo que recebe um tabuleiro e verifica se estᡰreenchido"
  (cond
   ((eq (limite-numero-caixas-fechadas tabuleiro)
        (numero-caixas-fechadas tabuleiro)) t)
   (t nil))
  )

;; tira-primeiras
(defun tira-primeiras (colunas)
"Recebe um conjunto de linhas de um tabuleiro e retira os primeiros arcos"
  (mapcar #'(lambda (coluna) (cdr coluna)) colunas)
)

;; numero-caixas-fechadas-horizontal
(defun numero-caixas-fechadas-horizontal (tabuleiro)
"Recebe um tabuleiro e verifica quantas caixas ja estao fechadas entre os dois primeiros conjuntos de linhas horizontais"
  (let ((linhas (car tabuleiro)) (colunas (cadr tabuleiro)))
    (cond
      ((or (eq (length linhas) 1) (eq (length colunas) 1)) 0)
      ((ver-caixa-fechada (car linhas) (cadr linhas) (car colunas) (cadr colunas)) 
        (1+ (numero-caixas-fechadas-horizontal (list (tira-primeiras linhas) (cdr colunas)))))
      (t (numero-caixas-fechadas-horizontal (list (tira-primeiras linhas) (cdr colunas))))
    )
  )
)

;; ver-caixa-fechada
(defun ver-caixa-fechada (linha1 linha2 linha3 linha4)
"Recebe 4 listas (linhas) e ve se a primeira caixa esta fechada, devolve t se tiver"
  (cond
    ((and (ver-se-1a-caixa-potencial linha1 linha2) (ver-se-1a-caixa-potencial linha3 linha4)) t)
    (t nil)
  )
)

;; ver-se-1a-caixa-potencial
(defun ver-se-1a-caixa-potencial (linha1 linha2)
"Recebe 2 linhas e devolve se a primeira caixa e uma possivel caixa fechada"
  (cond
    ((eq (car (intersecao (ver-pos-lista-ligadas linha1) (ver-pos-lista-ligadas linha2))) 1) t)
    (t nil)
  )
)

;; ver-pos-lista-ligadas
(defun ver-pos-lista-ligadas (lista-horizontal &optional (num 1))
"Recebe uma lista (linha) de nils e t's e devolve uma lista com as posicoes dos t's"
  (cond
    ((null lista-horizontal) nil)
    ((not (eq (car lista-horizontal) nil)) (cons num (ver-pos-lista-ligadas (cdr lista-horizontal) (1+ num))))
    (t (ver-pos-lista-ligadas (cdr lista-horizontal) (1+ num)))
  )
)

;; intersecao
(defun intersecao (c1 c2)
"Funcao generica que devolve a intersecao entre dois conjuntos"
  (cond
    ((null c2) nil)
    ((esta-no-conjunto (car c2) c1) (cons (car c2) (intersecao c1 (cdr c2))))
    (t (intersecao c1 (cdr c2)))
  )
)

;; esta-no-conjunto
(defun esta-no-conjunto (elem conjunto)
"Funcao que verifica se um elemento recebido esta dentro do conjunto recebido, devolve t se estiver"
  (cond
    ((null conjunto) nil)
    ((equal elem (car conjunto)) t)
    (t (esta-no-conjunto elem (cdr conjunto)))
  )
)


;;; auxiliares do no


;; get-tabuleiro-no
(defun get-tabuleiro-no (no)
(car no)
)

;;get-profundidade
(defun get-profundidade (no)
(second no)
)

;;; Operadores do problema
;; arco-vertical-tabuleiro
(defun arco-vertical-tabuleiro (linha coluna tabuleiro simbolo)
"Metodo que recebe uma linha, uma coluna, um tabuleiro e um simbolo (1 ou 2) e efetua uma jogada na linha e coluna nos arcos verticais do tabuleiro, metendo a jogada com o simbolo recebido"
  (cond
    ((or (zerop linha) (zerop coluna)) tabuleiro)
    (t (list (car tabuleiro) (arco-aux-tabuleiro linha coluna (get-arcos-verticais tabuleiro) simbolo)))
  )
)

;; arco-horizontal-tabuleiro
(defun arco-horizontal-tabuleiro (linha coluna tabuleiro simbolo)
"Metodo que recebe uma linha, uma coluna, um tabuleiro e um simbolo (1 ou 2) e efetua uma jogada na linha e coluna nos arcos horizontais do tabuleiro, metendo a jogada com o simbolo recebido"
  (cond
    ((or (zerop linha) (zerop coluna)) tabuleiro)
    (t (list (arco-aux-tabuleiro linha coluna (get-arcos-horizontais tabuleiro) simbolo) (cadr tabuleiro)))
  )
)

;;; Funcao auxiliar dos operadores do problema
;; arco-aux-tabuleiro
(defun arco-aux-tabuleiro (linha coluna arco simbolo)
"Metodo auxiliar que recebe uma linha e coluna, um arco e simbolo e faz jogada no arco recebido, na coluna e linha recebida, com o simbolo recebido"
  (cond
    ((= coluna 1) (cons (arco-na-posicao-tabuleiro linha (car arco) simbolo) (cdr arco)))
    (t (cons (car arco) (arco-aux-tabuleiro linha (1- coluna) (cdr arco) simbolo)))
  )
)

;; arco-na-posicao-tabuleiro
(defun arco-na-posicao-tabuleiro (num tabuleiro simbolo)
"Metodo que recebe um numero onde vai ser posto o simbolo recebido numa lista"
  (cond
    ((= num 1) (cons simbolo (cdr tabuleiro)))
    (t (cons (car tabuleiro) (arco-na-posicao-tabuleiro (1- num) (cdr tabuleiro) simbolo)))
  )
)

;; posicoes-dos-sucessores
(defun posicoes-dos-sucessores (tabuleiro)
"Recebe um tabuleiro e devolve uma lista com listas que teem as linhas e colunas das jogadas"
  (list 
    (lista-posicao 0 (get-arcos-horizontais tabuleiro)) 
    (lista-posicao 0 (get-arcos-verticais tabuleiro)))
)

;; lista-posicao
(defun lista-posicao (num linhas)
"recebe um conjunto de linhas e devolve listas com o nr da lista em que se encontra e a posicao dentro desta"
  (cond
    ((null linhas) nil)
    (t (append (posicao (1+ num) 0 (car linhas)) (lista-posicao (1+ num) (cdr linhas))))
  )
)

;; posicao
(defun posicao (num pos linha)
"Recebe um numero e uma linha e devolve listas com a informacao do numero e a posicao da jogada"
  (cond
    ((null linha) nil)
    ((null (car linha)) (cons (list (1+ pos) num) (posicao num (1+ pos) (cdr linha))))
    (t (posicao num (1+ pos) (cdr linha)))
  )
)

;; sucessores-no
;;; verificar o que faz mais sentido se é devolver o no ou (list no), tendo em conta que deveria devolver uma LISTA de sucessores e não o sucessor
(defun sucessores-no (no simbolo nr-caixas-fechadas-1 nr-caixas-fechadas-2 objetivo tempo-limite)
"Funcao que recebe um no, simbolo que representa qual jogador é a jogar, o nr de caixas fechadas pelo jog 1 e o jog 2"
(cond
 ((or (tabuleiro-preenchido-p (get-tabuleiro-no no)) (<= tempo-limite (get-universal-time))) (list no))
 
 (t (alisa-sucessores (append 
      (mapcar #'(lambda (pos) (sucessores-no-aux no 'arco-horizontal-tabuleiro (car pos) (cadr pos) simbolo objetivo nr-caixas-fechadas-1 nr-caixas-fechadas-2 (- tempo-limite (* (- tempo-limite (get-universal-time)) 0.25)))) (car (posicoes-dos-sucessores (get-tabuleiro-no no))))
      (mapcar #'(lambda (pos) (sucessores-no-aux no 'arco-vertical-tabuleiro (car pos) (cadr pos) simbolo objetivo nr-caixas-fechadas-1 nr-caixas-fechadas-2 (- tempo-limite (* (- tempo-limite (get-universal-time)) 0.25)))) (cadr (posicoes-dos-sucessores (get-tabuleiro-no no)))))
	      (1+ (get-profundidade no)))))
)

;; sucessores-no-aux
(defun sucessores-no-aux (no funcao linha coluna simbolo objetivo nr-caixas-fechadas-1 nr-caixas-fechadas-2 tempo-limite)
"Funcao auxiliar a funcao sucessores que cria os sucessores com logica implementada se uma caixa for fechada, subestitui na lista esse n󠰥los seus sucessores, isto repete-se caso haja sucessores a fecharem caixas"
  (let* ((tabAtual (funcall funcao linha coluna (get-tabuleiro-no no) simbolo))
        (nr-caixas-fechadas-total (numero-caixas-fechadas tabAtual)))
    (cond 
     ((and (< (+ nr-caixas-fechadas-1 nr-caixas-fechadas-2) nr-caixas-fechadas-total) (> tempo-limite (get-universal-time)))
	 
		 (let ((caixasJ1 (cond ((eq simbolo 1) (- nr-caixas-fechadas-total nr-caixas-fechadas-2)) (t nr-caixas-fechadas-1)))
			   (caixasJ2 (cond ((eq simbolo 2) (- nr-caixas-fechadas-total nr-caixas-fechadas-1)) (t nr-caixas-fechadas-2))))
				(sucessores-no (list tabAtual (1+ (get-profundidade no)) caixasJ1 caixasJ2) simbolo caixasJ1 caixasJ2 objetivo (- tempo-limite (* (- tempo-limite (get-universal-time)) 0.1)))))
	 (t (list tabAtual (1+ (get-profundidade no)) nr-caixas-fechadas-1 nr-caixas-fechadas-2))
	)
  )
)

;; alisa-sucessores
(defun alisa-sucessores (lista profundidade)
"Metodo que alisa os sucessores que fecharam caixas, pois estes estavam a ficar numa lista dentro da lista de sucessores"
  (cond
    ((null (first lista)) nil)
	((listp (second (first lista))) (append (alisa-sucessores-aux (first lista) profundidade) (alisa-sucessores (cdr lista) profundidade)))
    (t (cons (first lista) (alisa-sucessores (cdr lista) profundidade)))
  )
)


(defun alisa-sucessores-aux (lista-nos profundidade)

  (cond
    ((null (first lista-nos)) nil)
	(t (cons (list (get-tabuleiro-no (first lista-nos)) profundidade (third (first lista-nos)) (fourth (first lista-nos))) (alisa-sucessores-aux (cdr lista-nos) profundidade)))
  )
)

;; vencedor-p
(defun vencedor-p (tabuleiro nr-caixas-fechadas-1 nr-caixas-fechadas-2)
"Metodo que recebe um tabuleiro, numero de caixas fechadas pelo jogador 1 e pelo jgador 2 e verifica se existe um vencedor e diz qual"
  (cond
    ((and (eq (+ nr-caixas-fechadas-1 nr-caixas-fechadas-2) (limite-numero-caixas-fechadas tabuleiro)) (> nr-caixas-fechadas-1 nr-caixas-fechadas-2)) 1)
    ((and (eq (+ nr-caixas-fechadas-1 nr-caixas-fechadas-2) (limite-numero-caixas-fechadas tabuleiro)) (< nr-caixas-fechadas-1 nr-caixas-fechadas-2)) 2)
    (t nil)
  )
)

;; avaliar-folha
(defun avaliar-folha (no-final tipo-jogador)
"Metodo que avalia a folha de maneira a devolver 100 se o jogador recebido 顶encedor, -100 se perdeu e 0 se caso contrario"
(let ((vencedor (vencedor-p (get-tabuleiro-no no-final) (third no-final) (fourth no-final))))
  (cond
   ((and (eq tipo-jogador 1) (eq tipo-jogador *peca-maquina*)) (avaliar-folha-aux vencedor 1))
   ((and (eq tipo-jogador 2) (eq tipo-jogador *peca-maquina*)) (avaliar-folha-aux vencedor 2))
   ((eq tipo-jogador 2) (avaliar-folha-aux vencedor 1))
   (t (avaliar-folha-aux vencedor 2))
   )
)
)

;; avaliar-folha-aux
(defun avaliar-folha-aux (vencedor tipo-jogador)
"Metodo que recebe um vencedor e o jogador a jogar, devolve 100 se o vencedor 顯 jogador atual, -100 se n䯠for e 0 se nao houver vencedor"
(cond
   ((null vencedor) 0)
   ((eq vencedor tipo-jogador) 100)
   (T -100)
   )
)

;; solucaop
(defun solucaop (no)
"Recebe um valor que representa o objetivo a atingir e verifica se o no recebido atingiu esse mesmo objetivo, neste caso sao numero de caixas fechadas"
  (cond
    ((null no) nil)
    ((eq (numero-caixas-fechadas (get-tabuleiro-no no)) (limite-numero-caixas-fechadas (get-tabuleiro-no no))) t)
    (t nil)
  )
)

;; avaliar-folha-limite
(defun avaliar-folha-limite (no tipo-jogador)
"Metodo que recebe um no e um tipo de jogador e devolve 50 se houver uma caixa quase fechada desse jogador, devolve -50 se houver uma caixa quase fechada do jogador adversario e 0 caso nao hajam caixas quase fechadas"
  (cond
   ((eq tipo-jogador *peca-maquina*)
    (+ (cond
       ((= *peca-maquina* 1) (- (third no) (* (fourth no) 1.5)))
       (t (- (fourth no) (* (third no) 1.5))))
	 (* (numero-caixas-quase-fechadas (get-tabuleiro-no no)) 0.5)))
   ((eq tipo-jogador 1) (- (fourth no) (* (third no) 1.5)  (* (numero-caixas-quase-fechadas (get-tabuleiro-no no)) 0.5)))
   (t (- (third no) (* (fourth no) 1.5)  (* (numero-caixas-quase-fechadas (get-tabuleiro-no no)) 0.5)))
   )
  )


;; avaliar-no
(defun avaliar-no (no tipo-peca)
"Metodo que recebe um no e tipo de peca e se o tabuleiro estiver preenchido, avalia a folha, se nao executa o avaliar-folha-limite"
  (cond
    ((tabuleiro-preenchido-p (get-tabuleiro-no no)) (avaliar-folha no tipo-peca))
    (t (avaliar-folha-limite no tipo-peca))
  )
)

;; verifica o numero de caixas quase fechadas
(defun numero-caixas-quase-fechadas (tabuleiro)
"Recebe um tabuleiro e devolve o numero de caixas quase fechadas do mesmo"
  (let ((linhas (car tabuleiro)) (colunas (cadr tabuleiro)))
    (cond
      ((or (eq (length linhas) 1) (eq (length colunas) 1)) 0)
      (t (+ (numero-caixas-quase-fechadas-horizontal tabuleiro) 
        (numero-caixas-quase-fechadas (list (cdr linhas) (tira-primeiras colunas)))))
    )
  )
)

;; numero-caixas-quase-fechadas-horizontal
(defun numero-caixas-quase-fechadas-horizontal (tabuleiro)
"Recebe um tabuleiro e verifica quantas caixas estao quase fechadas entre os dois primeiros conjuntos de linhas horizontais"
  (let ((linhas (car tabuleiro)) (colunas (cadr tabuleiro)))
    (cond
      ((or (eq (length linhas) 1) (eq (length colunas) 1)) 0)
      ((ver-caixa-quase-fechada (car linhas) (cadr linhas) (car colunas) (cadr colunas)) 
        (1+ (numero-caixas-quase-fechadas-horizontal (list (tira-primeiras linhas) (cdr colunas)))))
      (t (numero-caixas-quase-fechadas-horizontal (list (tira-primeiras linhas) (cdr colunas))))
    )
  )
)

;; ver-caixa-quase-fechada
(defun ver-caixa-quase-fechada (linha1 linha2 linha3 linha4)
"Recebe 4 listas (linhas) e ve se a primeira caixa esta quase fechada, devolve t se tiver"
  (cond
    ((or (and (ver-se-1a-caixa-tem-possivel-quase-caixa linha1 linha2) (ver-se-1a-caixa-potencial linha3 linha4)) 
         (and (ver-se-1a-caixa-potencial linha1 linha2) (ver-se-1a-caixa-tem-possivel-quase-caixa linha3 linha4)))
		t)
    (t nil)
  )
)

;; ver-se-1a-caixa-tem-possivel-quase-caixa
(defun ver-se-1a-caixa-tem-possivel-quase-caixa (linha1 linha2)
"Recebe 2 linhas e devolve se a primeira caixa e uma possivel caixa quase fechada"
  (cond
    ((eq (car (intersecao (ver-pos-lista-ligadas linha1) (ver-pos-lista-ligadas linha2))) 1) nil)
    ((or (and (eq (car (ver-pos-lista-ligadas linha1)) 1) (not (eq (car (ver-pos-lista-ligadas linha2)) 1)))
     (and (eq (car (ver-pos-lista-ligadas linha2)) 1) (not (eq (car (ver-pos-lista-ligadas linha1)) 1)))) t)
    (t nil)
  )
)
  
  ;;; Manipulacao dos dados do jogo
;; get-tabuleiro
(defun get-tabuleiro (jogada-anterior)
"Retorna o tabuleiro a partir dos dados do jogo recebidos"
  (list (second (cadar jogada-anterior)) (third (cadar jogada-anterior)))
)

;; get-numero-caixas
(defun get-numero-caixas (jogada-anterior)
"Retorna o numero de caixas fechadas no tabuleiro a partir dos dados do jogo recebidos"
  (+ (get-numero-caixas-jogador1 jogada-anterior) (get-numero-caixas-jogador2 jogada-anterior))
)

;; get-numero-caixas-jogador1
(defun get-numero-caixas-jogador1 (jogada-anterior)
  (cdr (fourth (car jogada-anterior)))
)
  
;; get-numero-caixas-jogador2
(defun get-numero-caixas-jogador2 (jogada-anterior)
  (cdr (fifth (car jogada-anterior)))
)
  
;; get-peca-maquina
(defun get-peca-maquina (jogada-anterior)
  "Retorna o simbolo (1 ou 2)com o qual a maquina joga"
  (cdr (third (car jogada-anterior)))
)

  ;;; Manipulacao do tabuleiro

;; get-arcos-horizontais
(defun get-arcos-horizontais (tabuleiro)
"Retorna a lista dos arcos horizontais de um tabuleiro"
  (car tabuleiro)
)

;; get-arcos-verticais
(defun get-arcos-verticais (tabuleiro)
"Retorna a lista dos arcos verticiais de um tabuleiro"
  (cadr tabuleiro)
)

;; arco-na-posicao
(defun arco-na-posicao (indice peca lista &aux (casa (nth indice lista)))
"Recebe uma lista de arcos e insere um arco na posicao indice.
So insere o arco se nao existir nenhum arco no indice da lista"
  (cond
    (casa nil)
    (T (inserir-na-lista (1- indice) peca lista))
  )
)

;; arco-aux
(defun arco-aux (x y peca arcos)
"Recebe uma matriz de arcos e insere um arco na posicao x y"
  (let*
    (
      (lista (get-elemento-lista (1- x) arcos))
      (nova-lista (arco-na-posicao y peca lista))
    )
    (cond
      ((null nova-lista) nil)
      (T (inserir-na-lista (1- x) nova-lista arcos))
    )
  )
)

;; get-elemento-lista
(defun get-elemento-lista (indice lista)
"Retorna o elemento de uma lista de acordo com índice i"
  (cond
    ( (null lista) nil )
    ( (zerop indice) (car lista) )
    ( T (get-elemento-lista (1- indice) (cdr lista)))
  )
)

;; arco-horizontal
(defun arco-horizontal (x y peca tabuleiro)
"Recebe par de coordenadas, uma peca e um tabuleiro.
Retorna um novo tabuleiro onde foi inserido um arco na posição x y dos arcos horizontais. "
  (let*
    (
	  (arcos-horizontais (get-arcos-horizontais tabuleiro))
      (novos-arcos-horizontais (arco-aux x y peca arcos-horizontais)))
    (cond
	  ((null novos-arcos-horizontais) nil)
	  (t (inserir-na-lista 0 novos-arcos-horizontais tabuleiro))
    )
  )
)

;; arco-vertical
  (defun arco-vertical (x y peca tabuleiro)
    "Recebe par de coordenadas, uma peca e um tabuleiro.
    Retorna um novo tabuleiro onde foi inserido um arco na posição x y dos arcos verticas. "
    (let*
      (
        (arcos-verticais (get-arcos-verticais tabuleiro))
        (novos-arcos-verticais (arco-aux x y peca arcos-verticais))
      )
      (cond
        ( (null novos-arcos-verticais) nil)
        ( t (inserir-na-lista 1 novos-arcos-verticais tabuleiro) )
      )
    )
  )

;; inserir-na-lista
(defun inserir-na-lista (indice elemento lista)
"Insere um elemento numa lista, de acordo com indice.
O elemento, o indice e a lista sao recebidos por parametro."
  (cond
	((null lista) nil)
    ((zerop indice) (cons elemento (cdr lista)))
    (t (cons (car lista) (inserir-na-lista (1- indice) elemento (cdr lista))))
  )
)
