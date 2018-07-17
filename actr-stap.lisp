;; act-r device for tcp-tasks running STAP7.1 API
;;  this code de-serializes STAP display updates to create standard text/button elements in ACT-R visicon, and serializes ACT-R button clicks into STAP button-click actions
;;
;; to run your model:
;;  after you define your model, use (run-tcp-task :host HOST :port PORT :real-time REALTIME_OR_NOT :pause-between-actions PAUSE_OR_NOT) to connect to task served on HOST/PORT, and run in REALTIME_OR_NOT
;;		the suggestion is to always set :pause-between-actions flag to T; however, it you need the model-time to keep going even when there are no buttons to press and no scheduled display changes, set this flag to NIL
;;			(if you do set :pause-between-actions to NIL, you may want to also set :real-time to T -- otherwise a lot of model time may pass in the instances when new display updates are coming over the socket)
;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load modules
(eval-when (:compile-toplevel :load-toplevel :execute)
	(ql:quickload "usocket" :silent t)
	(ql:quickload "st-json" :silent t)
	(ql:quickload "bordeaux-threads" :silent t))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define constants
(defconstant +stap-implemented-options+ '("T" "U" "onedit"))
(defconstant +stap-load-event+ '(0))
(defconstant +pixel-offset+ 10)
(defconstant +refresh-rate+ .05)
(defconstant +socket-timeout+ 4)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global variables
(defvar *socket* nil)
(defvar *socket-read-thread* nil)
(defvar *hierarchical-display* (list nil))
(defvar *options* (make-hash-table :test 'eq))
(defvar *child-parent* (make-hash-table :test 'eq))
(defvar *level* (make-hash-table :test 'eq))
(defvar *running-in-real-time* t)
(defvar *pause-between-actions* t)
(defvar *updating* nil)
(defvar *q-lock* (uni-make-lock "q-lock"))
(defvar *stap-events-unscheduled* nil)
(defvar *stap-event-times* nil)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions
(defun jsop (o)
	(eq 'st-json:jso (type-of o)))
(defun eql-text (sym-or-str sym-or-text)
	(or (eq sym-or-str sym-or-text)
		(if (not (symbolp sym-or-text)) (equal sym-or-str (DIALOG-ITEM-TEXT sym-or-text)))))
(defun eql-car-text (sym-or-str sym-or-text-cons)
	(eql-text sym-or-str (car sym-or-text-cons)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; debugging (can get rid of these functions when ready for production)
;;
(defun print-hash (tbl &optional padding)
	(maphash #'(lambda (key val)
		(format t "~a~s : ~s~%" (or padding "") key val)) tbl))
(defun print-options ()
	(maphash #'(lambda (key val)
		(format t "~a:~%" key)
		(print-hash val "    ")) *options*))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; async socket client
;;
(defun socket-close (&optional (socket *socket*)) (if socket (usocket:socket-close socket)))
(defun socket-readlines (with-line-f &optional (socket *socket*))
	(setq *socket-read-thread*
		(bt:make-thread
			(lambda ()
				(handler-case
					(let ((line))
						(loop while (setq line (handler-case
								(read-line (usocket:socket-stream socket) nil nil)
								(stream-error (e) nil))) do
							(model-output "<= ~a" line)
							(funcall with-line-f line)))
					(error (e) (model-output "Error encountered: ~a" e)))
				(model-output "Closing socket.")
				(display-update)
				(socket-close socket)))))
(defun socket-writeline (s &optional (socket *socket*))
	(model-output "=> ~a" s)
	(write-line s (usocket:socket-stream socket))
	(force-output (usocket:socket-stream socket)))

(defun socket-writejson (data &optional (socket *socket*))
	(socket-writeline (st-json:write-json-to-string data)))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STAP for ACT-R
;;
(defun prep-device ()
	(when (not (current-device))
		(suppress-warnings
			(install-device (open-exp-window "task" :x 0 :y 0 :width 800 :height 1200 :visible t))))
	(add-word-characters #\_ #\- #\+ #\* #\# #\) #\( #\. #\, #\; #\! #\? #\space #\newline #\\ #\/ #\: #\* #\" #\')
	(clear-exp-window))
(defun prep-model ()
	(suppress-warnings
		(chunk-type (button (:include oval)))
		(define-chunks (button name oval))
		(merge-chunks button oval))
	(prep-device))
(defun run-tcp-task (&key (host "localhost") (port 9000) (real-time t) (pause-between-actions t))
	(if (current-model)
		(progn
			(prep-model)
			(setq *print-visicon* (no-output (car (sgp :v))))
			(setq *pause-between-actions* pause-between-actions)
			(socket-close *socket*)
			(setq *socket* (handler-case
				(usocket:socket-connect host port :timeout +socket-timeout+)
				(error (e) (print-warning "Could not connect to ~a:~a (~a)" host port e) nil)))
			(when *socket*
				(socket-writejson (list (get-time) 0 +stap-load-event+)) ;; let task-sw know that user-sw is ready
				(socket-readlines #'stap-update) ;; start reading from a socket (in new thread)
				(loop while (neq *updating* 'done)) ;; wait for the first display update
				(schedule-periodic-event +refresh-rate+ 'proc-and-print :maintenance t)
				(run-until-condition 'stop-condition :real-time (setq *running-in-real-time* real-time))))
		(print-warning "Please load a model before connecting to a task.")))

(defun stop-condition ()
	(and (not (bt:thread-alive-p *socket-read-thread*)) (not *stap-event-times*) (not *updating*) (not *stap-events-unscheduled*)))

;; update display based on stap string
(defun stap-update (json-string)
	(display-update 'in-progress)
	(let ((data (st-json:read-json-from-string json-string)))
		(etypecase data
			(st-json:jso
				(dolist (option (slot-value data 'st-json::alist))
					(cond
						((equal (car option) "require")
							(dolist (required (slot-value (cdr option) 'st-json::alist))
								(cond
									((equal (car required) "options")
										(dolist (option (cdr required))
											(when (not (find option +stap-implemented-options+ :test 'equal))
												(print-warning "This task requires unimplemented option: ~s" option)
												(socket-close *socket*))))
									(t
										(print-warning "This task requires something, and i don't know how to handle it:~%    { ~s : ~s }"
											(car required) (cdr required))
										(socket-close *socket*)))))
						(t
							(print-warning "Ignoring directive { ~s : ~s }" (car option) (cdr option))))))
			(list
				(update-element *hierarchical-display* nil data 1))
			((or keyword number string)
				(when (eq data :null)
					(setq *hierarchical-display* (list nil))
					(clear-exp-window)))))
	(display-update))

;; events
(defun past-time (time) (<= time (get-time)))
(defun remove-stap-events ()
	(setq *stap-event-times* (remove-if 'past-time *stap-event-times*)))
(defun stap-event (event-time fct)
	(uni-lock *q-lock*)
	(push (cons event-time fct) *stap-events-unscheduled*)
	(uni-unlock *q-lock*))

(defun stap-schedule-events ()
	(loop while *stap-events-unscheduled* do
		(let* ( (e (pop *stap-events-unscheduled*))
				(event-time (car e))
				(fct (cdr e)) )
		(schedule-event (/ event-time 1000) fct)
		(push event-time *stap-event-times*)
		(schedule-event (/ (1+ event-time) 1000) 'remove-stap-events))))

;; model actions
(defun button-click (btn)
	(let ((oninput (get-option btn "onedit")))
		(if oninput
			(let (  (valopt (st-json:jso))
					(btnpos (get-position btn (gethash btn *child-parent*))) )
				(push (cons "id" btnpos) (slot-value valopt 'st-json::alist))
				(if (jsop oninput)
					(dolist (option (slot-value oninput 'st-json::alist))
						(push option (slot-value valopt 'st-json::alist)))
					(push (cons "v" oninput) (slot-value valopt 'st-json::alist)))
				(display-update 'in-progress)
				(process-element (gethash btn *child-parent*) valopt (gethash btn *level*))
				(display-update))))
	(socket-writejson (list (get-time) (DIALOG-ITEM-TEXT btn) t)))

(defun pause-for-something-to-do ()
	(when *pause-between-actions*
		(let ((paused nil))
			(loop while (and
				(not (find 'button-vdi (mapcar 'type-of (flatten *hierarchical-display*))))
				(not *stap-event-times*)
				(not *stap-events-unscheduled*)
				(bt:thread-alive-p *socket-read-thread*)) do
					(when (not paused)
						; (model-output "*PAUSED...")
						(setq paused t)))
			;(if paused (model-output "...CONTINUING*"))
			)))

;; visicon updates
(defun display-update (&optional (val 'done))
	(uni-lock *q-lock*)
	(setq *updating* val)
	;(model-output "*updating* = ~a" *updating*)
	(uni-unlock *q-lock*))
(defun proc-and-print ()
	(let ((proc nil))
		(uni-lock *q-lock*)
		(stap-schedule-events)
		(when (eq *updating* 'done)
			(setq proc t)
			(setq *updating* nil))
		(uni-unlock *q-lock*)
		(when proc
			(correct-positions)
			(proc-display)
			(if *print-visicon* (print-visicon))
			(pause-for-something-to-do))))
(defun correct-positions ()
	(loop for element in (flatten *hierarchical-display*)
		and position from 0 do
		(typecase element
			(static-text-vdi
				(modify-text-for-exp-window element :y (* position +pixel-offset+)))
			(button-vdi
				(modify-button-for-exp-window element :y (* position +pixel-offset+))))))

;; add/update elements
(defgeneric add-element (key val level))
(defgeneric update-element (container key val level))
;; text and numeric elements
(defmethod add-element (key val level)
	(cons (if key (add-text-to-exp-window :text key :x (* level +pixel-offset+)) (gensym))
		(add-text-to-exp-window :text (format nil "~a" val) :x  (* (1+ level) +pixel-offset+))))
(defmethod update-element (container key val level)
	(cond
		((eq 'button-vdi (type-of (car container)))
			(remove-items-from-exp-window (car container))
			(let ((new-container (add-element key val level)))
				(setf (car container) (car new-container))
				(setf (cdr container) (cdr new-container))))
		((listp (cdr container))
			(apply #'remove-items-from-exp-window (flatten (cdr container)))
			(setf (cdr container) (add-text-to-exp-window :text (format nil "~a" val) :x  (* (1+ level) +pixel-offset+))))
		(t 
			(modify-text-for-exp-window (cdr container) :text (format nil "~a" val)))))
;; boolean elements (i.e. buttons)
(defmethod add-element (key (val (eql :false)) level)
	(list (add-button-to-exp-window :text (or key "") :x  (* level +pixel-offset+) :action #'button-click :color 'blue)))
(defmethod update-element (container key (val (eql :false)) level)
	(when (neq 'button-vdi (type-of (car container)))
		(apply #'remove-items-from-exp-window (flatten container))
		(setf (car container) (car (add-element key val level)))))
;; list elements (i.e. containers)
(defun update-options (key options)
	(if options
		(let ((current-options (or (gethash key *options*) (setf (gethash key *options*) (make-hash-table :test 'equal)))))
			(dolist (o options)
				(setf (gethash (car o) current-options) (cdr o))))))
(defun get-option (key option-name)
	(or
		(let ((options (gethash key *options*)))
			(if options 
				(gethash option-name options)))
		(let ((parent (gethash key *child-parent*)))
			(if parent
				(get-option (car parent) option-name)))))

(defmethod add-element (key (val list) level)
	(let (  (container (list (if key (add-text-to-exp-window :text key :x (* level +pixel-offset+)) (gensym))))
			(e-lvl (1+ level)) )
		(dolist (e val) (process-element container e e-lvl))
		container))

(defmethod update-element (container key (val list) level)
	(if (eq 'button-vdi (type-of (car container)))
		;; if this element was a button, remove it and replace with list (formed via add-element)
		(progn
			(remove-items-from-exp-window (car container))
			(let ((new-container (add-element key val level)))
				(setf (car container) (car new-container))
				(setf (cdr container) (cdr new-container))))
		(progn
			;; if this element was text/number, replace the value with empty list
			(when (not (listp (cdr container)))
				(remove-items-from-exp-window (cdr container))
				(setf (cdr container) nil))
			;; update all elements in list based on new values
			(let ((e-lvl (1+ level)))
				(dolist (e val) (process-element container e e-lvl)))))
	(if (car container)
		(modify-text-for-exp-window (car container) :height (* (length (flatten (cdr container))) +pixel-offset+))))

(defun process-element (container e e-lvl)
	(let (	(e-key nil)
			(e-val :undefined)
			(e-opt nil) )
		(if (jsop e)
			(setq
				e-opt (slot-value e 'st-json::alist)
				e-key (cdr (assoc "id" e-opt :test 'equal))
				e-val (cdr (assoc "v" e-opt :test 'equal)))
			(setq e-val e))
		; (model-output "processing {id:~a, v:~a, ~a}" e-key e-val e-opt)
		(let ( (delay (or (assoc "U" e-opt :test 'equal) (assoc "T" e-opt :test 'equal))) )
			(if delay
				(stap-event (if (equal (car delay) "U") (cdr delay) (+ (get-time) (* (cdr delay) 1000)))
						#'(lambda ()
							(display-update 'in-progress)
							(set-key-val-opt container e-key e-val e-opt e-lvl)
							(display-update)))
				(set-key-val-opt container e-key e-val e-opt e-lvl)))))

(defun set-key-val-opt (container e-key e-val e-opt e-lvl)
	(let ( (item (if (numberp e-key)
					(nth e-key (cdr container))
					(assoc e-key (cdr container) :test 'eql-text))) )
		(if (eq e-val :null)
			;; delete element
			(when item
				(apply #'remove-items-from-exp-window (remove-if 'symbolp (flatten item)))
				(remhash (car item) *options*)
				(remhash (car item) *child-parent*)
				(remhash (car item) *level*)
				(setf (cdr container) (remove item (cdr container))))
			(progn
				(if item
					;; update element
					(if (neq e-val :undefined)
						(update-element item e-key e-val e-lvl))
					;; add new element
					(progn
						(push-last
							(setq item
								(add-element
									(if (numberp e-key) nil e-key)
									(if (eq e-val :undefined) nil e-val)
									e-lvl))
							(cdr container))
						(setf (gethash (car item) *child-parent*) container)
						(setf (gethash (car item) *level*) e-lvl)
						))
				(update-options (car item) e-opt)))))

(defun get-position (item container)
	(position item (cdr container) :test 'eql-car-text))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
