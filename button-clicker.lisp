
;;model that finds and clicks buttons
;;load ACT-R prior to loading this model
;;  if you want ACT-R to be loaded automatically, insert real path to actr7 on line 8 below

#-:act-r
  (if (not (find :act-r *features*))
    (load "actr7/load-act-r.lisp"))

(defvar *clicks* 0)

(clear-all)
(define-model button-clicker
  
  (start-hand-at-mouse)

  (p start
     ?goal>
      buffer empty
     ?manual>
      state free
     ==>
	 +goal>
	  isa chunk
     +visual-location>
      isa visual-location 
	  color blue)
  
  (p found-link
     =goal>
      isa chunk
     =visual-location>
      isa visual-location
      color blue
     ?visual>
      state free
     ==>
	 -goal>
     +visual>
      isa move-attention
      screen-pos =visual-location)
  
  (p attended-link
     ?goal>
      buffer empty
     =visual>
	  color blue
     ?manual>
      state free
     ==>
	 +goal>
	  isa chunk
     +manual>
      isa move-cursor
      object =visual)
  
  (p click-link
     =goal>
      isa chunk
     ?manual>
      state free
     ?visual>
      state free
     ==>
	!eval! (model-output "CLICKING [~a]" (incf *clicks*))
     +manual>
      isa click-mouse
	 +visual-location>
      isa visual-location 
	  color blue)
  
  )
  
