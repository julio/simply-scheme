;; SIMPLE INTERFACE TO THE STK GRAPHICS TOOLBOX. - by Boley
(define gdraw:version "gdraw Mini-graphics package (version 2009-04-8) ready...\n")

;These are a simple set of graphics functions based on the STK interface

;to the Tk toolbox.  The coordinate system starts with the
;origin in the upper left corner and extends 540 pixels across in
;the x and down in the y directions (actually the extent can be adjusted by
;using a call of the form: (set-wIDorTagth! 0 600 600) ).


; To begin, use (init-graphics). This will create a window that you will
; be able to draw on.

;The following are the main functions intended for the user:
;
; (draw-line x1 y1 x2 y2)	  draws a line from (x1,y1) to (x2,y2).

; (draw-oval x1 y1 x2 y2)	  draws an oval from (x1,y1) to (x2,y2).
; (draw-rectangle x1 y1 x2 y2)	  draws a rectangle from (x1,y1) to (x2,y2).
; (draw-text string x y . options)puts text string at (x,y), centered.

;                                 The only option available is 'corner to
;                                 anchor the text at the lower left corner.
; (draw-image x1 y1 imagename)    draws an image at x1 y1 with that name


; (clear-graphics!)		  clears graphics window
; (delete! IDorTag)                    delete an item on the canvas
; (stall . optional-prompt)	  updates display.  If prompt is supplied
;                                 then wait for user to hit a key, which is

;                                 returned as the result.
; (print-canvas filename . opt)   create a postscript copy of the canvas.  [NEW]
;
;For the TK interface, the "draw" and "fill" functions above return a positive

;numerical id affiliated with the object just created.  By referring to that
;numerical id, one can modify many properties of that object using the
;following functions, of which the first three are the principal ones.

;Using IDorTag = 0 refers to the underlying canvas.
;
; (get-binding  IDorTag)               get binding for item # IDorTag
; (set-binding! IDorTag THUNK)         set binding for item # IDorTag (see below)
;

;  NOTE: You may also specify a type of binding as an option last argument.
;        The types are listed in the variable "binding-map"
;          i.e. (set-binding! 0 (lambda () (debug-message "hi")) 'doubleclick)

;        When the IDorTag is 0, the binding is set to the window/canvas.
;
; (get-mouse-coords)              get the current mouse coordinates
;
; The following functions access more of the TK interface, but you should 

; not have to used these for most simple things.
;
; (get-coords  IDorTag)                get coordinates for item # IDorTag
;                                 [for IDorTag=0 gives absolute position
;                                  of upper left corner]

; (set-coords! IDorTag CoordinateList) set coordinates for item # IDorTag
;                                 [IDorTag must be positive]
; (get-text  IDorTag)                  get text appearing for item # IDorTag

; (set-text! IDorTag new_text)         set text appearing for item # IDorTag
; (get-fill-color  IDorTag)            get fill color for item.
; (set-fill-color! IDorTag color)      set fill color for item (see colors listed below).

; (get-outline-color  IDorTag)         get outline color for item.
; (set-outline-color! IDorTag color)   set outline color for item (see colors listed below).
;Sample legal colors: red white green blue black yellow cyan magenta orange 

;purple magenta brown pink ; the empty string "" clears the color.
; (get-width  IDorTag)                 get width (or thickness) for item 
; (set-width! IDorTag new-width)       set width for item
;                                 [for IDorTag=0, gives 2D dimensions of canvas]

; (get-font-size  IDorTag)             get font size for text item 
; (set-font-size! IDorTag new-size)    set font size for text item, if size is legal.
;Sample legal font sizes 75 80 100 120 140 180 240 (and possibly 300)

; (raise! IDorTag)                     raise item above all other items
; (lower! IDorTag)                     lower item below all other items
; (get-motion-binding  IDorTag)        get motion binding for item
; (set-motion-binding! IDorTag thunk)  set motion binding for item (see below)

; (get-type IDorTag)                   get type of item
; (get-IDorTags)                       get list of all existing IDorTags
; (get-properties IDorTag)             get all TK properties I can think of
;                                 IDorTag=-1 -> root window, IDorTag=0 -> canvas.

; (set-property! IDorTag property . values) set arbitrary TK property (raw interface)
;
;In each of the above, use the "get" fcn to see the format for the
;new value to be used in the corresponding "set" fcn.

;In some cases, the format depends on the type of the item.
;All IDorTags are positive integers returned by the "draw" function that
;created the item.  IDorTag=0 refers to the whole canvas.

;Here we describe the use of bindings.   By saying

;             (set-binding! IDorTag THUNK)
;the procedure THUNK will be called whenever a mouse button is
;pressed on item # IDorTag.  Clear the binding by using THUNK equal to "".
;Setting the binding for IDorTag=0 will call the THUNK if the mouse is clicked

;while anywhere over the canvas.
;
;The call:    (set-motion-binding! IDorTag THUNK)
;works the same way, but this THUNK is called whenever the mouse is *moved*
;while over item IDorTag (or while anywhere over the canvas if IDorTag=0).


;;;
;;
;; Debugging Tools and Demos
;;
;;;


(define *last-debug-message* #F)

(define (debug-message txt)
  (if *last-debug-message*
      (set-text! *last-debug-message* txt)
      (begin (set! *last-debug-message* (draw-text txt 50 10))

	     (set-fill-color! *last-debug-message* 'red))))

(define (clear-debug-message)
  (delete! *last-debug-message*)
  (set! *last-debug-message* #F))
  
#|
Before you try these demos, type (init-graphics).


DEMO 1 -- 
; This demo shows you motion bindings and how the coordinates work.

(set-motion-binding! 0 (lambda () (debug-message (get-mouse-coords))))

; This should show you the mouse coordinates at the top left while you

; move your mouse.

; Clear the demo with these line
(set-motion-binding! 0 "")
(clear-debug-message)



; This shows off other types of bindings.
(draw-rectangle 100 100 300 200 'tag 'rect)

(set-binding! 'rect (lambda () (set-color! 'rect 'red)) 'enter)
(set-binding! 'rect (lambda () (set-color! 'rect 'black)) 'leave)
; This should change the color of the rectangle as you mouse over.


; Clear the demo with these line)
(delete! 'rect)

DEMO 2 -- 

(define txt (draw-text "SURPRISE!!!" 200 300))
(define rect (draw-rectangle 100 200 300 400))
(set-color! rect "red")

(set-binding! rect (lambda () (set-color! rect "")))

; Now if you click the rectangle it show you a surprise.

; Here is an example using motion.  Type in the above followed by:

(set-binding! 

 txt (lambda ()
       (set-motion-binding! 
	0
	(lambda ()
	  (set-binding! 0 (lambda () (set-binding! 0 "")
				  (set-motion-binding! 0 "")))
	  (set-coords! txt (get-mouse-coords))))))  


; If you click on the txt that says SURPRISE!!!, you should be
; able to drag it until you click again.

; Clear the demo with these lines
(delete! txt)
(delete! rect)

DEMO 3 --
(define rect (draw-rectangle 10 20 100 150))

(define (add-one n) (+ 1 n))
(define continue #T)
(define (move-once object)
  (set-coords! object (map add-one (get-coords object))))
(define (crawl)
  (move-once rect)
  (if continue (after 50 crawl)))



; The command       (after 50 crawl) means "call the crawl fcn after 50 msec".
; Now type          (crawl)                 and watch the rectangle crawl.
; To stop it type:  (set! continue #F)    

;
; You could instead trigger the motion with a mouse click by typing:

(set-binding! rect
	      (lambda () (set! continue (not continue))
		      (crawl)))

; Clear the demo with these lines
(delete! rect)


|#


;;;
;;
;; Drawing Tools
;;
;;;

(define (set-canvas-size x y)
  (set-width! 0 x y))

(define (retrieve-coordinates options)
   (if (or (null? options) (not (number? (car options))))

      '()
      (cons (car options) (retrieve-coordinates (cdr options)))))
      
(define (remove-coordinates options)
   (if (or (null? options) (not (number? (car options))))
      options
      (remove-coordinates (cddr options))))    



(define (make-options options-list)
   (if (null? options-list)
      '()
      (cons `(word '- ',(car options-list)) (cons `',(cadr options-list)
            (make-options (cddr options-list))))))


(define (draw-line . options)     ;; draws a line 
      (if (or (memq 'fill options) (member "fill" options))
         (eval (append '(.drawwindow 'create 'line)  
                                       (retrieve-coordinates options)

                                       (make-options (remove-coordinates options))))   
         (eval (append '(.drawwindow 'create 'line)  
                                       (retrieve-coordinates options)

                                       (list :fill *fill-color*)
                                       (make-options (remove-coordinates options)))))) 

(define (draw-curved-line . options)     ;; draws a curved-line

      (apply draw-line (append options (list 'smooth #t)))) 

(define (draw-text string x y . options) ;; draws text at (x,y)
   (if (or (memq 'fill options) (member "fill" options))
      (eval (append `(.drawwindow 'create 'text 

                                  ,x ,y
                                  :text ',string 
				  :font "-*-Arial-r-r-Normal--*-150-*-*-*-*-*-*"
				  )
                                  (make-options options)))   

      (eval (append `(.drawwindow 'create 'text  
                                  ,x ,y
                                  :text ',string  
                                  :fill *fill-color*
				  :font "-*-Arial-r-r-Normal--*-150-*-*-*-*-*-*"

				  )
                                  (make-options options))))) 

(define (draw-oval x1 y1 x2 y2 . options)     ;; draws an oval from (x1,y1) to (x2,y2).
   (eval (append `(.drawwindow 'create 'oval  

                                       ,x1 ,y1
                                       ,x2 ,y2)
                  (if (not (or (memq 'outline options) (member "outline" options)))
                     `('-outline *outline-color*)

                     '())
                  (if (not (or (memq 'fill options) (member "fill" options)))
                     `('-fill *fill-color*)
                     '())
                     (make-options options))))                                                         


(define (draw-rectangle x1 y1 x2 y2 . options);; draws a rectangle from (x1,y1) to (x2,y2)
   (eval (append `(.drawwindow 'create 'rectangle 
                                       ,x1 ,y1
                                       ,x2 ,y2)

                  (if (not (or (memq 'outline options) (member "outline" options)))
                     `('-outline *outline-color*)
                     '())
                  (if (not (or (memq 'fill options) (member "fill" options)))

                     `('-fill *fill-color*)
                     '())
                     (make-options options))))  

(define (draw-polygon . options)
    (eval (append '(.drawwindow 'create 'polygon)

                   (retrieve-coordinates options) 
                  (if (not (or (memq 'outline options) (member "outline" options)))
                     `('-outline *outline-color*)
                     '())

                  (if (not (or (memq 'fill options) (member "fill" options)))
                     `('-fill *fill-color*)
                     '())
                     (make-options (remove-coordinates options)))))

                     

(define (draw-arc x1 y1 x2 y2 . options)     ;; draws an arc from (x1,y1) to (x2,y2).
   (eval (append `(.drawwindow 'create 'arc  
                                       ,x1 ,y1

                                       ,x2 ,y2)
                  (if (not (or (memq 'outline options) (member "outline" options)))
                     `('-outline *outline-color*)
                     '())

                  (if (not (or (memq 'fill options) (member "fill" options)))
                     `('-fill *fill-color*)
                     '())
                     (make-options options))))     


(define (draw-image x y name . options) 
  (eval (append `(.drawwindow 'create 'image ,x ,y 
			      '-image ,(image 'create 'photo :file 
					      (if (and (symbol-bound? '*gamesman-path*) *gamesman-path*)

						  (string-append *gamesman-path* "images/" name)
						  name)))
		(make-options options))))
                               
(define (clear-graphics!)
  (for-each (lambda (item) (.drawwindow 'delete item))

	    (.drawwindow 'find 'all)))

(define (stall . optional-prompts) ; update display; optionally seek user input
  (if (null? optional-prompts)
      (begin (flush) (update) #T)
      (begin (for-each display optional-prompts) (flush) (update)

	     (read-char))))

;; now create a new empty canvas. ;; How to put it in a new window?
(define (init-graphics)
  (let ((window-exists (winfo 'exists '.drawwindow)))
    (if (number? window-exists)

	(display "You are using an old version of STK. This graphics package may not work.\n"))
    (if (or (not window-exists)
	    (and (number? window-exists) (= 0 window-exists)))
	(begin
	  (canvas '.drawwindow :height "6i" :width "6i" :bg "white")

	  (pack .drawwindow :fill "both" :expand #T)
	  (display gdraw:version))
	(clear-graphics!))))

(define (print-canvas filename . options)
  ; save a postscript image of the canvas
  ; options:

  ;     :colormap       varName 
  ;     :colormode      "color" | "grey" | "mono"
  ;     :file           fileName        e.g. "all.ps"
  ;     :fontmap        varName

  ;     :height         size            e.g. "8i"
  ;     :pageanchor     anchor
  ;     :pageheight     size            e.g. "8i"
  ;     :pagewidth      size            e.g. "8i"

  ;     :pagex          position
  ;     :pagey          position
  ;     :rotate         boolean
  ;     :width          size            e.g. "8i"
  ;     :x              position
  ;     :y              position

  (define item .drawwindow)
  (if (not (null? options))
      (cond ((procedure? (car options))     (set! item (car options))
                                            (set! options (cdr options)))
            ((not (keyword? (car options))) (set! options (cdr options)))))

  (apply item 'postscript :file filename options))

(define *fill-color* "black")
(define *outline-color* "black")

;;;  UTILITIES

(define (gdraw:last L)     ;; get last item in a list (used for properties)

  (if (pair? (cdr L)) (gdraw:last (cdr L)) (car L)))

;;; GET AND SET PROPERTIES....

(define (get-fill-color IDorTag)         ;; get color for item.
      (if (or (not (number? IDorTag)) (positive? IDorTag))

         (gdraw:last (.drawwindow 'itemconfigure IDorTAG :fill))
         (error "Canvas has no fill color")))

(define get-color get-fill-color)

(define (get-outline-color IDorTag)         ;; get color for item.

      (if (or (not (number? IDorTag)) (positive? IDorTag))
         (gdraw:last (.drawwindow 'itemconfigure IDorTag :outline))
         (error "Canvas has no outline color")))

(define (get-bg-color IDorTag)

      (if (and (number? IDorTag) (= IDorTag 0))
         (gdraw:last (.drawwindow 'configure :background))
         (gdraw:last (.drawwindow 'itemconfigure IDorTag :background))))

(define (set-fill-color! IDorTag color)  ;; set color for item.

  (if (or (null? color) (not color)) (set! color ""))
  (if (and (number? IDorTag) (= IDorTag 0)) 
      (.drawwindow 'configure :fill color)
      (.drawwindow 'itemconfigure IDorTag :fill color)))


(define set-color! set-fill-color!)
      
(define (set-outline-color! IDorTag color)  ;; set color for item.
  (if (or (null? color) (not color)) (set! color ""))
  (if (and (number? IDorTag) (= IDorTag 0)) 

      (.drawwindow 'configure :outline color)
      (.drawwindow 'itemconfigure IDorTag :outline color)))
      
(define (set-bg-color! IDorTag color)  ;; set color for item.
  (if (or (null? color) (not color)) (set! color ""))

  (if (and (number? IDorTag) (= IDorTag 0)) 
      (.drawwindow 'configure :background color)
      (.drawwindow 'itemconfigure IDorTag :background color)))

(define (get-text IDorTag)          ;; get displayed text for item

  (if (or (not (number? IDorTag)) (positive? IDorTag))
      (gdraw:last (.drawwindow 'itemconfigure IDorTag :text))
      ""))
      
(define (set-text! IDorTag new-text);; set displayed text for item

  (if (or (not (number? IDorTag)) (positive? IDorTag))
      (.drawwindow 'itemconfigure IDorTag :text new-text)))

(define (get-width IDorTag)          ;; get width for item 
  (if (or (not (number? IDorTag)) (positive? IDorTag))

      (gdraw:last (.drawwindow 'itemconfigure IDorTag :width))
      (map (lambda (I) (gdraw:last (.drawwindow 'configure I)))
	   (list :width :height))))
    
(define (set-width! IDorTag new-width . more) ;; set width for item, if legal.

  ;; for IDorTag=0, accepts either 2 numbers or a list of 2 numbers.
  (if (or (not (number? IDorTag)) (positive? IDorTag))
      (.drawwindow 'itemconfigure IDorTag :width new-width)
      (for-each (lambda (I V) (.drawwindow 'configure I V))

	   (list :width :height)
	   (if (null? more) new-width (cons new-width more)))))

(define (get-font-size IDorTag)       ;; get font size.
  (cadr (gdraw:get-font IDorTag)))
(define (set-font-size! IDorTag size) ;; set new font size

  (define result #F)
  (define font (gdraw:get-font IDorTag))
  (if (catch
       (set! result
	     (.drawwindow 'itemconfigure IDorTag :font
			  (string-append (car font)
					 "-" 
					 (& size)

					 "-"
					 (caddr font)))))
      (begin
       (.drawwindow 'itemconfigure IDorTag :font (cadddr font))
       (display (list 'set-font-size! IDorTag size " - FAILED, IGNORED.")) (newline)

       #F)
      result))

(define (set-image! IDorTag name)
   (if (or (not (number? IDorTag)) (positive? IDorTag))
      (let ((image (gdraw:last (.drawwindow 'itemconfigure IdorTag :image))))
         (image 'blank)

         (image 'configure :file
                (if (and (symbol-bound? '*gamesman-path*) *gamesman-path*) 
		    (string-append *gamesman-path* "images/" name)
		    name)))))

(define (gdraw:get-font IDorTag)

  (gdraw:extract-font-info
   (gdraw:last (.drawwindow 'itemconfigure IDorTag :font))))
(define (gdraw:extract-font-info font) ;; internal fcn to extract font info
  (define positions (cond (((string->regexp "-[0-9]*[0-9]-") font) => car)

			  (else #F)))
  (if positions
      (list (substring font 0 (car positions))
	    (string->number (substring font
				       (+ 1 (car positions))
				       (+ -1 (cadr positions))))
	    (substring font (cadr positions) (string-length font))

	    font)
      (begin (display "can't recognize fontname format\n")
	     (list #F #F #F font))))

(define (get-coords IDorTag)        ;; get X-Y coordinates of item
  (if (or (not (number? IDorTag)) (positive? IDorTag))

      (.drawwindow 'coords IDorTag)
      (list (winfo 'rootx .drawwindow)
	    (winfo 'rooty .drawwindow))))

(define (set-coords! IDorTag coords . more) ;; set X-Y coordinates of item
  ;; accepts either individual numbers or a single list of numbers.

  (if (not (list? coords)) (set! coords (cons coords more)))
  (if (or (not (number? IDorTag)) (positive? IDorTag))
      (apply .drawwindow 'coords IDorTag coords)
      (display "Use set-width! to change the shape of the canvas\n")))



(define (raise! IDorTag . above)                         ;; raise item on canvas
  (if (or (not (number? IDorTag)) (positive? IDorTag) )
     (if (null? above) 
        (.drawwindow 'raise IDorTag)

        (.drawwindow 'raise IDorTag (car above)))))
    
(define (lower! IDorTag . below)                         ;; lower item on canvas
  (if (or (not (number? IDorTag)) (positive? IDorTag))
     (if (null? below) 

        (.drawwindow 'lower IDorTag)
        (.drawwindow 'lower IDorTag (car below)))))


(define (set-default-outline-color! color)
      (set! *outline-color* color))

(define (set-default-fill-color! color)

      (set! *fill-color* color))


(define (get-type  IDorTag)            ;;get type
  (if (or (number? IDorTag) (not (number? IDorTag)))
      (.drawwindow 'type IDorTag)
      'canvas))


(define (get-mouse-coords) ;; get the mouse coordinates
  (map - (winfo 'pointerxy .drawwindow)
       (list (winfo 'rootx .drawwindow)
	     (winfo 'rooty .drawwindow))))

(define (gdraw:delete! IDorTag)                ;; delete an item on the canvas

  (cond ((eq? IDorTag 'all)         (clear-graphics!))
	((or (member IDorTag (get-IDs))
             (not (empty? (get-ids-with-tag IDorTag))))
              (.drawwindow 'delete IDorTag))
	((equal? IDorTag 0)         (clear-graphics!))

	(else                  #F)))
(define gdelete! gdraw:delete!)
(define delete! gdraw:delete!)


;;;
;; 
;; Bindings
;;
;;;

(define binding-map
  '((enter         "<Enter>")           ;; mouse motion

    (leave         "<Leave>")
    (motion        "<Motion>")
    (doubleclick   "<Double-Button-1>") ;; doubleclick
    (leftclick     "<Button-1>")

    (rightclick    "<Button-3>")
    ))
    
(define (get-event-type type)
  (let ((result (assoc type binding-map)))
    (if result
	(cadr result)
	(begin (format "Type not found: ~A" type)

	       (format "Set to mouse click")
	       "<Motion>"))))


(define (get-binding IDorTag . type) ;; get procedure binding for item
  (set! type (if (null? type) "<Button>" (get-event-type (car type))))

  (define binding-present
	  (member type
		 (if (or (not (number? IDorTag)) (positive? IDorTag))
		     (.drawwindow 'bind IDorTag)
		     (bind .drawwindow))))
  (if binding-present
      (if (or (not (number? IDorTag)) (positive? IDorTag))

	  (.drawwindow 'bind IDorTag type)
	  (bind .drawwindow type))
      #F))


(define (set-binding! IDorTag thunk . type) ;; set procedure binding for item
  (set! type (if (null? type) "<Button>" (get-event-type (car type))))

  (define binding
	  (if (or (null? thunk) (not thunk))
	      ""
	      thunk))
  (if (or (not (number? IDorTag)) (positive? IDorTag))
      (.drawwindow 'bind IDorTag type binding)
      (bind .drawwindow type binding)))


(define (get-motion-binding IDorTag)             ;; get procedure binding for item
  (get-binding IDorTag 'motion))

(define (set-motion-binding! IDorTag thunk)      ;; set motion binding for item
  (set-binding! IDorTag thunk 'motion))



;;;
;;
;; Tagging
;;
;;;

(define (remove-tag IDorTag tag-to-delete)
   (.drawwindow 'dtag IDorTag tag-to-delete))
      
(define (addtag IDorTag tag-to-add)
   (if (number? IDorTag)

      (.drawwindow 'itemconfigure IDorTag :tags (cons tag-to-add (get-tags IDorTag)))
      (.drawwindow 'addtag tag-to-add 'withtag IDorTag)))

(define (get-tags ID)
   (gdraw:last (.drawwindow 'itemconfigure ID :tags)))


(define (get-ids-with-tag tag)  ;;get a list of all id's with tag
      (.drawwindow 'find 'withtag tag))

(define (get-IDs)                  ;; get list of all existing IDs
  (.drawwindow 'find 'all))


(define (catch-winfo . args)
  (define result #F)
  (if (catch (set! result (apply winfo args)))
      #F
      result))

(define (get-properties IDorTag)        ;; get all properties I can think of...

  (cond ((or (not (number? IDorTag)) (positive? IDorTag))
	 (list (cons 'bind
		     (map (lambda (binding)
				  (cons binding (.drawwindow 'bind IDorTag binding)))
			  (.drawwindow 'bind IDorTag)))

	       (cons 'bbox (.drawwindow 'bbox IDorTag))
	       (cons 'coords (.drawwindow 'coords IDorTag))
	       (cons 'itemconfigure (.drawwindow 'itemconfigure IDorTag))
	       (list 'type  (.drawwindow 'type IDorTag))))

	((and (number? IDorTag) (zero? IDorTag))
	 (list (cons 'bind
		     (map (lambda (binding)
				  (cons binding (bind .drawwindow binding)))
			  (bind .drawwindow)))
	       (cons 'configure (.drawwindow 'configure))

	       (cons 'read-only-winfo
		     (map (lambda (prop)
				  (list prop (catch-winfo prop .drawwindow)))
			  '(atom  atomname  cells  children  class colormapfull
				  containing  depth  exists  fpixels  geometry

				  height IDorTag interps  ismapped  manager  name
				  parent  pathname pixels  pointerx pointerxy
				  pointery  reqheight  reqwIDorTagth rgb  rootx
				  rooty  screen screencells  screendepth
				  screenheight  screenmmheight  screenmmwIDorTagth

				  screenvisual screenwIDorTagth  server  toplevel
				  viewable  visual  visualIDorTag visualsavailable
				  vrootheight  vrootwIDorTagth  vrootx  vrooty wIDorTagth
				  x y)
			  ))))
	(else 
	 (list (cons 'configure (*root* 'configure))

	       (cons 'read-only-winfo
		     (map (lambda (prop) (list prop (catch-winfo prop *root*)))
			  '(atom  atomname  cells  children
				  class colormapfull  containing  depth
				  exists fpixels  geometry  height

				  IDorTag interps  ismapped  manager  name
				  parent  pathname  pixels  pointerx
				  pointerxy  pointery  reqheight
				  reqwIDorTagth  rgb  rootx  rooty  screen
				  screencells  screendepth  screenheight

				  screenmmheight  screenmmwIDorTagth
				  screenvisual  screenwIDorTagth  server
				  toplevel  viewable  visual  visualIDorTag
				  visualsavailable  vrootheight
				  vrootwIDorTagth  vrootx  vrooty  wIDorTagth  x y)

			  ))))))

(define (set-property! IDorTag property . values)
  (cond ((equal? "" (& property)) #F)
	((or (not (number? IDorTag)) (positive? IDorTag))
	 (cond ((char=? #\< (string-ref (& property) 0))

		(apply .drawwindow 'bind IDorTag property values))
	       ((char=? #\: (string-ref (& property) 0))
		(apply .drawwindow 'itemconfigure IDorTag property values))
	       ((equal? (& 'coords) (& property))

		(apply .drawwindow 'coords IDorTag values))
	       (else
		(apply .drawwindow property IDorTag values))))
	((and (number? IDorTag) (zero? IDorTag))
	 (cond ((char=? #\< (string-ref (& property) 0))

		(apply bind .drawwindow property values))
	       ((char=? #\: (string-ref (& property) 0))
		(apply .drawwindow 'configure property values))
	       (else
		(apply .drawwindow property values))))

	(else ; assume root-window
	 (cond ((char=? #\< (string-ref (& property) 0))
		(apply bind *root* property values))
	       ((char=? #\: (string-ref (& property) 0))
		(apply *root* 'configure property values))

	       (else
		(apply *root* property values))))))

;;;
;;
;; Stuff to try
;;
;;;


(define (position-modif coords xInc yInc)
	(let ((x1 (car coords))
	      (y1 (cadr coords))
	      (x2 (caddr coords))

	      (y2 (cadddr coords)))
	 (list (+ x1 xInc) (+ y1 yInc) (+ x2 xInc) (+ y2 yInc))))

(define (fly-to x y ID speed position-modif)
	(let* ((c (get-coords  ID))
	 	(x1 (car c))
	 	(y1 (cadr c))
	 	(xInc (ceiling (/ (abs (- x x1)) 30) ))

	 	(yInc (ceiling (/ (abs (- y y1)) 30) )))
	 (if (<= x x1)
	 	(set! xInc (- xInc)))
	 (if (<= y y1)
	 	(set! yInc (- yInc)))
	 (fly-to-help x y c xInc yInc speed position-modif ID)
	 ))
	 
	 

(define (fly-to-help xTo yTo coords xInc yInc speed position-modif ID)
	(if (and ( or (<= (abs (- (car coords) xTo)) (abs xInc)) (= xInc 0))
		 ( or (<= (abs (- (cadr coords)  yTo)) (abs yInc)) (= yInc 0)))

	    'done
	    (begin
	    	(if (<= (abs (- (car coords) xTo)) (abs xInc))
	    		(set! xInc 0))
	    	(if (<= (abs (- (cadr coords)  yTo)) (abs yInc))
	    		(set! yInc 0))
;	    	(after speed (lambda ()

;			       (set-coords! ID (position-modif coords xInc yInc))
;			       (fly-to-help xTo yTo (position-modif coords xInc yInc) xInc yInc speed position-modif ID))))))
	    	(begin
		  (update 'idletasks)

		  (set-coords! ID (position-modif coords xInc yInc))
		  (fly-to-help xTo yTo (position-modif coords xInc yInc) xInc yInc speed position-modif ID)))))

;;;;;;;;;;;;;;;;
;;;ANIMATIONS;;;
;;;;;;;;;;;;;;;;

;;;Note: all of these procedures work really well when you put them
;;;into GUI-HANDLE-MOVE in Gamesman

(define (fly-to x y ID speed position-modif)
	(let* ((c (get-coords  ID))
	 	(x1 (car c))
	 	(y1 (cadr c))

	 	(xInc (ceiling (/ (abs (- x x1)) 30) ))
	 	(yInc (ceiling (/ (abs (- y y1)) 30) )))
	 (if (<= x x1)
	 	(set! xInc (- xInc)))
	 (if (<= y y1)
	 	(set! yInc (- yInc)))
	 (fly-to-help x y c xInc yInc speed position-modif ID)

	 ))
	 
	 
(define (fly-to-help xTo yTo coords xInc yInc speed position-modif ID)
	(if (and ( or (<= (abs (- (car coords) xTo)) (abs xInc)) (= xInc 0))
		 ( or (<= (abs (- (cadr coords)  yTo)) (abs yInc)) (= yInc 0)))

	    'done
	    (begin
	    	(if (<= (abs (- (car coords) xTo)) (abs xInc))
	    		(set! xInc 0))
	    	(if (<= (abs (- (cadr coords)  yTo)) (abs yInc))
	    		(set! yInc 0))
;	    	(after speed (lambda ()

;			       (set-coords! ID (position-modif coords xInc yInc))
;			       (fly-to-help xTo yTo (position-modif coords xInc yInc) xInc yInc speed position-modif ID))))))
	    	(begin
		  (update 'idletasks)

		  (set-coords! ID (position-modif coords xInc yInc))
		  (fly-to-help xTo yTo (position-modif coords xInc yInc) xInc yInc speed position-modif ID)))))

     ;;;;;;;;;;;;;;;
;;;;;Crippled-X Code;;;;;
     ;;;;;;;;;;;;;;;


;;crippled-x is a generic procedure that
;;animates a given list of images if called
;;from a given game file
;;it takes a list of images, a tag, speed (such as 3 or 5),
;;the original x and y components of the position, and a list

;;of the old coordinates as arguments

(define crippled-x
  (lambda (picture-list tag speed new-origX new-origY old-coords)
    (let* ((a 1)
	   (x-pos (car old-coords))
	   (y-pos (cadr old-coords))

	   (total-time 1002)
	   (deltax (- new-origX x-pos))
	   (deltay (- new-origY y-pos))
	   (dx (/ (* deltax speed) total-time))
	   (dy (/ (* deltay speed) total-time))
	   (func 
	    (lambda (f)
	      (after speed (lambda ()

			     (set! a (remainder a (length picture-list)))
			     (set-image! tag (list-ref picture-list a))
			     (set! a (+ a 1))
			     (set! x-pos (+ dx x-pos))
			     (set! y-pos (+ dy y-pos))
			     (set-coords! tag (list x-pos y-pos))

			     (if (not (or
				       ((if (< dx 0) <= >=) x-pos new-origX)
				       ((if (< dy 0) <= >=) y-pos new-origY)))
				 (f f)
				 (set-image! tag (car picture-list))))))))
    (func func))))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;Appear-Transform-Disappear;;;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (round-up num)
  (if (<= (round num) num)
      (+ 1 (round num))
      (round num)))


(define (draw-rects coords dxy color rect-tag) ;; Draws an outline with a width equivalent to the (width of a rectangle drawn in given coords) divided by dxy
  (let* ((x1 (car coords))
	 (y1 (cadr coords))
	 (x2 (caddr coords))

	 (y2 (cadddr coords))
	 (delta-x (/ (- x2 x1) dxy))
	 (delta-y (/ (- y2 y1) dxy))
	 (draw-rec (lambda (a b c d)
		     (draw-rectangle a b c d 'fill color 'outline color 'tag rect-tag))))
    (draw-rec x1 y1 (+ x1 delta-x) y2)

    (draw-rec x1 y1 x2 (+ y1 delta-y))
    (draw-rec x1 y2 x2 (- y2 delta-y))
    (draw-rec (- x2 delta-x) y1 x2 y2)))

(define (convert-image-coord cent-coord width height) ;; Converts an image of a center coordinate, a width, and a height into the general gdraw four point coordinate

  (let* ((c1 (car cent-coord))
	 (c2 (cadr cent-coord))
	 (half-width (/ width 2))
	 (half-height (/ height 2)))
    (list (- c1 half-width) (- c2 half-height) (+ c1 half-width) (+ c2 half-height))))

;Given a tag of an object that requires 4 coords, a number change in x and y delx and dely, respectively, sets the new coords preserving dimensions.

(define (set-4coord! tag ncoords)
  (let* ((coords (get-coords tag))
	 (x1 (car coords))
	 (y1 (cadr coords))
	 (x2 (caddr coords))
	 (y2 (cadddr coords))
	 (width (abs (- x2 x1)))
	 (height (abs (- y2 y1))))

    (set-coords! tag (convert-image-coord ncoords width height))))

(define main-appear ;; Tag disappears at current location and appears in a new coordinate
  (lambda (tag ncoords . options)
    (let* ((coords (get-coords tag))

	   (four? (equal? (length coords) 4))
	   (dpear-spd 75)
	   (rpear-spd 30)
	   (picture-list '())
	   (start-color (get-bg-color 0))
	   (end-color (get-bg-color 0))
	   (pic-width 150)
	   (pic-height 150)

	   (bw-time 5000))
      
      (define (set-options! options) ;; Sets the options in a format '<name of option> <option arg>; WARNING: dpear-spd, rpear-spd, and bw-time will be buggy when used with spin (must use default speeds and bw-time)

	;;Note: you must generate the picture list to have the rotating/spinning effect on your own, if that
	;;is your desired effect. We cannot do that for you! Nevertheless, our method does not necessarily constrain its uses

	;;to spinning/rotating. That is, you may find that it works equally well with other effects, such as flashing
	;;a list of images with different fill or background colors. In other words, be creative and use your imagination.

	(if (not (null? options))
	    (let ((var (car options))
		  (value (cadr options)))
	      (cond ((equal? var 'dpear-spd) (set! dpear-spd value)) ;; Sets the disappear speed of the tagged object
		    ((equal? var 'rpear-spd) (set! rpear-spd value)) ;; Sets the reappear speed of the tagged object

		    ((equal? var 'bw-time) (set! bw-time value)) ;; Sets the time interval between the disappear and the reappear
		    ((equal? var 'spin) (set! picture-list value)) ;; Allows the tagged object to "spin"

		    ((equal? var 'pic-width) (set! pic-width value)) ;; Adjusts the picture width of a non-drawn image
		    ((equal? var 'pic-height) (set! pic-height value)) ;; Adjusts the picture height of a non-drawn image

		    ((equal? var 'start-color) (set! start-color value)) ;; Sets the starting color of the disappear
		    ((equal? var 'end-color) (set! end-color value)) ;; Sets the ending color of the reappear
		    (else (error "unknown option: " var)))

	(set-options! (cddr options)))))

      (set-options! options)

      (let* ((spin? (not (null? picture-list))))
	(disappear tag dpear-spd start-color spin? picture-list four? pic-width pic-height ncoords)

	(after bw-time (lambda ()
			 (reappear tag rpear-spd end-color spin? picture-list four? pic-width pic-height ncoords)))))))


; Disappear/Reappear

(define disappear ;; Disappears a tag at its current location

  (lambda (tag speed start-color spin? picture-list four? pic-width pic-height ncoords)
    (let* ((coords (get-coords tag))
	   (dxy 10)
	   (iter-change 0.95)
	   (limit 1)
	   (func
	    (lambda (f)

	      (after speed (lambda ()
			     (set! dxy (* iter-change dxy))
			     (draw-rects (if four? coords (convert-image-coord coords pic-width pic-height)) dxy start-color 'start)
			     (if (>= dxy limit)

				 (f f)
				 (begin
				   (transformer ncoords coords 'black))))))))
      (if spin?
	  (flicker-disappear picture-list tag))
      (func func))))


(define flicker-disappear ;; Flicker tailored to the disappear procedure (increasing speed of spin)

  (lambda (picture-list tag)
    (let* ((speed 200)
	   (limit 11)
	   (a 1)
	   (spin-cyc 1.1)
	   (func 
	    (lambda (f speed)
	      (after speed (lambda ()
			     (let ((speed-cyc (round (/ speed spin-cyc))))

			       (set! a (remainder a (length picture-list)))
			       (set-image! tag (list-ref picture-list a))
			       (set! a (+ a 1))
			       (if (>= speed limit)
				   (f f speed-cyc))))))))
	   (func func speed))))


(define reappear ;; Reappears a tag at a new location
  (lambda (tag speed end-color spin? picture-list four? pic-width pic-height coords)
    (let* ((coord (get-coords tag))
	   (dxy 1)
	   (iter-change 1.052631579)

	   (limit 200)
	   (spin-delay 500)
	   (gdraw-width
	    (if four?
		(+ (- (caddr coord)
		      (car coord))
		   (get-width tag))))
	   (gdraw-height
	    (if four?
		(+ (- (cadddr coord)

		      (cadr coord))
		   (get-width tag))))
	   (bg-box
	    (draw-rects
	     (if four?
		 (convert-image-coord coords gdraw-width gdraw-height) 
		 (convert-image-coord coords pic-width pic-height))

	     1 end-color 'end))
	   (func
	    (lambda (f)
	      (after speed (lambda ()
			     (delete! 'end)
			     (set! dxy (* iter-change dxy))
			     (draw-rects (if four?
					     (convert-image-coord coords gdraw-width gdraw-height)

					     (convert-image-coord coords pic-width pic-height)) dxy end-color 'end)
			     (if (<= dxy limit)
				 (f f)
				 (begin
				   (delete! 'start)
				   (delete! 'end))))))))
      (delete! 'start)

      bg-box
      (if four?
        (set-4coord! tag coords)
	(set-coords! tag coords))
      (if spin?
	  (after spin-delay (lambda ()
			      (flicker-reappear picture-list tag))))
      (func func))))


(define flicker-reappear ;; Flicker tailored to the reappear procedure (slows down as time progresses)
  (lambda (picture-list tag)
    (let* ((speed 5)
	   (limit 175)
	   (a 1)
	   (spin-cyc 1.04)

	   (func 
	    (lambda (f speed)
	      (after speed (lambda ()
			     (let ((speed-cyc (round-up (* speed spin-cyc))))
			       (set! a (remainder a (length picture-list)))
			       (set-image! tag (list-ref picture-list a))

			       (set! a (+ a 1))
			       (if (<= speed limit)
				   (f f speed-cyc))))))))
      (func func speed))))

(define tid 0)
(define (transformer new-coords old-coords color)
  (let* ((new-x (car new-coords))

	 (new-y (cadr new-coords))
	 (old-x (car old-coords))
	 (old-y (cadr old-coords))
	 (tag (word 'transformer tid)))
    (set! tid (1+ tid))
    (draw-oval (- old-x 5) (- old-y 5)
	       (+ 5 old-x) (+ old-y 5) 'fill color 'tag tag)

    (crawl-to tag new-x new-y 'delete-on-finish #t)))


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;Parabolas, Bouncing, Twirling, Shadows;;;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;crawl-to

;Given an image TAG/ID, an x position, and a y position, "crawls" to the new position in a default of 1 second. You can pass options 'total-time [time in msec] 'time-step [time in msec] if desired. Time-step determines how often the picture is re-drawn, so smaller => smoother, bigger = less lag. You can also specify to delete on completion with 'delete-on-finish #t

(define crawl-to
  (lambda (tag nx ny . new-options)
    (let ((options (make-options-list 'total-time 1000
				      'time-step 10
				      'delete-on-finish #f)))
      (change-options-list! options new-options)

      (let* ((time-step (cadr (assoc 'time-step options)))
	     (total-time (cadr (assoc 'total-time options)))
	     (delete-on-finish (cadr (assoc 'delete-on-finish options)))
	     (init-x (get-gcoord tag 'x))

	     (init-y (get-gcoord tag 'y))
	     (deltax (- nx init-x))
	     (deltay (- ny init-y))
	     (dx (/ (* time-step deltax) total-time))
	     (dy (/ (* time-step deltay) total-time))
	     (func 

	      (lambda (f)
		(after time-step (lambda ()
				   (let* ((x (get-gcoord tag 'x))
					  (y (get-gcoord tag 'y))
					  (newx (+ x dx))
					  (newy (+ y dy)))
				     (set-gcoords! tag newx newy)

				     (if (or ((if (negative? dx) < >)
					      newx nx)
					     ((if (negative? dy) < >)
					      newy ny))
					 (begin (set-gcoords! tag nx ny)
						(if delete-on-finish
						    (delete! tag))

						3)
					 (f f))))))))
	(func func)))))


;Demo for crawl-to
#|
(draw-demo-circle 100 100 'c 120)
(crawl-to 'c 400 200)
(crawl-to 'c 100 500 'total-time 500 'time-step 1)


(clear-graphics!)
|#

;Bouncy

;Bounces around the canvas, never going out of the borders. Takes a tag, and optional positive time interval number time-step in ms, a positive integer speed as factor of default speed 1, and width from center, and height from center (in pixels). Left-boundary, right-boundary, top-boundary, and bottom-boundary can be changed from their default values of the canvas bounds. Options entered as 'option value (e.g. 'width 75) after main argument.

(define bouncy
  (lambda (tag . new-options)
    (let ((options (make-options-list 'time-step 1
				      'speed 1
				      'width (if (four-coord? tag)
						 (/ (get-4coord-width tag) 2)
						 70)

				      'height (if (four-coord? tag)
						  (/ (get-4coord-height tag) 2)
						  70)
				      'left-boundary 0
				      'top-boundary 0
				      'right-boundary (car (get-width 0))

				      'bottom-boundary (cadr (get-width 0)))))
      (change-options-list! options new-options)
      (let* ((width (cadr (assoc 'width options)))
	     (height (cadr (assoc 'height options)))

	     (time-step (cadr (assoc 'time-step options)))
	     (speed (cadr (assoc 'speed options)))
	     (left-boundary (cadr (assoc 'left-boundary options)))
	     (right-boundary (cadr (assoc 'right-boundary options)))

	     (top-boundary (cadr (assoc 'top-boundary options)))
	     (bottom-boundary (cadr (assoc 'bottom-boundary options)))
	     (kill #f)
	     (inc+ (repeated 1+ speed))
	     (inc- (repeated 1- speed))

	     (lb (+ left-boundary width))
	     (tb (+ top-boundary height))
	     (rb (- right-boundary width))
	     (bb (- bottom-boundary height))
	     (func 
	      (lambda (f vx vy)
		(after time-step

		       (lambda ()
			 (let* ((x (get-gcoord tag 'x))
				(y (get-gcoord tag 'y))
				(dx
				 (cond ((<= x lb) inc+)
				       ((>= x rb) inc-)
				       (else vx)))
				(dy
				 (cond ((<= y tb) inc+)

				       ((>= y bb) inc-)
				       (else vy))))
			   (set-gcoords! tag (dx x) (dy y))
			   (if (not kill)
			       (f f dx dy))))))))
	(func func inc+ inc+)
	(lambda () (set! kill #t))))))


;Demo for bouncy.
#|
(init-graphics)
(set-canvas-size 800 600)

(draw-image 120 100 "*Your image here*" 'tag 'xi)
(draw-demo-circle 400 400 'c 120)
(define stopb (bouncy 'xi 'width 70 'height 60))

(define stopb2 (bouncy 'c 'left-boundary 100 'top-boundary 100 'right-boundary 700 'bottom-boundary 500 'speed 3))
(stopb)
(stopb2)

(clear-graphics!)
|#


;; Parabolic motion



; Given a tag, a new x coordinate, a new y coordinate, and optional options, animates a parabola-like move. tagged objects can be either an image (with length 2 coords) or a drawn object (with length 4 coords). Options are added as extra arguments as follows: 'option value. Options are, with (defaults):

;time-step [positive number in milliseconds] (time interval between drawsm smaller=>smoother, bigger=>less computation), (20)
;max-height [positive number in pixels] (max height of path, affects appearance of parabolic path), (125)

;total-time [positive number in milliseconds] (how long animation takes in ms), (1000)
;shadow-width [positive number in pixels] (shadow width), (120, or deduced from 4-coordinate object)
;shadow-y-offset [positive number in pixels] (y-offset of shadow from center of object). (deduced from shadow-width)

;spin [list of string image paths] (spin: cycles through images, creating an appearance of spin), (empty list)
;spin-speed [time in ms] (how often it changes the picture), (55)

(define parabola
  (lambda (tag nx ny . new-options)

    (let* ((four? (equal? (length (get-coords tag)) 4)) ;To distinguish between objects of 2/4 coords.
	   (options (make-options-list 'time-step 20
				       'max-height 125
				       'total-time 1000

				       'shadow-y-offset ""
				       'spin nil
				       'spin-speed 55
				       'shadow-width (if four?
							 (get-4coord-width tag 'ignore-outline)
							 120))))

      (change-options-list! options new-options)
      (let* ((time-step (cadr (assoc 'time-step options)))
	     (max-height (cadr (assoc 'max-height options)))
	     (total-time (cadr (assoc 'total-time options)))

	     (shadow-y-offset (cadr (assoc 'shadow-y-offset options)))
	     (pic-list (cadr (assoc 'spin options)))
	     (spin-speed (cadr (assoc 'spin-speed options)))
	     (shadow-width (cadr (assoc 'shadow-width options)))

	     (x0 (get-gcoord tag 'x))
	     (y0 (get-gcoord tag 'y))
	     (deltax (- nx x0))
	     (deltay (- ny y0))
	     (t 0)
	     (v0z (* 4 max-height (/ total-time)))
	     (v0y (/ deltay total-time))

	     (v0x (/ deltax total-time))
	     (a (- (* .5 v0z v0z (/ max-height))))
	     (y y0)
	     (x x0)
	     (z 0)
	     (s0y (+ y (if (empty? shadow-y-offset) ;sets initial y position of shadow to offset of half the width, or the specified y-offset.

			   (/ shadow-width 2)
			   shadow-y-offset)))
	     (original-shadow-width shadow-width)
	     (original-shadow-height (/ shadow-width 3))
	     (spin? (not (null? pic-list)))
	     (shadow-tag (draw-shadow x s0y shadow-width))

	     (func 
	      (lambda (f)
		(let* ((newx (+ (* v0x t) x0)) ;constant velocity motion
		       (newy (+ (* v0y t) y0)) 
		       (newz (+ (* v0z t) (* .5 a t t))) ;constant acceleration motion
		       (old-transformed-y (xyz-45xrot y z))      ;y coords are transformed due to z

		       (new-transformed-y (xyz-45xrot newy newz))
		       (delx (- newx x))
		       (dely (- newy y)))
		  (after time-step
			 (lambda ()
			   (set! x newx)
			   (set! y newy)
			   (set! z newz)	       

			   (set-gcoords! tag x new-transformed-y)
			   (scale-by-z! shadow-tag z max-height original-shadow-width original-shadow-height)
			   (inc-4coords! shadow-tag delx dely)
			   (if spin? (spin! tag pic-list (div t spin-speed)))

			   (set! t (+ t TIME-STEP))
			   (if (<= t total-time)
			       (f f)
			       (begin
				 (delete! shadow-tag)
				 (if spin? (set-image! tag (car pic-list)))))))))))
	(lower! shadow-tag tag)

	(func func)))))


;Helper and other procedures

;Given a tag, picture-list, and an index (possibly exceeding the range), sets the image to the index of the pic-list.
(define (spin! tag pic-list a)
  (let ((index (remainder a (length pic-list))))

    (set-image! tag (list-ref pic-list index))))

;Given two numbers, returns the mean of the two numbers.
(define (mean n1 n2)
  (/ (+ n1 n2) 2))

;Given an arbitrary amount of options (options are two values, a key and a value), returns an options list. This is the same format as an association list.

(define (make-options-list . options)
  (if (eq? (length options) 1)
		 (set! options (car options)))
  (if (null? options)
      nil
      (cons (list (car options) (cadr options))
	    (make-options-list (cddr options)))))


;Given an options-list (in the form of an association list) and a sequence of new options, sets the old options to the new options.
(define (change-options-list! old-options new-options-seq)
  (if (null? new-options-seq)

      'okay
      (let ((old-kv (assoc (car new-options-seq) old-options)))
	(set-cdr! old-kv (list (cadr new-options-seq)))
	(change-options-list! old-options (cddr new-options-seq)))))
  

;Given a tag and a word x or y, returns the x or y coord of the center of the object, respectively.

(define (get-gcoord tag x/y)
  (let ((coords (get-coords tag)))
    (cond ((equal? (length coords) 4)
	   (get-4coord tag x/y))
	  ((equal? x/y 'x) (car coords))
	  (else (cadr coords)))))

;Given a tag of a 4-coord object and a word x or y, returns the x or y coord.

(define (get-4coord tag x/y)
  (let ((coords (get-coords tag)))
    (if (equal? x/y 'x)
	(mean (car coords) (caddr coords))
	(mean (cadr coords) (cadddr coords)))))

;Given a tag, returns the apparent width of a drawn object (not image).

(define (get-4coord-width tag . ignore-outline?)
  (let ((coords (get-coords tag)))
    (+ (abs (- (car coords) (caddr coords)))
       (if (null? ignore-outline?)
	   (get-width tag)
	   0))))

;Given a tag, returns the apparent height of a drawn object (not image).

(define (get-4coord-height tag . ignore-outline?)
  (let ((coords (get-coords tag)))
    (+ (abs (- (cadr coords) (cadddr coords)))
       (if (null? ignore-outline?)
	   (get-width tag)
	   0))))

;Given a tag of an object that requires 4 coords, a number change in x and y delx and dely, respectively, increments the coords, preserving dimensions.

(define (inc-4coords! tag delx dely)
  (let* ((coords (get-coords tag))
	 (x1 (car coords))
	 (y1 (cadr coords))
	 (x2 (caddr coords))
	 (y2 (cadddr coords)))
    (set-coords! tag (+ x1 delx) (+ y1 dely) (+ x2 delx) (+ y2 dely))))


;A general set-coords! procedure. Given a tag, a new x-center, and a new y-center, sets the object to the new coordinates.
(define (set-gcoords! tag nx ny)
  (if (equal? (length (get-coords tag)) 4)
      (set-4coords! tag nx ny)

      (set-coords! tag (list nx ny))))

;Given a tag, a new x-center, and a new y-center, sets a 4-coord object to the new center.
(define (set-4coords! tag nx ny)
  (let ((coords (get-coords tag)))
    (let ((x1 (car coords))

	  (y1 (cadr coords))
	  (x2 (caddr coords))
	  (y2 (cadddr coords))
	  (deltax (- nx (get-4coord tag 'x)))
	  (deltay (- ny (get-4coord tag 'y))))
      (set-coords! tag (+ x1 deltax) (+ y1 deltay) (+ x2 deltax) (+ y2 deltay)))))


;Given a y and a z coordinate of a point in (x, y, z) left-handed space, returns an approximate y-value for the projection of the point onto a plane rotated 45 degrees around the x axis. Makes points in 3-space more 3D on a 2D plane.

(define (xyz-45xrot y z)
  (let ((cosval (/ (sqrt 2) 2)))
    (- y (* cosval z))))

;Given an x and y coord and a shadow-width, draws a shadow with the specified width.
(define (draw-shadow x y shadow-width)

  (let ((xw (/ shadow-width 2))
	(yw (/ shadow-width 6)))
    (draw-oval (- x xw) (- y yw) (+ x xw) (+ y yw) 'fill 'gray 'outline 'gray 'width 20)))

;Given two numbers, returns the mean of the two numbers.

(define (mean n1 n2)
  (/ (+ n1 n2) 2))

;Given a tag, returns whether or not the tagged object has 4 coordinates.
(define (four-coord? tag)
  (equal? (length (get-coords tag)) 4))

;Given a tag of an object that requires 4 coords, a number factor by which to scale the size, and optional list numbers original width and height, scales the object by the factor.

(define (scale-size! tag factor . original-width/height)
  (let* ((coords (get-coords tag))
	 (x1 (car coords))
	 (y1 (cadr coords))
	 (x2 (caddr coords))
	 (y2 (cadddr coords))
	 ;if original-width/height defined, use it. otherwise use current width/height

	 (width (if (null? original-width/height) (abs (- x1 x2)) (car original-width/height))) 
	 (height (if (null? original-width/height) (abs (- y1 y2)) (cadr original-width/height)))
	 (x (mean x1 x2))
	 (y (mean y1 y2))

	 (xdisp (* .5 width factor))
	 (ydisp (* .5 height factor)))
    (set-coords! tag (list (- x xdisp) (- y ydisp) (+ x xdisp) (+ y ydisp)))))

;Given a tag, a number z, a number maxz, and optional original width and height values, scales the tagged object by an amount determined by a linear relation: scale by 1 if z=0, by .5 if z=maxz.

(define (scale-by-z! tag z maxz . original-width/height)
  (let ((factor (- 1 (* .5 (/ z maxz)))))
    (if (null? original-width/height)
	(scale-size! tag factor)
	(scale-size! tag factor (car original-width/height) (cadr original-width/height)))))



;Given a string folder name(e.g. "Red_X"), string image name base (filename up until numbers), a final number, and #t or #f depending on whether this will be run from gamesman, returns a list of image paths. Hard coded four-digit, with under 100 images.

;To generate a list of 60 Black_X images to use in testing outside of gamesman, for instance, call (make-image-list "Black_X" "Black_x" 60 #f).
(define (make-image-list folder base end gamesman?)

  (let ((path (string-append (if gamesman? "" "images/")
			     folder
			     "/"
			     base
			     "00")))
    (define (mil-helper L-so-far i)
      (if (< i 0)

	  L-so-far
	  (let ((final-path (string-append path
					   (if (< i 10) "0" "")
					   (number->string i)
					   ".gif")))
	    (mil-helper (cons final-path L-so-far) (1- i)))))

    (mil-helper nil end)))



;Demos

;Given an x coord, y coord, and a tag, draws a hollow blue circle with the given tag of given width, plus outline width 20.
(define (draw-demo-circle x y tag width)

  (let ((disp (/ width 2)))
    (draw-oval (- x disp)  (- y disp) (+ x disp) (+ y disp) 'fill "" 'outline "blue" 'width 20 'tag tag)))

#|
(init-graphics)
(set-canvas-size 800 600)



(define pic-listbx (make-image-list "Black_X" "Black_x" 60 #f))
(define pic-listbo (make-image-list "White_O" "White_O" 60 #f))


;Parabola demo

(draw-demo-circle 100 100 'c 120)

(parabola 'c 500 300)
(parabola 'c 400 400 'max-height 550 'total-time 800 'shadow-width 100 'shadow-y-offset 70 'time-step 10)
(parabola 'c 700 100 'max-height 125 'total-time 1500 'shadow-width 120 'shadow-y-offset 65 'time-step 20)


;spinning parabola demo (large pic-lists)
(draw-image 200 200 "images/Black_X/Black_x0000.gif" 'tag 'nbx)
(parabola 'nbx 400 200 'max-height 180 'shadow-width 130 'spin pic-listbx 'spin-speed 8)


(draw-image 200 400 "images/White_O/White_O0000.gif" 'tag 'nbo)
(parabola 'nbo 440 200 'max-height 180 'shadow-y-offset 80 'spin pic-listbo 'spin-speed 8)

(clear-graphics!)

|#
