#lang racket/base
(require racket/match racket/list racket/port racket/string )
(require pict3d)

;;;
;;; OBJ Parser (Models are stored in obj format)
;;;

; Simplified obj-parser.

(define (read-obj [port (current-input-port)])
  (define lines         (for/list ([line (in-lines port)])
                          (with-input-from-string line
                            (λ ()
                              (if (string-prefix? line "#")
                                  (list 'comment line)
                                  (for/list ([x (in-port read)]) x))))))
  (define vertice-lines (filter (λ (l) (and (pair? l) (eq? (first l) 'v))) lines))
  (define face-lines    (filter (λ (l) (and (pair? l) (eq? (first l) 'f))) lines))
  (define nverts        (length vertice-lines))
  (define nfaces        (length face-lines))

  (define verts ; vector of points
    (for/vector ([v vertice-lines] [j (in-naturals)])
      (match v
        [(list 'v x y z) (pos x y z)])))
  (define (convert-triple t)
    ; input: 1/2/3
    ; we only use the vertex number (and ignore the texture vertex and
    ; vertex normal indices.)
    (first (map sub1 (map string->number (string-split (symbol->string t) "/")))))
    
  (define faces ; each face is a list containing indices into verts
    (for/list ([fl face-lines])
      (match fl
        [(list 'f (? number? a) ...) (map sub1 a)]
        [(list 'f (? symbol? a) ...) (map convert-triple a)])))        
  (values verts faces))
                          

;;;
;;; Example
;;;

(define-values (teapot teapot-faces) (read-obj (open-input-file "assets/teapot.obj")))
;(define-values (teapot teapot-faces) (read-obj (open-input-file "assets/teapot-low.obj")))

(define n   (vector-length teapot))
(define ref vector-ref)

;; Let's find the dimension of the model
(define xmax (for/fold ([m -inf.0]) ([j n]) (max m (pos-x (ref teapot j)))))
(define ymax (for/fold ([m -inf.0]) ([j n]) (max m (pos-y (ref teapot j)))))
(define zmax (for/fold ([m -inf.0]) ([j n]) (max m (pos-z (ref teapot j)))))
(define xmin (for/fold ([m +inf.0]) ([j n]) (min m (pos-x (ref teapot j)))))
(define ymin (for/fold ([m +inf.0]) ([j n]) (min m (pos-y (ref teapot j)))))
(define zmin (for/fold ([m +inf.0]) ([j n]) (min m (pos-z (ref teapot j)))))
;; Print the ranges
(list xmin xmax)
(list ymin ymax)
(list zmin zmax)
;; Calculate dimensions
(define width  (* 2 (max (abs xmin) (abs xmax))))
(define height (* 2 (max (abs ymin) (abs ymax))))
(define depth  (* 2 (max (abs zmin) (abs zmax))))

;;;
;;; Render the tepost using pict3d
;;;

(define p
  (combine
   (for/list ([f (in-list teapot-faces)])
     ; the point of the face
     (define pts (for/list ([i f]) (ref teapot i)))
     ; turn it into a triangle or a quadrilateral
     (case (length pts)
       [(3) (apply triangle pts #:back? #t)]
       [(4) (apply quad     pts #:back? #f)]
       [else (error)]))
   (light (pos 0 1.5 1.5) (emitted "white" 2))))


(current-pict3d-fov 80)
(current-pict3d-add-grid? #t)
(current-pict3d-add-wireframe 'color)
p
;(pict3d->bitmap p 600 600)
