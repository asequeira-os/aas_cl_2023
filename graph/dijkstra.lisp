(in-package :aas-graph)

;; todo
;; create dijkstra vertex encapsulating class
;; it will have v for vertex,  p (pointer to another/previous vertex) and d (distance as number) properites
;; this could be done after the fact so we can deal with any graph
;; create array to contain vertexes accodring to their cardinal number
;; create array to contain vertexes according to their weight. this array is obtained by copying and sorting the cardinal array first time
;; use this array as priority queue by resorting every time at the beginning of the loop in the dijkstra algo



;;'algorithms CLRS 2nd ed' pp. 595
;; DIJKSTRA(G,w,s)
;; INITIALIZE-SINGLE-SOURCE(G,s)
;; S <- {}
;; Q <- V[G]
;; while Q <>  {}
;;    do u <- EXTRACT-MIN(Q)
;;       S <- S U {u}
;;       for each vertex v E Adj[u]
;;           do RELAX (u,v,w)


(defun dijkstra-shortest-path (graph source-vertex)
  (let ((ssd (initialize-single-source graph source-vertex))
        (set (make-hash-table))
        (q (min-weight-queue (vertices graph))))
    (while (not (queue-empty-p q))
      (let ((u (heap-extract-max q)))
        (setf (gethash u set) u)
        (mapcar (lambda (v-w)
                  (relax ssd u (car v-w) (cdr v-w)))
                (get-adj-list graph u))))))

(defun min-weight-queue (vertices)
  (declare (ignorable vertices))
  (error "todo 5 min-weight-queue not implemented"))

