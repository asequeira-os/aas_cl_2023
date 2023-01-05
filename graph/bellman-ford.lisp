(in-package :aas-graph)

;;'algorithms CLRS 2nd ed' pp. 588
;; BELLMAN-FORD(G,w,s)
;; 1 INITIALIZE-SINGLE-SOURCE(G,s)
;; 2 for i <- 1 to |V[G]| - 1
;; 3     do for each edge (u,v) E E[G]
;; 4            do RELAX(u,v,w)
;; 5 for each edge (u,v) E E[G]
;; 6     do if d[v] > d[u] + w(u,v)
;; 7           then return FALSE
;; 8 return TRUE


(defun bellman-ford (graph source)
  (let ((ssd (initialize-single-source graph source)))
    (with-slots (distances) ssd
      (dotimes (i (1- (vertex-count graph)))
        (mapcar (lambda (e)
                  (relax ssd (vertex-from e) (vertex-to e) (edge-weight e)))
                (edges graph)))
      (mapcar (lambda (e)
                (if (> (aref distances (v-num (vertex-to e)))
                       (+ (aref distances (v-num (vertex-from e)))
                          (edge-weight e)))
                    (return-from bellman-ford nil)))
              (edges graph))
      ssd)))


