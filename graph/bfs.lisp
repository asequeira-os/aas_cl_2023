(in-package :aas-graph)

#|
todo 2 we could have the graph provide a event notifier
this class can hook into the grapoh event notifier to dirty any computed paths
|#

(defclass breadth-first-search (algo)
  ((graph :type adjacency-list-graph :accessor graph)
   (vertex-previouses :initform (make-hash-table) :accessor vertex-previouses)))

#|
breadth first seach algorithm from 'algorithms CLRS 2nd ed' pp. 532

BFS (G, s)
1 for each vertex u E V[G]- {s} ;i am ignoring the '- {s}' part
2   do color[u] <- white
3          d[u] <- inf
4          p[u] <- nil
|#
(defun bfs% (graph source-vertex)
  (let ((colors (make-hash-table))
        (distances (make-hash-table))
        (previouses (make-hash-table)))
    (map-vertices graph
                  (lambda (vertex)
                    (setf (gethash vertex colors) white)
                    (setf (gethash vertex distances) infinity)
                    (setf (gethash vertex previouses) nil)))
    #|
    5 colors[s] <- gray
    6 d[s] <- 0
    7 p[s] <- nil
    |#
    (setf (gethash source-vertex colors) gray)
    (setf (gethash source-vertex distances) 0)
    (setf (gethash source-vertex previouses) nil)
    #|
    8 Q <- empty
    9 Enqueue(Q,s)
    |#
    (let ((Q (make-queue)))
      (enqueue Q source-vertex)
      #|
      10 while Q <> empty
      11   u <- dequeue (Q)
      12   for each v E Adj[u]
      13     if color[v] = white
      14        color[v] <- gray
      15        d[v] <- d[u] + 1
      16        p[v] <- u
      17        enqueue(Q,v)
      18   color[u] <- black
      |#
      (do ((dummy 0)) ((empty-queue-p Q))
        (declare (ignorable dummy))
        (let ((u (dequeue Q)))
          (mapcar (lambda (v-w)
                    (let ((v (car v-w)))
                      (when (eq (gethash v colors) white)
                        (setf (gethash v colors) gray)
                        (setf (gethash v distances) (1+ (gethash u distances)))
                        (setf (gethash v previouses) u)
                        (enqueue Q v))))
                  (get-adj-list graph u))
          (setf (gethash u colors) black))))
    previouses))


#|
'algorithms CLRS 2nd ed' pp.538
PRINT-PATH(G,s,v)
1 if v = s
2   then print s
3   else if p[v] = nil
4           then print "no path from " s "to" v "exists"
5           else PRINT-PATH(G,s,p[v])
6                print v
|#

;;compute-path and compute-path-acc implement the
;;above pseudo code for PRINT-PATH
(defun compute-path (previouses source-vertex vertex)
  (compute-path-acc previouses source-vertex vertex nil))


(defun compute-path-acc (previouses source-vertex vertex acc)
  (if (eq source-vertex vertex)
      (cons vertex acc)
      (if (null (gethash vertex previouses))
          nil
          (progn
            (compute-path-acc previouses source-vertex
                              (gethash vertex previouses)
                              (cons vertex acc) )))))

(defmethod shortest-path ((algo breadth-first-search) source-vertex dest-vertex)
  (let ((v-p (vertex-previouses algo))
        (graph (graph algo)))
    (multiple-value-bind (previouses found) (gethash source-vertex v-p)
      (unless found
        (setf previouses (bfs% graph source-vertex))
        (setf (gethash source-vertex v-p) previouses))
      (when previouses
        (compute-path previouses source-vertex dest-vertex)))))

