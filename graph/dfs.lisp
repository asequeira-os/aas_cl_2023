(in-package :aas-graph)

#|
'algorithms CLRS 2nd ed' pp. 541

DFS(G)
1 for each vertex u E V[G]
2     do color[u] <- white
3            p[u] <- nil
4 time <- 0
5 for each vertex u E V[G]
6     do if color[u] = white
7           then DFS-Visit(u)

DFS-Visit(u)
1 color[u] <- gray ; white vertex has just been discovered
2 time <- time + 1
3 d[u] <- time
4 for each v E Adj[u] ; explore edge(u,v)
5     do if color[v] = white
6           then p[v] <- u
7                DFS-Visit(v)
8 color[u] <- black ;u is finished
9 f[u] <- time <- time + 1

topological sort pp. 550
Topological-Sort(G)
1 call DFS(G) to compute finishing times f[v] for each vertext of v of G
2 as each vertex is finished, insert it into the front of a linked list
3. return the linked list of vertices

Implementation of Topological-Sort(G) is achived by the
last line of dfs-visit method below
|#

(defclass depth-first-search (algo)
  ((graph :type adjacency-list-graph :accessor graph)
   (previouses :initform (make-hash-table) :accessor previouses)
   (colors :initform (make-hash-table) :accessor colors)
   (distances :initform (make-hash-table) :accessor distances)
   (finishings :initform (make-hash-table) :accessor finishings)
   (topology :initform nil :accessor topology)
   (time)))

(defmethod depth-first-search ((algo depth-first-search))
  (with-slots (graph previouses colors distances time ) algo
    (map-vertices graph
                  (lambda (u)
                    (setf (gethash u colors) white)
                    (setf (gethash u previouses) nil)))
    (setf time 0)
    (map-vertices graph
                  (lambda (u)
                    (when (eq (gethash u colors) white)
                      (dfs-visit algo u))))))

(defmethod dfs-visit ((algo depth-first-search) u)
  (with-slots (graph previouses colors distances
                     time finishings topology) algo
    (setf (gethash u colors) gray)
    (incf time)
    (setf (gethash u distances) time)
    (mapcar (lambda (v-w)
              (let ((v (car v-w)))
                (when (eq (gethash v colors) white)
                  (setf (gethash v previouses) u)
                  (dfs-visit algo v))))
            (get-adj-list graph u))
    (setf (gethash u colors) black)
    (incf time)
    (setf (gethash u finishings) time)
    (setf topology (cons u topology ))))


