(in-package :aas-graph)

(aseq-test:deftest adj-list-graph-test-1
  (let ((graph (make-instance 'adjacency-list-graph)))
    (make-test-graph-1 graph)))


;; 0->2--->9  6-->3  8  1
;;    |    |  |   |
;;    |    |  |   |
;;    |    V  V   |
;;    V    4  7<--
;;    5


;;this function can only be called inside a the context of a test
(defun make-test-graph-1 (graph)
  (let ((vertices (make-array 10)))
    (dotimes (i 10)
      (let ((vertex (make-vertex i)))
        (add-vertex graph vertex)
        (setf (aref vertices i) vertex)))
    (add-edge graph (make-edge (aref vertices 0) (aref vertices 2) 1))
    (add-edge graph (make-edge (aref vertices 2) (aref vertices 5) 1))
    (add-edge graph (make-edge (aref vertices 2) (aref vertices 9) 1))
    (add-edge graph (make-edge (aref vertices 9) (aref vertices 4) 1))
    (add-edge graph (make-edge (aref vertices 6) (aref vertices 7) 1))
    (add-edge graph (make-edge (aref vertices 6) (aref vertices 3) 1))
    (add-edge graph (make-edge (aref vertices 3) (aref vertices 7) 1))
    (dotimes (i 10)
      (aseq-test:is (= i (v-num (aref vertices i)))))
    (aseq-test:is (= 10 (length (vertices graph))))
    (aseq-test:is (= 10 (vertex-count graph)))
    (aseq-test:is (= 7 (length (edges graph))))
    graph))

(aseq-test:deftest graph-test-1
  (let ((graph (make-instance 'graph)))
    (make-test-graph-1 graph)))

;;graph from 'algorithms CLRS 2nd ed' pp. 589
(defun bellman-ford-test-graph ()
  (let ((graph (make-instance 'graph))
        (s (make-vertex 's))
        (vt (make-vertex 'vt))
        (x (make-vertex 'x))
        (y (make-vertex 'y))
        (z (make-vertex 'z)))
    (let ((st (make-edge s vt 6))
          (tx (make-edge vt x 5))
          (xt (make-edge x vt -2))
          (sy (make-edge s y 7))
          (ty (make-edge vt y 8))
          (yz (make-edge y z 9))
          (zs (make-edge z s 2))
          (yx (make-edge y x -3))
          (tz (make-edge vt z -4))
          (zx (make-edge z x 7)))
      (mapcar (lambda (v)
                (add-vertex graph v))
              (list s vt x y z))
      (mapcar (lambda (e)
                (add-edge graph e))
              (list st tx xt sy ty yz zs yx tz zx)))
    graph))

(defun bellman-ford-test-ssd (g u)
  (let ((ssd (bellman-ford g u)))
    (aseq-test:is (not (null ssd)))
    (print (vertex-data u))
    (print-path-bf (previouses ssd) (v-num u))
  ))

(defun print-path-bf (p i)
  (break)
  (print (vertex-data (aref p i)))
  (print-path-bf p (v-num (aref p i))))

(aseq-test:deftest bellman-ford-test
  (let ((g (bellman-ford-test-graph)))
    (map-vertices g (lambda (u)
                      (bellman-ford-test-ssd g u)))))

;;todo 9 this code seems to be working
;;but the results are yet to be interpreted in a useful way


(aseq-test:deftest bfs-graph-test-1
  (let ((graph (make-instance 'adjacency-list-graph)))
    (make-test-graph-1 graph)
    (let ((vertices (vertices graph))
          (algo (make-instance 'breadth-first-search)))
      (aseq-test:is vertices)
      (setf (graph algo) graph)
      (map-vertices
       graph
       (lambda (source-vertex)
         (map-vertices
          graph
          (lambda (dest-vertex)
            (let ((path (shortest-path algo source-vertex dest-vertex)))
              (when path
                (print-path source-vertex dest-vertex path))))))))))

(defun print-path (source-vertex dest-vertex path)
  "test only function to print computed path"
  (when path
    (aseq-test:test-message
     5
     (with-output-to-string (msg)
       (format msg "path from ~A to ~A:"
               (vertex-data source-vertex) (vertex-data dest-vertex))
       (mapcar (lambda (vertex)
                 (format msg  "~A" (vertex-data vertex))) path)))))





;;this function can only be called inside a the context of a test
(defun make-test-graph-topo-sort (graph)
  (let ((vh (make-hash-table :test #'equal)))
    (flet ((topo-vertex (d)
             (let ((v (make-vertex d)))
               (add-vertex graph v)
               (setf (gethash d vh) v)))
           (topo-edge (d1 d2)
             (add-edge graph
                       (make-edge (gethash d1 vh)
                                  (gethash d2 vh) 1))))
      (topo-vertex "pants")
      (topo-vertex "belt")
      (topo-vertex "shirt")
      (topo-vertex "tie")
      (topo-vertex "undershorts")
      (topo-vertex "jacket")
      (topo-vertex "socks")
      (topo-vertex "shoes")
      (topo-vertex "watch")

      ;; (topo-edge "pants" "undershorts" )
      ;; (topo-edge "belt" "pants" )
      ;; (topo-edge "jacket" "belt" )
      ;; (topo-edge "belt" "shirt" )
      ;; (topo-edge "tie" "shirt" )
      ;; (topo-edge "jacket" "tie" )
      ;; (topo-edge "shoes" "undershorts" )
      ;; (topo-edge "shoes" "pants" )
      ;; (topo-edge "shoes" "socks" )

      (topo-edge "undershorts" "pants")
      (topo-edge "pants" "belt")
      (topo-edge "belt" "jacket")
      (topo-edge "shirt" "belt")
      (topo-edge "shirt" "tie")
      (topo-edge "tie" "jacket")
      (topo-edge "undershorts" "shoes")
      (topo-edge "pants" "shoes")
      (topo-edge "socks" "shoes")



      (aseq-test:is (= 9 (length (vertices graph))))
      (aseq-test:is (= 9 (length (edges graph))))
      graph)))

(defun print-toplogy (topology)
  (mapcar (lambda (v)
            (aseq-test::test-message 5 (vertex-data v))
            ) topology))

(aseq-test:deftest dfs-graph-test-1
  (let ((graph (make-instance 'adjacency-list-graph)))
    (make-test-graph-topo-sort graph)
    (let ((algo (make-instance 'depth-first-search)))
      (setf (graph algo) graph)
      (depth-first-search algo)
      (print-toplogy (topology algo)))))



(aseq-test:deftest dijkstra-graph-test-1
  (let ((graph (make-instance 'adjacency-list-graph)))
    (make-dijkstra-test-graph-1 graph)
    (map-vertices graph
                  (lambda (source)
                    (let ((ssd (initialize-single-source graph source)))
                      (aseq-test:is ssd)
                      ;(break)
                      )))))




;;graph from 'algorithms CLRS 2nd ed' pp.596
;;vertices mapping s 0,t 1,x 2,y 3, z 4
(defun make-dijkstra-test-graph-1 (graph)
  (let ((vertices (make-array 5)))
    (dotimes (i 5)
      (let ((vertex (make-vertex i)))
        (add-vertex graph vertex)
        (setf (aref vertices i) vertex)))
    (add-edge graph (make-edge (aref vertices 0) (aref vertices 1) 10))
    (add-edge graph (make-edge (aref vertices 1) (aref vertices 2) 1))
    (add-edge graph (make-edge (aref vertices 0) (aref vertices 3) 5))
    (add-edge graph (make-edge (aref vertices 3) (aref vertices 1) 3))
    (add-edge graph (make-edge (aref vertices 1) (aref vertices 3) 2))
    (add-edge graph (make-edge (aref vertices 3) (aref vertices 4) 2))
    (add-edge graph (make-edge (aref vertices 4) (aref vertices 0) 7))
    (add-edge graph (make-edge (aref vertices 3) (aref vertices 2) 9))
    (add-edge graph (make-edge (aref vertices 4) (aref vertices 2) 6))
    (add-edge graph (make-edge (aref vertices 2) (aref vertices 4) 4))
    (aseq-test:is (= 5 (length (vertices graph))))
    (aseq-test:is (= 10 (length (edges graph))))
    graph))


(aseq-test:deftest all-tests
  (and (aas-priority-queue-test-1)
       (dijkstra-graph-test-1)
       (graph-test-1)
       (aas-priority-queue-test-2)
       (adj-list-graph-test-1)
       (dfs-graph-test-1)
       (bfs-graph-test-1)))
