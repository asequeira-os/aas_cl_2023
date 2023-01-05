(in-package :aas-misc)

(defun read-text-file (file)
  "reads each line from (text)file and returns them as a list"
  (with-open-file (stream file)
    (do ((retval)
         (line (read-line stream nil)
               (read-line stream nil)))
        ((null line) (reverse retval))
      (push line retval))))

(defun sub-dirs (dir)
  "return list of subdirs where dir is pathname designator for a dir"
  (remove-if-not #'cl-fad:directory-pathname-p (cl-fad:list-directory dir)))

(defun files-in-dir (dir)
  "return contents of this dir, except sub dirs.
might usually be just plain files."
  (remove-if #'cl-fad:directory-pathname-p (cl-fad:list-directory dir)))


