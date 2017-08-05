;;;; ICFPC2017 solution code chunks.
;;;; Copyright (C) 2017 cybevnm 

(defpackage :icfpc-2017
  (:use :cl
        :alexandria
        :anaphora
        :serapeum)
  ;; (:shadowing-import-from
  ;;  :alexandria
  ;;  :copy-stream
  ;;  :copy-file)
  )
(in-package :icfpc-2017)

(defparameter *name* "cvnm")
(defparameter *host* "punter.inf.ed.ac.uk")
(defparameter *port* 9064)
(defparameter *curr-socket* nil)
(defparameter *curr-stream* nil)
(defparameter *curr-game* nil)
(defparameter *render-pics* t)

(define-condition icfpc-2017-condition (error)
  ((text :initarg :text :reader text)))

(defun msg (format &rest vals)
  (let ((effective-format
         (concatenate 'string "[LOG] " format "~%")))
    (apply #'format *error-output* (cons effective-format vals)))
  (values))

(defun encode-message (encodable)
  (let ((s (json:encode-json-to-string encodable)))
    (format nil "~a:~a" (length s) s)))

(defun decode-message (decodable)
  (json:decode-json-from-string decodable))


(defun send-message (encodable &optional (stream *curr-stream*))
  (let ((encoded (encode-message encodable)))
    (msg "Sending: \"~a\"..." encoded)
    (send encoded stream)))

(defun receive-message (&optional (stream *curr-stream*))
  (let ((curr-char)
        (len-seq (make-array 10 :fill-pointer 0)))
    (loop
       :do (setf curr-char (receive-char stream))
       :while (not (char= curr-char #\:))
       :do (vector-push curr-char len-seq))
    (let ((len (parse-integer (concatenate 'string len-seq))))
      (assert (> len 0))
      (let ((decodable (receive len stream)))
        (msg "Received: \"~a\"" decodable)
        (decode-message decodable)))))

(defun @test-receive-message ()
  (with-input-from-string (stream "14:{\"you\":\"user\"}")
    (receive-message stream)))

(defclass site ()
  ((id :initarg :id :reader site-id)
   (x :initarg :x :reader site-x)
   (y :initarg :y :reader site-y)
   (rivers :initform (make-array 100 :adjustable t :fill-pointer 0)
           :reader site-rivers :documentation "Adjacent rivers.")
   (mine :initform nil :accessor site-mine-p
         :documentation "TRUE if the site is a mine.")))

(defclass river ()
  ((begin :initarg :source :reader river-source
          :documentation "Site object.")
   (end :initarg :target :reader river-target
        :documentation "Site object.")
   (claimed-by :initform nil :accessor river-claimed-by
               :documentation "NIL if is not claimed, otherwise
               integer id of a punter who claimed the river.")
   (mark :initform nil :accessor river-mark)))

(defun river-other-end (river site)
  (cond
    ((eq site (river-source river)) (river-target river))
    ((eq site (river-target river)) (river-source river))
    (t (assert nil))))

(defclass game ()
  ((my-id :initarg :my-id :reader game-my-id)
   (punters-num :initarg :punters-num :reader game-punters-num)
   (sites :initarg :sites :reader game-sites
          :documentation "Array of sites")
   (rivers :initarg :rivers :reader game-rivers
           :documentation "Array of rivers")
   (mines :initarg :mines :reader game-mines
          :documentation "Array of sites which are mines")))

(defun game-find-river-by-source-id-and-target-id (game source target)
  (find-if (lambda (river)
             (or (and (= (site-id (river-source river)) source)
                      (= (site-id (river-target river)) target))
                 (and (= (site-id (river-source river)) target)
                      (= (site-id (river-target river)) source))))
           (game-rivers game)))

(defun game-apply-move (game move)
  (when (eq (move-type move) :claim)
    (let ((river
           (game-find-river-by-source-id-and-target-id
            game
            (move-source move)
            (move-target move))))
      (setf (river-claimed-by river) (move-punter-id move)))))

(defun game-apply-moves (game moves)
  (loop
     :for move :being :the :elements :of moves
     :do (game-apply-move game move)))

(defun game-clean-rivers-marks (game)
  (loop
     :for river :across (game-rivers game)
     :do (setf (river-mark river) nil)))

(defclass move ()
  ((punter-id :initarg :punter-id :reader move-punter-id
              :documentation "Id of a punter who made this move.")
   (type :initarg :type :reader move-type
         :documentation ":PASS or :CLAIM.")
   (source :initarg :source :initform nil :reader move-source)
   (target :initarg :target :initform nil :reader move-target)))

(defun assoc->site (assoc)
  (make-instance 'site
                 :id (cdr (assoc :id assoc))
                 :x (cdr (assoc :x assoc))
                 :y (cdr (assoc :y assoc))))

(defun find-site-by-id (sites id)
  "SITES is an array of sites"
  (assert sites)
  (assert id)
  (find-if (lambda (site)
             (= (site-id site) id))
           sites))

(defun assoc->river (assoc sites)
  "SITES is an array of sites"
  (let* ((source-id (cdr (assoc :source assoc)))
         (target-id (cdr (assoc :target assoc)))
         (source (find-site-by-id sites source-id))
         (target (find-site-by-id sites target-id))
         (result (make-instance 'river :source source :target target)))
    ;; Just checking...
    (assert source)
    (assert target)
    (vector-push-extend result (site-rivers source))
    (vector-push-extend result (site-rivers target))
    result))

(defun assoc->move (assoc)
  (let* ((pass (assoc :pass assoc))
         (claim (assoc :claim assoc))
         (obj (if pass pass claim)))
    (assert obj)
    (if pass
        (let ((punter-id (cdr (assoc :punter (cdr obj)))))
          (make-instance 'move :punter-id punter-id :type :pass))
        (let ((punter-id (cdr (assoc :punter (cdr obj))))
              (source (cdr (assoc :source (cdr obj))))
              (target (cdr (assoc :target (cdr obj)))))
          (make-instance 'move :punter-id punter-id :type :claim
                         :source source :target target)))))

(defun @test-assoc->move ()
  (assoc->move '((:CLAIM (:PUNTER . 0) (:SOURCE . 0) (:TARGET . 1)))) 
  (assoc->move '((:PASS (:PUNTER . 0)))))

(defun assoc->moves (assoc)
  (let* ((moves-cons (cdr (assoc :move assoc)))
         (stop-cons (cdr (assoc :stop assoc))))
    (let ((moves-list (cdr (assoc :moves (or moves-cons stop-cons))))
          (moves-arr (make-array 10 :adjustable t :fill-pointer 0))
          (scores))
      (dolist (move moves-list)
        (vector-push-extend (assoc->move move) moves-arr))
      (when stop-cons
        (let ((scores-list (cdr (assoc :scores stop-cons))))
          (setf scores scores-list)))
      (values moves-arr scores))))


(defun @test-assoc->moves ()
  (assoc->moves
   '((:MOVE
      (:MOVES ((:CLAIM (:PUNTER . 0) (:SOURCE . 0) (:TARGET . 1)))
       ((:CLAIM (:PUNTER . 1) (:SOURCE . 1) (:TARGET . 2))))))))

(defun assoc->game (assoc)
  (let* ((id (cdr (assoc :punter assoc)))
         (punters-num (cdr (assoc :punters assoc)))
         (map (cdr (assoc :map assoc)))
         (sites-assoc (cdr (assoc :sites map)))
         (rivers-assoc (cdr (assoc :rivers map)))
         (mines-list (cdr (assoc :mines map)))
         (sites (let ((arr (make-array 10 :adjustable t :fill-pointer 0)))
                            (dolist (site-assoc sites-assoc)
                              (vector-push-extend (assoc->site site-assoc) arr))
                            arr)))
    (make-instance 'game :my-id id :punters-num punters-num
                   :sites sites
                   :rivers (let ((arr
                                  (make-array 10 :adjustable t :fill-pointer 0)))
                             (dolist (river-assoc rivers-assoc)
                               (vector-push-extend
                                (assoc->river river-assoc sites) arr))
                             arr)
                   :mines (let ((arr (make-array 10 :adjustable t :fill-pointer 0)))
                            (dolist (mine-id mines-list)
                              (let ((mine (find-site-by-id sites mine-id)))
                                (assert mine)
                                (setf (site-mine-p mine) t)
                                (vector-push-extend mine arr)))
                             arr))))

(defun @test-assoc->game ()
  (let* ((assoc
          '((:PUNTER . 1) (:PUNTERS . 2)
           (:MAP
            (:SITES ((:ID . 4) (:X . 2.0) (:Y . -2.0)) ((:ID . 1) (:X . 1.0) (:Y . 0.0))
                    ((:ID . 3) (:X . 2.0) (:Y . -1.0)) ((:ID . 6) (:X . 0.0) (:Y . -2.0))
                    ((:ID . 5) (:X . 1.0) (:Y . -2.0)) ((:ID . 0) (:X . 0.0) (:Y . 0.0))
                    ((:ID . 7) (:X . 0.0) (:Y . -1.0)) ((:ID . 2) (:X . 2.0) (:Y . 0.0)))
            (:RIVERS ((:SOURCE . 3) (:TARGET . 4)) ((:SOURCE . 0) (:TARGET . 1))
                     ((:SOURCE . 2) (:TARGET . 3)) ((:SOURCE . 1) (:TARGET . 3))
                     ((:SOURCE . 5) (:TARGET . 6)) ((:SOURCE . 4) (:TARGET . 5))
                     ((:SOURCE . 3) (:TARGET . 5)) ((:SOURCE . 6) (:TARGET . 7))
                     ((:SOURCE . 5) (:TARGET . 7)) ((:SOURCE . 1) (:TARGET . 7))
                     ((:SOURCE . 0) (:TARGET . 7)) ((:SOURCE . 1) (:TARGET . 2)))
            (:MINES 1 5))))
         (game (assoc->game assoc)))
    (assert (= (length (game-rivers game)) 12))
    (assert (= (length (game-sites game)) 8)))
  (values))

(defun ->cleanup (&optional (stream *curr-stream*))
  (declare (ignore stream))
  (setf *curr-game* nil))

(defun ->handshake (&optional (stream *curr-stream*))
  (msg "Starting handshake...")
  (send-message `((:me . ,*name*)))
  (let ((answer (receive-message stream)))
    (when (not (string= (cdr (assoc :you answer)) *name*))
      (error 'icfpc-2017-condition :text "Wrong handshake answer")))
  (msg "Handshake finished..."))

(defun ->setup (&optional (stream *curr-stream*))
  (msg "Starting setup...")
  (msg "Waiting for players...")
  (let ((answer (receive-message stream)))
    (setf *curr-game* (assoc->game answer))
    (send-message `((:ready . ,(game-my-id *curr-game*)))))
  (msg "Setup finished..."))


(defun assoc->winner (scores)
  (let ((table
         (reverse
          (sort scores (lambda (a b)
                         (< (cdr (assoc :score a))
                            (cdr (assoc :score b))))))))
    (values (cdr (assoc :punter (first table))) table)))


(defparameter *max-depth* 2)
(defun !claim-descent (site my-id curr-depth)
  (if (> curr-depth *max-depth*)
      nil
      (let ((rivers (site-rivers site)))
        (loop
           :for river :across rivers
           :do (progn
                 (msg "RIVER ~a ~a"
                      (site-id (river-source river))
                      (site-id (river-target river)))
                 (let ((claimer (river-claimed-by river)))
                   (cond
                     ((null claimer)
                      (return-from !claim-descent
                        (make-instance 'move :type :claim
                                       :punter-id my-id
                                       :source (site-id (river-source river))
                                       :target (site-id (river-target river)))))
                     ((and (= claimer my-id)
                           (not (river-mark river)))
                      (setf (river-mark river) t)
                      (let ((move (!claim-descent (river-other-end river site)
                                                  my-id
                                                  (1+ curr-depth))))
                        (when move (return-from !claim-descent move)))))))))))

(defun !claim (&optional (game *curr-game*))
  (let* ((mines (game-mines game)))
    (loop
       :for mine :across mines
       :do (progn
             (msg "MINE ~a" (site-id mine))
             (let ((move (!claim-descent mine (game-my-id game) 0)))
               (game-clean-rivers-marks game)
               (when move
                 (return-from !claim move)))))))

(defun !pass (&optional (game *curr-game*))
  (make-instance 'move :type :pass
                 :punter-id (game-my-id game)))

(defun !ai (&optional (game *curr-game*))
  (or (!claim game) (!pass game)))

(defun send-pass-message (&optional (stream *curr-stream*))
  (send-message `((:pass . ((:punter . ,(game-my-id *curr-game*)))))
                stream))

(defun send-claim-message (move &optional (stream *curr-stream*))
  (assert (move-source move))
  (assert (move-target move))
  (send-message `((:claim . ((:punter . ,(game-my-id *curr-game*))
                             (:source . ,(move-source move))
                             (:target . ,(move-target move)))))
                stream))

(defun send-decision-message (move &optional (stream *curr-stream*))
  (ecase (move-type move)
    (:pass (send-pass-message stream))
    (:claim (send-claim-message move stream))))



(defun ->play (&optional (stream *curr-stream*))
  (msg "Starting play...")
  (let ((pic-index 0))
    (block play-loop
      (loop
         (let ((moves-or-stop (receive-message stream)))
           (msg "Applying moves to game...")
           (multiple-value-bind (moves scores)
               (assoc->moves moves-or-stop)
             (game-apply-moves *curr-game* moves)
             (when *render-pics* 
               (render-cl-dot-graph *curr-game* (format nil "pic-~a.png" pic-index))
               (incf pic-index))
             (if scores
                 (progn
                   (msg "Received stop message, stopping...")
                   (multiple-value-bind (winner table)
                       (assoc->winner scores)
                     (let* ((my-id (game-my-id *curr-game*))
                            (victory (= winner my-id))
                            (victory-or-defeat-string
                             (if victory "VICTORY:)" "DEFEAT:(")))
                       (msg "[~a]   [Our id is ~a]   [Table: ~a]"
                            victory-or-defeat-string my-id table))
                     (return-from play-loop)))
                 (progn
                   (msg "Calculating... ")
                   (msg "Sending response...")
                   (send-decision-message (!ai) stream)))
             )))))
  (msg "Play finished..."))

(defun cycle ()
  (msg "Starting cycle...")
  (->cleanup)
  (->handshake)
  (->setup)
  (->play)
  (msg "Cycle finished...")
  )

(defun connect (&optional (host *host*) (port *port*))
  (msg "Connecting ~a ~a..." host port)
  (setf *curr-socket*
        (usocket:socket-connect host port))
  (when *curr-socket*
    (setf *curr-stream*
          (usocket:socket-stream *curr-socket*)))
  (values))

(defun disconnect ()
  (msg "Disconnecting...")
  (when *curr-socket*
    (usocket:socket-close *curr-socket*)
    (setf *curr-socket* nil)
    (setf *curr-stream* nil))
  (values))

(defun send (string &optional (stream *curr-stream*))
  (format stream string)
  (force-output stream)
  (values))

(defun receive (len &optional (stream *curr-stream*))
  (let* ((seq (make-array len
                          :element-type (stream-element-type stream)
                          :fill-pointer t))
         (len (read-sequence seq stream))
         (subseq (subseq seq 0 len)))
    subseq))

(defun receive-char (&optional (stream *curr-stream*) )
  (read-char stream))

(defun main (&key (host *host*) (port *port*))
  (connect host port)
  (unwind-protect
       (cycle)
    (disconnect)))

(defmethod cl-dot:graph-object-node ((graph game) (obj site))
  (make-instance 'cl-dot:node
                 :attributes `(:label ,(if (site-mine-p obj)
                                           (format nil "~a (M)" (site-id obj))
                                           (format nil "~a" (site-id obj)))
                                      :style :solid
                                      :color :black
                                      :shape :box
                                      :fontname "courier")))

(defmethod cl-dot:graph-object-edges ((graph game))
  (let ((arr (make-array 10 :adjustable t :fill-pointer 0)))
    (loop
       :for river :across (game-rivers graph)
       :do (vector-push-extend
            (list (river-source river)
                  (river-target river)
                  `(:color ,(cond
                              ((null (river-claimed-by river))
                               :black)
                              ((= (river-claimed-by river) (game-my-id graph))
                               :blue)
                              (t :red))))
            arr))
    arr))

(defmethod cl-dot:graph-object-points-to (graph (obj site))
  nil)

(defun render-cl-dot-graph (game filename)
  (cl-dot:dot-graph
   (cl-dot:generate-graph-from-roots game
                                     (list (elt (game-sites game) 0)))
                    filename
                    :format :png
                    :directed nil))



