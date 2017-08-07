;;;; ICFPC2017 solution code chunks.
;;;; Copyright (C) 2017 cybevnm

(defpackage :icfpc-2017
  (:use :cl :alexandria :cl-containers))
(in-package :icfpc-2017)

(defparameter *name* "cvnm")
(defparameter *host* "punter.inf.ed.ac.uk")
(defparameter *port* 9064)
(defparameter *curr-socket* nil)
(defparameter *curr-stream-out* nil)
(defparameter *curr-stream-in* nil)
(defparameter *curr-game* nil)
(defparameter *render-pics* nil)
(defparameter *peeked-message* nil)
(defparameter *print-received* nil)
(defparameter *print-sending* nil)

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


(defun send-message (encodable &optional (stream-out *curr-stream-out*))
  (let ((encoded (encode-message encodable)))
    (when *print-sending*
      (msg "Sending: \"~a\"..." encoded))
    (send encoded stream-out)))

(defun receive-message (&optional (stream-in *curr-stream-in*))
  (if *peeked-message*
      (let ((result *peeked-message*))
        (setf *peeked-message* nil)
        result)
      (let ((curr-char)
            (len-seq (make-array 10 :fill-pointer 0)))
        (loop
           :do (setf curr-char (receive-char stream-in))
           :while (not (char= curr-char #\:))
           :do (vector-push curr-char len-seq))
        (let ((len (parse-integer (concatenate 'string len-seq))))
          (assert (> len 0))
          (let ((decodable (receive len stream-in)))
            (when *print-received*
              (msg "Received: \"~a\"" decodable))
            (decode-message decodable))))))

(defun peek-message (&optional (stream-in *curr-stream-in*))
  (let ((message (receive-message stream-in)))
    (setf *peeked-message* message)
    message))

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
         :documentation "TRUE if the site is a mine.")
   (note :initform nil :accessor site-note
         :documentation "Visualisation puts this on nodes.")

   (profit :initform 0 :accessor site-profit
           :documentation "Greedy algorithm cache.")))

(defclass river ()
  ((begin :initarg :source :reader river-source
          :documentation "Site object.")
   (end :initarg :target :reader river-target
        :documentation "Site object.")
   (claimed-by :initform nil :accessor river-claimed-by
               :initarg :claimed-by
               :documentation "NIL if is not claimed, otherwise
               integer id of a punter who claimed the river.")
   (mark :initform nil :accessor river-mark
         :initarg :mark)))

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
          :documentation "Array of sites which are mines")

   (routes :initarg :routes :initform nil
           :accessor game-routes
           :documentation "Routes between mines we are planning to claim.")
   (curr-route :initarg :curr-route :initform nil
               :accessor game-curr-route
               :documentation "The route we are claiming now.")))

(defun make-set (&key (size 100))
  (make-hash-table :test #'eq :size size))

(defun set-add (set item)
  (assert set)
  (assert item)
  (setf (gethash item set) t)
  (values))

(defun set-find (set item)
  (assert set)
  (assert item)
  (gethash item set))

(defun make-queue ()
  (make-container 'basic-queue))

(defun game-path-bfs (game source target &key (shortest nil))
  "Return array of successive sites from SOURCE to TARGET, or NIL if
such path doesn't exist. If SHORTEST is T then river claimers are
completelly ignored. Otherwise only a path we may reach is taken into
account."
  (assert game)
  (assert source)
  (assert target)
  (let* ((sites-num (length (game-sites game)))
         (set (make-set :size sites-num))
         (queue (make-queue))
         (links (make-hash-table :test #'eq :size sites-num))
         (my-id (game-my-id game)))
    (set-add set source)
    (enqueue queue source)
    (flet
        ;; Builds path array from SOURCE to TARGET using LINKS as a
        ;; guide.
        ((build-path ()
           (let ((arr (make-array sites-num :fill-pointer 0))
                 (curr target))
             (vector-push-extend curr arr)
             (loop
                (let ((prev (gethash curr links)))
                  (vector-push-extend prev arr)
                  (when (eq prev source)
                    (return (nreverse arr)))
                  (setf curr prev))))))
      (loop
         :while (not (empty-p queue))
         :do (let ((curr (dequeue queue)))
               (when (eq curr target)
                 (return (build-path)))
               (loop
                  :for river :across (site-rivers curr)
                  :do (let ((adjacent (river-other-end river curr))
                            (claimer (river-claimed-by river)))
                        (when (and (or shortest
                                       (or (not claimer) (= claimer my-id)))
                                   (not (set-find set adjacent)))
                          (set-add set adjacent)
                          (enqueue queue adjacent)
                          (setf (gethash adjacent links) curr)))))))))

(defun game-path (game source target)
  (game-path-bfs game source target))

(defun game-find-river-by-source-id-and-target-id (game source target)
  (find-if (lambda (river)
             (or (and (= (site-id (river-source river)) source)
                      (= (site-id (river-target river)) target))
                 (and (= (site-id (river-source river)) target)
                      (= (site-id (river-target river)) source))))
           (game-rivers game)))

(defun game-find-common-river (source target)
  (loop
     :for a :across (site-rivers source)
     :do (loop :for b :across (site-rivers target)
            :do (when (eq a b)
                  (return-from game-find-common-river a)))))

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
         (claimed-by (cdr (assoc :claimed-by assoc)))
         (mark (cdr (assoc :mark assoc)))
         (result (make-instance 'river :source source :target target
                                :claimed-by claimed-by :mark mark)))
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

(defun assoc->moves-and-state (assoc)
  (let* ((moves-cons (cdr (assoc :move assoc)))
         (stop-cons (cdr (assoc :stop assoc)))
         (state-cons (cdr (assoc :state assoc))))
    (let ((moves-list (cdr (assoc :moves (or moves-cons stop-cons))))
          (moves-arr (make-array 10 :adjustable t :fill-pointer 0))
          (scores)
          (game))
      (dolist (move moves-list)
        (vector-push-extend (assoc->move move) moves-arr))
      (when stop-cons
        (let ((scores-list (cdr (assoc :scores stop-cons))))
          (setf scores scores-list)))
      (when state-cons
        (let ((*read-eval* nil))
          (with-input-from-string (s state-cons)
            (setf game (assoc->game (read s))))))
      (values moves-arr scores game))))

(defun @test-assoc->moves-and-state ()
  (assoc->moves-and-state
   '((:MOVE
      (:MOVES ((:CLAIM (:PUNTER . 0) (:SOURCE . 0) (:TARGET . 1)))
       ((:CLAIM (:PUNTER . 1) (:SOURCE . 1) (:TARGET . 2))))
      ))))

(defun assoc->map (assoc)
  (let* ((sites-assoc (cdr (assoc :sites assoc)))
         (rivers-assoc (cdr (assoc :rivers assoc)))
         (mines-list (cdr (assoc :mines assoc)))
         (sites (let ((arr (make-array 10 :adjustable t :fill-pointer 0)))
                            (dolist (site-assoc sites-assoc)
                              (vector-push-extend (assoc->site site-assoc) arr))
                            arr))
         (rivers (let ((arr
                                  (make-array 10 :adjustable t :fill-pointer 0)))
                             (dolist (river-assoc rivers-assoc)
                               (vector-push-extend
                                (assoc->river river-assoc sites) arr))
                             arr))
         (mines (let ((arr (make-array 10 :adjustable t :fill-pointer 0)))
                            (dolist (mine-id mines-list)
                              (let ((mine (find-site-by-id sites mine-id)))
                                (assert mine)
                                (setf (site-mine-p mine) t)
                                (vector-push-extend mine arr)))
                            arr)))
    (values sites rivers mines)))

(defun assoc->game (assoc)
  (let* ((id (cdr (assoc :punter assoc)))
         (punters-num (cdr (assoc :punters assoc)))
         (map (cdr (assoc :map assoc)))
         (routes (cdr (assoc :routes assoc)))
         (curr-route (cdr (assoc :curr-route assoc))))
    (multiple-value-bind (sites rivers mines) (assoc->map map)
      (make-instance 'game :my-id id :punters-num punters-num
                     :sites sites
                     :rivers rivers
                     :mines mines
                     :routes routes
                     :curr-route curr-route))))

(defun game->assoc (game)
  `((:punter . ,(game-my-id game)) (:punters . ,(game-punters-num game))
    (:map
     (:sites ,@(loop
                  :for site :across (game-sites game)
                  :collect `((:id . ,(site-id site))
                             (:x . ,(site-x site))
                             (:y . ,(site-y site)))))
     (:rivers ,@(loop
                   :for river :across (game-rivers game)
                   :collect `((:source . ,(site-id (river-source river)))
                              (:target . ,(site-id (river-target river)))
                              (:claimed-by . ,(river-claimed-by river))
                              (:mark . ,(river-mark river)))))
     (:mines ,@(loop
                  :for mine :across (game-mines game)
                  :collect (site-id mine))))

    
    (:routes . ,(game-routes game))
    (:curr-route . ,(game-curr-route game))
    ))

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

(defun ->cleanup ()
  (setf *curr-game* nil))

(defun ->handshake (&optional (stream-out *curr-stream-out*)
                      (stream-in *curr-stream-in*))
  (msg "Starting handshake...")
  (send-message `((:me . ,*name*)) stream-out)
  (let ((answer (receive-message stream-in)))
    (when (not (string= (cdr (assoc :you answer)) *name*))
      (error 'icfpc-2017-condition :text "Wrong handshake answer")))
  (msg "Handshake finished..."))

(defun ->setup (type &optional (stream-out *curr-stream-out*)
                      (stream-in *curr-stream-in*))
  (msg "Starting setup...")
  (msg "Waiting for players...")
  (let ((answer (receive-message stream-in)))
    (setf *curr-game* (assoc->game answer))
    (let ((ready-chunk `(:ready . ,(game-my-id *curr-game*)))
          (state-chunk `(:state . ,(let ((*print-pretty* nil))
                                     (with-output-to-string (s)
                                       (write (game->assoc *curr-game*)
                                              :stream s))))))
      (send-message (ecase type
                      (:online (list ready-chunk))
                      (:offline (list ready-chunk state-chunk)))
                    stream-out)))
  (msg "Setup finished..."))

(defun assoc->winner (scores)
  (let ((table
         (reverse
          (sort scores (lambda (a b)
                         (< (cdr (assoc :score a))
                            (cdr (assoc :score b))))))))
    (values (cdr (assoc :punter (first table))) table)))


(defparameter *max-depth* 2)

(defun !descent-impl (game mine site curr-depth)
  (when (<= curr-depth *max-depth*)
    (let* ((rivers (site-rivers site))
           (targets
            (remove mine
                    (map '(vector t)
                         (lambda (river)
                           (river-other-end river site))
                         rivers))))
      (when (> (length targets) 0)
        ;; Calc profits.
        (map nil (lambda (target)
                   (let ((path (game-path-bfs game mine target
                                              :shortest t)))
                     (setf (site-profit target)
                           (if path (length path) 0))))
             targets)
        (let* ((targets-sorted
                (sort targets
                      (lambda (a b) (> (site-profit a) (site-profit b)))))
               ;(biggest-profit (site-profit (elt targets-sorted 0)))
               (most-profitable-targets
                (subseq targets-sorted
                        0 (1+ (floor (length targets-sorted) 2))))
               (most-profitable-rivers
                (map '(vector t)
                     (lambda (target)
                       (game-find-common-river site target))
                     most-profitable-targets)))

          (loop
             :for river :across most-profitable-rivers
             :do (cond
                   ;; The river unclaimed, claim it.
                   ((not (river-claimed-by river))
                    (return
                      (make-instance 'move :type :claim
                                     :punter-id (game-my-id game)
                                     :source (site-id site)
                                     :target
                                     (site-id (river-other-end river site)))))
                   ;; The river is ours, descent.
                   ((= (river-claimed-by river) (game-my-id game))
                    (let ((move
                           (!greedy-impl game mine (river-other-end river site)
                                         (1+ curr-depth))))
                      (when move (return move))))
                   ;; The river is taken, ignore.
                   (t
                    (return nil)))))))))

(defun !descent (game)
  (msg "!Descent...")
  (let* ((mines (game-mines game)))
    (loop
       :for mine :across mines
       :do (progn
             ;; Block mine
             (loop
                :for river :across (site-rivers mine)
                :do (unless (river-claimed-by river)
                      (return-from !descent
                        (make-instance 'move :type :claim
                                       :punter-id (game-my-id game)
                                       :source (site-id (river-source river))
                                       :target (site-id (river-target river))))))
             ;; Descent
             (let ((move (!descent-impl game mine mine 0)))
               (game-clean-rivers-marks game)
               (when move
                 (return-from !descent move)))))))

(defun combinations-of-2 (list)
  (let ((result))
    (map-combinations (lambda (c) (push c result)) list :length 2)
    (reverse result)))

(defun !path->move (game path)
  "Return (VALUES MOVE RIVER-INDEX) or NIL."
  (loop
     :for river-index :from 0 :below (1- (length path))
     :do (let* ((source (elt path river-index))
                (target (elt path (1+ river-index)))
                (river (game-find-common-river source target))
                (claimer (river-claimed-by river)))
           (unless claimer
             (return (values (make-instance 'move
                                            :punter-id (game-my-id game)
                                            :type :claim
                                            :source (site-id source)
                                            :target (site-id target))
                             river-index))))))

(defun !connect-mines-impl (game)
  "Return a (VALUES move :RETRY or :EXHAUSTED)."
  (msg "!Connecting mines...")
  (let* ((mines (game-mines game))
         (mines-num (length mines)))
    (when (> mines-num 1)
      (with-accessors ((routes game-routes) (curr-route game-curr-route))
          game
        (unless routes
          (setf routes (combinations-of-2 (shuffle (iota mines-num))))
          (setf curr-route 0))
        (if (< curr-route (length routes))
            ;; We have rotes to explore...
            (progn
              (assert (> (length (game-mines game)) 1))
              (let* ((mines (game-mines game))
                     (route (elt routes curr-route))
                     (source-mine (elt mines (first route)))
                     (target-mine (elt mines (second route)))
                     (path (game-path game source-mine target-mine)))
                (flet ((switch-to-next-route-if-required
                           (&optional path river-index)
                         (when (or (or (not path) (not river-index))
                                   (= (+ river-index 2) (length path)))
                           (incf curr-route))))
                  (if path
                      (progn
                        (msg "Path found...")
                        (multiple-value-bind (move river-index)
                            (!path->move game path)
                          (if (and move river-index)
                              (progn
                                (msg "We have a successfull move...")
                                (switch-to-next-route-if-required path river-index)
                                (values move nil))
                              ;; 
                              (progn
                                (msg "The current path exhausted...")
                                (switch-to-next-route-if-required)
                                (values nil :retry)))))
                      (progn
                        (msg "No path, switching to the next...")
                        (switch-to-next-route-if-required)
                        (values nil :retry))))))
            (progn
              (msg "No more routes, give up...")
              (values nil :exhausted)))))))

(defun !connect-mines (game)
  (let (result)
    (loop
       :do (setf result (multiple-value-list
                         (!connect-mines-impl game)))
       :while (eq (second result) :retry))
    (first result)))

(defun !primary (game)
  "Primary algorithm"
  (msg "!Primary...")
  (!connect-mines game))

(defun !fallback (game)
  "Just trying to make some move..."
  (msg "!Fallbback...")
  (!descent game))

(defun !hinder-random (game)
  (msg "!Hinder random...")
  (loop
     :for river :across (game-rivers game)
     :do (unless (river-claimed-by river)
           (return-from !hinder-random
             (make-instance 'move :type :claim
                            :punter-id (game-my-id game)
                            :source (site-id (river-source river))
                            :target (site-id (river-target river)))))))

(defun !hinder (game)
  (msg "!Hinder")
  (!hinder-random game))

(defun !pass (&optional (game *curr-game*))
  (make-instance 'move :type :pass
                 :punter-id (game-my-id game)))

(defun !ai (&optional (game *curr-game*))
  (or (!primary game)
      (!fallback game)
      (!hinder game)
      (!pass game)))

(defun send-pass-message (game type &optional (stream-out *curr-stream-out*))
  (let ((pass-chunk `(:pass . ((:punter . ,(game-my-id *curr-game*)))))
        (state-chunk `(:state . ,(let ((*print-pretty* nil))
                                   (with-output-to-string (s)
                                     (write (game->assoc game)
                                            :stream s))))))
    (send-message (ecase type
                    (:online (list pass-chunk))
                    (:offline (list pass-chunk state-chunk)))
                  stream-out)))

(defun send-claim-message (move game type &optional (stream-out *curr-stream-out*))
  (assert (move-source move))
  (assert (move-target move))
  (let ((claim-chunk `(:claim . ((:punter . ,(game-my-id *curr-game*))
                                 (:source . ,(move-source move))
                                 (:target . ,(move-target move)))))
        (state-chunk `(:state . ,(let ((*print-pretty* nil))
                                   (with-output-to-string (s)
                                     (write (game->assoc game)
                                            :stream s))))))
    (send-message (ecase type
                    (:online (list claim-chunk))
                    (:offline (list claim-chunk state-chunk)))
                  stream-out)))

(defun send-decision-message (move game type &optional (stream-out *curr-stream-out*))
  (ecase (move-type move)
    (:pass (send-pass-message game type stream-out))
    (:claim (send-claim-message move game type stream-out))))

(defun ->play (type &optional (stream-out *curr-stream-out*)
                 (stream-in *curr-stream-in*))
  (msg "Starting play...")
  (let ((pic-index 0))
    (block play-loop
      (loop
         ;; Play.
         (let ((moves-or-stop-and-state (receive-message stream-in)))
           (msg "Applying moves to game...")
           (multiple-value-bind (moves scores game)
               (assoc->moves-and-state moves-or-stop-and-state)
             (when game
               (setf *curr-game* game))
             (assert *curr-game*)
             (game-apply-moves *curr-game* moves)
             (render-game-if-necessary *curr-game* :index pic-index)
             (incf pic-index)
             (if scores
                 (progn
                   (msg "Received stop message, stopping...")
                   (multiple-value-bind (winner table)
                       (assoc->winner scores)
                     (declare (ignore winner))
                     (let* ((my-id (game-my-id *curr-game*))
                            (position
                             (position my-id
                                       table
                                       :key (lambda (row)
                                              (cdr (assoc :punter row))))))
                       (msg "[Position is ~a]   [Our id is ~a]   [Table: ~a]"
                            position my-id table))
                     (return-from play-loop)))
                 (progn
                   (msg "Calculating... ")
                   (send-decision-message (!ai) *curr-game* type stream-out)))
             )))))
  (msg "Play finished..."))

(defun cycle (type)
  "TYPE is any of :ONLINE :OFFLINE."
  (msg "Starting cycle...")
  (->cleanup)
  (->handshake)
  (let* ((message (peek-message))
         (phase (if (cdr (assoc :punter message))
                    :setup :play)))
    (ecase phase
      (:setup (->setup type)
              (->play type))
      (:play (->play type))))
  
  (msg "Cycle finished..."))

(defun connect (&optional (host *host*) (port *port*))
  (msg "Connecting ~a ~a..." host port)
  (setf *curr-socket*
        (usocket:socket-connect host port))
  (when *curr-socket*
    (setf *curr-stream-in*
          (usocket:socket-stream *curr-socket*))
    (setf *curr-stream-out*
          (usocket:socket-stream *curr-socket*)))
  (values))

(defun disconnect ()
  (msg "Disconnecting...")
  (when *curr-socket*
    (usocket:socket-close *curr-socket*)
    (setf *curr-socket* nil)
    (setf *curr-stream-in* nil)
    (setf *curr-stream-out* nil))
  (values))

(defun send (string &optional (stream-out *curr-stream-out*))
  (format stream-out string)
  (force-output stream-out)
  (values))

(defun receive (len &optional (stream-in *curr-stream-in*))
  (let* ((seq (make-array len
                          :element-type (stream-element-type stream-in)
                          :fill-pointer t))
         (len (read-sequence seq stream-in))
         (subseq (subseq seq 0 len)))
    subseq))

(defun receive-char (&optional (stream-in *curr-stream-in*))
  (read-char stream-in))

(defun main-online (&key (host *host*) (port *port*) (name *name*))
  (let ((*name* name))
    (connect host port)
    (unwind-protect
         (cycle :online)
      (disconnect))))

(defun main-offline ()
  (let ((*render-pics* nil)
        (*curr-stream-in* *standard-input*)
        (*curr-stream-out* *standard-output*))
    (cycle :offline)))

(defun main-test-game-path (&key (iterations-num 20) (map "sample.json"))
  (let ((my-id 0)
        (punters-num 1)
        (assoc (decode-message
                (read-file-into-string (concatenate 'string "map/" map)))))
    (multiple-value-bind (sites rivers mines) (assoc->map assoc)
      (let* ((game (make-instance 'game :my-id my-id
                                  :punters-num punters-num
                                  :sites sites
                                  :rivers rivers
                                  :mines mines))
             (sites (game-sites game))
             (mines (game-mines game))
             (path (game-path-bfs game (elt mines 0) (elt mines 1)))
             (moves (make-array 10 :adjustable t :fill-pointer 0)))
        (loop
           :for x :from 0 :below (1- (length path))
           :do (vector-push-extend
                (make-instance 'move :punter-id 0 :type :claim
                               :source (site-id (elt path x))
                               :target (site-id (elt path (1+ x))))
                moves))
        (game-apply-moves game moves)
        (render-game-if-necessary game :name "bfs")
        ;; (loop :for x :from 0 :below iterations-num
        ;;    :do (let ((move (!ai game)))
        ;;          (game-apply-move game move)
        ;;          (render-game-if-necessary game :index x)))
        ))))

(defun main-test-ai (&key (iterations-num 20) (map "sample.json"))
  (let ((my-id 0)
        (punters-num 1)
        (assoc (decode-message
                (read-file-into-string (concatenate 'string "map/" map)))))
    (multiple-value-bind (sites rivers mines) (assoc->map assoc)
      (let* ((game (make-instance 'game :my-id my-id
                                  :punters-num punters-num
                                  :sites sites
                                  :rivers rivers
                                  :mines mines))
             (sites (game-sites game))
             (mines (game-mines game)))
        (loop :for x :from 0 :below iterations-num
           :do (let ((move (!ai game)))
                 (game-apply-move game move)
                 (render-game-if-necessary game :index x)))))))


(defmethod cl-dot:graph-object-node ((graph game) (obj site))
  (flet ((build-label ()
           (let ((result (format nil "~a" (site-id obj))))
             (when (site-mine-p obj)
               (setf result (concatenate 'string result " (M)")))
             (when (site-note obj)
               (setf result (concatenate 'string result
                                         " [~a]"
                                         (site-note obj))))
             result)))
    (make-instance 'cl-dot:node
                   :attributes `(:label ,(build-label)
                                        :style :solid
                                        :color :black
                                        :shape :box
                                        :fontname "courier"))))

(defmethod cl-dot:graph-object-edges ((graph game))
  (let ((arr (make-array 10 :adjustable t :fill-pointer 0))
        (colors '((0 . "#FF0000") (1 . "#FFBF00") (2 . "#7FFF00")
                  (3 . "#00FF3F") (4 . "#00FFFF") (5 . "#003FFF")
                  (6 . "#7F00FF") (7 . "#FF00BF") (8 . "#FF0000"))))
    (loop
       :for river :across (game-rivers graph)
       :do (vector-push-extend
            (list (river-source river) (river-target river)
                  (let* ((claimer (river-claimed-by river))
                         (color (cdr (assoc claimer colors))))
                    `(:color ,(cond
                                ((null claimer) :black)
                                ((= claimer (game-my-id graph)) :blue)
                                (color color)
                                (t :magneta))
                      :style :bold)))
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

(defun render-game-if-necessary (game &key index name)
  (when *render-pics*
    (render-cl-dot-graph game
                         (format nil (cond
                                       (name
                                        (concatenate 'string
                                                     "pic/" name ".png"))
                                       (index "pic/pic-~a.png")
                                       (t (assert nil)))
                                 index))))
