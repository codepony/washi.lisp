#|  washi.lisp - A simple waschi cl-i-ent written in CommonLisp (SBCL) - version current-git
		Copyright (C) 2013  @d3f <http://identi.ca/d3f>
		
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
    
    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#


; Load the DRAKMA-webclient:
(ql:quickload :drakma)
; Load a html de/encoder for @Revengedays random-word API
(ql:quickload :html-entities)
; If you get an error here like: `"Unable to load any of the alternatives:~%~S"("libssl.so.1.0.0" "libssl.so.0.9.8"
; "libssl.so" "libssl.so.4")' or`The loaded code expects an incompatible layout for class SB-PRETTY:PRETTY-STREAM.'
; you can just accept those 2


(defun replace-all (string part replacement &key (test #'char=))
  "(replace-all) function for the lists
   Stolen from: http://cl-cookbook.sourceforge.net/strings.html"
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
           while pos)))


(defun split-by-newline (string)
  "Also a (split-by-newline) function would be great:
   Stolen from: http://cl-cookbook.sourceforge.net/strings.html"
  (loop for i = 0 then (1+ j)
        as j = (position #\Newline string :start i)
        collect (subseq string i j)
        while j))


; Define system parameters:
(defparameter *username* "Hugo")
(defparameter *password* "mycock")
; You can change these here
; fetching the serverlist:
(defparameter *server-list-url* "http://waschi.org/servers.php")
; stored as variable to produce less traffic:
(defparameter *server-list* (list)) 
; Save obj-lists - use some ram:
(defparameter *all-obj-list* (list)) 
; It hurts to use a global variable for that:
(defparameter *isupdated* nil)


(defun get-server-list ()
  "Returns a list of all washi servers in the network"
  (setf *server-list* (mapcar #'symbol-name
                              ; remove empty list parts, to fix bugs (and improve logic):
                              (remove '|| (mapcar #'intern
                                   (split-by-newline
                                     (replace-all
                                       (drakma:http-request *server-list-url*)
                                       "receive.php" "found")))))))


(defun get-obj-list (found-site)
  "Parses a found-site and returns a list oft the site encoded in utf-8"
  (let ((working (ignore-errors (drakma:http-request found-site))))
    ; Some servers give a #\Return #\Newline back:
    (let ((working-list (concatenate 'list working)))
    ; sometimes you get a code-file and sometimes chars (fuck webservers) 
      (if (not (characterp (car working-list))) 
        (setf working-list (flexi-streams:octets-to-string working :external-format :utf-8))
        (setf working-list (flexi-streams:octets-to-string
                             (flexi-streams:string-to-octets working) :external-format :utf-8))
        )
    ; producing a list we can work with (chars instead of code) - Now with working utf-8
    (remove '|| (mapcar #'intern
            (split-by-newline (concatenate 'string (remove #\Return (concatenate 'list working-list)))))))))
; the (intern) functions changes the "strings" to '|Symbols| - easier to compare for me.


(defun save-obj-list ()
  "Creates a list of pairs of sites with their objects"
  (setf *all-obj-list* (list))
  (loop for i from 0 to (1- (length *server-list*)) do
        (push (cons (nth i *server-list*) (cons (get-obj-list (nth i *server-list*)) nil)) *all-obj-list*))
  *all-obj-list*)


(defun search-ui ()
  "A simple user interaction for the searching"
  (format t "What are you looking for?~%")
  (let ((str (read-line)))
    (cond ((equal str "back") (startup))
          ((equal str "quit") (exit))
          (t (search-my-clothing (intern str))))))
          

(defun clean-ui ()
  "A simple user interaction for the cleaning"
  (format t "What do you want to clean?~%")
  (let ((str (read-line)))
    (format t "~%")
    (cond ((equal str "back") (startup))
          ((equal str "random") (send-random-word))
          ((equal str "quit") (exit))
          (t (clean-my-clothing (string str) 0 )))))


(defun startup ()
  "Initialize the interface and give a simple overview"
  (format t "Do you want to '(s)earch' or '(c)lean' or change (u)sername and password or 'take-all' or (quit) something?~%")
  (let ((str (read-line)))
    (cond ((or (equal str "search") (equal str "s")) (search-ui))
          ((or (equal str "clean") (equal str "c")) (clean-ui))
          ((equal str "u") (change-username))
          ((equal str "take-all") (take-all))
          ((equal str "quit") (exit))
          (t (startup)))))


(defun succsess (obj found-server-list)
  "Function used when an object was found"
  (format t "Your ~a was found by me. ~a~%" obj found-server-list)
  (setf *isupdated* nil)
  (search-ui))


(defun failed (obj)
  "Function used when an object was not found"
  (format t "Your ~a was not found by me.~%" obj)
  (setf *isupdated* nil)
  (search-ui))


(defun search-my-clothing (obj)
  "This will lookup your object on the servers, and re-fetches the list if it's not found on the first time."
  (let ((obj-server-list (list)))
    ; -1 we start counting from 0, but length is human-like:
    (loop for i from 0 to (- (length *server-list*) 1)
          do
          (let ((obj-list (cadr (nth i *all-obj-list*))) (obj-server (car (nth i *all-obj-list*))))
            (when (find obj obj-list)
              (format t "~a ~%" (drakma:http-request (replace-all obj-server "found" "echowash.php")
                                   :method :post
                                   :parameters (list (cons "Kleidung" (symbol-name obj))
                                                     (cons "Username" *username*)
                                                     (cons "Password" *password*)
                                                     (cons "TakeAway" "True"))))
              (push (replace-all obj-server "found" "") obj-server-list))))
    (if obj-server-list
      (succsess obj obj-server-list)))
    (if *isupdated*
      (failed obj)
      (progn 
        (format t "Reloading Obj-list from all servers...~%")
        (get-server-list)
        (save-obj-list)
        (setf *isupdated* t)
        ; recursive call:
        (search-my-clothing obj))))


(defun clean-my-clothing (obj callcount)
  "Sends your laundry to a random server"
  (if (eq *server-list* NIL)
    (progn
      (format t "No serverlist found!~%")
      (get-server-list)
      (search-ui)))
  (let ((obj-send (flexi-streams:octets-to-string
              (flexi-streams:string-to-octets obj :external-format :utf-8)))
        (server-send (replace-all (nth (random (length *server-list*)) *server-list*) "found" "echowash.php")))
  (let ((resp (drakma:http-request server-send
                       :method :post
                       :parameters (list (cons "Kleidung" obj-send)
                                         (cons "Username" *username*)
                                         (cons "Password" *password*)))))
    (format t "Your ~a has been sent to ~a~%" obj (replace-all server-send "echowash.php" ""))
    (if resp
      ; See what the response was like - changed to 4, because reply looks fucked up atm.:
      (case (char resp 4)
        (#\H (format t "Your laundry (~a) was done, and is now clean~%" obj))
        (#\D (format t "Oh fuck, they lost your ~a~%" obj))
        (otherwise (format t "They can't clean that ~%")))
      (progn
        (format t "Oh well, they don't like you. Trying another server...~%")
        (if (> callcount (length *server-list*))
          (progn
            (format t "To many retries~%")
            (clean-ui))
          (progn
            (clean-my-clothing obj (+ 1 callcount))))))))
  (clean-ui))


(defun send-random-word ()
  "Creates a random word, using the API of @Revengeday"
  (clean-my-clothing (html-entities:decode-entities 
                                         (drakma:http-request "http://dev.revengeday.de/pointlesswords/api/")) 0))


(defun take-all ()
  "Tries your username and password for any object"
  (mapcar #'(lambda (x) (fetch-my-clothing x))
          (apply #'append (mapcar #'(lambda (x) (car (cdr x))) *all-obj-list*)))
  (startup))

(defun fetch-my-clothing (obj)
  "Just a simple search-my-clothing function for (take-all)"
  (loop for i from 0 to (- (length *server-list*) 1)
        do
        (let ((obj-list (cadr (nth i *all-obj-list*))) (obj-server (car (nth i *all-obj-list*))))
          (when (find obj obj-list)
            (format t "~a ~%" (drakma:http-request (replace-all obj-server "found" "echowash.php")
                                                   :method :post
                                                   :parameters (list (cons "Kleidung" (symbol-name obj))
                                                                     (cons "Username" *username*)
                                                                     (cons "Password" *password*)
                                                                     (cons "TakeAway" "True"))))))))


(defun change-username ()
  "Simple lets you reset the username and password for the current run"
  (format t "What username do you want to choose?~%")
  (setf *username* (read-line))
  (format t "And your password?~%")
  (setf *password* (read-line))
  (startup))


(defun first-init ()
  "Initial function, gives some instructions and a small how to."
  (format t "~%~%~T~TWelcome to washi.lisp a full cl-i-ent (commonlisp-commandlineinterface-client) for @MeikosDis' Waschi~%~%")
  (format t "~T~TAt first the basics (for people too lazy reading the source-code):~%")
  (format t "~T~T~T> You can choose with the input of `search' `clean' and `quit' what you want to do~%")
  (format t "~T~T~T> When you are in a mode you can go `back' to choose again (quit always works)~%")
  (format t "~T~T~T> In cleaning-mode you can wash a random item by typing `random'~%")
  (format t "~T~T~T> The laundry will be sent to a random waschi-server.~%")
  (format t "~%        [@][@]      /¯¯¯¯¯/|  _\\| _|<<<<|_  I'm your washi client.")
  (format t "~%        / \\/ \\     |¯¯¯¯¯|@|    \\ | *  * |  I will wash your socks")
  (format t "~%       [@][@][@]   | WSH |#|     \\|  <   |  better, and with love!")
  (format t "~%        \\ /\\ / \\~a~a~a|_____|/       |__~a~a__|  Trust me, because I am" "~" "~" "~" "~" "~")
  (format t "~%        [@][@]                      _||_    written in CommonLisp!~%")
  (format t "~%~T~TThis program comes with ABSOLUTELY NO WARRANTY; As This is free software~%")
  (format t "~T~Tyou are welcome to redistribute it under certain conditions; (AGPLv3+)")
  (format t "~T~T~%~%")
  (format t "~T~TThe sourcecode can be found at github <https://github.com/codepony/washi.lisp>~%")
  (format t "~T~TInformation about Waschi in general can be found at <http://waschi.meikodis.org>~%~%~%")
  (format t "Loading server-list (may take some time) ...~%~%")
  (get-server-list)
  (save-obj-list)
  (startup))

; start on load:
(first-init)

