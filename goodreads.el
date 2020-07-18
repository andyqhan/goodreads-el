;;; goodreads.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Andy Han
;;
;; Author: Andy Han <http://github/ibynx>
;; Maintainer: Andy Han
;; Created: July 04, 2020
;; Modified: July 04, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage:
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(require 'ivy)
(require 'cl)  ;; for assertions
(require 'oauth)
;; the thing in eval-when-compile wasn't loaded, which meant that oauth-nonce-function
;; was not activated to sasl-unique-id

(defcustom goodreads-my-key "CRkTPII6roYd9yGS9iTkag"
  "Your public goodreads key.")  ; set these to 47 when I'm done
(defcustom goodreads-my-secret "6Un4YARg5MWpW1gcQYGwi1ticXKkRECpZk6JXuzM"
  "Your secret goodreads key.")
(defvar goodreads-my-id 47
  "Your user_id.")

;; lmao remember when you spent like an hour debugging ouauth-authorize when you just forgot
;; to put www. in these urls
(defvar goodreads-req-url "https://www.goodreads.com/oauth/request_token")
(defvar goodreads-acc-url "https://www.goodreads.com/oauth/access_token")
(defvar goodreads-auth-url "https://www.goodreads.com/oauth/authorize")

(defcustom goodreads-token-location "~/.goodreads-el-token"
  "Location for the file containing your unique access token.
defaults to ~/.goodreads-el-token. windows users probably have to change it.")
(defvar goodreads-access-token nil)

(defvar test-query "ashley+knots+the")

;; TODO translate human input into the HTML-readable string


(defun goodreads-oauth-authorize ()
  "Use oauth.el to authorize this app so it can write to your goodreads.

remember to set `goodreads-my-secret' and `goodreads-my-key' first. when oauth
tells you to enter the access code, enter the code in the url that goodreads
redirects you to.

the first time you authorize, it'll create a file at `goodreads-token-location'
containing your token. on subsequent runs of this function, it'll read from
that file so you won't have to reauthenticate."
  (interactive)
  (if (or (equal 47 goodreads-my-key) (equal 47 goodreads-my-secret))
      ;; condition when you haven't set your keys
      (print "don't forget to set `goodreads-my-key' and `goodreads-my-secret'!")
    ;; stole much of token serializing from yammer.el
    (if (file-exists-p goodreads-token-location)
        ;; condition when you don't have a token file
        (progn
          (save-excursion
            (find-file goodreads-token-location)
            (let ((str (buffer-substring (point-min) (point-max))))
              (if (string-match "\\([^:]*\\):\\(.*\\)"
                                (buffer-substring (point-min) (point-max)))
                  ;; set `goodreads-access-token' from an oauth-access-token object
                  (setq goodreads-access-token
                        (make-oauth-access-token
                         :consumer-key goodreads-my-key
                         :consumer-secret goodreads-my-secret
                         :auth-t (make-oauth-t
                                  :token (match-string 1 str)
                                  :token-secret (match-string 2 str))))))
            (save-buffer)
            (kill-this-buffer))))
    (unless goodreads-access-token
      ;; condition when you don't have a token file
        (setq goodreads-access-token
              (oauth-authorize-app goodreads-my-key goodreads-my-secret
                                   goodreads-req-url goodreads-acc-url
                                   goodreads-auth-url))
        (save-excursion
          (find-file goodreads-token-location)
          (end-of-buffer)
          (let ((token (oauth-access-token-auth-t goodreads-access-token)))
            (insert (format "%s:%s\n"
                            (oauth-t-token token)
                            (oauth-t-token-secret token))))
          (save-buffer)
          (kill-this-buffer)))
      goodreads-access-token))


(defun goodreads-search-books (query)
  "Using public key, search Goodreads for QUERY.

this basically asks the api for the result of searching for QUERY, then
passes that to `goodreads-get-relevant-info'."
  (assert (not (equal goodreads-my-key 47))
          :string "`goodreads-my-key' not detected. make sure you setq it; see readme :)")

  (let (raw-search)
  (with-temp-buffer
    (shell-command (format "wget -qO- 'https://www.goodreads.com/search/index.xml?key=%s&q=%s'"
                           goodreads-my-key
                           query) t)  ; put output in temp buffer
    (goto-char (point-min))
    (setq raw-search (assoc 'search (assoc 'GoodreadsResponse (xml-parse-region (point)))))
    (goodreads-get-relevant-info raw-search))))


(defun goodreads-get-relevant-info (books)
  "Helper function that pulls out the important information out of BOOKS.

Returns a list of lists in the same order as the API gives. Use
with the output of goodreads-search-books. does not require
authentication"
  (let ((book-count (string-to-number (car (nthcdr 2 (assoc 'total-results books)))))  ; total number of books
        (results (nthcdr 3 (assoc 'results books)))  ; most-reduced list that still has all the books
        (relevant-list))  ; i use this variable later

    (dotimes (book-index book-count relevant-list)  ; iterate over every book in BOOKS
    ; not using dolist because RESULTS has weird quotation marks and shit everywhere
      (let (id title author rating rating-count year
               (this-book (assoc 'work (nthcdr book-index results))))
        (setq id (nth 2 (assoc 'id this-book)))  ; might have to use the one inside best_book. use `string-to-number'?
        (setq rating-count (nth 2 (assoc 'ratings_count this-book)))
        (setq year (nth 2 (assoc 'original_publication_year this-book)))
        (setq rating (nth 2 (assoc 'average_rating this-book)))
        (setq title (nth 2 (assoc 'title (assoc 'best_book this-book))))
        (setq author (nth 2 (assoc 'name (assoc 'author (assoc 'best_book this-book)))))
        ;; add the list of attributes for THIS-BOOK onto RELEVANT-LIST
        (setq relevant-list (cons (list (concat title "\t" author "\t" rating "/" rating-count "\t" year) id) relevant-list))))
    (nreverse relevant-list)))  ; it's built in reverse


(defun goodreads-get-user-id ()
  "Set `goodreads-my-id' with api."
  (assert goodreads-access-token
    :string "`goodreads-access-token' not detected. make sure to run `goodreads-oauth-authorize' first :)")
  (let (user-list)
    (with-current-buffer (oauth-fetch-url
                          goodreads-access-token
                          "https://www.goodreads.com/api/auth_user")
      ;; `oauth-fetch-url' returns buffer containing GET thingie
      (goto-char (point-min))
      (forward-line 21)  ;; idk if this is the best way to do this.
      ;; worried that header won't always be 21 lines
      (setq user-list (assoc 'user (assoc 'GoodreadsResponse (xml-parse-region (point))))))
    (setq goodreads-my-id (string-to-number (cdr (nth 0 (nth 1 user-list)))))))


(defun goodreads-get-shelves ()
  "Return your shelves.

requires `goodreads-my-secret' and `goodreads-my-id' to be set."

  (assert (not (equal goodreads-my-secret 47))
          :string "`goodreads-my-secret' not detected. make sure to register with the api first :)")
  (assert (not (equal goodreads-my-id 47))
          :string "`goodreads-my-id' not detected. make sure to run `goodreads-get-user-id' first :)")
  (let (shelves-list shelf-count pretty-shelf-list)
    (with-current-buffer (oauth-fetch-url
                          goodreads-access-token  ;; not asserting bc it's implied in my-id
                          (format "https://www.goodreads.com/shelf/list.xml?key=%s&user_id=%s"
                                  goodreads-my-secret
                                  goodreads-my-id))
      (goto-char (point-min))
      (forward-line 21)
      (setq shelves-list (assoc 'shelves (assoc 'GoodreadsResponse (xml-parse-region (point))))))
    (setq shelf-count (string-to-number (cdr (nth 2 (nth 1 shelves-list)))))

    (dotimes (shelf-index shelf-count pretty-shelf-list)
      (let (id name book-count this-shelf
               (this-shelf
                (assoc 'user_shelf (nthcdr (* 2 shelf-index) (nthcdr 3 shelves-list)))))
      (print shelf-index)
      (print this-shelf)
      (setq id (string-to-number (nth 2 (assoc 'id this-shelf))))
      (setq name (nth 2 (assoc 'name this-shelf)))
      (setq book-count (nth 2 (assoc 'book_count this-shelf)))  ;; concat cries if this is a number so keeping it a string

      (setq pretty-shelf-list (cons (list (concat name ", " book-count " book(s)") id) pretty-shelf-list))))
    (nreverse pretty-shelf-list)
  ))



;;;; ivy things

(defun goodreads-ivy-action (id)
  "Helper function for `goodreads-ivy-read'. Prints ID (for now;
later add it it shelf or something)."
  (print id)
  )

(defun goodreads-ivy-read (books-list)
  "Wrapper function that passes BOOKS-LIST to `ivy-read'."
  (ivy-read "Choose book: "
            books-list
            :require-match t
            :action (lambda (book) (goodreads-ivy-action (cdr book)))))



;; DONE integrate ivy
;; DONE write OAuth function
;; DONE test if i can access OAuth things with my token
;; DONE goodreads-get-user-id
;; TODO add on-a-shelf to candidate features
;; TODO write functions that move books from shelf to shelf. prolly have to write helper functions to get the list of shelves of user.

(provide 'goodreads)
;;; goodreads.el ends here
