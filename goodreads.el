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

;; see betterreads

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


(defun goodreads-get-user-id ()
  "Set `goodreads-my-id' with api."
  ;; TODO serialize this?
  (interactive)
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


(defun goodreads-search-books (query)
  "Using public key, search Goodreads for QUERY.

this basically asks the api for the xml result of searching for QUERY, then
cleans it up a bit and passes it to `goodreads-get-relevant-info'."
  (interactive "ssearch: ")

  (assert (not (equal goodreads-my-key 47))
          :string "`goodreads-my-key' not detected. make sure you setq it; see readme :)")

  (let (raw-search)
    (with-current-buffer
        (oauth-fetch-url
         goodreads-access-token
         (format "https://www.goodreads.com/search/index.xml?q=%s&key=%s"
                 (url-hexify-string query)
                 goodreads-my-key))  ; put output in temp buffer

      (goto-char (point-min))
      (setq raw-search (assoc 'search (assoc 'GoodreadsResponse (xml-parse-region (point)))))
      (goodreads-books nil  ; set book-list
                       (delete '("		 / 	" nil) (goodreads-get-relevant-info raw-search))
                       nil))))


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
        (setq id (nth 2 (assoc 'id (assoc 'best_book this-book))))  ;; the other id is the work id! doesn't work with add-to-shelf with work id.
        (setq rating-count (nth 2 (assoc 'ratings_count this-book)))
        (setq year (nth 2 (assoc 'original_publication_year this-book)))
        (setq rating (nth 2 (assoc 'average_rating this-book)))
        (setq title (nth 2 (assoc 'title (assoc 'best_book this-book))))
        (setq author (nth 2 (assoc 'name (assoc 'author (assoc 'best_book this-book)))))
        ;; add the list of attributes for THIS-BOOK onto RELEVANT-LIST
        (setq relevant-list (cons (list (concat title "\t" author "\t" rating " / " rating-count "\t" year) id) relevant-list))))
    (nreverse relevant-list)))  ; it's built in reverse


(defun goodreads-get-shelves ()
  "Return your shelves in the form of a list of tuples.

first element of the tuple is a string of the form 'shelf-name, book-count'.
second element is shelf id. requires `goodreads-my-secret' and `goodreads-my-id'
to be set."

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
      (setq id (string-to-number (nth 2 (assoc 'id this-shelf))))
      (setq name (nth 2 (assoc 'name this-shelf)))
      (setq book-count (nth 2 (assoc 'book_count this-shelf)))  ;; concat cries if this is a number so keeping it a string

      ;; (setq pretty-shelf-list (cons (list (concat name "\t" book-count " book(s)") id) pretty-shelf-list))
      ;; version with id as cdr
      (setq pretty-shelf-list (cons (list (concat name "\t" book-count " book(s)") name) pretty-shelf-list))))
    (nreverse pretty-shelf-list)))


(cl-defun goodreads-get-shelf-books (shelf-name &optional (sort "date_added"))
  "Return books on shelf SHELF-NAME in SORT order (optional; default 'date_added').

both parameters are strings.

you can see the possible values for SORT here: https://www.goodreads.com/api/index#reviews.list.
note that for the 'read' shelf, 'date_read' is appropriate, while for the
'to-read' shelf, 'date_added' is appropriate."

  ;; cl-defun bc i need default value of optional argument
  (interactive)
  (let ((total-reviews 1) (reviews-list nil) (this-page-reviews) (page-num 1))  ;; for whatever reason, goodreads calls books that are on shelves "reviews"
    (while (< (length reviews-list) total-reviews)
      (print (length reviews-list))
      ;; iterate through each page
      (with-current-buffer (oauth-fetch-url
                         goodreads-access-token
                         (format "https://www.goodreads.com/review/list.xml?v=2&id=%s&page=%s&shelf=%s&sort=%s&per_page=%s&key=%s"
                                 goodreads-my-id
                                 page-num
                                 shelf-name
                                 sort
                                 "200"  ;; per-page
                                 goodreads-my-secret))
                        (goto-char (point-min))
                        (forward-line 21)
                        (setq this-page-reviews (car (xml-parse-region (point)))))

      ;; minimal list with all the info in it
      (setq this-page-reviews (assoc 'reviews (nthcdr 7 this-page-reviews)))
      ; (print this-page-reviews)

      ;; pull out relevant info for each book on this page
      (let ((page-review-count (string-to-number (cdr (assoc 'end (nth 1 this-page-reviews))))))
        ; (print page-review-count)
        (dotimes (this-review-index page-review-count reviews-list)
          ; (print this-review-index)
          (let (id title author year rating rating-count
                   (this-review (assoc 'book (nth (* 2 this-review-index) (nthcdr 3 this-page-reviews)))))
            (setq id (nth 2 (assoc 'id this-review)))
            (setq title (nth 2 (assoc 'title_without_series this-review)))
            (setq year (nth 2 (assoc 'publication_year this-review)))
            (setq rating (nth 2 (assoc 'average_rating this-review)))
            (setq rating-count (nth 2 (assoc 'ratings_count this-review)))
            (setq author (nth 2 (assoc 'name (assoc 'author (assoc 'authors this-review)))))
            (setq reviews-list (cons (list (concat "'" title "'" ", " author " (" year "). " rating " / " rating-count)
                                           id) reviews-list)))))

      ;; set the total number of reviews if it's not already been set
      (if (not (eq total-reviews 1))
          (setq total-reviews
                (string-to-number (cdr (assoc 'total (nth 1 reviews-list))))))
      ;; go to the next page
      (1+ page-num))

    (print reviews-list)))


(defun goodreads-get-book-id-with-isbn (isbn)
  "Return book_id as a string given ISBN."
  (with-current-buffer (oauth-fetch-url
   goodreads-access-token
   (format "https://www.goodreads.com/book/isbn_to_id?key=%s&isbn=%s"
           goodreads-my-secret
           isbn))
  (goto-char (point-min))
  (forward-line 21)
  (current-word)))


(defun goodreads-get-work-id (book-id)
  "Return work_id given BOOK-ID.

not sure if i need this though"
  (with-current-buffer
      (oauth-fetch-url
       goodreads-access-token
       (format "https://www.goodreads.com/book/id_to_work_id?key=%s&id=%s"
               goodreads-my-secret
               book-id))
    (goto-char (point-min))
    (forward-line 21)
    (nth 2 (assoc 'item (assoc 'work-ids (car (xml-parse-region (point))))))))


(defun goodreads-add-to-shelf (book shelf-name)
  "Given BOOK, add it to SHELF-NAME (string).

if you try to add a book from one exclusive shelf to another, it'll delete the
book from the first.

BOOK is an element of the list produced by `goodreads-search-books' or
`goodreads-get-shelf-books'.

TODO if REMOVE is set to 'remove', then the book is removed from the shelf."

;; huh interestingly if shelf-name doesn't exist then the api creates it

  (let ((args
         ;; backtick and commas allow quoted lists with stuff evaluated inside
         `(("shelves" . ,shelf-name)
           ;; nth 1 in order to get id
           ("bookids" . ,(nth 1 book)))))
    (oauth-post-url
     goodreads-access-token
     ;; ok. so this api is complete trash. the reason i'm using this method,
     ;; instead of the singular /shelf/add_to_shelf.xml method, is because
     ;; that one for some reason doesn't work! with the same exact arguments!
     ;; i guess it's now a "feature" that this function can take a comma
     ;; separated list of book-ids and shelf-names... 🤢🤢
     "https://www.goodreads.com/shelf/add_books_to_shelves.xml"
     args)))

(defun goodreads-add-review (book-id shelf-name &optional date rating review-text)
  "Wrapper for API's review.create method.

probably want to call if shelf-name = read, and use add-to-shelf otherwise."

  (if (and (eq date nil) (eq shelf-name "read"))
    ;; if date doesn't exist and shelf is "read", then make it today by default
    (setq date (format-time-string "%Y-%m-%d")))
  (let* ((args
         `(("book_id" . ,book-id)
           ;("review[review]" . ,review-text)
           ;("review[rating]" . ,rating)
           ;("review[read_at]" . ,date)
           ;("finished" . "true")  ;; TODO only makes sense for read shelf
           ("shelf" . ,shelf-name)
           )))
    ;; TODO works now, but seems to break when adding in optional variables.
    ;; some options: set default values of optional vars to "" (not sure if
    ;; this will work); only initialize optional vars when they're passed
    ;; (not sure how to implement)
    ;; (if (stringp review-text)
    ;;     (setq args (append args `(("review" . ,review-text)))))
    ;; (if (stringp rating)
    ;;     (setq args (append args `(("rating" . ,rating)))))
    (if (stringp date)  ; if the date exists
        (setq args (append args `(("review[read_at]" . ,date)))))
    (print args)

  (oauth-post-url
   goodreads-access-token
   "https://www.goodreads.com/review.xml"
   args)
  )
  )

;;;; completion things

(defun goodreads-books (&optional shelf-name books-list search-string)
  "Search for books. you have to specify only one argument.

not meant to be used by end user.

if SHELF-NAME, look at the books in that shelf

if SEARCH-STRING, look at the books returend by that search

TODO: searching SEARCH-STRING on BOOKS-LIST"

  (cond ((and shelf-name (not books-list) (not search-string))
         (goodreads-books nil (goodreads-get-shelf-books shelf-name) nil))
        ((and search-string (not shelf-name) (not books-list))
         (goodreads-books nil (goodreads-search-books search-string) nil))
        ((and books-list (not shelf-name) (not search-string))
         (goodreads-book-action
          ;; TODO maintain order of list
          (assoc (completing-read "select a book: " books-list) books-list)))
        ('t (message "have to specify (only) one argument"))
        )
  )

(defun goodreads-book-action (book)
  "Helper function for completion actions on BOOK from `goodreads-books'.

like in `goodreads-add-to-shelf', BOOK is an element of the list returned by
`goodreads-search-books' or `goodreads-get-shelf-books'."

;; TODO add "edit" action, calling "edit a review"

;; i would use ivy's multi actions but it seems not to be a feature of
;; `completing-read' so i won't to maintain compatibility with non-ivy frameworks

  (let* ((possible-actions '("Move to shelf"
                             "Rate"
                             "View on goodreads.com"))
         ;; TODO make sure the list above maintains order in completing-read
         (chosen-action (completing-read "Choose an action: " possible-actions)))
    (print chosen-action)
    (if (equal chosen-action "Move to shelf")
        (let* ((shelf-names (goodreads-get-shelves))  ;; ugh i'm repeating this
               (shelf-name (cadr (assoc (completing-read "Select a shelf: "
                                                         shelf-names) shelf-names))))
          (goodreads-add-to-shelf book shelf-name)
          (print (format "Added book %s to shelf %s"
                         (car book)
                         shelf-name)))
    ;; TODO: write helper function for rate
    ;; TODO: write helper function for view
      )
    (cdr book)
  )
)

(defun goodreads-shelves ()
  "Select shelf to view.

basically a wrapper for `completing-read'"
  (interactive)
  (let* ((shelf-names (goodreads-get-shelves))
         ;; get name of selected shelf via completing-read
         ;; the next two let calls are an inelegant way of getting just the
         ;; name of the shelf
         (shelf-name (cadr (assoc (completing-read "Select a shelf: "
                                                         shelf-names) shelf-names))))

    (goodreads-books shelf-name nil nil)
    )
  )


(provide 'goodreads)
;;; goodreads.el ends here
