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

(defvar goodreads-my-key "CRkTPII6roYd9yGS9iTkag")
(defvar goodreads-my-secret "6Un4YARg5MWpW1gcQYGwi1ticXKkRECpZk6JXuzM")
(defvar test-query "ashley+knots+the")
;; TODO translate human input into the HTML-readable string

(defun goodreads-search-books (key query)
  "Using KEY, search Goodreads for QUERY. Currently outputs int, needs to be string."
  (with-temp-buffer
  (shell-command (format "wget -qO- 'https://www.goodreads.com/search/index.xml?key=%S&q=%S'"
                         key
                         query) t)  ; put output in temp buffer
    (goto-char (point-min))
    ;(forward-line 4)  ; <GoodreadsResponse> is on line 4
    (assoc 'search (assoc 'GoodreadsResponse (xml-parse-region (point))))))


(defun goodreads-get-relevant-info (books)
  "Helper function that pulls out the important information out of BOOKS.
Returns a list of lists in the same order as the API gives. Use with the output of goodreads-search-books."
  (let ((book-count (string-to-number (car (nthcdr 2 (assoc 'total-results books)))))  ; total number of books
        (results (nthcdr 3 (assoc 'results books)))  ; most-reduced list that still has all the books
        (relevant-list))  ; i use this variable later

    (dotimes (book-index book-count relevant-list)  ; iterate over every book in BOOKS
    ; not using dolist because RESULTS has weird quotation marks and shit everywhere
      (let (id title author rating rating-count year
               (this-book (assoc 'work (nthcdr book-index results))))
        (setq id (nth 2 (assoc 'id this-book)))  ; might have to use the one inside best_book
        (setq rating-count (nth 2 (assoc 'ratings_count this-book)))
        (setq year (nth 2 (assoc 'original_publication_year this-book)))
        (setq rating (nth 2 (assoc 'average_rating this-book)))
        (setq title (nth 2 (assoc 'title (assoc 'best_book this-book))))
        (setq author (nth 2 (assoc 'name (assoc 'author (assoc 'best_book this-book)))))
        ;; add the list of attributes for THIS-BOOK onto RELEVANT-LIST
        (setq relevant-list (cons (list id title author rating rating-count year) relevant-list))))
    (nreverse relevant-list)))  ; it's built in reverse

;; TODO integrate counsel
;; TODO write functions that move books from shelf to shelf. prolly have to write helper functions to get the list of shelves of user.

(provide 'goodreads)
;;; goodreads.el ends here
