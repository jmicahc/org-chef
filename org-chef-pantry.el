;;; org-chef-pantry.el --- Pantry feature for org-chef.    -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Calvin Beck

;; Author:  John Collins
;; URL: https://github.com/jmicahc/org-chef
;; Created: 2018

;; Copyright 2018 Calvin Beck

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Pantry feature for org-chef.

;;; Code:
(require 'org-element)
(require 'subr-x)

(defun org-chef-parse-pantry (pantry-headline)
  "Parse out item strings in plain-list element under PANTRY-HEADLINE element."
  (let ((ingredients '()))
    (org-element-map pantry-headline 'paragraph
      (lambda (item)
        (when-let (ingredient (caddr item))
          (setq ingredients (cons (string-trim (substring-no-properties ingredient))
                                  ingredients)))))
    ingredients))


(defun org-chef-read-pantry (org-tree)
  "Walks ORG-TREE, returning a list of parsed pantry regex strings."
  (org-element-map org-tree 'headline
    (lambda (item)
      item
      (when (equal (org-element-property :raw-value item) "Pantry")
        (org-chef-parse-pantry item)))
    nil
    t))


(defun org-chef-find-recipe-lists (org-tree)
  "Return a list of parsed recipe lists.

 Walks ORG-TREE looking for every list that is the child of
 a headline with a :recipe: tag."
  (let ((recipe-lists '()))
    (org-element-map org-tree 'headline
      (lambda (headline)
        (when (member "recipe" (org-element-property :tags headline))
          (when-let (lst (org-element-map headline 'plain-list 'identity nil t))
            (setq recipe-lists (cons lst recipe-lists))))))
    recipe-lists))


(defun org-chef-item-in-pantry-p (pantry item struct)
  "Return t if ITEM is in PANTRY."
  (when-let (pantry-ingredient (car pantry))
    (or (re-search-forward pantry-ingredient (org-list-get-item-end item struct) t)
        (org-chef-item-in-pantry-p (cdr pantry) item struct))))


(defun org-chef-checkmark-items-in-pantry (pantry)
  "Checkmark recipe items that match at least one of PANTRY items."
  (let* ((struct (org-list-struct))
         (struct-copy (copy-tree struct))
         (item (org-list-get-list-begin (org-list-get-item-begin)
                                        struct
                                        (org-list-prevs-alist struct)))
         (prevs (org-list-prevs-alist struct)))
    (while item
      (goto-char item)
      (if (org-chef-item-in-pantry-p pantry item struct)
          (org-list-set-checkbox item struct "[X]")
        (org-list-set-checkbox item struct "[ ]"))
      (setq item (org-list-get-next-item item struct prevs)))
    (org-list-write-struct struct (org-list-parents-alist struct) struct-copy)
    (org-update-checkbox-count)))


;;;###autoload
(defun org-chef-update-recipes ()
  "Checkmarks recipe items that are in the Pantry.

Reads ingredients listed in the Pantry headline and
checkmarks any recipe list item that matches at least one
ingredient in the pantry. An ingredient matches if
`re-search-foward' succeds using the pantry item as the
REGEXP."
  (interactive)
  (save-excursion
    (let* ((org-tree (org-element-parse-buffer))
           (pantry (org-chef-read-pantry org-tree))
           (recipe-lists (org-chef-find-recipe-lists org-tree)))
      (mapc (lambda (recipe-list)
              (goto-char (org-element-property :begin recipe-list))
              (org-chef-checkmark-items-in-pantry pantry))
            recipe-lists))))

;;; org-chef-pantry.el ends here
