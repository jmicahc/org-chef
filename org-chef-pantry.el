(defun parse-recipe-ingredient (ingredients-paragraph)
  (substring-no-properties (caddr ingredients-paragraph)))


(defun ingredient-in-pantry (pantry-ingredients recipe-ingredient)
  (when-let ((pantry-ingredient (car pantry-ingredients)))
    (if (string-match-p pantry-ingredient recipe-ingredient) t
      (ingredient-in-pantry (cdr pantry-ingredients) recipe-ingredient))))


(defun check-ingredients-list (pantry ingredients-headline)
  (org-element-map ingredients-headline 'item
    (lambda (item)
      (if (ingredient-in-pantry pantry (parse-recipe-ingredient
                                        (org-element-map item 'paragraph
                                          (lambda (item) item) nil t)))
          (org-element-put-property item :checkbox 'on)
        (org-element-put-property item :checkbox 'off)))
    nil
    t))


(defun checkbox-ingredients (pantry org-tree)
  (org-element-map org-tree 'headline
    (lambda (item)
      (when (member "recipe" (org-element-property :tags item))
        (check-ingredients-list pantry item))
      item)))


(defun parse-pantry-ingredient (paragraph-item)
  (caddr paragraph-item))


(defun parse-pantry (pantry-headline)
  (let ((ingredients '()))
    (org-element-map pantry-headline 'paragraph
      (lambda (item)
        (when-let ((ingredient (parse-pantry-ingredient item)))
          (setq ingredients (cons (string-trim (substring-no-properties ingredient))
                                  ingredients)))))
    ingredients))


(defun read-pantry (org-tree)
  (org-element-map org-tree 'headline
    (lambda (item) item
      (when (equal (org-element-property :raw-value item) "Pantry")
        (parse-pantry item)))
    nil
    t))


(defun find-recipe-lists (org-tree)
  (let ((recipe-lists '()))
    (org-element-map org-tree 'headline
      (lambda (headline)
        (when (member "recipe" (org-element-property :tags headline))
          (when-let (lst (org-element-map headline 'plain-list 'identity nil t))
            (setq recipe-lists (cons lst recipe-lists))))))
    recipe-lists))


;;;###autoload
(defun update-recipes ()
  (interactive)
  (let* ((org-tree (org-element-parse-buffer))
         (pantry (read-pantry org-tree)))
    (checkbox-ingredients pantry org-tree)
    (setf (buffer-string) "")
    (insert (org-element-interpret-data org-tree))
    (org-update-checkbox-count-maybe t)))




(with-current-buffer "recipes.org"
  (setq org-tree (org-element-parse-buffer))
  (setq recipe-lists (find-recipe-lists org-tree))
  (save-excursion
    (mapc (lambda (plain-list)
            (goto-char (org-element-property :begin plain-list))
            (update-checkbox))
          recipe-lists))
   ;; (setq pantry (read-pantry org-tree))
   ;; (checkbox-ingredients pantry org-tree)
   ;;(with-current-buffer "*scratch*"
   ;;  (setf (buffer-string) "")
   ;;  (insert (format "%s" (length recipe-lists)))
   ;;  ;; (elisp-format-buffer)
   ;;  ;;(org-update-checkbox-count-maybe t)
   ;;  )
   )

(let ((org-tree (org-element-parse-buffer))
      (recipe-lists (find-recipe-lists org-tree)))
  (mapc (lambda (plain-list)
          (goto-char (org-element-property :begin plain-list))
          (update-checkbox))
        recipe-lists))

(defun item-contains-ingredient-p (ingredient item struct)
  (re-search-forward ingredient (org-list-get-item-end item struct) t))


(defun item-in-pantry-p (pantry item struct)
  (when-let (pantry-ingredient (car pantry))
    (or (item-contains-ingredient-p pantry-ingredient item struct)
        (item-in-pantry-p (cdr pantry) item struct))))


;;;###autoload
(defun update-checkbox ()
  (interactive)
  (save-excursion
    (let* ((org-tree (org-element-parse-buffer))
           (pantry (read-pantry org-tree))
           (struct (org-list-struct))
           (struct-copy (copy-tree struct))
           (item (org-list-get-list-begin (org-list-get-item-begin)
                                          struct
                                          (org-list-prevs-alist struct)))
           (prevs (org-list-prevs-alist struct)))
      (while item
        (goto-char item)
        (if (item-in-pantry-p pantry item struct)
            (org-list-set-checkbox item struct "[X]")
          (org-list-set-checkbox item struct "[ ]"))
        (setq item (org-list-get-next-item item struct prevs)))
      (org-list-write-struct struct (org-list-parents-alist struct) struct-copy)
      (org-update-checkbox-count))))
