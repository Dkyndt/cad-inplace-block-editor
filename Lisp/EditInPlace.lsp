;;; =========================================================================
;;; EATT  - Interactive Block Attribute Grip Editor & Box Clearer
;;; CLATT - Standalone Quick Box-Clear for Block Attributes
;;; =========================================================================

(defun harvest-block-attributes ( blk / ent data ipt apt h72 v74 p-grip u-grip u10 u11 lst )
  (setq ent (entnext blk) lst nil)
  (while (and ent (/= (cdr (assoc 0 (entget ent))) "SEQEND"))
    (setq data (entget ent))
    (if (= (cdr (assoc 0 data)) "ATTRIB")
      (progn
        (setq ipt (cdr (assoc 10 data))
              apt (cdr (assoc 11 data))
              h72 (cond ((cdr (assoc 72 data))) (0))
              v74 (cond ((cdr (assoc 74 data))) (0)))

        ;; Determine true grip position
        (if (and apt (or (/= h72 0) (/= v74 0)) (/= apt '(0.0 0.0 0.0)))
          (setq p-grip apt)
          (setq p-grip ipt))

        ;; Convert coordinates to active UCS for accurate window checking
        (setq u-grip (trans p-grip ent 1)
              u10    (trans ipt ent 1)
              u11    (if (and apt (/= apt '(0.0 0.0 0.0))) (trans apt ent 1) nil))

        (setq lst (cons (list ent u-grip u10 u11 (cdr (assoc 2 data)) (cdr (assoc 1 data))) lst))
      )
    )
    (setq ent (entnext ent))
  )
  (reverse lst)
)

(defun clear-atts-in-window ( blk attList pt1 pt2 / x1 x2 y1 y2 cnt ent tag data val u-grip u10 u11 )
  (setq x1 (min (car pt1) (car pt2))
        x2 (max (car pt1) (car pt2))
        y1 (min (cadr pt1) (cadr pt2))
        y2 (max (cadr pt1) (cadr pt2))
        cnt 0)

  (defun in-bounds (pt)
    (if pt
      (and (>= (car pt) x1) (<= (car pt) x2)
           (>= (cadr pt) y1) (<= (cadr pt) y2))
      nil))

  (foreach item attList
    (setq ent    (nth 0 item)
          u-grip (nth 1 item)
          u10    (nth 2 item)
          u11    (nth 3 item)
          tag    (nth 4 item))
    ;; Catch the attribute if its grip, insertion, or alignment point is inside the box
    (if (or (in-bounds u-grip) (in-bounds u10) (in-bounds u11))
      (progn
        (setq data (entget ent))
        (if (/= (cdr (assoc 1 data)) "")
          (progn
            (setq data (subst (cons 1 "") (assoc 1 data) data)) ; Set value to empty text ""
            (entmod data)
            (entupd ent)
            (setq cnt (1+ cnt))
            (princ (strcat "\n  -> Emptied: [" tag "]"))
          )
        )
      )
    )
  )
  (entupd blk)
  (princ (strcat "\nTotal attributes emptied: " (itoa cnt)))
  cnt
)

;; Main Command: Edit by grip OR clear by rectangle
(defun c:EATT ( / ss blk attList inp pt pt1 pt2 min-dist closest d ent tag cur-val newval data loop-flag )
  (setvar "cmdecho" 0)

  ;; Check if block is pre-selected
  (setq ss (cadr (ssgetfirst)))
  (if (and ss (= (sslength ss) 1))
    (setq blk (ssname ss 0))
    (setq blk nil)
  )

  (if (or (null blk) (/= (cdr (assoc 0 (entget blk))) "INSERT"))
    (progn
      (setvar "cmdecho" 1)
      (setq blk (car (entsel "\nSelect block: ")))
      (setvar "cmdecho" 0)
    )
  )

  (if (and blk (= (cdr (assoc 0 (entget blk))) "INSERT"))
    (progn
      (sssetfirst nil (ssadd blk)) ; Show blue grip boxes
      (setq attList (harvest-block-attributes blk))

      (if (null attList)
        (princ "\nThis block contains no attributes.")
        (progn
          (setq loop-flag T)
          (while loop-flag
            (sssetfirst nil (ssadd blk))
            (initget "Clear Box Exit")
            (setq inp (getpoint "\nClick grip to edit, or [Clear-box/Exit] <Exit>: "))
            (cond
              ;; Mode: Drag a rectangle to clear
              ((or (= inp "Clear") (= inp "Box"))
                (setq pt1 (getpoint "\nSpecify first corner of rectangle to clear: "))
                (if pt1
                  (progn
                    (setq pt2 (getcorner pt1 "\nSpecify opposite corner: "))
                    (if pt2
                      (progn
                        (clear-atts-in-window blk attList pt1 pt2)
                        (setq attList (harvest-block-attributes blk))
                      )
                    )
                  )
                )
              )
              ;; Exit command
              ((or (null inp) (= inp "Exit"))
                (setq loop-flag nil)
              )
              ;; Mode: Click single grip to edit
              ((listp inp)
                (setq pt inp
                      min-dist 1e99
                      closest nil)
                (foreach item attList
                  (setq d (distance (list (car pt) (cadr pt))
                                    (list (car (nth 1 item)) (cadr (nth 1 item)))))
                  (if (< d min-dist)
                    (setq min-dist d
                          closest item))
                )
                (if closest
                  (progn
                    (setq ent     (nth 0 closest)
                          tag     (nth 4 closest)
                          data    (entget ent)
                          cur-val (cdr (assoc 1 data)))
                    (princ (strcat "\n>>> [Tag: " tag "]  Current Value: \"" cur-val "\""))
                    (setq newval (getstring T (strcat "\nEnter text for [" tag "] (or Space to blank): ")))
                    (setq data (subst (cons 1 newval) (assoc 1 data) data))
                    (entmod data)
                    (entupd ent)
                    (entupd blk)
                    (princ (strcat "\nSaved [" tag "] = \"" newval "\""))
                    (setq attList (harvest-block-attributes blk))
                  )
                )
              )
            )
          )
        )
      )
      (sssetfirst nil nil)
    )
    (princ "\nInvalid block selection.")
  )
  (setvar "cmdecho" 1)
  (princ)
)

;; Dedicated quick command to just drag a rectangle and wipe attributes
(defun c:CLATT ( / ss blk attList pt1 pt2 )
  (setvar "cmdecho" 0)
  (setq ss (cadr (ssgetfirst)))
  (if (and ss (= (sslength ss) 1))
    (setq blk (ssname ss 0))
    (setq blk nil)
  )
  (if (or (null blk) (/= (cdr (assoc 0 (entget blk))) "INSERT"))
    (progn
      (setvar "cmdecho" 1)
      (setq blk (car (entsel "\nSelect block: ")))
      (setvar "cmdecho" 0)
    )
  )
  (if (and blk (= (cdr (assoc 0 (entget blk))) "INSERT"))
    (progn
      (sssetfirst nil (ssadd blk))
      (setq attList (harvest-block-attributes blk))
      (if attList
        (progn
          (setq pt1 (getpoint "\nSpecify first corner of rectangle to clear: "))
          (if pt1
            (progn
              (setq pt2 (getcorner pt1 "\nSpecify opposite corner: "))
              (if pt2
                (clear-atts-in-window blk attList pt1 pt2)
              )
            )
          )
        )
        (princ "\nNo attributes found in this block.")
      )
      (sssetfirst nil nil)
    )
    (princ "\nInvalid block selection.")
  )
  (setvar "cmdecho" 1)
  (princ)
)