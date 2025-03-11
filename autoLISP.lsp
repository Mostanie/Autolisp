(setq points '())

(defun c:TabloOlustur ()
  (if (and (boundp 'points) (not (null points)))
    (progn
      (setq insPt (getpoint "\nTablonun yerlestirilecegi noktayi secin: "))
      (if insPt
        (progn
          (setq *doc* (vla-get-ActiveDocument (vlax-get-Acad-Object)))
          (setq *ms* (vla-get-ModelSpace *doc*))

          (setq rowCount (+ (length points) 1))
          (setq colCount 3)

          (setq tablo (vla-AddTable *ms* (vlax-3d-point insPt) rowCount colCount 12.0 70.0))

          (vla-put-StyleName tablo "Standard")
          (vla-SetColumnWidth tablo 0 35.0)
          (vla-SetColumnWidth tablo 1 80.0)
          (vla-SetColumnWidth tablo 2 80.0)

          (vla-UnmergeCells tablo 0 0 0 2)

          (vla-SetCellValue tablo 0 0 "No")
          (vla-SetCellValue tablo 0 1 "E (X)")
          (vla-SetCellValue tablo 0 2 "N (Y)")

          (setq i 1)
          (foreach pt points
            (vla-SetCellValue tablo i 0 (itoa i))
            (vla-SetCellValue tablo i 1 (rtos (car pt) 2 3))
            (vla-SetCellValue tablo i 2 (rtos (cadr pt) 2 3))
            (setq i (1+ i))
          )

          (vla-Regen *doc* acActiveViewport)
          (princ "\nTablo basariyla olusturuldu!")
        )
        (princ "\nTablo yerlestirme noktasi secilmedi.")
      )
    )
    (princ "\nHata: Noktalar bulunamadi! Lutfen once noktalari ekleyin (c:selectPoints).")
  )
  (princ)
)



(defun c:selectPoints ()
  (setq points '())
  (setq counter 1)
  (setvar "PDSIZE" 10)
  (setvar "PDMODE" 34)

  (princ "\nNokta secmeye baslayin. Secimi bitirmek icin ENTER'a basin.\n")

  (while (setq p (getpoint (strcat (itoa counter) ". Noktayi secin (ENTER ile sonlandirin): \n")))

    (setq px (car p))
    (setq py (cadr p))

    (setq tempPoint (entmakex (list (cons 0 "POINT") (cons 10 p))))

    (setq zoomFactor (getvar "ZOOMFACTOR"))

    (if (< zoomFactor 1.5)
      (command "zoom" "extents")
      (command "zoom" "window" (list (- px 125) (- py 125)) (list (+ px 125) (+ py 125)))
    )

    (setq p2 (getpoint "\nBir yon secin (mouse ile tiklayin): "))

    (if p2
      (progn
        (setq dx (- (car p2) (car p)))
        (setq dy (- (cadr p2) (cadr p)))

        (cond
          ((and (> dx 0) (> dy 0)) (setq direction 1))
          ((and (> dx 0) (< dy 0)) (setq direction 2)) 
          ((and (< dx 0) (> dy 0)) (setq direction 3)) 
          ((and (< dx 0) (< dy 0)) (setq direction 4)) 
          (t (princ "\nGecersiz secim."))
        )
	(if tempPoint (entdel tempPoint))
      )
    )

    (setq end1 (list (+ px 30.0) (+ py 30.0)))
    (setq end2 (list (+ (car end1) 75.0) (cadr end1)))
    (setq xVal (car end1))  
    (setq offset 8.0)

    (cond
      ((= direction 2)
       (setq end1 (list (+ px 30.0) (- py 30.0)))
       (setq end2 (list (+ (car end1) 75.0) (cadr end1))))

      ((= direction 3)
       (setq end1 (list (- px 30.0) (+ py 30.0)))
       (setq end2 (list (- (car end1) 75.0) (cadr end1)))
       (setq xVal (car end2))
       (setq offset (* offset -1)))

      ((= direction 4)
       (setq end1 (list (- px 30.0) (- py 30.0)))
       (setq end2 (list (- (car end1) 75.0) (cadr end1)))
       (setq xVal (car end2))
       (setq offset (* offset -1)))
    )

    (command "LINE" p end1 "")  
    (command "LINE" end1 end2 "")

    (setq xText (strcat "E: " (rtos px 2 2)))
    (setq xTextPos (list (+ xVal 3.0) (+ (cadr end1) 10.0)))  
    (command "TEXT" "J" "L" xTextPos 8 0 xText)

    (setq yText (strcat "N: " (rtos py 2 2)))
    (setq yTextPos (list (+ xVal 3.0) (- (cadr end1) 20.0)))  
    (command "TEXT" "J" "L" yTextPos 8 0 yText)

    (setq circleCenter (list (+ (car end2) offset) (cadr end2)))
    (setq circleRadius 8.0)
    (command "CIRCLE" circleCenter circleRadius)

    (setq numText (itoa counter))
    (setq numTextPos circleCenter)
    (command "TEXT" "J" "MC" numTextPos 8 0 numText)

    (setq points (append points (list (list px py))))  
    (setq counter (1+ counter))  
  )

  (princ "\nSecim islemi tamamlandi.\n")
  (princ)
)