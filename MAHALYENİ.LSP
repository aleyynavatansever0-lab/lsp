(defun c:MAHALYAZ (/ sel obj alan alan_m2 yuvarlanmis_alan pt old_dimzin txt_str mahal mtext_obj)
  (vl-load-com)
  (if (setq sel (entsel "\nMetrekaresi hesaplanacak kapalı objeyi seçin: "))
    (progn
      (setq obj (vlax-ename->vla-object (car sel)))
      (if (vlax-property-available-p obj 'Area)
        (progn
          (setq mahal (getstring T "\nMahal adını girin (Örn: Yatak Odasi): "))

          (setq alan (vla-get-Area obj))
          (setq alan_m2 (/ alan 10000.0))
          (setq yuvarlanmis_alan (/ (float (fix (+ (* alan_m2 20.0) 0.5))) 20.0))

          (setq old_dimzin (getvar "DIMZIN"))
          (setvar "DIMZIN" 0)
          (setq txt_str (strcat (rtos yuvarlanmis_alan 2 2) " m2"))
          (setvar "DIMZIN" old_dimzin)

          (setq pt (getpoint "\nYazının tam ortalanacağı noktayı tıklayın: "))
          
          (if pt
            (progn
              (if (not (tblsearch "LAYER" "n-yazı"))
                (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord") '(2 . "n-yazı") '(70 . 0)))
              )

              (setq mtext_obj
                (entmakex (list
                            '(0 . "MTEXT")
                            '(100 . "AcDbEntity")
                            '(100 . "AcDbMText")
                            (cons 8 "n-yazı")
                            (cons 10 (trans pt 1 0)) ; DÜZELTME: UCS'den WCS'ye koordinat çevirisi
                            (cons 40 17.50)
                            '(71 . 5)
                            (cons 1 (strcat mahal "\\P" txt_str))
                          ))
              )
              (if mtext_obj
                (princ "\nBaşarılı: Yazı eklendi.")
                (princ "\nHata: Yazı oluşturulamadı (Katman kilitli olabilir).")
              )
            )
            (princ "\nİşlem iptal edildi: Nokta seçilmedi.")
          )
        )
        (princ "\nHata: Seçilen obje alan içermiyor.")
      )
    )
    (princ "\nİşlem iptal edildi: Obje seçilmedi.")
  )
  (princ)
)