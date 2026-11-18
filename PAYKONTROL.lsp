(defun c:PAYKONTROL (/ arsa yuk toplam emsal fark sonuc pt)

  (setq arsa (getreal "\nArsa sahibi toplam m2: "))
  (if arsa
    (progn
      (setq yuk (getreal "\nYuklenici toplam m2: "))
      (if yuk
        (progn
          (setq emsal (getreal "\nToplam emsal m2: "))
          (if emsal
            (progn
              (setq toplam (+ arsa yuk))
              (setq fark (- emsal toplam))

              (setq sonuc
                (strcat
                  "ARSA SAHIBI: " (rtos arsa 2 2) " m2"
                  " | YUKLENICI: " (rtos yuk 2 2) " m2"
                  " | TOPLAM: " (rtos toplam 2 2) " m2"
                  " | EMSAL: " (rtos emsal 2 2) " m2"
                  " | FARK: " (rtos fark 2 2) " m2"
                )
              )

              (princ (strcat "\n\n" sonuc))

              (if (< (abs fark) 0.005)
                (princ "\nDURUM: TOPLAM EMSAL ILE ESIT.")
                (princ "\nDURUM: FARK VAR, KONTROL EDIN.")
              )

              (setq pt (getpoint "\nSonucu yerlestirmek icin nokta secin: "))

              (if pt
                (progn
                  (if (not (tblsearch "LAYER" "N-YAZI"))
                    (command "_.-LAYER" "_M" "N-YAZI" "")
                  )
                  (entmake
                    (list
                      '(0 . "TEXT")
                      '(100 . "AcDbEntity")
                      '(100 . "AcDbText")
                      '(8 . "N-YAZI")
                      (cons 10 pt)
                      '(40 . 85.0)
                      (cons 1 sonuc)
                      '(50 . 0.0)
                      (cons 7 (getvar "TEXTSTYLE"))
                      '(72 . 0)
                      '(73 . 0)
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  (princ)
)