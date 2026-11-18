;;; PAYLASIM HESAPLAMA - AutoCAD LT 2027
;;; Komutlar: YUKLENICI / ARSASAHIBI
;;; Secilen TEXT, MTEXT ve BLOK (INSERT) nesnelerinin adedini sayar.
;;; Her grup icin birim m2 degeri ister ve islemi MTEXT olarak yazar.
;;; Cikti layer'i: N-YAZI | Yazi yuksekligi: 85

(vl-load-com)

(defun p:fmt (n / olddz str)
  ;; Sondaki sifirlari gizlemeyi kapat, ondalik ayiracini noktaya sabitle ve 2 basamak goster.
  (setq olddz (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)
  (setq str (vl-string-subst "." "," (rtos n 2 2)))
  (setvar "DIMZIN" olddz)
  str
)

(defun p:ensure-layer (lay /)
  (if (not (tblsearch "LAYER" lay))
    (entmake
      (list
        '(0 . "LAYER")
        '(100 . "AcDbSymbolTableRecord")
        '(100 . "AcDbLayerTableRecord")
        (cons 2 lay)
        '(70 . 0)
        '(62 . 7)
        '(6 . "Continuous")
      )
    )
  )
)

(defun p:run (cmd-label out-label / *error* oldecho groups ss cnt val subtotal
                                    terms term-str idx body continue answer pt
                                    result cmd-result oldos)
  (setq oldecho (getvar "CMDECHO"))
  (setq oldos (getvar "OSMODE"))

  (defun *error* (msg)
    (setvar "CMDECHO" oldecho)
    (setvar "OSMODE" oldos)
    (if (and msg
             (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*")))
      (princ (strcat "\nHata: " msg))
    )
    (princ)
  )

  (setvar "CMDECHO" 0)
  (setq groups 0
        subtotal 0.0
        terms '()
        continue T)

  (princ (strcat "\n--- " cmd-label " PAYLASIM HESABI ---"))
  (princ "\nHer grup icin TEXT, MTEXT veya BLOK nesnelerini secin ve Enter'a basin.")
  (princ "\nSecim penceresi yerine nesnelerin uzerine tiklayarak da secebilirsiniz.")

  (while continue
    (if (= groups 0)
      (princ (strcat "\n\n" cmd-label " m2'sini secin: "))
      (princ (strcat "\n\n" (itoa (1+ groups)) ". grup - " cmd-label " m2'sini secin: "))
    )
    (setq ss (ssget '((0 . "TEXT,MTEXT,INSERT"))))

    (if ss
      (progn
        (setq cnt (sslength ss))
        (setq val (getreal
          (strcat "\nBu gruptaki her birinin emsal alani (m2): ")))

        (if (and val (> val 0.0))
          (progn
            (setq groups (1+ groups))
            (setq subtotal (+ subtotal (* cnt val)))
            (setq term-str
              (if (= cnt 1)
                (p:fmt val)
                (strcat "(" (p:fmt val) " x " (itoa cnt) ")")
              )
            )
            (setq terms (append terms (list term-str)))
            (princ
              (strcat "\nGrup eklendi: "
                      (itoa cnt) " adet x "
                      (p:fmt val) " = "
                      (p:fmt (* cnt val)) " m2"))
            (initget "Evet Hayir")
            (setq answer
              (getkword "\nBaska bir grup eklemek istiyor musunuz? [Evet/Hayir] <Hayir>: "))
            (if (= answer "Evet")
              (setq continue T)
              (setq continue nil)
            )
          )
          (progn
            (princ "\nGecersiz deger. Bu grup eklenmedi.")
            (initget "Evet Hayir")
            (setq answer
              (getkword "\nTekrar grup secmek istiyor musunuz? [Evet/Hayir] <Hayir>: "))
            (if (= answer "Evet")
              (setq continue T)
              (setq continue nil)
            )
          )
        )
      )
      (setq continue nil)
    )
  )

  (if (> groups 0)
    (progn
      ;; Ilk 3 grubu ayni satira yaz, 4. gruptan itibaren alt satira gec
      (setq idx 0
            body "")
      (foreach item terms
        (cond
          ((= idx 0)
           (setq body item))
          ((= (rem idx 3) 0)
           (setq body (strcat body "\\P+ " item)))
          (T
           (setq body (strcat body " + " item)))
        )
        (setq idx (1+ idx))
      )

      ;; Sonucu esittirle birlikte ayri bir alt satira yaz
      (setq result
        (strcat out-label " = " body "\\P= " (p:fmt subtotal) " m2")
      )

      (setq cmd-result result)
      (while (vl-string-search "\\P" cmd-result)
        (setq cmd-result (vl-string-subst " " "\\P" cmd-result))
      )
      (princ (strcat "\n\nHESAP: " cmd-result))

      (setq pt (getpoint "\nSonucun yazilacagi noktayi secin: "))
      (if pt
        (progn
          (p:ensure-layer "N-YAZI")
          (if
            (entmake
              (list
                '(0 . "MTEXT")
                '(100 . "AcDbEntity")
                '(100 . "AcDbMText")
                (cons 8 "N-YAZI")
                (cons 10 (trans pt 1 0))
                (cons 11 (trans '(1.0 0.0 0.0) 1 0 T))
                (cons 40 85.0)
                '(41 . 0.0)
                '(71 . 1)
                '(72 . 5)
                (cons 1 result)
                (cons 7 (getvar "TEXTSTYLE"))
              )
            )
            (princ "\nSonuc N-YAZI layer'ina yerlestirildi.")
            (princ "\nYazi olusturulamadi.")
          )
        )
        (princ "\nNokta secilmedi; hesap komut satirinda birakildi.")
      )
    )
    (princ "\nHesaplama icin grup secilmedi.")
  )

  (setvar "CMDECHO" oldecho)
  (setvar "OSMODE" oldos)
  (princ)
)

(defun c:YUKLENICI ()
  (p:run "YUKLENICI" "YÜKLENİCİ")
)

(defun c:ARSASAHIBI ()
  (p:run "ARSA SAHIBI" "ARSA SAHİBİ")
)

(princ "\nPaylasim hesaplama LISP'i yuklendi. Komutlar: YUKLENICI, ARSASAHIBI")
(princ)