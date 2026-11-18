(defun po:find-number (s / pos i sub len idx code found)
  (setq pos nil i 1)
  (if (and s (= (type s) 'STR))
    (progn
      ;; Once "=" isaretinden sonrasini ara, yoksa ":" isaretinden sonrasini ara
      (while (setq i (vl-string-search "=" s i))
        (setq pos i
              i (+ i 1))
      )
      (if (not pos)
        (progn
          (setq i 1)
          (while (setq i (vl-string-search ":" s i))
            (setq pos i
                  i (+ i 1))
          )
        )
      )
      (if pos
        (setq sub (substr s (+ pos 2)))
        (setq sub s)
      )
      ;; Metin icindeki ilk rakamin basladigi yeri bul
      (setq len (strlen sub)
            idx 1
            found nil)
      (while (and (<= idx len) (not found))
        (setq code (ascii (substr sub idx 1)))
        (if (and (>= code 48) (<= code 57))
          (setq found idx)
          (setq idx (1+ idx))
        )
      )
      (if found
        (atof (substr sub found))
        nil
      )
    )
  )
)

(defun po:fmt (n / olddz str)
  (setq olddz (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)
  (setq str (vl-string-subst "." "," (rtos n 2 2)))
  (setvar "DIMZIN" olddz)
  str
)

(defun po:make-mtext (lay pt txt)
  (entmake
    (list
      '(0 . "MTEXT")
      '(100 . "AcDbEntity")
      '(100 . "AcDbMText")
      (cons 8 lay)
      (cons 10 (trans pt 1 0))
      (cons 11 (trans '(1.0 0.0 0.0) 1 0 T))
      (cons 40 100.0)
      '(71 . 1)
      (cons 1 txt)
      (cons 7 (getvar "TEXTSTYLE"))
    )
  )
)

(defun po:pick-text (promptMsg / oldOs pt ss ent loop curPrompt)
  ;; Yazi secerken Osnap'in baska cizgiye kaymasini gecici olarak kapat
  (setq oldOs (getvar "OSMODE"))
  (setvar "OSMODE" 0)

  (setq loop T
        ent nil
        curPrompt promptMsg)

  (while loop
    ;; getpoint komutu imlec yaninda AutoCAD'in gri bilgi kutusunu acar
    (setq pt (getpoint curPrompt))
    (if pt
      (progn
        (setq ss (ssget pt '((0 . "TEXT,MTEXT"))))
        (if ss
          (progn
            (setq ent (ssname ss 0))
            (setq loop nil)
          )
          (setq curPrompt "\nMetin bulunamadı! Yazının üzerine tıklayın: ")
        )
      )
      (setq loop nil)
    )
  )

  (setvar "OSMODE" oldOs)
  ent
)

(defun c:OraniYaz ( / *error* oldDynMode oldDynPrompt oldDynPivis oldTooltips oldOs
                      ent1 txt1 valYuklenici ent2 txt2 valArsa toplam
                      oranYuklenici oranArsa strYuklenici strArsa pt1 pt2 obj layName)
  (vl-load-com)

  ;; Mevcut ayarlari sakla ve imlec yanindaki gri kutuyu aktif et
  (setq oldDynMode (getvar "DYNMODE"))
  (setq oldDynPrompt (getvar "DYNPROMPT"))
  (setq oldDynPivis (getvar "DYNPIVIS"))
  (setq oldTooltips (getvar "TOOLTIPS"))
  (setq oldOs (getvar "OSMODE"))

  (if oldDynMode (setvar "DYNMODE" 3))
  (if oldDynPrompt (setvar "DYNPROMPT" 1))
  (if oldDynPivis (setvar "DYNPIVIS" 1))
  (if oldTooltips (setvar "TOOLTIPS" 1))

  ;; Hata veya ESC ile iptal durumunda vurgulari ve ayarlari geri yukle
  (defun *error* (msg)
    (if ent1 (redraw ent1 4))
    (if ent2 (redraw ent2 4))
    (if oldOs (setvar "OSMODE" oldOs))
    (if oldDynMode (setvar "DYNMODE" oldDynMode))
    (if oldDynPrompt (setvar "DYNPROMPT" oldDynPrompt))
    (if oldDynPivis (setvar "DYNPIVIS" oldDynPivis))
    (if oldTooltips (setvar "TOOLTIPS" oldTooltips))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*")))
      (princ (strcat "\nHata: " msg))
    )
    (princ)
  )

  ;; Hedef katman adi
  (setq layName "n-yazı")

  ;; Katman cizimde yoksa otomatik olustur
  (if (not (tblsearch "LAYER" layName))
    (entmake
      (list
        '(0 . "LAYER")
        '(100 . "AcDbSymbolTableRecord")
        '(100 . "AcDbLayerTableRecord")
        (cons 2 layName)
        '(70 . 0)
        '(62 . 7)
      )
    )
  )

  ;; 1. Yuklenici m2 metnini sec
  (setq ent1 (po:pick-text "\n[1/4] Yüklenici m2 metnini seçin: "))
  (if ent1
    (progn
      (redraw ent1 3)
      (setq obj (entget ent1))
      (setq txt1 (cdr (assoc 1 obj)))
      (if txt1
        (setq valYuklenici (po:find-number txt1))
      )
    )
  )

  (if (and valYuklenici (numberp valYuklenici) (> valYuklenici 0.0))
    (progn
      ;; 2. Arsa Sahibi m2 metnini sec
      (setq ent2 (po:pick-text "\n[2/4] Arsa Sahibi m2 metnini seçin: "))
      (if ent2
        (progn
          (redraw ent2 3)
          (setq obj (entget ent2))
          (setq txt2 (cdr (assoc 1 obj)))
          (if txt2
            (setq valArsa (po:find-number txt2))
          )
        )
      )

      (if (and valArsa (numberp valArsa) (> valArsa 0.0))
        (progn
          ;; Toplam alani ve yuzdeleri hesapla
          (setq toplam (+ (float valYuklenici) (float valArsa)))
          (setq oranYuklenici (* (/ (float valYuklenici) toplam) 100.0))
          (setq oranArsa (* (/ (float valArsa) toplam) 100.0))

          (setq strYuklenici (strcat "Yüklenici: %" (po:fmt oranYuklenici)))
          (setq strArsa (strcat "Arsa Sahibi: %" (po:fmt oranArsa)))

          ;; 3. Yuklenici oraninin yazilacagi noktayi sec ve yaz
          (setq pt1 (getpoint "\n[3/4] Yüklenici oranının yazılacağı yeri seçin: "))
          (if pt1
            (progn
              (po:make-mtext layName pt1 strYuklenici)
              ;; 4. Arsa Sahibi oraninin yazilacagi noktayi sec ve yaz
              (setq pt2 (getpoint "\n[4/4] Arsa Sahibi oranının yazılacağı yeri seçin: "))
              (if pt2
                (progn
                  (po:make-mtext layName pt2 strArsa)
                  (princ "\nYüklenici ve Arsa Sahibi oranları ayrı ayrı eklendi!")
                )
                (princ "\nArsa Sahibi oranı için nokta seçilmedi.")
              )
            )
            (princ "\nYüklenici oranı için nokta seçilmedi.")
          )
        )
        (princ "\nHata: Arsa Sahibi metninden sayı okunamadı!")
      )
    )
    (princ "\nHata: Yüklenici metni seçilmedi veya metinden sayı okunamadı!")
  )

  ;; Secim vurgularini ve ayarlari eski haline getir
  (if ent1 (redraw ent1 4))
  (if ent2 (redraw ent2 4))
  (if oldOs (setvar "OSMODE" oldOs))
  (if oldDynMode (setvar "DYNMODE" oldDynMode))
  (if oldDynPrompt (setvar "DYNPROMPT" oldDynPrompt))
  (if oldDynPivis (setvar "DYNPIVIS" oldDynPivis))
  (if oldTooltips (setvar "TOOLTIPS" oldTooltips))
  (princ)
)