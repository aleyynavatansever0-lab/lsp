(defun c:HESAPLA (/ *error* _strip-mtext _extract-number _read-field-dict _get-obj-num
                    _get-num-from-ent _highlight-ent _pick-value islem sayi1 sayi2 sayi3
                    ent1 last-ent sonuc sonuc-str ss sel-ss i ent val okunan-list txt-lyr pt)
  (vl-load-com)

  ;; Komut ESC ile iptal edilirse mavi seçimleri temizle
  (defun *error* (msg)
    (sssetfirst nil nil)
    (redraw)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\nHata: " msg))
    )
    (princ)
  )

  ;; Seçilen nesneyi mavi kutucuk (grips) ve seçim vurgusu ile işaretler
  (defun _highlight-ent (ent)
    (if ent
      (progn
        (if (not sel-ss) (setq sel-ss (ssadd)))
        (if (not (ssmemb ent sel-ss))
          (ssadd ent sel-ss)
        )
        (redraw ent 3)
        (sssetfirst nil sel-ss)
      )
    )
  )

  ;; MText biçimlendirme kodlarını (\f, \H, \S, \U+00B2, \M+ vb.) temizler
  (defun _strip-mtext (str / len i ch nxt out semi)
    (setq len (strlen str) i 1 out "")
    (while (<= i len)
      (setq ch (substr str i 1))
      (cond
        ((or (= ch "{") (= ch "}"))
         (setq i (1+ i))
        )
        ((= ch "\\")
         (setq nxt (if (< i len) (substr str (1+ i) 1) ""))
         (cond
           ((or (= nxt "\\") (= nxt "{") (= nxt "}"))
            (setq out (strcat out nxt) i (+ i 2))
           )
           ((or (= nxt "P") (= nxt "~"))
            (setq out (strcat out " ") i (+ i 2))
           )
           ((member nxt '("L" "l" "O" "o" "K" "k"))
            (setq i (+ i 2))
           )
           ((or (= nxt "U") (= nxt "u"))
            (setq i (+ i 7))
           )
           ((or (= nxt "M") (= nxt "m"))
            (setq i (+ i 8))
           )
           (T
            (setq semi (vl-string-search ";" str i))
            (if semi
              (setq i (+ semi 2))
              (setq i (+ i 2))
            )
           )
         )
        )
        (T
         (setq out (strcat out ch) i (1+ i))
        )
      )
    )
    out
  )

  ;; Temizlenmiş metinden ilk sayı bloğunu çıkarır
  (defun _extract-number (raw-str / str clean-str len idx ch nxt dot-found started done)
    (if (and raw-str (= (type raw-str) 'STR))
      (progn
        (setq str (_strip-mtext raw-str))
        (setq str (vl-string-translate "," "." str))
        (setq clean-str "" len (strlen str) idx 1 dot-found nil started nil done nil)
        (while (and (<= idx len) (not done))
          (setq ch (substr str idx 1))
          (setq nxt (if (< idx len) (substr str (1+ idx) 1) ""))
          (cond
            ((and (>= (ascii ch) 48) (<= (ascii ch) 57))
             (setq clean-str (strcat clean-str ch) started T)
            )
            ((and (= ch ".") (not dot-found) started)
             (setq clean-str (strcat clean-str ch) dot-found T)
            )
            ((and (= ch "-") (not started) (>= (ascii nxt) 48) (<= (ascii nxt) 57))
             (setq clean-str (strcat clean-str ch) started T)
            )
            (started
             (setq done T)
            )
          )
          (setq idx (1+ idx))
        )
        (if (/= clean-str "")
          (atof clean-str)
          nil
        )
      )
      nil
    )
  )

  ;; Eğer TextString ham %<...>% dönerse ACAD_FIELD sözlüğünden güncel metni okur
  (defun _read-field-dict (ent / elist dict fdict ftxt res)
    (setq elist (entget ent))
    (if (setq dict (cdr (assoc 360 elist)))
      (if (setq fdict (dictsearch dict "ACAD_FIELD"))
        (if (setq ftxt (cdr (assoc 360 fdict)))
          (setq res (cdr (assoc 1 (entget ftxt))))
        )
      )
    )
    res
  )

  ;; VLA nesnesinden metni alıp sayıya çevirir
  (defun _get-obj-num (obj ent / raw fraw)
    (setq raw "")
    (if (vlax-property-available-p obj 'TextString)
      (setq raw (vla-get-TextString obj))
    )
    (if (and ent (or (= raw "") (wcmatch raw "*%<*>%*")))
      (if (setq fraw (_read-field-dict ent))
        (setq raw fraw)
      )
    )
    (_extract-number raw)
  )

  ;; TEXT, MTEXT, MULTILEADER veya BLOK içindeki sayısal değeri okur
  (defun _get-num-from-ent (ent / obj etype num atts att tag val cand-list best-val bname bent)
    (setq obj (vlax-ename->vla-object ent))
    (setq etype (cdr (assoc 0 (entget ent))))
    (cond
      ((or (wcmatch etype "*TEXT") (= etype "MULTILEADER"))
       (if (not txt-lyr)
         (setq txt-lyr (vla-get-Layer obj))
       )
       (_get-obj-num obj ent)
      )
      ((= etype "INSERT")
       (if (= (vla-get-HasAttributes obj) :vlax-true)
         (progn
           (setq atts (append (vlax-invoke obj 'GetAttributes)
                              (vlax-invoke obj 'GetConstantAttributes)))
           (foreach att atts
             (setq tag (strcase (vla-get-TagString att)))
             (setq val (_get-obj-num att (vlax-vla-object->ename att)))
             (if val
               (progn
                 (if (not txt-lyr)
                   (setq txt-lyr (vla-get-Layer att))
                 )
                 (if (wcmatch tag "*ALAN*,*AREA*,*M2*,*METRE*")
                   (setq best-val val)
                   (setq cand-list (append cand-list (list val)))
                 )
               )
             )
           )
           (cond
             (best-val (setq num best-val))
             (cand-list
              (setq num (car (vl-remove 0.0 cand-list)))
              (if (null num) (setq num (car cand-list)))
             )
           )
         )
       )
       (if (null num)
         (progn
           (setq bname (if (vlax-property-available-p obj 'EffectiveName)
                         (vla-get-EffectiveName obj)
                         (vla-get-Name obj)))
           (setq bent (tblobjname "BLOCK" bname))
           (while (and bent (not num))
             (if (wcmatch (cdr (assoc 0 (entget bent))) "*TEXT")
               (setq num (_get-obj-num (vlax-ename->vla-object bent) bent))
             )
             (setq bent (entnext bent))
           )
         )
       )
       num
      )
    )
  )

  ;; Ekrandan yazı/blok seçme (seçileni mavi kutucukla işaretler)
  (defun _pick-value (msg / sel ent val)
    (setq last-ent nil)
    (initget "Manuel")
    (setq sel (entsel (strcat "\n" msg " [Manuel sayı için M]: ")))
    (cond
      ((= sel "Manuel")
       (getreal "\nSayısal değeri girin: ")
      )
      ((and (listp sel) (wcmatch (cdr (assoc 0 (entget (car sel)))) "*TEXT,INSERT,MULTILEADER"))
       (setq ent (car sel))
       (setq val (_get-num-from-ent ent))
       (if val
         (progn
           (setq last-ent ent)
           (_highlight-ent ent)
           (princ (strcat " -> Okunan: " (rtos val 2 2)))
           val
         )
         (progn
           (princ "\nSeçilen nesnede sayısal değer bulunamadı!")
           (_pick-value msg)
         )
       )
      )
      ((null sel)
       nil
      )
      (T
       (princ "\nGeçersiz seçim! Lütfen bir yazı/blok seçin veya M yazıp sayı girin.")
       (_pick-value msg)
      )
    )
  )

  ;; 1. Adım: İşlem Seçimi
  (initget "Toplama CIkarma CArpma Bolme Oranla")
  (setq islem (getkword "\nYapmak istediğiniz işlem nedir? [Toplama/CIkarma/CArpma/Bolme/Oranla] <Toplama>: "))
  (if (null islem) (setq islem "Toplama"))

  (setq sonuc nil txt-lyr nil okunan-list nil sel-ss (ssadd))
  (sssetfirst nil nil)

  ;; 2. Adım: Seçilen İşleme Göre Hesaplama
  (cond
    ;; TOPLAMA
    ((= islem "Toplama")
     (princ "\nToplanacak yazıları veya blokları seçin: ")
     (if (setq ss (ssget '((0 . "*TEXT,INSERT,MULTILEADER"))))
       (progn
         (setq sonuc 0.0 i 0)
         (while (< i (sslength ss))
           (setq ent (ssname ss i))
           (_highlight-ent ent)
           (setq val (_get-num-from-ent ent))
           (if val
             (progn
               (setq sonuc (+ sonuc val))
               (setq okunan-list (append okunan-list (list (rtos val 2 2))))
             )
             (princ (strcat "\n[Atlandı] Sayı okunamayan nesne (" (cdr (assoc 0 (entget ent))) ")"))
           )
           (setq i (1+ i))
         )
         (if okunan-list
           (princ (strcat "\nOkunan değerler: " (car okunan-list)
                          (apply 'strcat (mapcar '(lambda (x) (strcat " + " x)) (cdr okunan-list)))))
         )
       )
     )
    )

    ;; ÇIKARMA
    ((= islem "CIkarma")
     (setq sayi1 (_pick-value "Eksilecek (ana) değeri seçin"))
     (setq ent1 last-ent)
     (if sayi1
       (progn
         ;; ssget'in mevcut seçimi atlamaması için geçici temizle, çizgili vurguyu koru
         (sssetfirst nil nil)
         (if ent1 (redraw ent1 3))
         (princ "\nÇıkarılacak yazıları veya blokları seçin: ")
         (if (setq ss (ssget '((0 . "*TEXT,INSERT,MULTILEADER"))))
           (progn
             (setq sayi2 0.0 i 0)
             (while (< i (sslength ss))
               (setq ent (ssname ss i))
               (_highlight-ent ent)
               (setq val (_get-num-from-ent ent))
               (if val
                 (setq sayi2 (+ sayi2 val))
               )
               (setq i (1+ i))
             )
             (setq sonuc (- sayi1 sayi2))
           )
         )
       )
     )
    )

    ;; ÇARPMA
    ((= islem "CArpma")
     (setq sayi1 (_pick-value "Çarpılacak 1. değeri seçin"))
     (if sayi1
       (progn
         (setq sayi2 (_pick-value "Çarpılacak 2. değeri seçin"))
         (if sayi2
           (setq sonuc (* sayi1 sayi2))
         )
       )
     )
    )

    ;; BÖLME
    ((= islem "Bolme")
     (setq sayi1 (_pick-value "Bölünecek (1.) değeri seçin"))
     (if sayi1
       (progn
         (setq sayi2 (_pick-value "Bölen (2.) değeri seçin"))
         (if sayi2
           (if (/= sayi2 0.0)
             (setq sonuc (/ sayi1 sayi2))
             (princ "\nHata: Sıfıra bölme yapılamaz!")
           )
         )
       )
     )
    )

    ;; ORANLA
    ((= islem "Oranla")
     (setq sayi1 (_pick-value "1. Referans değeri seçin (Temel değer)"))
     (if sayi1
       (if (/= sayi1 0.0)
         (progn
           (setq sayi2 (_pick-value "2. Karşılık gelen değeri seçin"))
           (if sayi2
             (progn
               (setq sayi3 (_pick-value "3. Oranlanacak değeri seçin"))
               (if sayi3
                 (setq sonuc (/ (* sayi2 sayi3) sayi1))
               )
             )
           )
         )
         (princ "\nHata: Referans değer sıfır olamaz!")
       )
     )
    )
  )

  ;; 3. Adım: Sonucu "m2" Ekleyerek ve 25 Yazı Boyutuyla Yazdırma
  (if sonuc
    (progn
      (setq sonuc-str (strcat (rtos sonuc 2 2) " m2"))
      (princ "\n====================================")
      (princ (strcat "\nHesaplanan Sonuç: " sonuc-str))
      (princ "\n====================================")
      (setq pt (getpoint "\nSonucu ekrana yazdırmak için nokta seçin [Atlamak için Enter]: "))
      (if pt
        (progn
          (if (not txt-lyr) (setq txt-lyr (getvar "CLAYER")))
          (entmakex
            (list
              '(0 . "TEXT")
              (cons 8 txt-lyr)
              (cons 10 (trans pt 1 0))
              (cons 40 25.0)
              (cons 1 sonuc-str)
            )
          )
        )
      )
    )
  )

  ;; İşlem bitince seçimleri ve vurguları temizle
  (sssetfirst nil nil)
  (redraw)
  (princ)
)