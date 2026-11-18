(defun c:ALANEKLE ( / txtEnt objEnt txtObj objObj doc util objID fieldStr oldText newText pos)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq util (vla-get-Utility doc))

  ;; 1. Yazıyı Seçme (Oda adı)
  (setq txtEnt (car (entsel "\nOda adını içeren yazıyı (Örn: SALON) seçin: ")))
  (if txtEnt
    (progn
      (setq txtObj (vlax-ename->vla-object txtEnt))
      (if (wcmatch (vla-get-ObjectName txtObj) "AcDbText,AcDbMText")
        (progn
          
          ;; Eğer seçilen yazı Tek Satır (Text) ise, alt satıra geçebilmesi için MText'e çeviriyoruz
          (if (= (vla-get-ObjectName txtObj) "AcDbText")
            (progn
              (command "_.TXT2MTXT" txtEnt "")
              (setq txtEnt (entlast))
              (setq txtObj (vlax-ename->vla-object txtEnt))
            )
          )

          ;; 2. Alanı Alınacak Objeyi (Polyline vb.) Seçme
          (setq objEnt (car (entsel "\nAlanı alınacak sınır objesini (Polyline vb.) seçin: ")))
          (if objEnt
            (progn
              (setq objObj (vlax-ename->vla-object objEnt))
              
              ;; Seçilen objenin "Area" özelliği var mı kontrolü
              (if (vlax-property-available-p objObj 'Area)
                (progn
                  ;; 64-bit uyumlu ObjectID alma
                  (if (vlax-method-applicable-p util 'GetObjectIdString)
                    (setq objID (vlax-invoke-method util 'GetObjectIdString objObj :vlax-false))
                    (setq objID (itoa (vla-get-ObjectID objObj)))
                  )
                  
                  ;; Alan formülü (0.0001 çarpanı dahil)
                  (setq fieldStr (strcat "%<\\AcObjProp Object(%<\\_ObjId " objID ">%).Area \\f \"%lu2%pr2%ct8[0.0001]\">%"))
                  
                  ;; Mevcut yazıyı (Örn: "SALON") hafızaya alıyoruz
                  (setq oldText (vla-get-TextString txtObj))
                  
                  ;; Eğer komutu yanlışlıkla aynı yazıda 2. kez çalıştırırsanız, eski alanı silip yenisini eklemesi için temizlik:
                  (if (setq pos (vl-string-search "\\P" oldText))
                    (setq oldText (substr oldText 1 pos))
                  )
                  (if (setq pos (vl-string-search "\n" oldText))
                    (setq oldText (substr oldText 1 pos))
                  )
                  
                  ;; Yeni metni birleştir: [Oda Adı] + [Alt Satıra Geç] + [Alan Field] + [m2]
                  (setq newText (strcat oldText "\\P" fieldStr "m2"))
                  
                  ;; Yazıyı ve sahneyi güncelle
                  (vla-put-TextString txtObj newText)
                  (command "_.UPDATEFIELD" txtEnt "")
                  
                  (princ "\n✅ Alan başarıyla eklendi!")
                )
                (princ "\n❌ Hata: Seçilen objenin alan (Area) özelliği yok!")
              )
            )
            (princ "\nİşlem iptal edildi (Alan objesi seçilmedi).")
          )
        )
        (princ "\n❌ Hata: Lütfen geçerli bir Yazı seçin!")
      )
    )
    (princ "\nİşlem iptal edildi (Yazı seçilmedi).")
  )
  (princ)
)
(princ "\n>>> Komutu çalıştırmak için ALANEKLE yazın <<<")
(princ)