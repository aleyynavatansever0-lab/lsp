(defun c:TUMOLCU (/ ssAks ssPlan i j aksObj planObj rawPts pts pList cleanList sortedList pA pB dist oldLayer oldDimTxt minOda ptRef p1 k duplicate cp)
  (vl-load-com)

  ;; 1. Layer ve Ölçü Ayarları
  (setq oldLayer (getvar "CLAYER"))
  (setq oldDimTxt (getvar "DIMTXT"))

  (if (not (tblsearch "LAYER" "n-içölçü"))
    (command "-layer" "m" "n-içölçü" "")
  )
  (setvar "CLAYER" "n-içölçü")
  (setvar "DIMTXT" 12)

  ;; 2. Duvar/Kolon Kalınlığı Filtresi
  (setq minOda (getreal "\nDuvar/Kolon kalınlığını atlamak için maksimum ölçüyü girin <15>: "))
  (if (not minOda) (setq minOda 15.0))

  ;; 3. Aksları Seçme
  (princ "\nADIM 1: Ölçü atılacak AKS ÇİZGİLERİNİ (Line veya Xline) seçin: ")
  (setq ssAks (ssget '((0 . "LINE,XLINE"))))
  
  (if ssAks
    (progn
      ;; 4. Planı Seçme (n-duvar, n-kolon ve BLOK objeleri dahil edildi)
      (princ "\nADIM 2: Tüm planı seçin ('n-duvar' ve 'n-kolon' layerındaki çizgiler ile bloklar dikkate alınacaktır): ")
      (setq ssPlan (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC,CIRCLE,INSERT") (8 . "n-duvar,n-kolon"))))
      
      (if ssPlan
        (progn
          (setq i 0)
          ;; Seçilen her bir aks çizgisi için döngü
          (while (< i (sslength ssAks))
            (setq aksObj (vlax-ename->vla-object (ssname ssAks i)))
            (setq pList nil)
            
            (setq j 0)
            ;; Aksı "n-duvar" ve "n-kolon" objeleriyle/bloklarıyla kesiştir
            (while (< j (sslength ssPlan))
              (setq planObj (vlax-ename->vla-object (ssname ssPlan j)))
              
              (if (not (equal (vla-get-ObjectID aksObj) (vla-get-ObjectID planObj)))
                (progn
                  (setq rawPts (vl-catch-all-apply 'vlax-invoke (list aksObj 'IntersectWith planObj acExtendNone)))
                  (if (and rawPts (not (vl-catch-all-error-p rawPts)) (listp rawPts))
                    (progn
                      (setq pts rawPts)
                      (while pts
                        (setq pList (cons (list (car pts) (cadr pts) (caddr pts)) pList))
                        (setq pts (cdddr pts))
                      )
                    )
                  )
                )
              )
              (setq j (1+ j))
            )
            
            ;; Noktaları temizle ve ölçüyü at
            (if (and pList (> (length pList) 1))
              (progn
                ;; Duplicate noktaları temizle
                (setq cleanList nil)
                (foreach p pList
                  (setq duplicate nil)
                  (foreach cp cleanList
                    (if (< (distance p cp) 0.1) (setq duplicate T))
                  )
                  (if (not duplicate) (setq cleanList (cons p cleanList)))
                )
                
                (if (> (length cleanList) 1)
                  (progn
                    ;; Referans noktası bul ve sırala
                    (setq p1 (car cleanList))
                    (setq ptRef p1)
                    (foreach p cleanList
                      (if (> (distance p1 p) (distance p1 ptRef))
                        (setq ptRef p)
                      )
                    )
                    (setq sortedList (vl-sort cleanList '(lambda (a b) (< (distance ptRef a) (distance ptRef b)))))
                    
                    ;; Duvar/Kolonları (15 cm ve altı) es geç, odaları ölçülendir
                    (setq k 0)
                    (while (< k (1- (length sortedList)))
                      (setq pA (nth k sortedList))
                      (setq pB (nth (1+ k) sortedList))
                      (setq dist (distance pA pB))
                      
                      (if (> dist minOda)
                        (command "_dimaligned" pA pB pA) 
                      )
                      (setq k (1+ k))
                    )
                  )
                )
              )
            )
            (setq i (1+ i))
          )
          (princ (strcat "\nBAŞARILI: Toplam " (itoa (sslength ssAks)) " adet aks çizgisi üzerinden DUVARLAR ve KOLONLAR arası ölçülendirildi!"))
        )
        (princ "\nHata: Seçtiğiniz alanda 'n-duvar' veya 'n-kolon' layerına ait hiçbir obje bulunamadı.")
      )
    )
    (princ "\nHata: Aks çizgisi seçilmedi.")
  )

  ;; Eski ayarlara dön
  (setvar "CLAYER" oldLayer)
  (setvar "DIMTXT" oldDimTxt)
  (princ)
)