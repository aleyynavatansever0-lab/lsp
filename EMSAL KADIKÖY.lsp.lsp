(defun c:EMSAL (/ ent obj area katsayi toplamAlan pt txtStr)
  (vl-load-com)
  
  ;; 1. "n-yazı" katmanı yoksa arka planda sessizce oluştur
  (if (not (tblsearch "LAYER" "n-yazı"))
    (entmake '((0 . "LAYER") 
               (100 . "AcDbSymbolTableRecord") 
               (100 . "AcDbLayerTableRecord") 
               (2 . "n-yazı") 
               (70 . 0) 
               (62 . 7))) ; 7 = Beyaz/Siyah (Varsayılan renk)
  )

  ;; Kadıköy Emsal Katsayısı
  (setq katsayi 2.07)
  
  (princ (strcat "\n--- Kadıköy Emsal Hesabı (Katsayı: " (rtos katsayi 2 2) ") ---"))
  
  ;; Parsel sınırını seçtir
  (if (setq ent (car (entsel "\nEmsal hesabı yapılacak parsel sınırını (Polyline) seçin: ")))
    (progn
      (setq obj (vlax-ename->vla-object ent))
      
      (if (vlax-property-available-p obj 'Area)
        (progn
          (setq area (vla-get-Area obj))
          (setq toplamAlan (* area katsayi))
          
          ;; Sonuçları Komut Satırına Yazdır
          (princ "\n----------------------------------------")
          (princ (strcat "\nParsel Alanı               : " (rtos area 2 2) " m²"))
          (princ (strcat "\nEmsal (KAKS)               : " (rtos katsayi 2 2)))
          (princ (strcat "\nTOPLAM İNŞAAT ALANI (EMSAL): " (rtos toplamAlan 2 2) " m²"))
          (princ (strcat "\n----------------------------------------"))
          
          ;; Ekrana Otomatik Yazdırma (100 Puntosuyla ve n-yazı katmanında)
          (if (setq pt (getpoint "\nSonucun yazılacağı noktaya tıklayın: "))
            (progn
              (setq txtStr (strcat (rtos toplamAlan 2 2) " m²"))
              (entmake (list
                          '(0 . "TEXT")
                          '(8 . "n-yazı")        ;; Belirttiğin layer
                          (cons 10 pt)
                          (cons 40 100.0)        ;; Sabit 100 yazı yüksekliği
                          (cons 1 txtStr)
                          '(50 . 0.0)
                          '(72 . 0)
                          '(73 . 0)
                          (cons 11 pt)
                        ))
              (princ "\nSonuç 'n-yazı' katmanına 100 yükseklikte yazıldı.")
            )
          )
        )
        (princ "\nHata: Seçilen objenin kapalı bir alanı yok.")
      )
    )
    (princ "\nHata: Parsel seçimi yapılmadı.")
  )
  (princ)
)