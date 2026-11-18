(defun c:MERDIVEN (/ pathEnt pt_user pathObj closestPt offsetDist param1 deriv1 ang_tangent1 ang_user ang_diff start_dist minPt maxPt p1 p2 ss i treadEnt intVar intArr ptList intPt dist lst clean_lst j d1 d2 mid_dists d_last d_prev final_dists td text_pt param deriv ang_tangent baslangic k step_w max_dist end_pt end_param end_deriv end_ang txt_hgt)
  (vl-load-com)
  
  ;; 1. Katman kontrolü
  (if (not (tblsearch "LAYER" "n-merdiven"))
    (entmake '((0 . "LAYER") (100 . "AcDbSymbolTableRecord") (100 . "AcDbLayerTableRecord") (2 . "n-merdiven") (70 . 0) (62 . 3)))
  )
  
  (setq baslangic (getint "\nBaşlangıç numarası <1>: "))
  (if (not baslangic) (setq baslangic 1))

  ;; 2. SADECE ÇIKIŞ HATTINI SEÇ (Kılavuz Çizgi)
  (if (setq pathEnt (car (entsel "\n1. Sadece ÇIKIŞ HATTINI (Ortadaki Kıvrımlı Ok Çizgisini) seçin: ")))
    (progn
      (setq pathObj (vlax-ename->vla-object pathEnt))
      
      ;; 3. YAZININ BAŞLAYACAĞI İLK YERİ SEÇ
      (if (setq pt_user (getpoint "\n2. İLK BASAMAĞIN İÇİNE TIKLAYIN (Yazının durmasını istediğiniz boşluk): "))
        (progn
          
          ;; Hiza Hesaplaması
          (setq closestPt (vlax-curve-getClosestPointTo pathObj pt_user))
          (setq offsetDist (distance closestPt pt_user))
          (setq param1 (vlax-curve-getParamAtPoint pathObj closestPt))
          (setq deriv1 (vlax-curve-getFirstDeriv pathObj param1))
          (setq ang_tangent1 (angle '(0.0 0.0 0.0) deriv1))
          (setq ang_user (angle closestPt pt_user))
          (setq ang_diff (- ang_user ang_tangent1))
          
          (setq start_dist (vlax-curve-getDistAtPoint pathObj closestPt))
          
          ;; OTOMATİK TARAMA
          (vla-GetBoundingBox pathObj 'minPt 'maxPt)
          (setq p1 (list (- (car (vlax-safearray->list minPt)) 5.0) (- (cadr (vlax-safearray->list minPt)) 5.0) 0.0))
          (setq p2 (list (+ (car (vlax-safearray->list maxPt)) 5.0) (+ (cadr (vlax-safearray->list maxPt)) 5.0) 0.0))
          
          (princ "\nOtomatik taranıyor ve basamak araları bulunuyor...")
          (setq ss (ssget "_C" p1 p2 '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC,SPLINE"))))

          ;; Otomatik tarama bulamazsa blok kontrolü yap ve manuel seçim iste
          (if (not ss)
            (progn
              (if (ssget "_C" p1 p2 '((0 . "INSERT")))
                (princ "\nUyarı: Merdiven BLOK halinde! Önce EXPLODE ile patlatın veya çizgileri seçin.")
              )
              (princ "\nBasamak çizgilerini pencere içine alarak seçin: ")
              (setq ss (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC,SPLINE"))))
            )
          )

          (if ss
            (progn
              (setq lst '())
              (setq i 0)
              (while (< i (sslength ss))
                (setq treadEnt (ssname ss i))
                (if (not (equal pathEnt treadEnt))
                  (progn
                    (setq intVar (vla-IntersectWith pathObj (vlax-ename->vla-object treadEnt) acExtendNone))
                    (setq intArr (vlax-variant-value intVar))
                    (if (and (= (type intArr) 'safearray)
                             (>= (vlax-safearray-get-u-bound intArr 1) 2))
                      (progn
                        (setq ptList (vlax-safearray->list intArr))
                        (setq k 0)
                        (while (< k (length ptList))
                          (setq intPt (list (nth k ptList) (nth (+ k 1) ptList) (nth (+ k 2) ptList)))
                          (setq dist (vlax-curve-getDistAtPoint pathObj (vlax-curve-getClosestPointTo pathObj intPt)))
                          (setq lst (cons dist lst))
                          (setq k (+ k 3))
                        )
                      )
                    )
                  )
                )
                (setq i (1+ i))
              )

              ;; Mesafeleri sırala ve çakışan kesişimleri temizle
              (setq lst (vl-sort lst '<))
              (setq clean_lst '())
              (foreach d lst
                (if (or (null clean_lst) (> (abs (- d (car clean_lst))) 0.01))
                  (setq clean_lst (cons d clean_lst))
                )
              )
              (setq clean_lst (reverse clean_lst))

              ;; Çıkış hattı tersten çizilmişse yönü kullanıcının tıkladığı başlangıca göre hizala
              (if (and (>= (length clean_lst) 2)
                       (> (abs (- start_dist (car clean_lst)))
                          (abs (- start_dist (last clean_lst)))))
                (setq clean_lst (reverse clean_lst))
              )

              ;; Basamak ortalarını ve son basamak/sahanlık konumunu hesapla
              (if (>= (length clean_lst) 2)
                (progn
                  (setq mid_dists '())
                  (setq j 0)
                  (while (< j (1- (length clean_lst)))
                    (setq d1 (nth j clean_lst))
                    (setq d2 (nth (1+ j) clean_lst))
                    (setq mid_dists (cons (/ (+ d1 d2) 2.0) mid_dists))
                    (setq j (1+ j))
                  )
                  (setq mid_dists (reverse mid_dists))

                  (setq d_last (last clean_lst))
                  (setq d_prev (nth (- (length clean_lst) 2) clean_lst))
                  (setq step_w (abs (- d_last d_prev)))
                  (setq final_dists (append mid_dists (list (+ d_last (/ (- d_last d_prev) 2.0)))))

                  ;; Yazı yüksekliğini basamak genişliğine göre ölçekle
                  (setq txt_hgt (* 0.35 step_w))
                  (if (<= txt_hgt 0.0) (setq txt_hgt (getvar "TEXTSIZE")))

                  (setq max_dist (vlax-curve-getDistAtParam pathObj (vlax-curve-getEndParam pathObj)))

                  ;; Numaraları yerleştir
                  (foreach td final_dists
                    (cond
                      ((and (>= td 0.0) (<= td max_dist))
                       (setq closestPt (vlax-curve-getPointAtDist pathObj td))
                       (setq param (vlax-curve-getParamAtDist pathObj td))
                       (setq deriv (vlax-curve-getFirstDeriv pathObj param))
                       (setq ang_tangent (angle '(0.0 0.0 0.0) deriv))
                      )
                      ((> td max_dist)
                       (setq end_param (vlax-curve-getEndParam pathObj))
                       (setq end_pt (vlax-curve-getEndPoint pathObj))
                       (setq end_deriv (vlax-curve-getFirstDeriv pathObj end_param))
                       (setq end_ang (angle '(0.0 0.0 0.0) end_deriv))
                       (setq closestPt (polar end_pt end_ang (- td max_dist)))
                       (setq ang_tangent end_ang)
                      )
                      ((< td 0.0)
                       (setq end_param (vlax-curve-getStartParam pathObj))
                       (setq end_pt (vlax-curve-getStartPoint pathObj))
                       (setq end_deriv (vlax-curve-getFirstDeriv pathObj end_param))
                       (setq end_ang (angle '(0.0 0.0 0.0) end_deriv))
                       (setq closestPt (polar end_pt (+ end_ang pi) (abs td)))
                       (setq ang_tangent end_ang)
                      )
                    )

                    (setq text_pt (polar closestPt (+ ang_tangent ang_diff) offsetDist))
                    (entmake
                      (list
                        '(0 . "TEXT")
                        '(8 . "n-merdiven")
                        (cons 10 text_pt)
                        (cons 11 text_pt)
                        (cons 40 txt_hgt)
                        '(72 . 1)
                        '(73 . 2)
                        (cons 1 (itoa baslangic))
                      )
                    )
                    (setq baslangic (1+ baslangic))
                  )
                  (princ (strcat "\nToplam " (itoa (length final_dists)) " basamak numaralandırıldı."))
                )
                (princ "\nHata: Çıkış hattı ile kesişen yeterli basamak çizgisi bulunamadı (Merdiven blok ise EXPLODE yapın).")
              )
            )
            (princ "\nHata: Basamak çizgisi seçilmedi veya bulunamadı.")
          )
        )
      )
    )
  )
  (princ)
)