(vl-load-com)

(setq *num:highlighted* nil)

;;; --- VURGU VE SEÇİM YARDIMCILARI ---

(defun num:highlight (ent / )
  (if (and ent (entget ent))
    (progn
      (redraw ent 3)
      (if (not (member ent *num:highlighted*))
        (setq *num:highlighted* (cons ent *num:highlighted*))
      )
    )
  )
)

(defun num:clear-highlights ( / ent)
  (foreach ent *num:highlighted*
    (if (and ent (entget ent))
      (redraw ent 4)
    )
  )
  (setq *num:highlighted* nil)
)

(defun num:pick-entity (msg allow_enter / done res)
  (setq done nil
        res nil)
  (while (not done)
    (setvar "ERRNO" 0)
    (setq res (entsel msg))
    (cond
      (res
       (num:highlight (car res))
       (setq done t)
      )
      ((and allow_enter (= (getvar "ERRNO") 52))
       (setq res nil
             done t)
      )
      (t
       (princ "\nNesne seçilmedi, tekrar tıklayın...")
      )
    )
  )
  res
)

;;; --- GENEL YARDIMCI FONKSİYONLAR ---

(defun num:resolve-layer (target / def exact_name)
  (setq def (tblnext "LAYER" T))
  (while (and def (not exact_name))
    (if (= (strcase (cdr (assoc 2 def))) (strcase target))
      (setq exact_name (cdr (assoc 2 def)))
    )
    (setq def (tblnext "LAYER"))
  )
  (if exact_name
    exact_name
    (progn
      (entmake
        (list
          '(0 . "LAYER")
          '(100 . "AcDbSymbolTableRecord")
          '(100 . "AcDbLayerTableRecord")
          (cons 2 target)
          '(70 . 0)
          '(62 . 7)
        )
      )
      target
    )
  )
)

;;; İlgili layerde mevcut yazı varsa yüksekliğini okur, yoksa varsayılan değeri döner
(defun num:get-layer-text-h (lay def_h / ss i ed h found)
  (setq ss (ssget "_X" (list '(0 . "TEXT,MTEXT") (cons 8 lay))))
  (if ss
    (progn
      (setq i (1- (sslength ss)))
      (while (and (>= i 0) (not found))
        (setq ed (entget (ssname ss i))
              h (cdr (assoc 40 ed)))
        (if (and h (> h 0.0))
          (setq found h)
        )
        (setq i (1- i))
      )
    )
  )
  (if found found def_h)
)

(defun num:v-add (a b) (mapcar '+ a b))
(defun num:v-sub (a b) (mapcar '- a b))
(defun num:v-scale (v s) (mapcar '(lambda (x) (* x s)) v))
(defun num:v-dot (a b) (apply '+ (mapcar '* a b)))
(defun num:v-len (v) (sqrt (num:v-dot v v)))
(defun num:v-dist (a b) (num:v-len (num:v-sub a b)))

(defun num:v-unit (v / l)
  (setq l (num:v-len v))
  (if (> l 1e-9)
    (num:v-scale v (/ 1.0 l))
    '(0.0 0.0 0.0)
  )
)

(defun num:v-right-normal (v / u)
  (setq u (num:v-unit v))
  (list (cadr u) (- (car u)) 0.0)
)

(defun num:v-cross-z (a b)
  (- (* (car a) (cadr b)) (* (cadr a) (car b)))
)

;;; Türkçe karakter olmadan A-Z dönüşümü (1->A, 26->Z, 27->AA)
(defun num:num->alpha (n / res r)
  (setq res "")
  (while (> n 0)
    (setq r (rem (1- n) 26))
    (setq res (strcat (chr (+ 65 r)) res))
    (setq n (/ (1- n) 26))
  )
  res
)

(defun num:alpha->num (str / s i len c acc ok)
  (setq s (strcase str)
        len (strlen s)
        i 1
        acc 0
        ok (> len 0))
  (while (and ok (<= i len))
    (setq c (ascii (substr s i 1)))
    (if (and (>= c 65) (<= c 90))
      (setq acc (+ (* acc 26) (- c 64)))
      (setq ok nil)
    )
    (setq i (1+ i))
  )
  (if ok acc nil)
)

(defun num:digits-p (str / i len c ok)
  (setq len (strlen str)
        i 1
        ok (> len 0))
  (while (and ok (<= i len))
    (setq c (ascii (substr str i 1)))
    (if (or (< c 48) (> c 57))
      (setq ok nil)
    )
    (setq i (1+ i))
  )
  (if (and ok (> (atoi str) 0)) t nil)
)

;;; İşaretli tam sayı kontrolü (0, 5, -2 vb.)
(defun num:signed-int-p (str / len i c ok)
  (setq len (strlen str)
        i 1
        ok (> len 0))
  (if (and ok (member (substr str 1 1) '("+" "-")))
    (progn
      (setq i 2)
      (if (< len 2) (setq ok nil))
    )
  )
  (while (and ok (<= i len))
    (setq c (ascii (substr str i 1)))
    (if (or (< c 48) (> c 57))
      (setq ok nil)
    )
    (setq i (1+ i))
  )
  ok
)

(defun num:make-text (pt str h lay col / elst)
  (setq elst
    (list
      '(0 . "TEXT")
      '(100 . "AcDbEntity")
      (cons 8 lay)
    )
  )
  (if col
    (setq elst (append elst (list (cons 62 col))))
  )
  (setq elst
    (append elst
      (list
        '(100 . "AcDbText")
        (cons 10 pt)
        (cons 40 h)
        (cons 1 str)
        '(50 . 0.0)
        '(72 . 1)
        (cons 11 pt)
        '(100 . "AcDbText")
        '(73 . 2)
      )
    )
  )
  (entmake elst)
)

(defun num:make-circle (pt rad lay col / elst)
  (setq elst
    (list
      '(0 . "CIRCLE")
      '(100 . "AcDbEntity")
      (cons 8 lay)
    )
  )
  (if col
    (setq elst (append elst (list (cons 62 col))))
  )
  (setq elst
    (append elst
      (list
        '(100 . "AcDbCircle")
        (cons 10 pt)
        (cons 40 rad)
      )
    )
  )
  (entmake elst)
)

(defun num:collides-p (pt centers min_d / hit c)
  (setq hit nil)
  (foreach c centers
    (if (< (num:v-dist pt c) min_d)
      (setq hit t)
    )
  )
  hit
)

(defun num:replace-nth (lst idx val / res i)
  (setq res '()
        i 0)
  (foreach x lst
    (if (= i idx)
      (setq res (cons val res))
      (setq res (cons x res))
    )
    (setq i (1+ i))
  )
  (reverse res)
)

(defun num:spread-1d-positions (pos_list min_sep / n arr iter i d1 d2 diff push_amt changed)
  (setq n (length pos_list))
  (if (< n 2)
    pos_list
    (progn
      (setq arr pos_list
            iter 0
            changed t)
      (while (and changed (< iter 50))
        (setq changed nil
              i 0)
        (while (< i (1- n))
          (setq d1 (nth i arr)
                d2 (nth (1+ i) arr)
                diff (- d2 d1))
          (if (< diff (- min_sep 1e-4))
            (progn
              (setq push_amt (* 0.5 (- min_sep diff))
                    arr (num:replace-nth arr i (- d1 push_amt))
                    arr (num:replace-nth arr (1+ i) (+ d2 push_amt))
                    changed t)
            )
          )
          (setq i (1+ i))
        )
        (setq iter (1+ iter))
      )
      arr
    )
  )
)

;;; --- MOD 1: MERDİVEN NUMARALANDIRMA ---

(defun num:run-stairs (/ lay_out sel_ax ent_ax obj_ax tot_len sel_st ent_st obj_st step_lay
                         start_len min_step_len nosing_tol raw_st p_st d_raw_st
                         ss_steps i ent_s obj_s s_len raw_pts k p d_raw
                         all_raw_d cnt_fwd cnt_bwd rev_dir d_start
                         hits sorted_hits risers cur_grp h_item
                         tangs idx r_item d_along param deriv tang
                         cross_sum j side_sign outer_norm
                         str_in cur_num placed_pts
                         s_st s_en p_ax e_out v_out v_in proj u_along
                         lbl len_lbl off_along off_in pt_place min_sep guard)
  (setq lay_out (num:resolve-layer "n-merdiven"))

  (setq sel_ax (num:pick-entity "\nMerdiven aks çizgisini (çıkış hattını) seçin: " nil))
  (if sel_ax
    (progn
      (setq ent_ax (car sel_ax)
            obj_ax (vlax-ename->vla-object ent_ax))
      (if (vl-catch-all-error-p (vl-catch-all-apply 'vlax-curve-getEndParam (list obj_ax)))
        (princ "\nSeçilen aks nesnesi geçerli bir çizgi veya polyline değil.")
        (progn
          (setq tot_len (vlax-curve-getDistAtParam obj_ax (vlax-curve-getEndParam obj_ax)))

          (setq sel_st (num:pick-entity "\nBaşlangıç basamağını (1 numaranın başlayacağı çizgiyi) seçin: " nil))
          (if sel_st
            (progn
              (setq ent_st (car sel_st)
                    obj_st (vlax-ename->vla-object ent_st)
                    step_lay (cdr (assoc 8 (entget ent_st))))

              (if (vl-catch-all-error-p (vl-catch-all-apply 'vlax-curve-getEndParam (list obj_st)))
                (princ "\nSeçilen başlangıç basamağı geçerli bir çizgi değil.")
                (progn
                  (setq raw_st (vl-catch-all-apply 'vlax-invoke (list obj_ax 'IntersectWith obj_st acExtendNone)))
                  (if (or (vl-catch-all-error-p raw_st) (not raw_st))
                    (princ "\nSeçilen başlangıç basamağı merdiven aks çizgisiyle kesişmiyor.")
                    (progn
                      (setq p_st (list (car raw_st) (cadr raw_st) (caddr raw_st))
                            d_raw_st (vlax-curve-getDistAtPoint obj_ax (vlax-curve-getClosestPointTo obj_ax p_st))
                            start_len (vlax-curve-getDistAtParam obj_st (vlax-curve-getEndParam obj_st))
                            min_step_len (* 0.45 start_len)
                            nosing_tol (* 0.07 start_len))

                      (setq ss_steps (ssget "_X" (list '(0 . "LINE,LWPOLYLINE,POLYLINE,ARC") (cons 8 step_lay))))
                      (setq i 0
                            all_raw_d '())
                      (if ss_steps
                        (while (< i (sslength ss_steps))
                          (setq ent_s (ssname ss_steps i))
                          (if (not (equal ent_s ent_ax))
                            (progn
                              (setq obj_s (vlax-ename->vla-object ent_s))
                              (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vlax-curve-getEndParam (list obj_s))))
                                (progn
                                  (setq s_len (vlax-curve-getDistAtParam obj_s (vlax-curve-getEndParam obj_s)))
                                  (if (>= s_len min_step_len)
                                    (progn
                                      (setq raw_pts (vl-catch-all-apply 'vlax-invoke (list obj_ax 'IntersectWith obj_s acExtendNone)))
                                      (if (and (not (vl-catch-all-error-p raw_pts)) raw_pts)
                                        (progn
                                          (setq k 0)
                                          (while (< k (length raw_pts))
                                            (setq p (list (nth k raw_pts) (nth (+ k 1) raw_pts) (nth (+ k 2) raw_pts))
                                                  d_raw (vlax-curve-getDistAtPoint obj_ax (vlax-curve-getClosestPointTo obj_ax p)))
                                            (setq all_raw_d (cons (list d_raw p obj_s) all_raw_d))
                                            (setq k (+ k 3))
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                          (setq i (1+ i))
                        )
                      )

                      (setq cnt_fwd 0
                            cnt_bwd 0)
                      (foreach item all_raw_d
                        (cond
                          ((> (car item) (+ d_raw_st nosing_tol)) (setq cnt_fwd (1+ cnt_fwd)))
                          ((< (car item) (- d_raw_st nosing_tol)) (setq cnt_bwd (1+ cnt_bwd)))
                        )
                      )
                      (setq rev_dir (> cnt_bwd cnt_fwd))
                      (setq d_start (if rev_dir (- tot_len d_raw_st) d_raw_st))

                      (setq hits '())
                      (foreach item all_raw_d
                        (setq d_raw (if rev_dir (- tot_len (car item)) (car item)))
                        (if (>= d_raw (- d_start nosing_tol))
                          (setq hits (cons (list d_raw (cadr item) (caddr item)) hits))
                        )
                      )

                      (setq sorted_hits (vl-sort hits '(lambda (a b) (< (car a) (car b)))))
                      (setq risers '())
                      (if sorted_hits
                        (progn
                          (setq cur_grp (list (car sorted_hits)))
                          (foreach h_item (cdr sorted_hits)
                            (if (<= (- (car h_item) (caar cur_grp)) nosing_tol)
                              (setq cur_grp (cons h_item cur_grp))
                              (progn
                                (setq risers (cons (car cur_grp) risers)
                                      cur_grp (list h_item))
                              )
                            )
                          )
                          (setq risers (reverse (cons (car cur_grp) risers)))
                        )
                      )

                      (if (< (length risers) 1)
                        (princ "\nAks üzerinde numaralandırılacak basamak bulunamadı.")
                        (progn
                          (setq tangs '())
                          (foreach r_item risers
                            (setq d_along (if rev_dir (- tot_len (car r_item)) (car r_item))
                                  param (vlax-curve-getParamAtDist obj_ax (max 0.0 (min tot_len d_along)))
                                  deriv (vlax-curve-getFirstDeriv obj_ax param)
                                  tang (if rev_dir
                                         (num:v-scale (num:v-unit deriv) -1.0)
                                         (num:v-unit deriv)))
                            (setq tangs (cons tang tangs))
                          )
                          (setq tangs (reverse tangs))

                          (setq cross_sum 0.0
                                j 0)
                          (while (< j (1- (length tangs)))
                            (setq cross_sum (+ cross_sum (num:v-cross-z (nth j tangs) (nth (1+ j) tangs))))
                            (setq j (1+ j))
                          )

                          (cond
                            ((< cross_sum -0.05) (setq side_sign -1.0))
                            ((> cross_sum 0.05)  (setq side_sign 1.0))
                            (t                   (setq side_sign -1.0))
                          )

                          (setq str_in (getstring "\nBaşlangıç basamak numarası <1>: "))
                          (setq cur_num (if (num:digits-p str_in) (atoi str_in) 1))

                          (setq placed_pts '()
                                idx 0)
                          (while (< idx (length risers))
                            (setq r_item (nth idx risers)
                                  p_ax (cadr r_item)
                                  obj_s (caddr r_item)
                                  tang (nth idx tangs)
                                  outer_norm (num:v-scale (num:v-right-normal tang) side_sign)
                                  s_st (vlax-curve-getStartPoint obj_s)
                                  s_en (vlax-curve-getEndPoint obj_s))

                            (if (> (num:v-dot (num:v-sub s_en p_ax) outer_norm)
                                   (num:v-dot (num:v-sub s_st p_ax) outer_norm))
                              (setq e_out s_en)
                              (setq e_out s_st)
                            )

                            (setq v_out (num:v-unit (num:v-sub e_out p_ax))
                                  v_in (num:v-scale v_out -1.0)
                                  proj (num:v-sub tang (num:v-scale v_out (num:v-dot tang v_out))))
                            (if (> (num:v-len proj) 1e-4)
                              (setq u_along (num:v-unit proj))
                              (setq u_along tang)
                            )

                            (setq lbl (itoa cur_num)
                                  len_lbl (strlen lbl)
                                  off_along (+ 2.5 (* 3.3 len_lbl))
                                  off_in (+ 6.5 (* 0.25 len_lbl))
                                  pt_place (num:v-add
                                             (num:v-add e_out (num:v-scale u_along off_along))
                                             (num:v-scale v_in off_in))
                                  min_sep (+ 11.5 (* 2.5 len_lbl))
                                  guard 0)

                            (while (and (num:collides-p pt_place placed_pts min_sep) (< guard 12))
                              (setq pt_place (num:v-add pt_place (num:v-scale v_in 4.0))
                                    guard (1+ guard))
                            )

                            (setq placed_pts (cons pt_place placed_pts))
                            (num:make-text pt_place lbl 10.0 lay_out nil)

                            (setq cur_num (1+ cur_num)
                                  idx (1+ idx))
                          )

                          (princ (strcat "\n" (itoa (length risers)) " basamak [" lay_out "] layerinde dış kenara hizalı numaralandırıldı."))
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
    )
  )
)

;;; --- MOD 2: KOLON AKSI ETİKETLEME ---

(defun num:collect-axis-group (sel1 sel2 / ent1 pk1 ent2 pk2 obj1 obj2 lay_src
                                           s1 e1 tip1 base1 u1 u2 perp_dir
                                           s2 e2 ref_len span_min span_max margin
                                           t_start t_end t_min t_max
                                           ss_all i ent_c obj_c sc ec tip_c base_c uc tc
                                           c_u1 c_u2 cand_list sorted_axes merged_axes cur_grp g_item
                                           best_e1 best_e2 max_p1 min_p2 p1_val p2_val)
  (setq ent1 (car sel1) pk1 (cadr sel1)
        ent2 (car sel2) pk2 (cadr sel2)
        obj1 (vlax-ename->vla-object ent1)
        obj2 (vlax-ename->vla-object ent2)
        lay_src (cdr (assoc 8 (entget ent1))))

  (setq s1 (vlax-curve-getStartPoint obj1)
        e1 (vlax-curve-getEndPoint obj1))
  (if (< (num:v-dist pk1 e1) (num:v-dist pk1 s1))
    (setq tip1 e1 base1 s1)
    (setq tip1 s1 base1 e1)
  )
  (setq u1 (num:v-unit (num:v-sub tip1 base1))
        u2 (num:v-scale u1 -1.0)
        perp_dir (num:v-right-normal u1))

  (setq s2 (vlax-curve-getStartPoint obj2)
        e2 (vlax-curve-getEndPoint obj2))

  (setq ref_len (* 0.5 (+ (num:v-dist s1 e1) (num:v-dist s2 e2)))
        margin (max 250.0 (* 0.25 ref_len))
        span_min (- (min (num:v-dot s1 u1) (num:v-dot e1 u1) (num:v-dot s2 u1) (num:v-dot e2 u1)) margin)
        span_max (+ (max (num:v-dot s1 u1) (num:v-dot e1 u1) (num:v-dot s2 u1) (num:v-dot e2 u1)) margin))

  (setq t_start (num:v-dot tip1 perp_dir)
        t_end (num:v-dot s2 perp_dir)
        t_min (- (min t_start t_end) 1.0)
        t_max (+ (max t_start t_end) 1.0))

  (setq ss_all (ssget "_X" (list '(0 . "LINE,LWPOLYLINE,POLYLINE") (cons 8 lay_src))))
  (setq i 0
        cand_list '())
  (if ss_all
    (while (< i (sslength ss_all))
      (setq ent_c (ssname ss_all i)
            obj_c (vlax-ename->vla-object ent_c)
            sc (vlax-curve-getStartPoint obj_c)
            ec (vlax-curve-getEndPoint obj_c))
      (if (> (num:v-dot ec u1) (num:v-dot sc u1))
        (setq tip_c ec base_c sc)
        (setq tip_c sc base_c ec)
      )
      (setq uc (num:v-unit (num:v-sub tip_c base_c)))
      (if (> (abs (num:v-dot uc u1)) 0.95)
        (progn
          (setq tc (num:v-dot tip_c perp_dir)
                c_u1 (num:v-dot base_c u1)
                c_u2 (num:v-dot tip_c u1))
          (if (and (>= tc t_min) (<= tc t_max)
                   (>= c_u2 span_min) (<= c_u1 span_max))
            (setq cand_list (cons (list tc tip_c base_c) cand_list))
          )
        )
      )
      (setq i (1+ i))
    )
  )

  (if (<= t_start t_end)
    (setq sorted_axes (vl-sort cand_list '(lambda (a b) (< (car a) (car b)))))
    (setq sorted_axes (vl-sort cand_list '(lambda (a b) (> (car a) (car b)))))
  )

  (setq merged_axes '())
  (if sorted_axes
    (progn
      (setq cur_grp (list (car sorted_axes)))
      (foreach g_item (cdr sorted_axes)
        (if (<= (abs (- (car g_item) (caar cur_grp))) 1.0)
          (setq cur_grp (cons g_item cur_grp))
          (progn
            (setq best_e1 (cadar cur_grp)
                  best_e2 (caddar cur_grp)
                  max_p1 (num:v-dot best_e1 u1)
                  min_p2 (num:v-dot best_e2 u1))
            (foreach m (cdr cur_grp)
              (setq p1_val (num:v-dot (cadr m) u1)
                    p2_val (num:v-dot (caddr m) u1))
              (if (> p1_val max_p1) (setq max_p1 p1_val best_e1 (cadr m)))
              (if (< p2_val min_p2) (setq min_p2 p2_val best_e2 (caddr m)))
            )
            (if (>= (- max_p1 min_p2) (* 0.25 ref_len))
              (setq merged_axes (cons (list best_e1 best_e2) merged_axes))
            )
            (setq cur_grp (list g_item))
          )
        )
      )
      (setq best_e1 (cadar cur_grp)
            best_e2 (caddar cur_grp)
            max_p1 (num:v-dot best_e1 u1)
            min_p2 (num:v-dot best_e2 u1))
      (foreach m (cdr cur_grp)
        (setq p1_val (num:v-dot (cadr m) u1)
              p2_val (num:v-dot (caddr m) u1))
        (if (> p1_val max_p1) (setq max_p1 p1_val best_e1 (cadr m)))
        (if (< p2_val min_p2) (setq min_p2 p2_val best_e2 (caddr m)))
      )
      (if (>= (- max_p1 min_p2) (* 0.25 ref_len))
        (setq merged_axes (cons (list best_e1 best_e2) merged_axes))
      )
      (setq merged_axes (reverse merged_axes))
    )
  )

  (list merged_axes u1 u2)
)

(defun num:place-aligned-axis-bubbles (grp_data is_alpha txt_h rad gap lay_aks
                                       / axes u1 u2 max_p1 max_p2 ax p1 p2 d1 d2
                                         base_pts1 base_pts2 shift1 shift2
                                         orig1 orig2 row_dir s_list1 s_list2
                                         min_sep spread1 spread2 idx cur_idx lbl c_pt1 c_pt2)
  (setq axes (car grp_data)
        u1 (cadr grp_data)
        u2 (caddr grp_data)
        max_p1 -1e99
        max_p2 -1e99)

  (foreach ax axes
    (setq p1 (car ax)
          p2 (cadr ax)
          d1 (num:v-dot p1 u1)
          d2 (num:v-dot p2 u2))
    (if (> d1 max_p1) (setq max_p1 d1))
    (if (> d2 max_p2) (setq max_p2 d2))
  )

  (setq base_pts1 '()
        base_pts2 '())
  (foreach ax axes
    (setq p1 (car ax)
          p2 (cadr ax)
          shift1 (+ (- max_p1 (num:v-dot p1 u1)) gap rad)
          shift2 (+ (- max_p2 (num:v-dot p2 u2)) gap rad)
          base_pts1 (cons (num:v-add p1 (num:v-scale u1 shift1)) base_pts1)
          base_pts2 (cons (num:v-add p2 (num:v-scale u2 shift2)) base_pts2))
  )
  (setq base_pts1 (reverse base_pts1)
        base_pts2 (reverse base_pts2))

  (setq orig1 (car base_pts1)
        orig2 (car base_pts2))
  (if (and (> (length base_pts1) 1) (> (num:v-dist orig1 (last base_pts1)) 1e-6))
    (setq row_dir (num:v-unit (num:v-sub (last base_pts1) orig1)))
    (setq row_dir (num:v-right-normal u1))
  )

  (setq s_list1 (mapcar '(lambda (pt) (num:v-dot (num:v-sub pt orig1) row_dir)) base_pts1)
        s_list2 (mapcar '(lambda (pt) (num:v-dot (num:v-sub pt orig2) row_dir)) base_pts2)
        min_sep (+ (* 2.0 rad) 6.0)
        spread1 (num:spread-1d-positions s_list1 min_sep)
        spread2 (num:spread-1d-positions s_list2 min_sep))

  (setq idx 0
        cur_idx 1)
  (while (< idx (length axes))
    (setq lbl (if is_alpha (num:num->alpha cur_idx) (itoa cur_idx))
          c_pt1 (num:v-add orig1 (num:v-scale row_dir (nth idx spread1)))
          c_pt2 (num:v-add orig2 (num:v-scale row_dir (nth idx spread2))))

    (num:make-circle c_pt1 rad lay_aks 2)
    (num:make-text c_pt1 lbl txt_h lay_aks 3)

    (num:make-circle c_pt2 rad lay_aks 2)
    (num:make-text c_pt2 lbl txt_h lay_aks 3)

    (setq idx (1+ idx)
          cur_idx (1+ cur_idx))
  )
)

(defun num:run-axes (/ lay_aks sel1_a sel2_a grp1 sel1_b sel2_b grp2
                       txt_h rad gap msg_cnt)
  (setq lay_aks (num:resolve-layer "n-aks"))

  (setq sel1_a (num:pick-entity "\nHarf aksları (A, B, C...) için İLK aks çizgisini seçin: " nil))
  (if sel1_a
    (progn
      (setq sel2_a (num:pick-entity "\nHarf aksları için SON aks çizgisini seçin: " nil))
      (if sel2_a
        (progn
          (setq grp1 (num:collect-axis-group sel1_a sel2_a))

          (setq sel1_b (num:pick-entity "\nSayı aksları (1, 2, 3...) için İLK aks çizgisini seçin [Atlamak için Enter]: " t))
          (if sel1_b
            (progn
              (setq sel2_b (num:pick-entity "\nSayı aksları için SON aks çizgisini seçin: " nil))
              (if sel2_b
                (setq grp2 (num:collect-axis-group sel1_b sel2_b))
              )
            )
          )

          (if (< (length (car grp1)) 1)
            (princ "\nHarf aksları aralığında uygun aks çizgisi bulunamadı.")
            (progn
              (setq txt_h 25.0
                    rad 31.25
                    gap 15.0)

              (num:place-aligned-axis-bubbles grp1 t txt_h rad gap lay_aks)
              (setq msg_cnt (length (car grp1)))

              (if (and grp2 (> (length (car grp2)) 0))
                (progn
                  (num:place-aligned-axis-bubbles grp2 nil txt_h rad gap lay_aks)
                  (setq msg_cnt (+ msg_cnt (length (car grp2))))
                )
              )

              (princ (strcat "\nToplam " (itoa msg_cnt) " aksın her iki ucu [" lay_aks "] layerinde etiketlendi."))
            )
          )
        )
      )
    )
  )
)

;;; --- MOD 3: ARDIŞIK HARF / SAYI YERLEŞTİRME ---

(defun num:run-sequential (/ lay_seq sel_ax ent_ax obj_ax valid_ax
                             str_in is_alpha cur_val step_in step_val txt_h
                             pt_raw cp1 vec1 off_dist param1 deriv1 tang1 norm1 side_sign
                             cp_k param_k deriv_k tang_k norm_k pt_place lbl cnt)
  (setq lay_seq (num:resolve-layer "n-ardisik")
        txt_h (num:get-layer-text-h lay_seq 15.0))

  ;; 1. Adım: Hizalama aksını seçtir (mavi vurgu ile)
  (setq sel_ax (num:pick-entity "\nHizalama aksını seçin [Serbest yerleşim için Enter]: " t))
  (if sel_ax
    (progn
      (setq ent_ax (car sel_ax)
            obj_ax (vlax-ename->vla-object ent_ax))
      (if (vl-catch-all-error-p (vl-catch-all-apply 'vlax-curve-getEndParam (list obj_ax)))
        (progn
          (princ "\nSeçilen nesne geçerli bir eğri/çizgi değil, serbest yerleşime geçiliyor.")
          (setq obj_ax nil)
        )
        (setq valid_ax t)
      )
    )
  )

  ;; 2. Adım: Başlangıç değerini al (Harf ise direkt B, C... devam eder; Sayı ise artış miktarı sorar)
  (setq str_in (getstring "\nBaşlangıç değeri (Harf veya Sayı) <A>: "))
  (cond
    ((= str_in "")
     (setq is_alpha t
           cur_val 1
           step_val 1))
    ((num:alpha->num str_in)
     (setq is_alpha t
           cur_val (num:alpha->num str_in)
           step_val 1))
    ((num:signed-int-p str_in)
     (setq is_alpha nil
           cur_val (atoi str_in))
     (setq step_in (getstring "\nKaçar kaçar artsın? <1>: "))
     (if (num:signed-int-p step_in)
       (setq step_val (atoi step_in))
       (setq step_val 1)
     ))
    (t
     (princ "\nGeçersiz giriş, varsayılan 'A' ile devam ediliyor.")
     (setq is_alpha t
           cur_val 1
           step_val 1))
  )

  ;; 3. Adım: Tıklanan noktalara aksa hizalı şekilde sırayla yerleştir
  (setq cnt 0
        off_dist nil
        side_sign 1.0)
  (while (setq pt_raw (getpoint (strcat "\n["
                                        (if is_alpha (num:num->alpha (max 1 cur_val)) (itoa cur_val))
                                        "] için nokta seçin [Bitirmek için Enter]: ")))
    (if valid_ax
      (progn
        (if (null off_dist)
          (progn
            ;; İlk tıklamada aksa olan dik mesafeyi ve yönü kilitle
            (setq cp1 (vlax-curve-getClosestPointTo obj_ax pt_raw)
                  vec1 (num:v-sub pt_raw cp1)
                  off_dist (num:v-len vec1))
            (if (>= off_dist 1e-4)
              (progn
                (setq param1 (vlax-curve-getParamAtPoint obj_ax cp1)
                      deriv1 (vlax-curve-getFirstDeriv obj_ax param1)
                      tang1 (num:v-unit deriv1)
                      norm1 (num:v-right-normal tang1))
                (setq side_sign (if (>= (num:v-dot vec1 norm1) 0.0) 1.0 -1.0))
              )
              (setq off_dist 0.0)
            )
          )
        )
        ;; Sonraki tüm tıklamaları aynı aks hizasında tut
        (setq cp_k (vlax-curve-getClosestPointTo obj_ax pt_raw)
              param_k (vlax-curve-getParamAtPoint obj_ax cp_k)
              deriv_k (vlax-curve-getFirstDeriv obj_ax param_k)
              tang_k (num:v-unit deriv_k)
              norm_k (num:v-right-normal tang_k)
              pt_place (num:v-add cp_k (num:v-scale norm_k (* side_sign off_dist))))
      )
      (setq pt_place pt_raw)
    )

    (setq lbl (if is_alpha (num:num->alpha (max 1 cur_val)) (itoa cur_val)))
    (num:make-text pt_place lbl txt_h lay_seq nil)

    (setq cur_val (+ cur_val step_val)
          cnt (1+ cnt))
  )

  (if (> cnt 0)
    (princ (strcat "\n" (itoa cnt) " adet ardışık etiket [" lay_seq "] layerinde yerleştirildi."))
  )
)

;;; --- ANA KOMUT ---

(defun c:NUM (/ doc old_err mode)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq old_err *error*)
  (defun *error* (msg)
    (num:clear-highlights)
    (if doc (vl-catch-all-apply 'vla-EndUndoMark (list doc)))
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\nHata: " msg))
    )
    (setq *error* old_err)
    (princ)
  )

  (vla-StartUndoMark doc)

  (initget "Merdiven Aks Ardisik")
  (setq mode (getkword "\nNumaralandırma türü seçin [Merdiven/AKs/ARdisik] <Merdiven>: "))
  (if (not mode) (setq mode "Merdiven"))

  (cond
    ((= mode "Merdiven") (num:run-stairs))
    ((= mode "Aks") (num:run-axes))
    ((= mode "Ardisik") (num:run-sequential))
  )

  (num:clear-highlights)
  (vla-EndUndoMark doc)
  (setq *error* old_err)
  (princ)
)

(defun c:NUMARALA () (c:NUM))

(princ "\n[NUM] veya [NUMARALA] komutu yüklendi.")
(princ)