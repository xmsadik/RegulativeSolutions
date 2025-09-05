  METHOD init_global_data.
    CLEAR:gs_t001,gv_bukrs,gv_bukrs_tmp,gv_bcode,gv_gjahr,
          gv_gjahr_buk,gv_monat,gv_monat_buk,gt_return,gt_return[],
          gs_bukrs,gv_waers,gv_alternative,
          gs_params,gv_tasfiye,gv_tsfy_durum,gt_skb1,gt_skb1[],
          gt_blart,gt_blart[],gv_f51_blart,gv_f51_tcode, gt_smm,
          gv_datbi,gv_datab,gt_ledger_part,gt_ledger_part[],
          gv_partn_n,gv_partn,gt_hspplan,gt_hspplan[],gt_skat,gt_skat[],
          gv_partial,gv_month_last, gt_check_ref,
          gr_budat,gr_budat[],gr_belnr,gr_belnr[],gr_gsber,gr_gsber[],
          gr_bstat,gr_bstat[],gr_ldgrp,gr_ldgrp[],gr_hkont,gr_hkont[],
          gr_blart,gr_blart[],gv_ledger, gt_ledger, gt_ledger[], gs_bcode,
          gr_ablart,gr_ablart[],gr_kblart,gr_kblart[], gs_defter, gt_username.
    gv_bukrs = iv_bukrs.
  ENDMETHOD.