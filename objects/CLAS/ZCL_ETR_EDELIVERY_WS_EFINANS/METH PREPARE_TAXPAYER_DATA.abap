  METHOD prepare_taxpayer_data.
    DATA ls_taxpayer TYPE zetr_t_inv_ruser.
    CLEAR ls_taxpayer.
    CASE is_user-tip.
      WHEN 'Özel'.
        ls_taxpayer-txpty = 'OZEL'.
      WHEN OTHERS.
        ls_taxpayer-txpty = 'KAMU'.
    ENDCASE.
    IF is_user-kayitzamani IS NOT INITIAL.
      ls_taxpayer-regdt = is_user-kayitzamani(8).
      ls_taxpayer-regtm = is_user-kayitzamani+8(6).
    ENDIF.
    ls_taxpayer-title = is_user-unvan.
    ls_taxpayer-taxid = is_user-vkntckn.
    IF is_user-aktifetiket IS NOT INITIAL.
      LOOP AT is_user-aktifetiket INTO DATA(ls_alias).
        ls_taxpayer-aliass = ls_alias-etiket.
        APPEND ls_taxpayer TO ct_list.
      ENDLOOP.
    ELSE.
      APPEND ls_taxpayer TO ct_list.
    ENDIF.
  ENDMETHOD.