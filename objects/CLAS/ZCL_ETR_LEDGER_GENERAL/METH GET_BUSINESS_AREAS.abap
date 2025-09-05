  METHOD get_business_areas.
    CLEAR:gr_gsber,gr_gsber[].

    SELECT *
      FROM zetr_t_srisa
     WHERE bukrs = @gv_bukrs
       AND bcode = @gv_bcode
     INTO TABLE @DATA(lt_bareas).

    IF gv_bcode IS NOT INITIAL.
      IF gs_bcode-sub_gsber IS NOT INITIAL.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = gs_bcode-sub_gsber ) TO gr_gsber.
      ENDIF.
    ENDIF.

    LOOP AT lt_bareas INTO DATA(ls_bareas).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_bareas-gsber ) TO gr_gsber.
    ENDLOOP.

    SORT gr_gsber BY low.
    DELETE ADJACENT DUPLICATES FROM gr_gsber COMPARING low.
  ENDMETHOD.