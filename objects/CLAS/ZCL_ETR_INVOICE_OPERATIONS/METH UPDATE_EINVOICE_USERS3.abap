  METHOD update_einvoice_users3.
    SELECT taxid, aliass
      FROM zetr_t_inv_ruser
      WHERE defal = @abap_true
      INTO TABLE @DATA(lt_default_aliases).
    SORT lt_default_aliases BY taxid aliass.

    DELETE FROM zetr_t_inv_ruser.

    zcl_etr_einvoice_ws=>factory( mv_company_code )->download_registered_taxpayers3( ).

    LOOP AT lt_default_aliases INTO DATA(ls_default_aliases).
      UPDATE zetr_t_inv_ruser
          SET defal = @abap_true
          WHERE taxid = @ls_default_aliases-taxid
            AND aliass = @ls_default_aliases-aliass.
    ENDLOOP.
  ENDMETHOD.