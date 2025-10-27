  METHOD validate_prerequisites.
    " Defter zaten var mı?
    SELECT SINGLE bukrs
      FROM zetr_t_defcl
      WHERE bukrs = @mv_bukrs
        AND gjahr = @mv_gjahr
        AND monat = @mv_monat
      INTO @DATA(lv_exists).

    IF lv_exists IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_etr_regulative_exception
        MESSAGE e114(zetr_edf_msg) WITH mv_bukrs mv_gjahr mv_monat.
    ENDIF.
  ENDMETHOD.