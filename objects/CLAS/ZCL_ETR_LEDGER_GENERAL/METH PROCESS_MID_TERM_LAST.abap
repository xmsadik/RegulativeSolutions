  METHOD process_mid_term_last.
    DATA: ls_oldef TYPE zetr_t_oldef,
          ls_defcl TYPE zetr_t_defcl,
          lv_datab TYPE datab,
          lv_datbi TYPE datbi.

    " Initialize result
    CLEAR rs_result.

    " Validate input parameters
    IF validate_input( is_params ) = abap_false.
      rs_result = VALUE #( type = 'E' message = 'Input validation failed' ).
      RETURN.
    ENDIF.

    " Calculate dates
    lv_datab = |{ is_params-gjahr }{ is_params-monat }01|.

    last_day_of_months(
       EXPORTING
         day_in            = lv_datab
       RECEIVING
         last_day_of_month = lv_datbi
     ).

    " Prepare and insert OLDEF record
    ls_oldef = VALUE #(
      bukrs  = is_params-bukrs
      bcode  = is_params-bcode
      gjahr  = is_params-gjahr
      monat  = is_params-monat
      datbi  = lv_datbi
      partn  = '0'
      parno  = is_params-parno
      datab  = lv_datab
      syevno = '1'
      eyevno = is_params-eyevno
      slinen = '1'
      elinen = is_params-elinen
      debit  = 1
      credit = 1
      pdatab = lv_datab
      pdatbi = lv_datbi
      serok  = abap_true
      yevok  = abap_true
      yvbok  = abap_true
      kebok  = abap_true
      kbbok  = abap_true
      gbyok  = abap_true
      derok  = abap_true
      gbkok  = abap_true
      ernam  = sy-uname
      erdat  = sy-datum
      erzet  = sy-uzeit
      currency = 'TRY'
    ).

    " Prepare and insert DEFCL record
    ls_defcl = VALUE #(
      bukrs = is_params-bukrs
      gjahr = is_params-gjahr
      monat = is_params-monat
      ernam  = sy-uname
      erdat  = sy-datum
      erzet  = sy-uzeit
      elprc = abap_true
      stldr = abap_true
      etldr = abap_true
      stsds = abap_true
      etsds = abap_true
      sgbsn = abap_true
      egbsn = abap_true
    ).

    " Database operations
    INSERT zetr_t_oldef FROM @ls_oldef.
    IF sy-subrc = 0.
      INSERT zetr_t_defcl FROM @ls_defcl.
      IF sy-subrc = 0.
        rs_result = VALUE #( type = 'S' message = 'Process completed successfully' ).
      ELSE.
        rs_result = VALUE #( type = 'E' message = 'Error in DEFCL insert' ).
      ENDIF.
    ELSE.
      rs_result = VALUE #( type = 'E' message = 'Error in OLDEF insert' ).
    ENDIF.

  ENDMETHOD.