  METHOD set_ledger_partial.
    DATA: gs_budat  TYPE RANGE OF datum, "fud_s_budat_range,
          gs_ledval TYPE zetr_t_defcl.

    gv_bukrs = i_bukrs .
    gv_GJAHR = i_gjahr .
    gv_monat = i_monat .

    CLEAR: gv_subrc.

    me->calculate_budat_range(
      EXPORTING
        iv_gjahr = gv_gjahr
        iv_monat = gv_monat
      IMPORTING
        et_budat = gr_budat
    ).


    me->get_company( ).
    me->set_date( ).
    me->set_blart( ).

    me->modify_partial_ledger( ).


  ENDMETHOD.