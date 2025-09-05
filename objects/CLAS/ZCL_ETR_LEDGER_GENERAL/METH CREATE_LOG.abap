  METHOD create_log.
    " Local variables
    DATA: ls_log     TYPE zetr_t_dfcll,
          ls_bapiret TYPE bapiret2.

    " Get current time
*    GET TIME.

    " Fill BAPI return structure
    ls_bapiret-id        = iv_msgid.
    ls_bapiret-type      = iv_msgty.
    ls_bapiret-number    = iv_msgno.
    ls_bapiret-message_v1 = iv_msgv1.
    ls_bapiret-message_v2 = iv_msgv2.
    ls_bapiret-message_v3 = iv_msgv3.
    ls_bapiret-message_v4 = iv_msgv4.

    " Fill log structure
    ls_log-datum = sy-datum."xco_cp=>sy->date( xco_cp_time=>time_zone->user )->as( xco_cp_time=>format->iso_8601_extended )->value.
    ls_log-uzeit = sy-uzeit."xco_cp=>sy->time( xco_cp_time=>time_zone->user )->as( xco_cp_time=>format->iso_8601_extended )->value.
    ls_log-bukrs = iv_bukrs.
    ls_log-bcode = iv_bcode.
    ls_log-gjahr = iv_gjahr.
    ls_log-monat = iv_monat.
    ls_log-mstyp = iv_msgty.
    ls_log-partn = iv_partn.

    " Set message text - either provided or generated from message class
    IF iv_lgmes IS NOT INITIAL.
      ls_log-lgmes = condense( iv_lgmes ).
    ELSE.
      " Form message from message class
      MESSAGE ID ls_bapiret-id
              TYPE 'S'
              NUMBER ls_bapiret-number
              WITH ls_bapiret-message_v1 ls_bapiret-message_v2
                   ls_bapiret-message_v3 ls_bapiret-message_v4
              INTO ls_log-lgmes.
    ENDIF.

    " Get next number for log
    TRY.
        " Use number range object service in ABAP Cloud

        DATA: lo_nr_service TYPE REF TO cl_numberrange_runtime.

        lo_nr_service = NEW cl_numberrange_runtime( ).

        lo_nr_service->number_get(
          EXPORTING
            nr_range_nr = '01'
            object      = 'ZETR_LOG'
          IMPORTING
            number   = DATA(lv_number)
        ).

        ls_log-lgsno = lv_number.


      CATCH cx_number_ranges INTO DATA(lx_nr_error).
        " Handle number range error
        " In case of error, we'll still attempt to insert the log without a number
    ENDTRY.

    " Insert log record
    INSERT zetr_t_dfcll FROM @ls_log.
  ENDMETHOD.