  METHOD if_oo_adt_classrun~main.
    TRY.
        zcl_etr_invoice_operations=>factory( '3000' )->update_einvoice_users4(
*          EXPORTING
*            iv_db_write = abap_true
*          RECEIVING
*            rt_list     =
        ).
*        CATCH zcx_etr_regulative_exception.
      CATCH cx_root INTO DATA(lx_root).
        out->write( |Error : { lx_root->get_text( ) }| ).
    ENDTRY.
*    SELECT SINGLE bukrs
*      FROM zetr_t_cmpin
*      INTO @DATA(lv_exists).
*    IF lv_exists IS NOT INITIAL.
*      TRY.
*          DATA: ls_job_info TYPE cl_apj_rt_api=>ty_job_info.
*          GET TIME STAMP FIELD DATA(lv_timestamp).
*          TRY.
*              DATA(lv_timezone) = cl_abap_context_info=>get_user_time_zone( ).
*            CATCH cx_abap_context_info_error.
*          ENDTRY.
*          lv_timestamp += 1000.
*          cl_apj_rt_api=>schedule_job_commit_immstart(
*            EXPORTING
*                iv_job_template_name = 'ZETR_AJT_INVOICE_USERS'
*                iv_job_text = 'eSolutions - Invoice Users'
*                is_start_info = VALUE #( start_immediately = abap_true )
*            IMPORTING
*              ev_jobname = DATA(lv_jobname)
*              ev_jobcount = DATA(lv_jobcount)
*              et_message = DATA(lt_message) ).
**          cl_apj_rt_api=>schedule_job(
**            EXPORTING
**                iv_job_template_name = 'ZETR_AJT_INVOICE_USERS'
**                iv_job_text = 'eSolutions - Invoice Users'
**                is_start_info = VALUE #( timestamp = lv_timestamp )
**                is_scheduling_info = VALUE #( periodic_granularity = abap_false
**                                              timezone = lv_timezone )
**                it_job_parameter_value = VALUE #( ( name = 'P_BUKRS'
**                                                    t_value = VALUE #( ( sign = 'I'
**                                                                         option = 'EQ'
**                                                                         low = lv_exists ) ) ) )
**            IMPORTING
**              ev_jobname = DATA(lv_jobname)
**              ev_jobcount = DATA(lv_jobcount)
**              et_message = DATA(lt_message) ).
*          IF lv_jobname IS INITIAL OR line_exists( lt_message[ type = 'E' ] ).
*            LOOP AT lt_message INTO DATA(ls_message).
*              MESSAGE ID ls_message-id TYPE ls_message-type NUMBER ls_message-number
*                      WITH ls_message-message_v1 ls_message-message_v2 ls_message-message_v3 ls_message-message_v4
*                      INTO ls_message-message.
*              out->write( ls_message-message ).
*            ENDLOOP.
*          ELSE.
*            ls_job_info = cl_apj_rt_api=>get_job_details( iv_jobname = lv_jobname
*                                                          iv_jobcount = lv_jobcount ).
*            MESSAGE ID 'ZETR_COMMON' TYPE 'E' NUMBER '082'
*                    INTO ls_message-message.
*            out->write( ls_message-message ).
*          ENDIF.
*        CATCH cx_apj_rt INTO DATA(lx_regulative_exception).
*          MESSAGE ID 'ZETR_COMMON' TYPE 'E' NUMBER '000' WITH lx_regulative_exception->get_text( )
*                  INTO ls_message-message.
*          out->write( ls_message-message ).
*      ENDTRY.
*    ELSE.
*      MESSAGE ID 'ZETR_COMMON' TYPE 'E' NUMBER '001'
*              INTO ls_message-message.
*      out->write( ls_message-message ).
*    ENDIF.
*
*
*
    IF 1 = 2.        " Example of using the class

      DATA: gr_budat TYPE RANGE OF datum,
            gr_belnr TYPE RANGE OF belnr_d.
      TYPES: BEGIN OF ty_budat,
               sign   TYPE c LENGTH 1,
               option TYPE c LENGTH 2,
               low    TYPE datum,
               high   TYPE datum,
             END OF ty_budat.
      DATA: lr_budat TYPE ty_budat.
      DATA(ledger_general) = NEW zcl_etr_ledger_general( ).
      DATA: mv_bukrs TYPE bukrs,
            mv_monat TYPE monat,
            mv_gjahr TYPE gjahr.
      " Prepare date range
      lr_budat-option = 'BT'.
      lr_budat-sign = 'I'.

      " Calculate first and last day of the month
      DATA: lv_first_day TYPE datum,
            lv_last_day  TYPE datum.
      mv_bukrs = '1000'.
      mv_monat = '02'.
      mv_gjahr = '2025'.
      CONCATENATE mv_gjahr mv_monat '01' INTO lv_first_day.

      ledger_general->delete_eledger_logs(
        EXPORTING
          iv_bukrs      = mv_bukrs
          iv_gjahr      = mv_gjahr
          iv_monat      = mv_monat
        RECEIVING
          rv_is_deleted = DATA(lv_is_deleted)
      ).

      DATA: ls_record TYPE zetr_t_defcl.

      ledger_general->read_and_update_eledger_record(
        EXPORTING
          iv_bukrs      = mv_bukrs
          iv_gjahr      = mv_gjahr
          iv_monat      = mv_monat
        CHANGING
          cs_record     = ls_record
        RECEIVING
          rv_is_updated = DATA(lv_is_updated)
      ).

      ledger_general->set_gib_confirmation_flags(
        EXPORTING
          iv_bukrs      = mv_bukrs
          iv_bcode      = space
          iv_gjahr      = mv_gjahr
          iv_monat      = mv_monat
  RECEIVING
    rv_is_updated = DATA(lv_is_updated2)
      ).


*      CHECK 1 = 2.
*      ledger_general->generate_ledger_data(
*        EXPORTING
*          i_bukrs  = mv_bukrs
*          i_bcode  = space
*          i_tsfyd  = space
*          i_ledger = space
*          tr_budat = gr_budat
*          tr_belnr = gr_belnr
*        IMPORTING
*          te_return = DATA(lt_return)
*          te_ledger = DATA(ledger) " Can be removed if not used later
*      ).
*
*      ledger_general->set_ledger_partial(
*        i_bukrs = mv_bukrs
*        i_monat = mv_monat
*        i_gjahr = mv_gjahr
*      ).
*      " Consider checking for errors from set_ledger_partial if possible
*
*      ledger_general->process_xml_data(
*        EXPORTING
*          iv_bukrs       = mv_bukrs
*          it_budat_range = gr_budat
*        IMPORTING
*          ev_subrc       = DATA(lv_subrc)
*          " et_return      = DATA(lt_xml_return) " Add if process_xml_data returns messages
*      ).
*
*      CHECK 1 = 2.
*      ledger_general->delete_ledger(
*        EXPORTING
*          iv_bukrs  = mv_bukrs
*          iv_gjahr  = mv_gjahr
*          iv_monat  = mv_monat
*        RECEIVING
*          rs_return = DATA(lt_return2)
*      ).
*
    ENDIF.

  ENDMETHOD.