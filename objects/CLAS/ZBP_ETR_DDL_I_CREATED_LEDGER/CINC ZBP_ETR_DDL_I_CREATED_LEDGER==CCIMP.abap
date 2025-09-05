CLASS lhc_ledgerparts DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR LedgerParts RESULT result.

    METHODS LoadExcelContent FOR MODIFY
      IMPORTING keys FOR ACTION LedgerParts~LoadExcelContent RESULT result.

ENDCLASS.

CLASS lhc_ledgerparts IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD LoadExcelContent.



    " Get the first parameter record
    DATA(ls_params) = VALUE #( keys[ 1 ]-%key DEFAULT VALUE #( ) ).




  ENDMETHOD.

ENDCLASS.

CLASS lhc_zetr_ddl_i_created_ledger DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR CreatedLedger RESULT result.

    METHODS create_ledger FOR MODIFY
      IMPORTING keys FOR ACTION CreatedLedger~create_ledger.
    METHODS delete_ledger FOR MODIFY
      IMPORTING keys FOR ACTION CreatedLedger~delete_ledger RESULT result.

    METHODS send_ledger FOR MODIFY
      IMPORTING keys FOR ACTION CreatedLedger~send_ledger RESULT result.

    METHODS resend_ledger FOR MODIFY
      IMPORTING keys FOR ACTION CreatedLedger~resend_ledger RESULT result.

ENDCLASS.

CLASS lhc_zetr_ddl_i_created_ledger IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD create_ledger.
*    " Data declarations
*    DATA: ls_reported TYPE RESPONSE FOR REPORTED zetr_ddl_i_created_ledger,
*          gr_budat    TYPE RANGE OF datum,
*          gr_belnr    TYPE RANGE OF belnr_d.
*
*    TYPES: BEGIN OF ty_budat,
*             sign   TYPE c LENGTH 1,
*             option TYPE c LENGTH 2,
*             low    TYPE datum,
*             high   TYPE datum,
*           END OF ty_budat.
*    DATA: lr_budat TYPE ty_budat.
*
*    " Get the first parameter record
*    DATA(ls_params) = VALUE #( keys[ 1 ]-%param DEFAULT VALUE #( ) ).
*
*    " Simple validations for initial values
*    IF ls_params-CompanyCode IS INITIAL.
*      APPEND VALUE #( %msg = new_message_with_text(
*                       severity = if_abap_behv_message=>severity-error
*                       text     = 'Please fill Company Code' )
*                     %element-bukrs = if_abap_behv=>mk-on )
*        TO ls_reported-createdledger.
*    ENDIF.
*
*    IF ls_params-FiscalYear IS INITIAL.
*      APPEND VALUE #( %msg = new_message_with_text(
*                       severity = if_abap_behv_message=>severity-error
*                       text     = 'Please fill Fiscal Year' )
*                     %element-gjahr = if_abap_behv=>mk-on )
*        TO ls_reported-createdledger.
*    ENDIF.
*
*    IF ls_params-FinancialPeriod IS INITIAL.
*      APPEND VALUE #( %msg = new_message_with_text(
*                       severity = if_abap_behv_message=>severity-error
*                       text     = 'Please fill Fiscal Period' )
*                     %element-monat = if_abap_behv=>mk-on )
*        TO ls_reported-createdledger.
*    ENDIF.
*
*    " Check for existing records in defcl table
*    SELECT SINGLE @abap_true
*      FROM zetr_t_defcl
*      WHERE bukrs = @ls_params-CompanyCode
*        AND gjahr = @ls_params-FiscalYear
*        AND monat = @ls_params-FinancialPeriod
*      INTO @DATA(lv_defcl_exists).
*
*    IF lv_defcl_exists IS NOT INITIAL.
*      APPEND VALUE #( %msg = new_message_with_text(
*                       severity = if_abap_behv_message=>severity-error
*                       text     = 'Girilen Mali Yıl ve Döneme Ait Kayıt Mevcut, Kayıt oluşturalamaz' )
*                     %element-bukrs = if_abap_behv=>mk-on
*                     %element-gjahr = if_abap_behv=>mk-on
*                     %element-monat = if_abap_behv=>mk-on )
*        TO ls_reported-createdledger.
*    ENDIF.
*
*    " If any validation failed, return messages and exit
*    IF ls_reported IS NOT INITIAL.
*      reported = CORRESPONDING #( DEEP ls_reported ).
*      RETURN.
*    ENDIF.
*
*    " If validation passed, proceed with ledger generation
*    IF ls_reported IS INITIAL.
*
*      " Create instance of ledger generation class
*      DATA(ledger_general) = NEW zcl_etr_ledger_general( ).
*
*      " Prepare date range based on fiscal year and period
*      lr_budat-option = 'BT'.
*      lr_budat-sign = 'I'.
*
*      " Calculate first and last day of the month
*      DATA: lv_first_day TYPE datum,
*            lv_last_day  TYPE datum.
*
*      CONCATENATE ls_params-FiscalYear ls_params-FinancialPeriod '01' INTO lv_first_day.
*
*      ledger_general->last_day_of_months(
*        EXPORTING
*          day_in            = lv_first_day
*        RECEIVING
*          last_day_of_month = lv_last_day
*      ).
*
*      lr_budat-low  = lv_first_day.
*      lr_budat-high = lv_last_day.
*      APPEND lr_budat TO gr_budat.
*
*      " Generate ledger data
*      TRY.
*          ledger_general->generate_ledger_data(
*            EXPORTING
*              i_bukrs  = ls_params-CompanyCode
*              i_bcode  = space
*              i_tsfyd  = space
*              i_ledger = space
*              tr_budat = gr_budat
*              tr_belnr = gr_belnr
*            IMPORTING
*              te_return = DATA(lt_return)
*              te_ledger = DATA(ledger)
*          ).
*
*          ledger_general->set_ledger_partial(
*            i_bukrs = ls_params-CompanyCode
*            i_monat = ls_params-FinancialPeriod
*            i_gjahr = ls_params-FiscalYear
*          ).
*
*          ledger_general->process_xml_data(
*            EXPORTING
*              iv_bukrs       = ls_params-CompanyCode
**              iv_tsfyd       =
**              iv_auto        =
*              it_budat_range = gr_budat
*            IMPORTING
*              ev_subrc       = DATA(lv_subrc)
*          ).
*
*
*          " Process BAPIRET2 messages
*          LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<fs_return>).
*            CASE <fs_return>-type.
*              WHEN 'E' OR 'A'. " Error or Abort
*                APPEND VALUE #( %msg = new_message_with_text(
*                               severity = if_abap_behv_message=>severity-error
*                               text     = <fs_return>-message )
*                             %element-bukrs = if_abap_behv=>mk-on )
*                  TO ls_reported-createdledger.
*
*              WHEN 'W'. " Warning
*                APPEND VALUE #( %msg = new_message_with_text(
*                               severity = if_abap_behv_message=>severity-warning
*                               text     = <fs_return>-message )
*                             %element-bukrs = if_abap_behv=>mk-on )
*                  TO ls_reported-createdledger.
*
*              WHEN 'I' OR 'S'. " Information or Success
*                APPEND VALUE #( %msg = new_message_with_text(
*                               severity = if_abap_behv_message=>severity-success
*                               text     = <fs_return>-message )
*                             %element-bukrs = if_abap_behv=>mk-on )
*                  TO ls_reported-createdledger.
*            ENDCASE.
*          ENDLOOP.
*
*          " If no messages were returned, add a default success message
*          IF lt_return IS INITIAL.
*            APPEND VALUE #( %msg = new_message_with_text(
*                           severity = if_abap_behv_message=>severity-success
*                           text     = |Ledger generated successfully for Company Code { ls_params-CompanyCode }| )
*                         %element-bukrs = if_abap_behv=>mk-on )
*              TO ls_reported-createdledger.
*          ENDIF.
*
*        CATCH cx_root INTO DATA(lx_error).
*          " Handle any exceptions
*          APPEND VALUE #( %msg = new_message_with_text(
*                         severity = if_abap_behv_message=>severity-error
*                         text     = lx_error->get_text( ) )
*                       %element-bukrs = if_abap_behv=>mk-on )
*            TO ls_reported-createdledger.
*      ENDTRY.
*    ENDIF.
*
*    " Return messages
*    reported = CORRESPONDING #( DEEP ls_reported ).

    DATA: lt_job_parameter     TYPE cl_apj_rt_api=>tt_job_parameter_value,
          lv_job_text          TYPE cl_apj_rt_api=>ty_job_text,
          lv_has_error_message TYPE abap_boolean.

    CHECK keys IS NOT INITIAL.
    DATA(ls_key_single) = keys[ 1 ].
    DATA(ls_params) = ls_key_single-%param.

    " --- Senkron Doğrulamalar ---
    IF ls_params-CompanyCode IS INITIAL.
      APPEND VALUE #( %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'Lütfen Şirket Kodunu doldurun' )
                      %element-bukrs = if_abap_behv=>mk-on )
        TO reported-createdledger.
    ENDIF.

    IF ls_params-FiscalYear IS INITIAL.
      APPEND VALUE #( %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'Lütfen Mali Yılı doldurun' )
                      %element-gjahr = if_abap_behv=>mk-on )
        TO reported-createdledger.
    ENDIF.

    IF ls_params-FinancialPeriod IS INITIAL.
      APPEND VALUE #( %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'Lütfen Mali Dönemi doldurun' )
                      %element-monat = if_abap_behv=>mk-on )
        TO reported-createdledger.
    ENDIF.

    " defcl ön kontrolü
    IF ls_params-CompanyCode IS NOT INITIAL AND ls_params-FiscalYear IS NOT INITIAL AND ls_params-FinancialPeriod IS NOT INITIAL.
      SELECT SINGLE @abap_true FROM zetr_t_defcl
             WHERE bukrs = @ls_params-CompanyCode AND gjahr = @ls_params-FiscalYear AND monat = @ls_params-FinancialPeriod
             INTO @DATA(lv_defcl_exists).
      IF lv_defcl_exists IS NOT INITIAL.
        APPEND VALUE #( %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'Girilen Mali Yıl ve Döneme Ait Kayıt Mevcut, Kayıt oluşturalamaz (İş gönderilmedi)' )
                        %element-bukrs = if_abap_behv=>mk-on %element-gjahr = if_abap_behv=>mk-on %element-monat = if_abap_behv=>mk-on )
         TO reported-createdledger.
      ENDIF.
    ENDIF.

    " Herhangi bir doğrulama başarısızsa çık
    IF reported IS NOT INITIAL AND reported-createdledger IS NOT INITIAL.
      APPEND VALUE #( %tky = ls_key_single-%param-CompanyCode ) TO failed-createdledger. " %tky'nin ls_key_single'dan gelmesi gerekir
      RETURN.
    ENDIF.

    " --- Arka Plan İşini Gönder ---
    TRY.
        " İş Parametrelerini Hazırla
        lt_job_parameter = VALUE cl_apj_rt_api=>tt_job_parameter_value(
            ( name = 'P_BUKRS' t_value = VALUE cl_apj_rt_api=>tt_value_range( ( sign = 'I' option = 'EQ' low = ls_params-CompanyCode ) ) )
            ( name = 'P_GJAHR' t_value = VALUE cl_apj_rt_api=>tt_value_range( ( sign = 'I' option = 'EQ' low = ls_params-FiscalYear ) ) )
            ( name = 'P_MONAT' t_value = VALUE cl_apj_rt_api=>tt_value_range( ( sign = 'I' option = 'EQ' low = ls_params-FinancialPeriod ) ) )
        ).

        " İş Metnini Hazırla
        lv_job_text = |E-Defter Oluşturma: { ls_params-CompanyCode }/{ ls_params-FiscalYear }/{ ls_params-FinancialPeriod }|.

        " İşi Zamanla
        cl_apj_rt_api=>schedule_job(
          EXPORTING
            iv_job_template_name = 'ZETR_ELEDGER_CREATION_JT'
            iv_job_text          = lv_job_text
            it_job_parameter_value = lt_job_parameter " Parametre adı kontrol edildi, tt_job_parameter_value doğruysa kalmalı
            is_start_info        = VALUE #( start_immediately = abap_true )
          IMPORTING
            ev_jobname           = DATA(lv_jobname)
            ev_jobcount          = DATA(lv_jobcount)
            et_message           = DATA(lt_message)
        ).

        " --- schedule_job'dan dönen mesajları işle ---
        lv_has_error_message = abap_false.
        LOOP AT lt_message INTO DATA(ls_message).
          DATA lv_severity TYPE if_abap_behv_message=>t_severity.
          CASE ls_message-type.
            WHEN 'S'. lv_severity = if_abap_behv_message=>severity-success.
            WHEN 'I'. lv_severity = if_abap_behv_message=>severity-information.
            WHEN 'W'. lv_severity = if_abap_behv_message=>severity-warning.
            WHEN 'E' OR 'A'.
              lv_severity = if_abap_behv_message=>severity-error.
              lv_has_error_message = abap_true.
            WHEN OTHERS. lv_severity = if_abap_behv_message=>severity-none.
          ENDCASE.

          IF lv_severity IS NOT INITIAL AND lv_severity <> if_abap_behv_message=>severity-none.
            APPEND VALUE #(
                %msg = new_message(
                           id       = ls_message-id
                           number   = ls_message-number
                           severity = lv_severity
                           v1       = ls_message-message_v1
                           v2       = ls_message-message_v2
                           v3       = ls_message-message_v3
                           v4       = ls_message-message_v4
                           " text     = COND #(...) " İsteğe bağlı metin ekleme
                       )
            ) TO reported-createdledger.
          ENDIF.
        ENDLOOP.

        " SADECE schedule_job'dan hata mesajı dönmediyse genel başarı mesajını ekle
        IF lv_has_error_message = abap_false.
          APPEND VALUE #(

              %msg = new_message_with_text( severity = if_abap_behv_message=>severity-success text = |Defter oluşturma işi başarıyla gönderildi (İş Adı: { lv_jobname }, İş Sayacı: { lv_jobcount })| )
          ) TO reported-createdledger.
        ELSE.
          " Eğer schedule_job hata döndürdüyse, örneği başarısız olarak işaretle
          APPEND VALUE #( %tky = ls_key_single-%param-CompanyCode ) TO failed-createdledger. " %tky'nin ls_key_single'dan gelmesi gerekir
        ENDIF.

        " --- TRY...CATCH Blokları Eklendi ---
      CATCH cx_apj_rt INTO DATA(lx_schedule_error). " İş zamanlama veya çalışma zamanı hatası
        APPEND VALUE #( %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = |Defter oluşturma işi gönderilirken hata (APJ): { lx_schedule_error->get_text( ) }| ) )
                 TO reported-createdledger.


      CATCH cx_root INTO DATA(lx_error). " Diğer tüm beklenmedik hatalar
        APPEND VALUE #(  " %tky eklendi
                        %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = |İş gönderimi sırasında beklenmeyen hata: { lx_error->get_text( ) }| ) )
                 TO reported-createdledger.


    ENDTRY.
  ENDMETHOD.

  METHOD delete_ledger.
    " Data declarations
    DATA: ls_reported TYPE RESPONSE FOR REPORTED zetr_ddl_i_created_ledger.
    DATA(ledger_general) = NEW zcl_etr_ledger_general( ).

    " Read selection parameters
    READ ENTITIES OF zetr_ddl_i_created_ledger IN LOCAL MODE
        ENTITY CreatedLedger
        ALL FIELDS WITH CORRESPONDING #( keys )
        RESULT DATA(lt_params).

    " Get the first parameter record
    DATA(ls_params) = VALUE #( keys[ 1 ]-%key DEFAULT VALUE #( ) ).

    " Perform deletion logic
    TRY.
        ledger_general->delete_ledger(
            EXPORTING
              iv_bukrs  = ls_params-bukrs
              iv_gjahr  = ls_params-gjahr
              iv_monat  = ls_params-monat
            RECEIVING
              rs_return = DATA(return_message)
          ).

        " Process BAPIRET2 messages
        CASE return_message-type.
          WHEN 'S'. " Success
            APPEND VALUE #( %msg = new_message_with_text(
                           severity = if_abap_behv_message=>severity-success
                           text     = return_message-message )
                         %element-bukrs = if_abap_behv=>mk-on )
              TO ls_reported-createdledger.
          WHEN 'E'. " Error
            APPEND VALUE #( %msg = new_message_with_text(
                           severity = if_abap_behv_message=>severity-error
                           text     = return_message-message )
                         %element-bukrs = if_abap_behv=>mk-on )
              TO ls_reported-createdledger.
          WHEN 'W'. " Warning
            APPEND VALUE #( %msg = new_message_with_text(
                           severity = if_abap_behv_message=>severity-warning
                           text     = return_message-message )
                         %element-bukrs = if_abap_behv=>mk-on )
              TO ls_reported-createdledger.
          WHEN 'I'. " Information
            APPEND VALUE #( %msg = new_message_with_text(
                           severity = if_abap_behv_message=>severity-information
                           text     = return_message-message )
                         %element-bukrs = if_abap_behv=>mk-on )
              TO ls_reported-createdledger.
        ENDCASE.


      CATCH cx_root INTO DATA(lx_error).
        " Handle any exceptions
        APPEND VALUE #( %msg = new_message_with_text(
                       severity = if_abap_behv_message=>severity-error
                       text     = lx_error->get_text( ) )
                     %element-bukrs = if_abap_behv=>mk-on )
          TO ls_reported-createdledger.
    ENDTRY.

    " Return messages
    reported = CORRESPONDING #( DEEP ls_reported ).

  ENDMETHOD.

  METHOD send_ledger.

    DATA: lt_job_parameter     TYPE cl_apj_rt_api=>tt_job_parameter_value,
          lv_job_text          TYPE cl_apj_rt_api=>ty_job_text,
          lv_has_error_message TYPE abap_boolean,
          ls_reported          TYPE RESPONSE FOR REPORTED zetr_ddl_i_created_ledger.

    " Parametreleri Oku
    READ ENTITIES OF zetr_ddl_i_created_ledger IN LOCAL MODE
        ENTITY CreatedLedger
        ALL FIELDS WITH CORRESPONDING #( keys )
        RESULT DATA(lt_params).

    " İlk parametre kaydını al
    DATA(ls_params) = VALUE #( keys[ 1 ]-%key DEFAULT VALUE #( ) ).

    TRY.
        " İş Parametrelerini Hazırla
        lt_job_parameter = VALUE cl_apj_rt_api=>tt_job_parameter_value(
            ( name = 'P_BUKRS' t_value = VALUE cl_apj_rt_api=>tt_value_range( ( sign = 'I' option = 'EQ' low = ls_params-bukrs ) ) )
            ( name = 'P_GJAHR' t_value = VALUE cl_apj_rt_api=>tt_value_range( ( sign = 'I' option = 'EQ' low = ls_params-gjahr ) ) )
            ( name = 'P_MONAT' t_value = VALUE cl_apj_rt_api=>tt_value_range( ( sign = 'I' option = 'EQ' low = ls_params-monat ) ) )
            ( name = 'P_RESEND' t_value = VALUE cl_apj_rt_api=>tt_value_range( ( sign = 'I' option = 'EQ' low = abap_false ) ) ) " Gönderim tekrarı için bayrak
        ).

        " İş Metnini Hazırla
        lv_job_text = |E-Defter Gönderim: { ls_params-bukrs }/{ ls_params-gjahr }/{ ls_params-monat }|.

        " İşi Zamanla
        cl_apj_rt_api=>schedule_job(
          EXPORTING
            iv_job_template_name = 'ZETR_ELEDGER_SEND_JT'
            iv_job_text          = lv_job_text
            it_job_parameter_value = lt_job_parameter
            is_start_info        = VALUE #( start_immediately = abap_true )
          IMPORTING
            ev_jobname           = DATA(lv_jobname)
            ev_jobcount          = DATA(lv_jobcount)
            et_message           = DATA(lt_message)
        ).

        " Mesajları İşle
        lv_has_error_message = abap_false.
        LOOP AT lt_message INTO DATA(ls_message).
          DATA lv_severity TYPE if_abap_behv_message=>t_severity.
          CASE ls_message-type.
            WHEN 'S'. lv_severity = if_abap_behv_message=>severity-success.
            WHEN 'I'. lv_severity = if_abap_behv_message=>severity-information.
            WHEN 'W'. lv_severity = if_abap_behv_message=>severity-warning.
            WHEN 'E' OR 'A'.
              lv_severity = if_abap_behv_message=>severity-error.
              lv_has_error_message = abap_true.
            WHEN OTHERS. lv_severity = if_abap_behv_message=>severity-none.
          ENDCASE.

          IF lv_severity IS NOT INITIAL AND lv_severity <> if_abap_behv_message=>severity-none.
            APPEND VALUE #(
                %msg = new_message(
                           id       = ls_message-id
                           number   = ls_message-number
                           severity = lv_severity
                           v1       = ls_message-message_v1
                           v2       = ls_message-message_v2
                           v3       = ls_message-message_v3
                           v4       = ls_message-message_v4
                       )
            ) TO  ls_reported-createdledger.
          ENDIF.
        ENDLOOP.

        " Başarı Mesajı
        IF lv_has_error_message = abap_false.
          APPEND VALUE #(
              %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-success
                         text     = |Defter gönderim işi başlatıldı (İş Adı: { lv_jobname }, İş Sayacı: { lv_jobcount })| )
          ) TO ls_reported-createdledger.
        ENDIF.

      CATCH cx_apj_rt INTO DATA(lx_schedule_error).
        " İş zamanlama hatası
        APPEND VALUE #( %msg = new_message_with_text(
                           severity = if_abap_behv_message=>severity-error
                           text     = |Defter gönderim işi sırasında hata: { lx_schedule_error->get_text( ) }| )
        ) TO  ls_reported-createdledger.

      CATCH cx_root INTO DATA(lx_error).
        " Genel hata
        APPEND VALUE #( %msg = new_message_with_text(
                           severity = if_abap_behv_message=>severity-error
                           text     = |Beklenmeyen hata: { lx_error->get_text( ) }| )
        ) TO  ls_reported-createdledger.
    ENDTRY.

    " Mesajları Döndür
    reported = CORRESPONDING #( DEEP ls_reported ).

  ENDMETHOD.

  METHOD resend_ledger."Hatalı , gitmeyen defter parçalarını tekrar gönderir
    DATA: lt_job_parameter     TYPE cl_apj_rt_api=>tt_job_parameter_value,
          lv_job_text          TYPE cl_apj_rt_api=>ty_job_text,
          lv_has_error_message TYPE abap_boolean,
          ls_reported          TYPE RESPONSE FOR REPORTED zetr_ddl_i_created_ledger,
          lv_resend            TYPE c LENGTH 1.

    " Parametreleri Oku
    READ ENTITIES OF zetr_ddl_i_created_ledger IN LOCAL MODE
        ENTITY CreatedLedger
        ALL FIELDS WITH CORRESPONDING #( keys )
        RESULT DATA(lt_params).

    " İlk parametre kaydını al
    DATA(ls_params) = VALUE #( keys[ 1 ]-%key DEFAULT VALUE #( ) ).

    lv_resend = abap_true. " Gönderim tekrarı için bayrak

    TRY.
        " İş Parametrelerini Hazırla
        lt_job_parameter = VALUE cl_apj_rt_api=>tt_job_parameter_value(
            ( name = 'P_BUKRS' t_value = VALUE cl_apj_rt_api=>tt_value_range( ( sign = 'I' option = 'EQ' low = ls_params-bukrs ) ) )
            ( name = 'P_GJAHR' t_value = VALUE cl_apj_rt_api=>tt_value_range( ( sign = 'I' option = 'EQ' low = ls_params-gjahr ) ) )
            ( name = 'P_MONAT' t_value = VALUE cl_apj_rt_api=>tt_value_range( ( sign = 'I' option = 'EQ' low = ls_params-monat ) ) )
            ( name = 'P_RESEND' t_value = VALUE cl_apj_rt_api=>tt_value_range( ( sign = 'I' option = 'EQ' low = lv_resend ) ) )
        ).

        " İş Metnini Hazırla
        lv_job_text = |E-Defter Tekrar Gönderim: { ls_params-bukrs }/{ ls_params-gjahr }/{ ls_params-monat }|.

        " İşi Zamanla
        cl_apj_rt_api=>schedule_job(
          EXPORTING
            iv_job_template_name = 'ZETR_ELEDGER_SEND_JT'
            iv_job_text          = lv_job_text
            it_job_parameter_value = lt_job_parameter
            is_start_info        = VALUE #( start_immediately = abap_true )
          IMPORTING
            ev_jobname           = DATA(lv_jobname)
            ev_jobcount          = DATA(lv_jobcount)
            et_message           = DATA(lt_message)
        ).

        " Mesajları İşle
        lv_has_error_message = abap_false.
        LOOP AT lt_message INTO DATA(ls_message).
          DATA lv_severity TYPE if_abap_behv_message=>t_severity.
          CASE ls_message-type.
            WHEN 'S'. lv_severity = if_abap_behv_message=>severity-success.
            WHEN 'I'. lv_severity = if_abap_behv_message=>severity-information.
            WHEN 'W'. lv_severity = if_abap_behv_message=>severity-warning.
            WHEN 'E' OR 'A'.
              lv_severity = if_abap_behv_message=>severity-error.
              lv_has_error_message = abap_true.
            WHEN OTHERS. lv_severity = if_abap_behv_message=>severity-none.
          ENDCASE.

          IF lv_severity IS NOT INITIAL AND lv_severity <> if_abap_behv_message=>severity-none.
            APPEND VALUE #(
                %msg = new_message(
                           id       = ls_message-id
                           number   = ls_message-number
                           severity = lv_severity
                           v1       = ls_message-message_v1
                           v2       = ls_message-message_v2
                           v3       = ls_message-message_v3
                           v4       = ls_message-message_v4
                       )
            ) TO  ls_reported-createdledger.
          ENDIF.
        ENDLOOP.

        " Başarı Mesajı
        IF lv_has_error_message = abap_false.
          APPEND VALUE #(
              %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-success
                         text     = |Tekrar(Gitmeyenler) Defter gönderim işi başlatıldı (İş Adı: { lv_jobname }, İş Sayacı: { lv_jobcount })| )
          ) TO ls_reported-createdledger.
        ENDIF.

      CATCH cx_apj_rt INTO DATA(lx_schedule_error).
        " İş zamanlama hatası
        APPEND VALUE #( %msg = new_message_with_text(
                           severity = if_abap_behv_message=>severity-error
                           text     = |Tekrar(Gitmeyenler) Defter gönderim işi sırasında hata: { lx_schedule_error->get_text( ) }| )
        ) TO  ls_reported-createdledger.

      CATCH cx_root INTO DATA(lx_error).
        " Genel hata
        APPEND VALUE #( %msg = new_message_with_text(
                           severity = if_abap_behv_message=>severity-error
                           text     = |Beklenmeyen hata: { lx_error->get_text( ) }| )
        ) TO  ls_reported-createdledger.
    ENDTRY.

    " Mesajları Döndür
    reported = CORRESPONDING #( DEEP ls_reported ).

  ENDMETHOD.

ENDCLASS.