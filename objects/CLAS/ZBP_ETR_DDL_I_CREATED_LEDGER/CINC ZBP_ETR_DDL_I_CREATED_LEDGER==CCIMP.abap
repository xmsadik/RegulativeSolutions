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





    " ========== OLUŞTURMA JOB DURUM KONTROLÜ ==========
    DATA(lv_creation_job_text) = |E-Defter Oluşturma: { ls_params-bukrs }/{ ls_params-gjahr }/{ ls_params-monat }|.

    TRY.
        " Job Catalog Entry'ye göre çalışan job'ları bul
        DATA(lt_running_jobs) = cl_apj_rt_api=>find_jobs_with_jce(
          iv_catalog_name = 'ZETR_ELEDGER_JCE'
        ).

        " Her job'ın detayını kontrol et
        LOOP AT lt_running_jobs INTO DATA(ls_job_short)
          WHERE status = cl_apj_rt_api=>status_running
             OR status = cl_apj_rt_api=>status_ready
             OR status = cl_apj_rt_api=>status_scheduled.

          " Job detaylarını al (job_text dahil)
          DATA(ls_job_detail) = cl_apj_rt_api=>get_job_details(
            iv_jobname  = ls_job_short-jobname
            iv_jobcount = ls_job_short-jobcount
          ).

          " Job text kontrolü
          IF ls_job_detail-job_text CS lv_creation_job_text.
            " OLUŞTURMA JOB'I HALA ÇALIŞIYOR - HATA VER VE ÇIK
            APPEND VALUE #(
              %msg = new_message_with_text(
                       severity = if_abap_behv_message=>severity-error
                       text = |Defter oluşturma işi halen devam ediyor ({ ls_params-bukrs }/{ ls_params-gjahr }/{ ls_params-monat }). Job: { ls_job_short-jobname }/{ ls_job_short-jobcount }. Lütfen tamamlanmasını bekleyin.| )
            ) TO ls_reported-createdledger.

            reported = CORRESPONDING #( DEEP ls_reported ).
            RETURN. " İşlemi DURDUR
          ENDIF.

        ENDLOOP.

      CATCH cx_apj_rt INTO DATA(lx_job_check_error).
        " Job kontrolü yapılamadı - uyarı ver ama devam et
        APPEND VALUE #(
          %msg = new_message_with_text(
                   severity = if_abap_behv_message=>severity-warning
                   text = |Job durumu kontrol edilemedi: { lx_job_check_error->get_text( ) }| )
        ) TO ls_reported-createdledger.
    ENDTRY.
    " ========== JOB KONTROL SONU ==========













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