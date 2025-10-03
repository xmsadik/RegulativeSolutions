" Mevcut ZBP_ETR_DDL_I_EDF_YEVMIYE class'ının içine bu kodu ekleyin:

CLASS lhc_YevmiyeKayit DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_csv_line,
             linen TYPE string,
             belnr TYPE string,
             yevno TYPE string,
             budat TYPE string,
           END OF ty_csv_line.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR YevmiyeKayit RESULT result.

    METHODS uploadCSVFile FOR MODIFY
      IMPORTING keys FOR ACTION YevmiyeKayit~uploadCSVFile RESULT result.

ENDCLASS.

CLASS lhc_YevmiyeKayit IMPLEMENTATION.

  METHOD get_instance_authorizations.
    " Yetkilendirme kontrolü - basit implementasyon
    result = VALUE #( FOR key IN keys
                     ( %tky = key-%tky
                       %update = if_abap_behv=>auth-allowed ) ).
  ENDMETHOD.

  METHOD uploadCSVFile.
    " Bu method şimdilik basit bir implementasyon
    " Gerçek CSV upload işlemi Fiori uygulamasında yapılacak

    LOOP AT keys INTO DATA(ls_key).
      " Başarılı sonuç döndür
      result = VALUE #( BASE result ( %tky = ls_key-%tky ) ).

      " RAP'te mesaj göstermek için reported kullanılır
      APPEND VALUE #( %tky = ls_key-%tky
                     %msg = new_message_with_text(
                       severity = if_abap_behv_message=>severity-success
                       text = 'CSV yükleme işlemi başlatıldı' ) ) TO reported-yevmiyekayit.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.