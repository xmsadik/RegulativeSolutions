  METHOD convert_api_return.

    DATA : lv_fdpos   TYPE sy-fdpos.
    DATA : lv_length TYPE i.

    DATA(lv_json_str) = iv_result.

    " Bilinen JSON format sorunlarını düzeltmek için string değişiklikleri
    REPLACE ALL OCCURRENCES OF '"success":true'  IN lv_json_str WITH '"success":"true"'.
    REPLACE ALL OCCURRENCES OF '"success":false' IN lv_json_str WITH '"success":"false"'.
    REPLACE ALL OCCURRENCES OF '"message":null'  IN lv_json_str WITH '"message":"null"'.
    REPLACE ALL OCCURRENCES OF '"data":[null]'   IN lv_json_str WITH '"data":[]"'.

    " Eğer "data": kısmında dizi başlangıcı doğru değilse, düzenleme yapılır

    FIND '"data":' IN lv_json_str MATCH OFFSET lv_fdpos.
    IF sy-subrc = 0.
      lv_fdpos = sy-fdpos + 7.
      IF lv_json_str+lv_fdpos(1) NE '['.
        lv_length = strlen( lv_json_str ).
        lv_length = lv_length - lv_fdpos - 1.
        CONCATENATE lv_json_str+0(lv_fdpos)
                    '['
                    lv_json_str+lv_fdpos(lv_length)
                    ']}'
                    INTO lv_json_str.
      ENDIF.
    ENDIF.

    REPLACE ALL OCCURRENCES OF ',"data":[]' IN lv_json_str WITH ''.

    " Cloud ABAP'da /ui2/cl_json sınıfı kullanılarak JSON deserialize işlemi
    TRY.
        DATA(lo_json) = NEW /ui2/cl_json( ).
        lo_json->deserialize(
          EXPORTING
            json = lv_json_str
          CHANGING
            data = es_apiret
        ).
      CATCH cx_root INTO DATA(lx_exception).
        " Hata durumunda, API dönüş yapısı başarısız olarak işaretlenir ve hata mesajı atanır
        es_apiret-success = abap_false.
        es_apiret-message = lx_exception->get_text( ).
    ENDTRY.
  ENDMETHOD.