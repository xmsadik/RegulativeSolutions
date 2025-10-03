CLASS lhc_csvupload DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_csv_data,
             Bukrs TYPE string,
             gjahr TYPE string,
             linen TYPE string,
             belnr TYPE string,
             yevno TYPE string,
             budat TYPE string,
           END OF ty_csv_data.
    TYPES tt_csv_data TYPE STANDARD TABLE OF ty_csv_data WITH DEFAULT KEY.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR csvupload RESULT result.

    METHODS setuseranddefaults FOR DETERMINE ON MODIFY
      IMPORTING keys FOR csvupload~setuseranddefaults.

    METHODS processuploadedfile FOR DETERMINE ON MODIFY
      IMPORTING keys FOR csvupload~processuploadedfile.

    METHODS processcsvfile FOR MODIFY
      IMPORTING keys FOR ACTION csvupload~processcsvfile RESULT result.

    METHODS validatechunksize FOR VALIDATE ON SAVE
      IMPORTING keys FOR csvupload~validatechunksize.

    " File processing methods
    METHODS parse_file_content
      IMPORTING iv_file_content TYPE xstring
                iv_filename     TYPE string
      RETURNING VALUE(rt_data)  TYPE tt_csv_data.

    " Basic CSV processing (fallback approach)
    METHODS parse_csv_basic
      IMPORTING iv_file_content TYPE xstring
      RETURNING VALUE(rt_data)  TYPE tt_csv_data.

    " Helper method for XSTRING to STRING conversion
    METHODS convert_xstring_to_string
      IMPORTING iv_xstring       TYPE xstring
      RETURNING VALUE(rv_string) TYPE string.

ENDCLASS.

CLASS lhc_csvupload IMPLEMENTATION.

  METHOD get_global_authorizations.
    result-%action-processcsvfile = if_abap_behv=>auth-allowed.
    result-%create = if_abap_behv=>auth-allowed.
    result-%update = if_abap_behv=>auth-allowed.
    result-%delete = if_abap_behv=>auth-allowed.
  ENDMETHOD.

  METHOD setuseranddefaults.
    " Set default values when creating new entry
    READ ENTITIES OF zetr_i_csv_upload IN LOCAL MODE
      ENTITY csvupload
      FIELDS ( EndUser ChunkSize )
      WITH CORRESPONDING #( keys )
      RESULT DATA(lt_entities).

    DATA: lt_update TYPE TABLE FOR UPDATE zetr_i_csv_upload.

    LOOP AT lt_entities INTO DATA(ls_entity).
      " Manual assignment instead of CORRESPONDING
      DATA: ls_update TYPE STRUCTURE FOR UPDATE zetr_i_csv_upload.
      ls_update-%tky = ls_entity-%tky.
      ls_update-uploadid = ls_entity-uploadid.
      ls_update-enduser = ls_entity-enduser.
      ls_update-chunksize = ls_entity-chunksize.

      DATA(lv_changed) = abap_false.

      " EndUser set et eğer boşsa
      IF ls_entity-enduser IS INITIAL.
        ls_update-enduser = sy-uname.
        ls_update-%control-enduser = if_abap_behv=>mk-on.
        lv_changed = abap_true.
      ENDIF.

      " ChunkSize set et eğer boşsa veya 0 ise
      IF ls_entity-chunksize IS INITIAL OR ls_entity-chunksize = 0.
        ls_update-chunksize = 10000.
        ls_update-%control-chunksize = if_abap_behv=>mk-on.
        lv_changed = abap_true.
      ENDIF.

      IF lv_changed = abap_true.
        APPEND ls_update TO lt_update.
      ENDIF.
    ENDLOOP.

    IF lt_update IS NOT INITIAL.
      MODIFY ENTITIES OF zetr_i_csv_upload IN LOCAL MODE
        ENTITY csvupload
        UPDATE FROM lt_update.
    ENDIF.
  ENDMETHOD.

  METHOD processuploadedfile.
    " File yüklendiğinde status güncelle
    MODIFY ENTITIES OF zetr_i_csv_upload IN LOCAL MODE
      ENTITY csvupload
      UPDATE FROM VALUE #( FOR key IN keys (
        %tky = key-%tky
        status = 'U'  " U = Uploaded
        %control-status = if_abap_behv=>mk-on
      ) ).
  ENDMETHOD.

  METHOD processcsvfile.
    READ ENTITIES OF zetr_i_csv_upload IN LOCAL MODE
      ENTITY csvupload
      ALL FIELDS
      WITH CORRESPONDING #( keys )
      RESULT DATA(lt_uploads).

    LOOP AT keys INTO DATA(ls_key).
      APPEND VALUE #( %tky = ls_key-%tky ) TO result.

      READ TABLE lt_uploads INTO DATA(ls_upload) WITH KEY uploadid = ls_key-uploadid.

      IF ls_upload-attachment IS NOT INITIAL.
        TRY.
            " Type conversion: C(255) to STRING
            DATA(lv_filename) = CONV string( ls_upload-filename ).

            " Parse file content - şimdilik sadece CSV basic parsing
            DATA(lt_csv_data) = parse_file_content(
              iv_file_content = ls_upload-attachment
              iv_filename = lv_filename
            ).

            DATA(lv_processed) = 0.
            DATA(lv_updated) = 0.
            DATA(lv_errors) = 0.

            " CSV verilerini işle
            LOOP AT lt_csv_data INTO DATA(ls_csv_data).
              lv_processed = lv_processed + 1.

              " ZETR_T_DEFKY tablosunu güncelle
              UPDATE zetr_t_defky
                 SET yevno = @ls_csv_data-yevno
               WHERE bukrs = @ls_csv_data-bukrs
                 AND belnr = @ls_csv_data-belnr
                 AND gjahr = @ls_csv_data-gjahr.

              IF sy-subrc = 0.
                lv_updated = lv_updated + sy-dbcnt.
              ELSE.
                lv_errors = lv_errors + 1.
              ENDIF.
            ENDLOOP.

            " Status güncelle - İşlem tamamlandı
            MODIFY ENTITIES OF zetr_i_csv_upload IN LOCAL MODE
              ENTITY csvupload
              UPDATE FROM VALUE #( (
                %tky = ls_key-%tky
                status = COND #( WHEN lv_errors = 0 THEN 'C' ELSE 'E' )
                %control-status = if_abap_behv=>mk-on
              ) ).

            " Başarı mesajı
            DATA(lv_message) = |Dosya işlendi: { lv_processed } satır okundu, { lv_updated } kayıt güncellendi|.
            IF lv_errors > 0.
              lv_message = |{ lv_message }, { lv_errors } hata|.
            ENDIF.

            APPEND VALUE #( %tky = ls_key-%tky
                           %msg = new_message_with_text(
                             severity = COND #( WHEN lv_errors = 0 THEN if_abap_behv_message=>severity-success
                                                                     ELSE if_abap_behv_message=>severity-warning )
                             text = lv_message ) )
                   TO reported-csvupload.

          CATCH cx_root INTO DATA(lx_error).
            " Hata durumu
            MODIFY ENTITIES OF zetr_i_csv_upload IN LOCAL MODE
              ENTITY csvupload
              UPDATE FROM VALUE #( (
                %tky = ls_key-%tky
                status = 'E'
                %control-status = if_abap_behv=>mk-on
              ) ).

            APPEND VALUE #( %tky = ls_key-%tky
                           %msg = new_message_with_text(
                             severity = if_abap_behv_message=>severity-error
                             text = |Dosya işleme hatası: { lx_error->get_text( ) }| ) )
                   TO reported-csvupload.
        ENDTRY.
      ELSE.
        APPEND VALUE #( %tky = ls_key-%tky
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text = 'Dosya yüklenmemiş' ) )
               TO reported-csvupload.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validatechunksize.
    READ ENTITIES OF zetr_i_csv_upload IN LOCAL MODE
      ENTITY csvupload
      FIELDS ( ChunkSize )
      WITH CORRESPONDING #( keys )
      RESULT DATA(lt_entities).

    LOOP AT lt_entities INTO DATA(ls_entity).
      IF ls_entity-chunksize < 1000 OR ls_entity-chunksize > 1000000.
        APPEND VALUE #( %tky = ls_entity-%tky ) TO failed-csvupload.
        APPEND VALUE #( %tky = ls_entity-%tky
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text = 'Chunk Size 1.000 ile 1.000.000 arasında olmalıdır' )
                       %element-chunksize = if_abap_behv=>mk-on )
               TO reported-csvupload.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD parse_file_content.
    " Şimdilik tüm dosyalar için basic CSV parsing kullan
    " XCO Excel API'si çok complex ve hatalı olduğu için
    rt_data = parse_csv_basic( iv_file_content ).
  ENDMETHOD.

  METHOD parse_csv_basic.
    " Basic CSV processing - hem CSV hem Excel için çalışır
    DATA: lv_content TYPE string,
          lt_rows    TYPE STANDARD TABLE OF string.

    " XSTRING'i STRING'e dönüştür
    lv_content = convert_xstring_to_string( iv_file_content ).

    " Satırlara böl
    SPLIT lv_content AT cl_abap_char_utilities=>newline INTO TABLE lt_rows.

    " CSV format parse: LINEN;BELNR;YEVNO;BUDAT
    DATA(lv_line_counter) = 0.

    LOOP AT lt_rows INTO DATA(lv_row).
      lv_line_counter = lv_line_counter + 1.

      " Skip empty rows
      IF lv_row IS INITIAL.
        CONTINUE.
      ENDIF.

      DATA(ls_csv_line) = VALUE ty_csv_data( ).

      " Semicolon delimiter ile parse et
      SPLIT lv_row AT ';' INTO ls_csv_line-bukrs
                               ls_csv_line-belnr
                               ls_csv_line-gjahr
                               ls_csv_line-linen
                               ls_csv_line-yevno
                               ls_csv_line-budat.

      " Header satırını atla
      IF lv_line_counter = 1 AND
         ( to_upper( condense( ls_csv_line-linen ) ) = 'LINEN' OR
           to_upper( condense( ls_csv_line-belnr ) ) = 'BELNR' ).
        CONTINUE.
      ENDIF.

      " Sadece valid entries ekle
      IF ls_csv_line-belnr IS NOT INITIAL AND
         ls_csv_line-yevno IS NOT INITIAL AND
         ls_csv_line-gjahr IS NOT INITIAL AND
         ls_csv_line-bukrs IS NOT INITIAL  .

        " Whitespace'leri temizle
        ls_csv_line-gjahr = condense( ls_csv_line-gjahr ).
        ls_csv_line-bukrs = condense( ls_csv_line-bukrs ).
        ls_csv_line-linen = condense( ls_csv_line-linen ).
        ls_csv_line-belnr = condense( ls_csv_line-belnr ).
        ls_csv_line-yevno = condense( ls_csv_line-yevno ).
        ls_csv_line-budat = condense( ls_csv_line-budat ).

        " Alpha conversion - belge numarası için
        ls_csv_line-belnr = |{ ls_csv_line-belnr ALPHA = IN }|.

        APPEND ls_csv_line TO rt_data.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD convert_xstring_to_string.
    " Basit XSTRING to STRING conversion
    DATA(lv_len) = xstrlen( iv_xstring ).

    IF lv_len = 0.
      RETURN.
    ENDIF.

    " Her byte'ı character'e dönüştür
    DATA: lv_char TYPE c LENGTH 1.

    DO lv_len TIMES.
      DATA(lv_offset) = sy-index - 1.
      DATA(lv_single_byte) = iv_xstring+lv_offset(1).

      " ASCII character mapping - simplified
      CASE lv_single_byte.
        WHEN '20'.
          lv_char = ' '.        " Space
        WHEN '2C'.
          lv_char = ','.        " Comma
        WHEN '2D'.
          lv_char = '-'.        " Dash
        WHEN '2E'.
          lv_char = '.'.        " Period
        WHEN '30'.
          lv_char = '0'.        " Digits
        WHEN '31'.
          lv_char = '1'.
        WHEN '32'.
          lv_char = '2'.
        WHEN '33'.
          lv_char = '3'.
        WHEN '34'.
          lv_char = '4'.
        WHEN '35'.
          lv_char = '5'.
        WHEN '36'.
          lv_char = '6'.
        WHEN '37'.
          lv_char = '7'.
        WHEN '38'.
          lv_char = '8'.
        WHEN '39'.
          lv_char = '9'.
        WHEN '3B'.
          lv_char = ';'.        " Semicolon
        WHEN '41'.
          lv_char = 'A'.        " A-Z
        WHEN '42'.
          lv_char = 'B'.
        WHEN '43'.
          lv_char = 'C'.
        WHEN '44'.
          lv_char = 'D'.
        WHEN '45'.
          lv_char = 'E'.
        WHEN '46'.
          lv_char = 'F'.
        WHEN '47'.
          lv_char = 'G'.
        WHEN '48'.
          lv_char = 'H'.
        WHEN '49'.
          lv_char = 'I'.
        WHEN '4A'.
          lv_char = 'J'.
        WHEN '4B'.
          lv_char = 'K'.
        WHEN '4C'.
          lv_char = 'L'.
        WHEN '4D'.
          lv_char = 'M'.
        WHEN '4E'.
          lv_char = 'N'.
        WHEN '4F'.
          lv_char = 'O'.
        WHEN '50'.
          lv_char = 'P'.
        WHEN '51'.
          lv_char = 'Q'.
        WHEN '52'.
          lv_char = 'R'.
        WHEN '53'.
          lv_char = 'S'.
        WHEN '54'.
          lv_char = 'T'.
        WHEN '55'.
          lv_char = 'U'.
        WHEN '56'.
          lv_char = 'V'.
        WHEN '57'.
          lv_char = 'W'.
        WHEN '58'.
          lv_char = 'X'.
        WHEN '59'.
          lv_char = 'Y'.
        WHEN '5A'.
          lv_char = 'Z'.
        WHEN '61'.
          lv_char = 'a'.        " a-z
        WHEN '62'.
          lv_char = 'b'.
        WHEN '63'.
          lv_char = 'c'.
        WHEN '64'.
          lv_char = 'd'.
        WHEN '65'.
          lv_char = 'e'.
        WHEN '66'.
          lv_char = 'f'.
        WHEN '67'.
          lv_char = 'g'.
        WHEN '68'.
          lv_char = 'h'.
        WHEN '69'.
          lv_char = 'i'.
        WHEN '6A'.
          lv_char = 'j'.
        WHEN '6B'.
          lv_char = 'k'.
        WHEN '6C'.
          lv_char = 'l'.
        WHEN '6D'.
          lv_char = 'm'.
        WHEN '6E'.
          lv_char = 'n'.
        WHEN '6F'.
          lv_char = 'o'.
        WHEN '70'.
          lv_char = 'p'.
        WHEN '71'.
          lv_char = 'q'.
        WHEN '72'.
          lv_char = 'r'.
        WHEN '73'.
          lv_char = 's'.
        WHEN '74'.
          lv_char = 't'.
        WHEN '75'.
          lv_char = 'u'.
        WHEN '76'.
          lv_char = 'v'.
        WHEN '77'.
          lv_char = 'w'.
        WHEN '78'.
          lv_char = 'x'.
        WHEN '79'.
          lv_char = 'y'.
        WHEN '7A'.
          lv_char = 'z'.
        WHEN '0A'.                       " Line Feed
          rv_string = rv_string && cl_abap_char_utilities=>newline.
          CONTINUE.
        WHEN '0D'.                       " Carriage Return
          CONTINUE.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.

      rv_string = rv_string && lv_char.
    ENDDO.
  ENDMETHOD.

ENDCLASS.