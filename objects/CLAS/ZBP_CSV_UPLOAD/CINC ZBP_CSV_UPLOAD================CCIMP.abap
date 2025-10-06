CLASS lhc_csvupload DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_csv_data,
             bukrs TYPE bukrs,
             gjahr TYPE gjahr,
             linen TYPE string,
             belnr TYPE belnr_d,
             yevno TYPE string,
             budat TYPE budat,
           END OF ty_csv_data.
    TYPES tt_csv_data TYPE STANDARD TABLE OF ty_csv_data WITH DEFAULT KEY.

    " ⭐ SADECE EARLY NUMBERING
    METHODS earlynumbering_create FOR NUMBERING
      IMPORTING entities FOR CREATE csvupload.

    " ⭐ AUTHORIZATION METODLARI YOK!

    METHODS setuseranddefaults FOR DETERMINE ON MODIFY
      IMPORTING keys FOR csvupload~setuseranddefaults.

    METHODS processuploadedfile FOR DETERMINE ON MODIFY
      IMPORTING keys FOR csvupload~processuploadedfile.

    METHODS processcsvfile FOR MODIFY
      IMPORTING keys FOR ACTION csvupload~processcsvfile RESULT result.

    METHODS validatechunksize FOR VALIDATE ON SAVE
      IMPORTING keys FOR csvupload~validatechunksize.

    METHODS parse_csv_basic
      IMPORTING iv_file_content TYPE xstring
      RETURNING VALUE(rt_data)  TYPE tt_csv_data.

    METHODS convert_xstring_to_string
      IMPORTING iv_xstring       TYPE xstring
      RETURNING VALUE(rv_string) TYPE string.

ENDCLASS.

CLASS lhc_csvupload IMPLEMENTATION.

  METHOD earlynumbering_create.
    DATA: lt_mapped TYPE RESPONSE FOR MAPPED EARLY zetr_i_csv_upload.

    LOOP AT entities INTO DATA(ls_entity).
      DATA(lv_uuid) = cl_system_uuid=>create_uuid_x16_static( ).

      APPEND VALUE #(
        %cid = ls_entity-%cid
        uploadid = lv_uuid
      ) TO lt_mapped-csvupload.
    ENDLOOP.

    mapped-csvupload = lt_mapped-csvupload.
  ENDMETHOD.

  METHOD setuseranddefaults.
    READ ENTITIES OF zetr_i_csv_upload IN LOCAL MODE
      ENTITY csvupload
      FIELDS ( EndUser ChunkSize )
      WITH CORRESPONDING #( keys )
      RESULT DATA(lt_entities).

    DATA: lt_update TYPE TABLE FOR UPDATE zetr_i_csv_upload.

    LOOP AT lt_entities INTO DATA(ls_entity).
      DATA: ls_update TYPE STRUCTURE FOR UPDATE zetr_i_csv_upload.
      ls_update-%tky = ls_entity-%tky.

      DATA(lv_changed) = abap_false.

      IF ls_entity-enduser IS INITIAL.
        ls_update-enduser = sy-uname.
        ls_update-%control-enduser = if_abap_behv=>mk-on.
        lv_changed = abap_true.
      ENDIF.

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
    MODIFY ENTITIES OF zetr_i_csv_upload IN LOCAL MODE
      ENTITY csvupload
      UPDATE FROM VALUE #( FOR key IN keys (
        %tky = key-%tky
        status = 'U'
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
            DATA(lt_csv_data) = parse_csv_basic( ls_upload-attachment ).

            DATA(lv_processed) = 0.
            DATA(lv_updated) = 0.
            DATA(lv_errors) = 0.

            LOOP AT lt_csv_data INTO DATA(ls_csv_data).
              lv_processed = lv_processed + 1.

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

            MODIFY ENTITIES OF zetr_i_csv_upload IN LOCAL MODE
              ENTITY csvupload
              UPDATE FROM VALUE #( (
                %tky = ls_key-%tky
                status = COND #( WHEN lv_errors = 0 THEN 'C' ELSE 'E' )
                %control-status = if_abap_behv=>mk-on
              ) ).

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

  METHOD parse_csv_basic.
    DATA: lv_content TYPE string,
          lt_rows    TYPE STANDARD TABLE OF string.

    lv_content = convert_xstring_to_string( iv_file_content ).

    SPLIT lv_content AT cl_abap_char_utilities=>newline INTO TABLE lt_rows.

    DATA(lv_line_counter) = 0.

    LOOP AT lt_rows INTO DATA(lv_row).
      lv_line_counter = lv_line_counter + 1.

      IF lv_row IS INITIAL.
        CONTINUE.
      ENDIF.

      DATA(ls_csv_line) = VALUE ty_csv_data( ).

      SPLIT lv_row AT ',' INTO ls_csv_line-bukrs
                               ls_csv_line-budat
                               ls_csv_line-belnr
                               ls_csv_line-gjahr
                               ls_csv_line-linen
                               ls_csv_line-yevno
                               .

      IF lv_line_counter = 1 AND
         ( to_upper( condense( ls_csv_line-linen ) ) = 'LINEN' OR
           to_upper( condense( ls_csv_line-belnr ) ) = 'BELNR' ).
        CONTINUE.
      ENDIF.

      IF ls_csv_line-belnr IS NOT INITIAL AND
         ls_csv_line-yevno IS NOT INITIAL AND
         ls_csv_line-gjahr IS NOT INITIAL AND
         ls_csv_line-bukrs IS NOT INITIAL.

        ls_csv_line-gjahr = condense( ls_csv_line-gjahr ).
        ls_csv_line-bukrs = condense( ls_csv_line-bukrs ).
        ls_csv_line-linen = condense( ls_csv_line-linen ).
        ls_csv_line-belnr = condense( ls_csv_line-belnr ).
        ls_csv_line-yevno = condense( ls_csv_line-yevno ).
        ls_csv_line-budat = condense( ls_csv_line-budat ).

        ls_csv_line-belnr = |{ ls_csv_line-belnr ALPHA = IN }|.

        APPEND ls_csv_line TO rt_data.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD convert_xstring_to_string.
    DATA(lv_len) = xstrlen( iv_xstring ).

    IF lv_len = 0.
      RETURN.
    ENDIF.

    DATA: lv_char TYPE c LENGTH 1.

    DO lv_len TIMES.
      DATA(lv_offset) = sy-index - 1.
      DATA(lv_single_byte) = iv_xstring+lv_offset(1).

      CASE lv_single_byte.
        WHEN '20'. lv_char = ' '.
        WHEN '2C'. lv_char = ','.
        WHEN '2D'. lv_char = '-'.
        WHEN '2E'. lv_char = '.'.
        WHEN '30'. lv_char = '0'.
        WHEN '31'. lv_char = '1'.
        WHEN '32'. lv_char = '2'.
        WHEN '33'. lv_char = '3'.
        WHEN '34'. lv_char = '4'.
        WHEN '35'. lv_char = '5'.
        WHEN '36'. lv_char = '6'.
        WHEN '37'. lv_char = '7'.
        WHEN '38'. lv_char = '8'.
        WHEN '39'. lv_char = '9'.
        WHEN '3B'. lv_char = ';'.
        WHEN '41'. lv_char = 'A'.
        WHEN '42'. lv_char = 'B'.
        WHEN '43'. lv_char = 'C'.
        WHEN '44'. lv_char = 'D'.
        WHEN '45'. lv_char = 'E'.
        WHEN '46'. lv_char = 'F'.
        WHEN '47'. lv_char = 'G'.
        WHEN '48'. lv_char = 'H'.
        WHEN '49'. lv_char = 'I'.
        WHEN '4A'. lv_char = 'J'.
        WHEN '4B'. lv_char = 'K'.
        WHEN '4C'. lv_char = 'L'.
        WHEN '4D'. lv_char = 'M'.
        WHEN '4E'. lv_char = 'N'.
        WHEN '4F'. lv_char = 'O'.
        WHEN '50'. lv_char = 'P'.
        WHEN '51'. lv_char = 'Q'.
        WHEN '52'. lv_char = 'R'.
        WHEN '53'. lv_char = 'S'.
        WHEN '54'. lv_char = 'T'.
        WHEN '55'. lv_char = 'U'.
        WHEN '56'. lv_char = 'V'.
        WHEN '57'. lv_char = 'W'.
        WHEN '58'. lv_char = 'X'.
        WHEN '59'. lv_char = 'Y'.
        WHEN '5A'. lv_char = 'Z'.
        WHEN '61'. lv_char = 'a'.
        WHEN '62'. lv_char = 'b'.
        WHEN '63'. lv_char = 'c'.
        WHEN '64'. lv_char = 'd'.
        WHEN '65'. lv_char = 'e'.
        WHEN '66'. lv_char = 'f'.
        WHEN '67'. lv_char = 'g'.
        WHEN '68'. lv_char = 'h'.
        WHEN '69'. lv_char = 'i'.
        WHEN '6A'. lv_char = 'j'.
        WHEN '6B'. lv_char = 'k'.
        WHEN '6C'. lv_char = 'l'.
        WHEN '6D'. lv_char = 'm'.
        WHEN '6E'. lv_char = 'n'.
        WHEN '6F'. lv_char = 'o'.
        WHEN '70'. lv_char = 'p'.
        WHEN '71'. lv_char = 'q'.
        WHEN '72'. lv_char = 'r'.
        WHEN '73'. lv_char = 's'.
        WHEN '74'. lv_char = 't'.
        WHEN '75'. lv_char = 'u'.
        WHEN '76'. lv_char = 'v'.
        WHEN '77'. lv_char = 'w'.
        WHEN '78'. lv_char = 'x'.
        WHEN '79'. lv_char = 'y'.
        WHEN '7A'. lv_char = 'z'.
        WHEN '0A'.
          rv_string = rv_string && cl_abap_char_utilities=>newline.
          CONTINUE.
        WHEN '0D'.
          CONTINUE.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.

      rv_string = rv_string && lv_char.
    ENDDO.
  ENDMETHOD.


ENDCLASS.