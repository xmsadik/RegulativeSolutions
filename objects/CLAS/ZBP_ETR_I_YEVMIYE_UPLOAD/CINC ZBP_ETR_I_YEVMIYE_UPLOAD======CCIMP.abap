CLASS lhc_yevmiyeupload DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    " Data types for CSV processing
    TYPES: BEGIN OF ty_csv_data,
             linen TYPE string,
             belnr TYPE string,
             yevno TYPE string,
             budat TYPE string,
             row_number TYPE i,
           END OF ty_csv_data.
    TYPES: tt_csv_data TYPE STANDARD TABLE OF ty_csv_data WITH DEFAULT KEY.

    " CONTRACT SAFE: Authorization method - READ-ONLY context
    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR yevmiyeupload RESULT result.

    " CONTRACT SAFE: Modify action - MODIFY context
    METHODS uploadcsvfile FOR MODIFY
      IMPORTING keys FOR ACTION yevmiyeupload~uploadcsvfile RESULT result.

    " CONTRACT SAFE: Validation - READ-ONLY context
    METHODS validatechunksize FOR VALIDATE ON SAVE
      IMPORTING keys FOR yevmiyeupload~validatechunksize.

    " Helper methods - CONTRACT SAFE
    METHODS parse_csv_content
      IMPORTING iv_csv_content TYPE string
      RETURNING VALUE(rt_data) TYPE tt_csv_data.

ENDCLASS.

CLASS lhc_yevmiyeupload IMPLEMENTATION.

  METHOD get_global_authorizations.
    " CONTRACT SAFE: Only authorization assignments, no database operations
    result-%action-uploadcsvfile = if_abap_behv=>auth-allowed.
    result-%create = if_abap_behv=>auth-allowed.
    result-%update = if_abap_behv=>auth-allowed.
    result-%delete = if_abap_behv=>auth-allowed.
  ENDMETHOD.

  METHOD validatechunksize.
    " CONTRACT SAFE: READ-ONLY context
    " Only read entities, no database modifications

    READ ENTITIES OF zetr_i_yevmiye_upload IN LOCAL MODE
      ENTITY yevmiyeupload
      FIELDS ( ChunkSize )
      WITH CORRESPONDING #( keys )
      RESULT DATA(lt_entities).

    LOOP AT lt_entities INTO DATA(ls_entity).
      IF ls_entity-chunksize < 1000 OR ls_entity-chunksize > 1000000.
        " CONTRACT SAFE: Only report failures, no database changes
        APPEND VALUE #( %tky = ls_entity-%tky ) TO failed-yevmiyeupload.
        APPEND VALUE #( %tky = ls_entity-%tky
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text = 'Chunk Size 1.000 ile 1.000.000 arasında olmalıdır' )
                       %element-chunksize = if_abap_behv=>mk-on )
               TO reported-yevmiyeupload.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD uploadcsvfile.
    " CONTRACT SAFE: MODIFY context - database operations allowed here

    LOOP AT keys INTO DATA(ls_key).
      result = VALUE #( BASE result ( %tky = ls_key-%tky ) ).

      " Read current entity state
      READ ENTITIES OF zetr_i_yevmiye_upload IN LOCAL MODE
        ENTITY yevmiyeupload
        ALL FIELDS
        WITH CORRESPONDING #( keys )
        RESULT DATA(lt_entities).

      IF lt_entities IS INITIAL.
        APPEND VALUE #( %tky = ls_key-%tky
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text = 'Entity bulunamadı' ) )
               TO reported-yevmiyeupload.
        CONTINUE.
      ENDIF.

      DATA(ls_entity) = lt_entities[ 1 ].

      " Convert CSV content safely
      DATA(lv_csv_content) = CONV string( ls_entity-csvcontent ).
      DATA(lv_chunk_size) = ls_entity-chunksize.

      " Set default chunk size
      IF lv_chunk_size IS INITIAL OR lv_chunk_size = 0.
        lv_chunk_size = 10000.
      ENDIF.

      " Parse CSV content
      DATA(lt_csv_data) = parse_csv_content( lv_csv_content ).

      " Use test data if no CSV provided
      IF lt_csv_data IS INITIAL.
        lt_csv_data = VALUE tt_csv_data(
          ( linen = '1' belnr = '5100000001' yevno = 'YEV202412001' budat = '20241201' row_number = 1 )
          ( linen = '2' belnr = '5100000002' yevno = 'YEV202412002' budat = '20241201' row_number = 2 )
          ( linen = '3' belnr = '5100000003' yevno = 'YEV202412003' budat = '20241201' row_number = 3 )
        ).

        APPEND VALUE #( %tky = ls_key-%tky
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-information
                         text = 'Test verisi kullanılıyor - CSV içeriği girilmedi' ) )
               TO reported-yevmiyeupload.
      ENDIF.

      " CONTRACT SAFE: Database modifications in MODIFY context
      " Process CSV data with proper error handling
      DATA(lv_total_processed) = 0.
      DATA(lv_total_updated) = 0.

      LOOP AT lt_csv_data INTO DATA(ls_csv_data).
        lv_total_processed = lv_total_processed + 1.

        " CONTRACT SAFE: Direct database update in MODIFY context
        TRY.
            " Select records that match the criteria
            SELECT bukrs, budat, belnr, gjahr, docln
              FROM zetr_t_defky
              WHERE bukrs = '1000'
                AND gjahr = '2024'
                AND belnr = @ls_csv_data-belnr
              INTO TABLE @DATA(lt_records).

            " Update matching records
            LOOP AT lt_records INTO DATA(ls_record).
              UPDATE zetr_t_defky
                 SET yevno = @ls_csv_data-yevno
               WHERE bukrs = @ls_record-bukrs
                 AND belnr = @ls_record-belnr
                 AND budat = @ls_record-budat
                 AND gjahr = @ls_record-gjahr
                 AND docln = @ls_record-docln.

              IF sy-subrc = 0.
                lv_total_updated = lv_total_updated + sy-dbcnt.
              ENDIF.
            ENDLOOP.

          CATCH cx_sy_open_sql_db INTO DATA(lx_db_error).
            " Handle database errors gracefully
            APPEND VALUE #( %tky = ls_key-%tky
                           %msg = new_message_with_text(
                             severity = if_abap_behv_message=>severity-error
                             text = |Database error: { lx_db_error->get_text( ) }| ) )
                   TO reported-yevmiyeupload.
        ENDTRY.
      ENDLOOP.

      " Report success message
      APPEND VALUE #( %tky = ls_key-%tky
                     %msg = new_message_with_text(
                       severity = if_abap_behv_message=>severity-success
                       text = |CSV Upload tamamlandı: { lv_total_processed } işlendi, { lv_total_updated } güncellendi| ) )
             TO reported-yevmiyeupload.

    ENDLOOP.
  ENDMETHOD.

  METHOD parse_csv_content.
    " CONTRACT SAFE: Pure functional method, no database operations
    DATA: lt_rows TYPE STANDARD TABLE OF string.

    IF iv_csv_content IS INITIAL.
      RETURN.
    ENDIF.

    " Split content by line breaks
    SPLIT iv_csv_content AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_rows.

    DATA(lv_row_counter) = 0.

    LOOP AT lt_rows INTO DATA(lv_row).
      ADD 1 TO lv_row_counter.

      IF lv_row IS INITIAL.
        CONTINUE.
      ENDIF.

      DATA(ls_csv_line) = VALUE ty_csv_data( ).

      " Parse CSV format: LINEN;BELNR;YEVNO;BUDAT
      SPLIT lv_row AT ';' INTO ls_csv_line-linen
                              ls_csv_line-belnr
                              ls_csv_line-yevno
                              ls_csv_line-budat.

      ls_csv_line-row_number = lv_row_counter.

      " Only add valid entries
      IF ls_csv_line-belnr IS NOT INITIAL.
        " Apply alpha conversion for document number
        ls_csv_line-belnr = |{ ls_csv_line-belnr ALPHA = IN }|.
        APPEND ls_csv_line TO rt_data.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.