  METHOD get_f51_params.
*    """satıcıya ibraz belge türü"""
*    DATA : ls_tstcp     TYPE tstcp,
*           lv_temp1(50) TYPE c,
*           lv_temp2(50) TYPE c,
*           lv_temp3(50) TYPE c.
*
*    CLEAR : ls_tstcp,
*            gv_f51_blart,
*            gv_f51_tcode,
*            lv_temp1,lv_temp2,lv_temp3.
*
*    SELECT SINGLE *
*      INTO CORRESPONDING FIELDS OF ls_tstcp
*      FROM tstcp
*      WHERE tcode EQ 'F-51'.
*    IF ls_tstcp-param+0(2) EQ '/N'.
*      ls_tstcp-param+0(2) = space.
*      CONDENSE ls_tstcp-param.
*      SPLIT ls_tstcp-param AT space INTO lv_temp1 lv_temp2.
*
*      IF lv_temp1 IS NOT INITIAL.
*        SELECT SINGLE COUNT(*)
*          FROM tstc
*         WHERE tcode EQ lv_temp1.
*        IF sy-subrc EQ 0.
*          gv_f51_tcode = lv_temp1.
*
*          CLEAR : lv_temp1.
*          SPLIT lv_temp2 AT ';'
*          INTO lv_temp1 lv_temp3.
*
*          FIND 'BKPF-BLART=' IN lv_temp1.
*          IF sy-subrc EQ 0.
*            REPLACE 'BKPF-BLART=' IN lv_temp1
*            WITH space.
*            CONDENSE lv_temp1.
*
*            SELECT SINGLE COUNT(*) FROM t003
*              WHERE blart EQ lv_temp1.
*            IF sy-subrc EQ 0.
*              gv_f51_blart = lv_temp1.
*            ENDIF.
*
*          ENDIF.
*
*        ENDIF.
*      ENDIF.
*    ENDIF.
  ENDMETHOD.