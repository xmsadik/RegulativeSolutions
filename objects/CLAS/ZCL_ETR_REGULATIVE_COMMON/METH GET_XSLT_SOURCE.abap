  METHOD get_xslt_source.
    DATA lv_xslt_source TYPE string.
    DATA(lo_transformation) = xco_cp_abap_repository=>object->xslt->for( CONV #( iv_xslt_name ) ).
    DATA(ls_transformation) = lo_transformation->content( )->get( ).
    DATA(lt_source_code) = ls_transformation-source_code->if_xco_text~get_lines( )->value.
    lv_xslt_source = REDUCE #( INIT concat TYPE string
                                    sep = ``
                               FOR ls_xslt_source IN lt_source_code
                               NEXT concat = concat && sep && ls_xslt_source
                                    sep = ` ` ).
    rv_xslt_source = cl_abap_conv_codepage=>create_out( )->convert( source = lv_xslt_source ).
  ENDMETHOD.