  METHOD main.
    DATA(lo_transformation) = xco_cp_abap_repository=>object->xslt->for( co_transformation_name ).
    out->write( lo_transformation ).

    " LS_TRANSFORMATION will contain both the short description and the source code of the
    " transformation.
    DATA(ls_transformation) = lo_transformation->content( )->get( ).
    out->write( ls_transformation ).

    " The source code of the transformation is represented as an object of type IF_XCO_TF_SOURCE_CODE.
    " Via the IF_XCO_TEXT~GET_LINES method the source code can also be easily obtained as a string_table.
    DATA(lt_source_code) = ls_transformation-source_code->if_xco_text~get_lines( )->value.
    out->write( lt_source_code ).
  ENDMETHOD.