  METHOD check_regex.
    TRY.
        DATA(lo_regex) = cl_abap_regex=>create_pcre( pattern = iv_regex
                                                     ignore_case = abap_true   ).
        DATA(lo_matcher) = NEW cl_abap_matcher( regex = lo_regex
                                                text = iv_text ).
        WHILE lo_matcher->find_next( ) = abap_true.
          rv_submatch = lo_matcher->get_submatch( 1 ).
          IF rv_submatch IS NOT INITIAL.
            EXIT.
          ENDIF.
        ENDWHILE.
      CATCH cx_root INTO DATA(lx_regex_error).
    ENDTRY.
  ENDMETHOD.