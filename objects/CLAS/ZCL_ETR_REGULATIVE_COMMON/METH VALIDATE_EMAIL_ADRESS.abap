  METHOD validate_email_adress.
    DATA: lv_regular_expression TYPE string.
    DATA: lv_regular_expr_01 TYPE string
            VALUE '^([\w''-]+(?:\.[\w''-]+)*)@((?:[\w-]+\.)*\w[\w-]{0,66})'.
    DATA: lv_regular_expr_02 TYPE string
            VALUE '\.([a-z]{2,6}(?:\.[a-z]{2})?)$'.
    DATA: lo_matcher TYPE REF TO cl_abap_matcher,
          lv_match   TYPE c LENGTH 1.

* concatenate complete regular expression for a valid e-mail address
* ^([\w-]+(?:\.[\w-]+)*)@((?:[\w-]+\.)*\w[\w-]{0,66})\.([a-z]{2,6}(?:\.
* [a-z]{2})?)$
    CONCATENATE
    lv_regular_expr_01
    lv_regular_expr_02
    INTO lv_regular_expression.

    IF iv_email IS NOT INITIAL.
      lo_matcher = cl_abap_matcher=>create( pattern     = lv_regular_expression
                                            text        = iv_email
                                            ignore_case = 'X' ).

      lv_match = lo_matcher->match( ).
      IF lv_match IS NOT INITIAL.
        rv_ok = 'X'.
      ENDIF.
    ENDIF.
  ENDMETHOD.