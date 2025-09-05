  METHOD unit.

    FIELD-SYMBOLS <fs_unit> TYPE zif_etr_edf_xml_y=>ty_unit.

    FIELD-SYMBOLS <ft_unit> TYPE zif_etr_edf_xml_y=>tty_unit.

    CASE iv_xmlty.
      WHEN 1.
        ASSIGN ('ms_root_y-unit')      TO <ft_unit>.
      WHEN 2.
        ASSIGN ('ms_root_yb-unit')     TO <ft_unit>.
      WHEN 3.
        ASSIGN ('ms_root_gib_yb-unit') TO <ft_unit>.
      WHEN 4.
        ASSIGN ('ms_root_k-unit')      TO <ft_unit>.
      WHEN 5.
        ASSIGN ('ms_root_kb-unit')     TO <ft_unit>.
      WHEN 6.
        ASSIGN ('ms_root_gib_kb-unit') TO <ft_unit>.
      WHEN 7.
        ASSIGN ('ms_root_dr-unit')     TO <ft_unit>.
    ENDCASE.

    DO 2 TIMES.
      APPEND INITIAL LINE TO <ft_unit> ASSIGNING <fs_unit>.
      IF <fs_unit> IS ASSIGNED.
        CASE sy-tabix.
          WHEN 1.
            <fs_unit>-id = ms_header-waers.
            TRANSLATE <fs_unit>-id TO LOWER CASE.
            CONCATENATE 'iso4217:'
                        ms_header-waers
                        INTO
                        <fs_unit>-measure-content.
          WHEN 2.
            <fs_unit>-id = 'countable'.
            <fs_unit>-measure-content = 'xbrli:pure'.
        ENDCASE.
        UNASSIGN <fs_unit>.
      ENDIF.
    ENDDO.

  ENDMETHOD.