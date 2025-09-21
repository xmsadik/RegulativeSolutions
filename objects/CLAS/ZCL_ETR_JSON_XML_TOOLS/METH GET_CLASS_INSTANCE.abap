  METHOD get_class_instance.
    IF lo_object IS INITIAL.
      CREATE OBJECT lo_object.
    ENDIF.

    Rt_OBJECT = lo_object.
  ENDMETHOD.