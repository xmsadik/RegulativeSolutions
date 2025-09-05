  PRIVATE SECTION.

    METHODS deserialize_node
      IMPORTING
        !json   TYPE string
      CHANGING
        !offset TYPE i DEFAULT 0
        !node   TYPE any .
    METHODS deserialize_object
      IMPORTING
        !json   TYPE string
      CHANGING
        !offset TYPE i DEFAULT 0
        !node   TYPE any .
    METHODS deserialize_array
      IMPORTING
        !json   TYPE string
      CHANGING
        !offset TYPE i DEFAULT 0
        !node   TYPE any .