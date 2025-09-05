INTERFACE zif_etr_xml_types
  PUBLIC .
*INTERFACE /itetr/if_edf_xml_types PUBLIC.

  "! HTK CSV Type
  TYPES ty_htk_csv TYPE zetr_s_htk_csv.

  "! SOVOS TXT Type
  TYPES ty_sovos_txt TYPE zetr_s_sov_txt.

  "! EFINANS CSV Type
  TYPES ty_efinans_csv TYPE zetr_s_efn_csv.

  "! EDOKSIS TXT Type
*  TYPES ty_edoksis_txt TYPE ZETR_tt_sdx_txt_item.

  "! ELG CSV Table Type
  TYPES ty_elg_csv TYPE TABLE OF zetr_s_elg_csv.

  "! Line Number Type
  TYPES linenumber TYPE string.

  "! Journal Detail Attributes Structure
  TYPES BEGIN OF ty_journaldetail_att.
*  INCLUDE TYPE if_any_include.
  TYPES:
    linenumber              TYPE string,
    accountmainid           TYPE string,
    accountmaindescription  TYPE string,
    accountsubid            TYPE string,
    accountsubdescription   TYPE string,
    amount                  TYPE string,
    debitcreditcode         TYPE string,
    documenttype            TYPE string,
    documenttypedescription TYPE string,
    documentnumber          TYPE string,
    documentreference       TYPE string,
    documentdate            TYPE string,
    paymentmethod           TYPE string,
    detailcomment           TYPE string.
  TYPES END OF ty_journaldetail_att.

  "! Account Main ID Type
  TYPES accountmainid TYPE string.

  "! Journal Item Attributes Structure
  TYPES BEGIN OF ty_journalitem_att.
  TYPES:
    enteredby          TYPE string,
    entereddate        TYPE string,
    entrynumber        TYPE string,
    entrycomment       TYPE string,
    totaldebit         TYPE string,
    totalcredit        TYPE string,
    entrynumbercounter TYPE string.
  TYPES END OF ty_journalitem_att.

  "! Content Structure
  TYPES BEGIN OF ty_content.
  TYPES content TYPE string.
  TYPES END OF ty_content.

  "! Journal Detail Structure
  TYPES BEGIN OF ty_journaldetail.
  INCLUDE TYPE ty_journaldetail_att.
  INCLUDE TYPE ty_content.
  TYPES END OF ty_journaldetail.

  "! Journal Detail Table Type
  TYPES tty_journaldetail TYPE STANDARD TABLE OF ty_journaldetail WITH EMPTY KEY.

  "! Journal Item Structure
  TYPES BEGIN OF ty_journalitem.
  INCLUDE TYPE ty_journalitem_att.
  INCLUDE TYPE ty_content.
  TYPES:
    journaldetail TYPE tty_journaldetail.
  TYPES END OF ty_journalitem.

  "! Journal Item Table Type
  TYPES tty_journalitem TYPE STANDARD TABLE OF ty_journalitem WITH EMPTY KEY.

  "! Journal List Structure
  TYPES BEGIN OF ty_journallist.
  INCLUDE TYPE ty_content.
  TYPES:
    journalitem TYPE tty_journalitem.
  TYPES END OF ty_journallist.

  "! Journal List Table Type
  TYPES tty_journallist TYPE STANDARD TABLE OF ty_journallist WITH EMPTY KEY.

  "! Information Structure
  TYPES BEGIN OF ty_information.
  TYPES:
    branchcode           TYPE ty_content,
    branchdescription    TYPE ty_content,
    filecontentstartdate TYPE ty_content,
    filecontentenddate   TYPE ty_content.
  TYPES END OF ty_information.

  "! Upload File Result Container Structure
  TYPES BEGIN OF ty_uploadfileresult_container.
  TYPES:
    status      TYPE ty_content,
    description TYPE ty_content,
    date        TYPE ty_content.
  TYPES END OF ty_uploadfileresult_container.

  "! Upload File Result Structure
  TYPES BEGIN OF ty_uploadfileresult.
  TYPES uploadfileresult TYPE ty_uploadfileresult_container.
  TYPES END OF ty_uploadfileresult.

  "! Upload File Response Structure
  TYPES BEGIN OF ty_uploadfileresponse.
  TYPES uploadfileresponse TYPE ty_uploadfileresult.
  TYPES END OF ty_uploadfileresponse.

  "! Body Structure
  TYPES BEGIN OF ty_body.
  TYPES body TYPE ty_uploadfileresponse.
  TYPES END OF ty_body.

  "! Envelope Structure
  TYPES BEGIN OF ty_envelope.
  TYPES envelope TYPE ty_body.
  TYPES END OF ty_envelope.

  "! Line Structure
  TYPES BEGIN OF ty_line.
  TYPES:
    information TYPE ty_information,
    journallist TYPE tty_journallist.
  TYPES END OF ty_line.

  "! Line Table Type
  TYPES tty_line TYPE STANDARD TABLE OF ty_line WITH EMPTY KEY.

  "! KOC Structure
  TYPES BEGIN OF ty_koc.
  TYPES lines TYPE tty_line.
  TYPES END OF ty_koc.

  "! VBT Detail Structure
  TYPES BEGIN OF ty_vbt_detail.
  TYPES:
    linenumber              TYPE string,
    linenumbercounter       TYPE string,
    accountmainid           TYPE string,
    accountmaindescription  TYPE string,
    accountsubdescription   TYPE string,
    accountsubid            TYPE string,
    amount                  TYPE string,
    debitcreditcode         TYPE string,
    postingdate             TYPE string,
    documenttype            TYPE string,
    documenttypedescription TYPE string,
    documentnumber          TYPE string,
    documentreference       TYPE string,
    documentdate            TYPE string,
    paymentmethod           TYPE string,
    detailcomment           TYPE string.
  TYPES END OF ty_vbt_detail.

  "! VBT Header Structure
  TYPES BEGIN OF ty_vbt_header.
  TYPES:
    enteredby        TYPE string,
    entereddate      TYPE string,
    entrynumber      TYPE string,
    entrycomment     TYPE string,
    totaldebit       TYPE string,
    totalcredit      TYPE string,
    entrynumbercount TYPE string,
    entry_detail     TYPE TABLE OF ty_vbt_detail WITH EMPTY KEY.
  TYPES END OF ty_vbt_header.

  "! VBT Structure
  TYPES BEGIN OF ty_vbt.
  TYPES:
    companyvkn   TYPE string,
    periodstart  TYPE string,
    periodend    TYPE string,
    branchcode   TYPE string,
    entry_header TYPE TABLE OF ty_vbt_header WITH EMPTY KEY.
  TYPES END OF ty_vbt.

  "! Yevmiye Kayet XML Result Structure
  TYPES BEGIN OF ty_yevmiyekayetxmlresult.
  TYPES:
    success     TYPE string,
    code        TYPE string,
    description TYPE string.
  TYPES END OF ty_yevmiyekayetxmlresult.

  "! Yevmiye Kayet XML Response Structure
  TYPES BEGIN OF ty_yevmiyekayetxmlresponse.
  TYPES yevmiyekayetxmlresult TYPE ty_yevmiyekayetxmlresult.
  TYPES END OF ty_yevmiyekayetxmlresponse.

  "! Body VBT Structure
  TYPES BEGIN OF ty_body_vbt.
  TYPES yevmiyekayetxmlresponse TYPE ty_yevmiyekayetxmlresponse.
  TYPES END OF ty_body_vbt.

  "! Envelope VBT Structure
  TYPES BEGIN OF ty_envelope_vbt.
  TYPES body TYPE ty_body_vbt.
  TYPES END OF ty_envelope_vbt.

  "! Response VBT Structure
  TYPES BEGIN OF ty_response_vbt.
  TYPES envelope TYPE ty_envelope_vbt.
  TYPES END OF ty_response_vbt.

*ENDINTERFACE.
ENDINTERFACE.