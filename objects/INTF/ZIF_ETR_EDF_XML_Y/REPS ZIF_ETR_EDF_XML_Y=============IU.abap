INTERFACE zif_etr_edf_xml_y
  PUBLIC .

  TYPES BEGIN OF ty_identifier.
  TYPES scheme TYPE string.
  TYPES content TYPE string.
  TYPES END OF ty_identifier.

  TYPES BEGIN OF ty_instant.
  TYPES content TYPE string.
  TYPES END OF ty_instant.

  TYPES BEGIN OF ty_entity.
  TYPES identifier TYPE ty_identifier.
  TYPES END OF ty_entity.

  TYPES BEGIN OF ty_measure.
  TYPES content TYPE string.
  TYPES END OF ty_measure.

  TYPES BEGIN OF ty_unit.
  TYPES id      TYPE string.
  TYPES measure TYPE ty_measure.
  TYPES END OF ty_unit.

  TYPES tty_unit TYPE TABLE OF ty_unit.

  TYPES BEGIN OF ty_period.
  TYPES instant TYPE ty_instant.
  TYPES END OF ty_period.

  TYPES BEGIN OF ty_context.
  TYPES entity TYPE ty_entity.
  TYPES period TYPE ty_period.
  TYPES END OF ty_context.

  TYPES BEGIN OF ty_common.
  TYPES contextref TYPE string.
  TYPES content    TYPE string.
  TYPES END OF ty_common.

  TYPES BEGIN OF ty_common_dec.
  TYPES contextref TYPE string.
  TYPES decimals   TYPE string.
  TYPES unitref    TYPE string.
  TYPES content    TYPE string.
  TYPES END OF ty_common_dec.

  TYPES BEGIN OF ty_documentinfo.
  TYPES entriestype        TYPE ty_common.
  TYPES uniqueid           TYPE ty_common.
  TYPES language           TYPE ty_common.
  TYPES creationdate       TYPE ty_common.
  TYPES creator            TYPE ty_common.
  TYPES entriescomment     TYPE ty_common.
  TYPES periodcoveredstart TYPE ty_common.
  TYPES periodcoveredend   TYPE ty_common.
  TYPES sourceapplication  TYPE ty_common.
  TYPES END OF ty_documentinfo.

  TYPES BEGIN OF ty_entityphonenumber.
  TYPES phonenumberdescription TYPE ty_common.
  TYPES phonenumber            TYPE ty_common.
  TYPES END OF ty_entityphonenumber.


  TYPES BEGIN OF ty_entityfaxnumberstructure.
  TYPES entityfaxnumber TYPE ty_common.
  TYPES END OF ty_entityfaxnumberstructure.

  TYPES BEGIN OF ty_entityemailaddressstructure.
  TYPES entityemailaddress TYPE ty_common.
  TYPES END OF ty_entityemailaddressstructure.

  TYPES BEGIN OF ty_organizationidentifiers.
  TYPES organizationidentifier TYPE ty_common.
  TYPES organizationdescription TYPE ty_common.
  TYPES END OF ty_organizationidentifiers.

  TYPES BEGIN OF ty_organizationaddress.
  TYPES organizationbuildingnumber         TYPE ty_common.
  TYPES organizationaddressstreet          TYPE ty_common.
  TYPES organizationaddressstreet2         TYPE ty_common.
  TYPES organizationaddresscity            TYPE ty_common.
  TYPES orgaddressziporpostalcode          TYPE ty_common."organizationAddressZipOrPostalCode
  TYPES organizationaddresscountry         TYPE ty_common.
  TYPES END OF ty_organizationaddress.

  TYPES BEGIN OF ty_entitywebsite.
  TYPES websiteurl TYPE ty_common.
  TYPES END OF ty_entitywebsite.

  TYPES BEGIN OF ty_accountantaddress.
  TYPES accountantbuildingnumber   TYPE ty_common.
  TYPES accountantstreet           TYPE ty_common.
  TYPES accountantaddressstreet2   TYPE ty_common.
  TYPES accountantcity             TYPE ty_common.
  TYPES accountantcountry          TYPE ty_common.
  TYPES accountantziporpostalcode  TYPE ty_common.
  TYPES END OF ty_accountantaddress.

  TYPES BEGIN OF ty_accountantcontactphone.
  TYPES accountantcontphonenumberdesc TYPE ty_common. "accountantcontactphonenumberdescription
  TYPES accountantcontactphonenumber  TYPE ty_common.
  TYPES END OF ty_accountantcontactphone.

  TYPES BEGIN OF ty_accountantcontactfax.
  TYPES accountantcontactfaxnumber TYPE ty_common.
  TYPES END OF ty_accountantcontactfax.

  TYPES BEGIN OF ty_accountantcontactemail.
  TYPES accountantcontactemailaddress TYPE ty_common.
  TYPES END OF ty_accountantcontactemail.


  TYPES BEGIN OF ty_accountantcontactinform. "accountantContactInformation
  TYPES accountantcontactphone TYPE ty_accountantcontactphone.
  TYPES accountantcontactfax   TYPE ty_accountantcontactfax.
  TYPES accountantcontactemail TYPE ty_accountantcontactemail.
  TYPES END OF ty_accountantcontactinform.


  TYPES BEGIN OF ty_accountantinformation.
  TYPES accountantname                      TYPE ty_common.
  TYPES accountantaddress                   TYPE ty_accountantaddress.
  TYPES accountantengagementtypedesc        TYPE ty_common.
  TYPES accountantcontactinformation        TYPE ty_accountantcontactinform.
  TYPES END OF ty_accountantinformation.

  TYPES tty_accountantinformation TYPE STANDARD TABLE OF ty_accountantinformation WITH DEFAULT KEY.

  TYPES BEGIN OF ty_entityinformation.
  TYPES entityphonenumber                   TYPE ty_entityphonenumber.
  TYPES entityfaxnumberstructure            TYPE ty_entityfaxnumberstructure.
  TYPES entityemailaddressstructure         TYPE ty_entityemailaddressstructure.
  TYPES organizationidentifiers             TYPE ty_organizationidentifiers.
  TYPES organizationaddress                 TYPE ty_organizationaddress.
  TYPES entitywebsite                       TYPE ty_entitywebsite.
  TYPES businessdescription                 TYPE ty_common.
  TYPES fiscalyearstart                     TYPE ty_common.
  TYPES fiscalyearend                       TYPE ty_common.
  TYPES accountantinformation               TYPE tty_accountantinformation.
  TYPES END OF ty_entityinformation.

  TYPES BEGIN OF ty_accountsub.
  TYPES accountsubdescription TYPE ty_common.
  TYPES accountsubid          TYPE ty_common.
  TYPES END OF ty_accountsub.

  TYPES BEGIN OF ty_account.
  TYPES accountmainid          TYPE ty_common.
  TYPES accountmaindescription TYPE ty_common.
  TYPES accountsub             TYPE ty_accountsub.
  TYPES END OF ty_account.

  TYPES BEGIN OF ty_entrydetail.
  TYPES linenumber        TYPE ty_common.
  TYPES linenumbercounter TYPE ty_common_dec.
  TYPES account           TYPE ty_account.
  TYPES amount            TYPE ty_common_dec.
  TYPES debitcreditcode   TYPE ty_common.
  TYPES postingdate       TYPE ty_common.
  TYPES documentType      TYPE ty_common.
  TYPES documentNumber    TYPE ty_common.
  TYPES documentreference TYPE ty_common.
  TYPES documentdate      TYPE ty_common.
  TYPES paymentMethod     TYPE ty_common.
  TYPES detailcomment     TYPE ty_common.
  TYPES END OF ty_entrydetail.

  TYPES tty_entrydetail TYPE STANDARD TABLE OF ty_entrydetail WITH DEFAULT KEY.

  TYPES BEGIN OF ty_entryheader.
  TYPES enteredby          TYPE ty_common.
  TYPES entereddate        TYPE ty_common.
  TYPES entrynumber        TYPE ty_common.
  TYPES entrycomment       TYPE ty_common.
  TYPES totaldebit         TYPE ty_common_dec.
  TYPES totalcredit        TYPE ty_common_dec.
  TYPES entrynumbercounter TYPE ty_common_dec.
  TYPES entrydetail        TYPE tty_entrydetail.
  TYPES END OF ty_entryheader.

  TYPES BEGIN OF ty_accountingentries.
  TYPES documentinfo      TYPE ty_documentinfo.
  TYPES entityinformation TYPE ty_entityinformation.
  TYPES entryheader       TYPE STANDARD TABLE OF ty_entryheader WITH DEFAULT KEY.
  TYPES END OF ty_accountingentries.

  TYPES BEGIN OF ty_root.
  TYPES context TYPE ty_context.
  TYPES unit    TYPE STANDARD TABLE OF ty_unit WITH DEFAULT KEY.
  TYPES accountingentries TYPE ty_accountingentries.
  TYPES END OF ty_root.
ENDINTERFACE.