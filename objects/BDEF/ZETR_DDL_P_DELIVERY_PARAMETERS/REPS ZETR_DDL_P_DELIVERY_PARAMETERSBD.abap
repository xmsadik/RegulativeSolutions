projection;
strict ( 2 );
use side effects;

define behavior for zetr_ddl_p_delivery_parameters alias DeliveryParameters
{
  use create;
  use update;
  use delete;

  use association _customParameters { create; }
  use association _deliverySerials { create; }
  use association _xsltTemplates { create; }
  use association _deliveryRules { create; }
}

define behavior for zetr_ddl_p_delivery_custom alias CustomParameters
{
  use update;
  use delete;

  use association _eDeliveryParameters;
}

define behavior for zetr_ddl_p_delivery_serials alias Serials
{
  use update;
  use delete;

  use association _eDeliveryParameters;
  use association _numberStatus { }
  use action createNumbers;
}

define behavior for zetr_ddl_p_delivery_numstat alias NumberStatus
{
  use update;
  use delete;

  use association _deliverySerials;
  use association _eDeliveryParameters;
}

define behavior for zetr_ddl_p_delivery_xslttemp alias XSLTTemplates
{
  use update;
  use delete;

  use association _eDeliveryParameters;
}

define behavior for zetr_ddl_p_delivery_rules alias DeliveryRules
{
  use update;
  use delete;

  use association _eDeliveryParameters;
}