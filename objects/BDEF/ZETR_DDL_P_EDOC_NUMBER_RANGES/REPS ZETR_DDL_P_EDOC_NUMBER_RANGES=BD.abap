projection;
strict ( 2 );

define behavior for zetr_ddl_p_edoc_number_ranges alias NumberRanges
{
  use update;
  use association _NumberStatus;
}

define behavior for zetr_ddl_p_edoc_number_status alias NumberStatus
{
  use update;
  use delete;

  use association _Ranges;
}