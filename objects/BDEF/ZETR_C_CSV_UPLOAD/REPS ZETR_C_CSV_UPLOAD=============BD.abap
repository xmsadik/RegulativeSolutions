projection;
// strict ( 2 );  ⭐ STRICT KALDIRILDI!

define behavior for ZETR_C_CSV_UPLOAD alias CSVUpload
{
  use create;
  use update;
  use delete;

  use action processcsvfile;
}