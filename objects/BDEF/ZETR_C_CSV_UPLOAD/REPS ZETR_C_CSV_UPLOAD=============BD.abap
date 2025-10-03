projection;
strict ( 2 );
//  use draft;
define behavior for ZETR_C_CSV_UPLOAD alias CSVUpload
{

  use create;
  use update;
  use delete;

  // ⭐ ACTION'I EXPOSE ET
  use action processcsvfile;

}