projection;
strict ( 1 );

define behavior for zetr_ddl_p_uploaded_documents alias UploadedDocuments
{
  use create;
  use update;
  use delete;

  use action processDocument;
}