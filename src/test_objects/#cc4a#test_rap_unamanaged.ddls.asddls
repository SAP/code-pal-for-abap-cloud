@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity /CC4A/TEST_RAP_UNAMANAGED
  as select from /cc4a/test_rap
{
  key key_field      as KeyFieldRoot,
      data_field     as DataFieldRoot,
      char_field     as CharFieldRoot,
      crea_date_time as Timestamp,
      lchg_date_time as LastChangedAt
}
