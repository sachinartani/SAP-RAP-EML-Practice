@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Material Header Projection'
@Metadata.allowExtensions: true
define root view entity ZSAC_C_MAT_HEADER
  provider contract transactional_query
  as projection on ZSAC_I_MAT_HEADER
{
  key material_number,
      material_type,
      material_group,
      base_unit,
      material_desc,
      industry_sector,
      createdby,
      createdat,
      lastchangedby,
      lastchangedat,
      locallastchangedat,
      
      // Associations
      _MaterialPlant : redirected to composition child ZSAC_C_MAT_PLANT
}
