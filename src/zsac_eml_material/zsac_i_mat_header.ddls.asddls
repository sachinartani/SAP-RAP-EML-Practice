@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Material Header'
define root view entity ZSAC_I_MAT_HEADER
  as select from zsac_mat_header
  composition [0..*] of ZSAC_I_MAT_PLANT as _MaterialPlant
{
  key material_number,
      material_type,
      material_group,
      base_unit,
      material_desc,
      industry_sector,
      @Semantics.user.createdBy: true
      createdby,
      @Semantics.systemDateTime.createdAt: true
      createdat,
      @Semantics.user.lastChangedBy: true
      lastchangedby,
      @Semantics.systemDateTime.lastChangedAt: true
      lastchangedat,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      locallastchangedat,
      
      // Associations
      _MaterialPlant
}
