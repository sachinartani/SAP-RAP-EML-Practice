@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Material Plant Data'
define view entity ZSAC_I_MAT_PLANT
  as select from zsac_mat_plant
  association to parent ZSAC_I_MAT_HEADER as _MaterialHeader on $projection.material_number = _MaterialHeader.material_number
{
  key material_number,
  key plant,
      procurement_type,
      mrp_controller,
      lot_size,
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
      _MaterialHeader
}
