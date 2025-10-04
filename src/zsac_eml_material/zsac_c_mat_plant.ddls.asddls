@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Material Plant Projection'
@Metadata.allowExtensions: true
define view entity ZSAC_C_MAT_PLANT
  as projection on ZSAC_I_MAT_PLANT
{
  key material_number,
  key plant,
      procurement_type,
      mrp_controller,
      lot_size,
      createdby,
      createdat,
      lastchangedby,
      lastchangedat,
      locallastchangedat,
      
      // Associations
      _MaterialHeader : redirected to parent ZSAC_C_MAT_HEADER
}
