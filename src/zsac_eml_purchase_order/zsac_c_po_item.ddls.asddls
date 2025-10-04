@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Purchase Order Item Consumption'
@Metadata.allowExtensions: true
define view entity ZSAC_C_PO_ITEM
  as projection on ZSAC_I_PO_ITEM
{
  key po_number,
  key item_number,
      material_number,
      plant,
      quantity,
      unit_of_measure,
      net_price,
      item_value,
      item_text,
      delivery_date,
      createdby,
      createdat,
      lastchangedby,
      lastchangedat,
      locallastchangedat,
      @EndUserText.label: 'Currency'
      currency,
      
      // Associations - redirected to consumption views
      _PurchaseOrderHeader : redirected to parent ZSAC_C_PO_HEADER,
      _PurchaseOrderAccount : redirected to composition child ZSAC_C_PO_ACCOUNT,
      _PurchaseOrderHistory : redirected to composition child ZSAC_C_PO_HISTORY
}
