@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Purchase Order Account Consumption'
@Metadata.allowExtensions: true
define view entity ZSAC_C_PO_ACCOUNT
  as projection on ZSAC_I_PO_ACCOUNT
{
  key po_number,
  key item_number,
  key assignment_number,
      account_category,
      cost_center,
      gl_account,
      assignment_value,
      percentage,
      createdby,
      createdat,
      lastchangedby,
      lastchangedat,
      locallastchangedat,
      @EndUserText.label: 'Currency'
      currency,
      
      // Just expose associations defined in interface view
      _PurchaseOrderItem : redirected to parent ZSAC_C_PO_ITEM,
      _PurchaseOrderHeader : redirected to ZSAC_C_PO_HEADER
}
