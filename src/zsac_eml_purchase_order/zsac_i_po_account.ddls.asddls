@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Purchase Order Account Assignment'
define view entity ZSAC_I_PO_ACCOUNT
  as select from zsac_po_account
  association to parent ZSAC_I_PO_ITEM as _PurchaseOrderItem on $projection.po_number = _PurchaseOrderItem.po_number
                                                              and $projection.item_number = _PurchaseOrderItem.item_number
  association [1..1] to ZSAC_I_PO_HEADER as _PurchaseOrderHeader on $projection.po_number = _PurchaseOrderHeader.po_number
{
  key po_number,
  key item_number,
  key assignment_number,
      account_category,
      cost_center,
      gl_account,
      @Semantics.amount.currencyCode: 'currency'
      assignment_value,
      percentage,
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
      
      // Get currency from header
      _PurchaseOrderHeader.currency,
      
      // Associations
      _PurchaseOrderItem,
      _PurchaseOrderHeader
}
