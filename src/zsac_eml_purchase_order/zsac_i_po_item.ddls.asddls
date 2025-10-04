@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Purchase Order Item'
define view entity ZSAC_I_PO_ITEM
  as select from zsac_po_item
  association to parent ZSAC_I_PO_HEADER as _PurchaseOrderHeader on $projection.po_number = _PurchaseOrderHeader.po_number
  composition [0..*] of ZSAC_I_PO_ACCOUNT as _PurchaseOrderAccount
  composition [0..*] of ZSAC_I_PO_HISTORY as _PurchaseOrderHistory
{
  key po_number,
  key item_number,
      material_number,
      plant,
      @Semantics.quantity.unitOfMeasure: 'unit_of_measure'
      quantity,
      unit_of_measure,
      @Semantics.amount.currencyCode: 'currency'
      net_price,
      @Semantics.amount.currencyCode: 'currency'
      item_value,
      item_text,
      delivery_date,
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
      _PurchaseOrderHeader,
      _PurchaseOrderAccount,
      _PurchaseOrderHistory
}
