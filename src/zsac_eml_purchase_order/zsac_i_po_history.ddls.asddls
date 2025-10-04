@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Purchase Order History'
define view entity ZSAC_I_PO_HISTORY
  as select from zsac_po_history
  association to parent ZSAC_I_PO_ITEM as _PurchaseOrderItem on $projection.po_number = _PurchaseOrderItem.po_number
                                                              and $projection.item_number = _PurchaseOrderItem.item_number
  association [1..1] to ZSAC_I_PO_HEADER as _PurchaseOrderHeader on $projection.po_number = _PurchaseOrderHeader.po_number
{
  key po_number,
  key item_number,
  key history_number,
      document_number,
      movement_type,
      posting_date,
      @Semantics.quantity.unitOfMeasure: 'unit_of_measure'
      quantity,
      @Semantics.amount.currencyCode: 'currency'
      amount,
      reference_doc,
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
      
      // Get currency and UoM from header and item
      _PurchaseOrderHeader.currency,
      _PurchaseOrderItem.unit_of_measure,
      
      // Associations
      _PurchaseOrderItem,
      _PurchaseOrderHeader
}
