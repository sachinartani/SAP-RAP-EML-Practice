@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Purchase Order History Consumption'
@Metadata.allowExtensions: true
define view entity ZSAC_C_PO_HISTORY
  as projection on ZSAC_I_PO_HISTORY
{
  key po_number,
  key item_number,
  key history_number,
      document_number,
      movement_type,
      posting_date,
      quantity,
      amount,
      reference_doc,
      createdby,
      createdat,
      lastchangedby,
      lastchangedat,
      locallastchangedat,
      @EndUserText.label: 'Currency'
      currency,
      unit_of_measure,
      
      // Just expose associations defined in interface view
      _PurchaseOrderItem : redirected to parent ZSAC_C_PO_ITEM,
      _PurchaseOrderHeader : redirected to ZSAC_C_PO_HEADER
}
