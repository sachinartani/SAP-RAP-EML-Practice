@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Purchase Order Header'
define root view entity ZSAC_I_PO_HEADER
  as select from zsac_po_header
  association [1] to ZSAC_I_PO_STATUS_VH as _POStatus on $projection.po_status = _POStatus.Value
  composition [0..*] of ZSAC_I_PO_ITEM as _PurchaseOrderItem
{
  key po_number,
      company_code,
      purch_organization,
      vendor_number,
      document_date,
      currency,
      @Semantics.amount.currencyCode: 'currency'
      total_value,
      po_status,
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
      
      _POStatus.Description as POStatusDesc,
      
      // Associations
      _PurchaseOrderItem,
      _POStatus
}
