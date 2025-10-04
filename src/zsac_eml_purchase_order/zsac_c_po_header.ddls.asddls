@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Purchase Order Header Consumption'
@Metadata.allowExtensions: true
define root view entity ZSAC_C_PO_HEADER
  provider contract transactional_query
  as projection on ZSAC_I_PO_HEADER
{
  key po_number,
      company_code,
      purch_organization,
      vendor_number,
      document_date,
      currency,
      total_value,
      @ObjectModel.text.element: [ 'POStatusDesc' ]
      po_status,
      createdby,
      createdat,
      lastchangedby,
      lastchangedat,
      locallastchangedat,
      POStatusDesc,
      
      // Associations - redirected to consumption views
      _PurchaseOrderItem : redirected to composition child ZSAC_C_PO_ITEM,
      _POStatus
}
