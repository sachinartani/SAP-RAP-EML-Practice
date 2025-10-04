@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Purchase Order Status Value Help'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSAC_I_PO_STATUS_VH as select from DDCDS_CUSTOMER_DOMAIN_VALUE_T( p_domain_name:'ZSAC_PO_STATUS_DOM' )

{
  key   value_low as Value,
      @Semantics.text: true
      text      as Description
}
where language =  $session.system_language
