@ClientHandling.algorithm: #SESSION_VARIABLE
@ClientHandling.type: #CLIENT_DEPENDENT
@VDM: {
  viewType: #CONSUMPTION
 }
@AbapCatalog.sqlViewName: 'ZPOSC_BILLPRTNR'
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Bill Doc Partner - ARM Invoice Search'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #TRANSACTIONAL
}
define view ZPOSC_BILLDOC_PARTNER
  as select from ZPOSTF_BILLDOC_PARTNER( clnt: $session.client )  as BillingDetails
{
  DocumentNumber,
  SalesOrg,
  DistChannel,
  Division,
  DocumentDate,
  SoldToParty,
  Currency,
  OrderCategory,
  BillingCategory,
  BillingType,
  IsCancelled,
  InvoiceNetAmount,
  InvoiceTaxAmount,
  InvoiceGrossAmount,
  VATNumber,
  DocumentItem,
  Article,
  GTIN,
  ArticleDescription,
  Unit,
  Quantity,
  OpenQuantity,
  NetAmount,
  TaxAmount,
  GrossAmount,
  InvoiceRowCount,
  Search1,
  Search2,
  Name,
  PersonNumber,
  AddressID,
  EmailAddress,
  Mobile
}
