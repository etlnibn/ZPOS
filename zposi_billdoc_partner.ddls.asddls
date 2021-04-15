@ClientHandling.algorithm: #SESSION_VARIABLE
@ClientHandling.type: #CLIENT_DEPENDENT
@VDM: {
  viewType: #BASIC
 }
@AbapCatalog.sqlViewName: 'ZPOS_BILLPRTNR'
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Bill Doc Partner - ARM Invoice Search'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #TRANSACTIONAL
}
define view ZPOSI_BILLDOC_PARTNER as select from I_BillingDocumentBasic as BillingDocument 
       inner join I_BusinessPartner as Partner on  Partner.BusinessPartner = BillingDocument.SoldToParty

{
      @Consumption.valueHelpDefinition: [ 
        { entity:  { name:    'I_BillingDocumentStdVH',
                     element: 'BillingDocument' }
        }]
      // ]--GENERATED
    key BillingDocument.BillingDocument   as DocumentNumber,                                                                                                   
    $session.client                       as client,
    BillingDocument.SalesOrganization     as SalesOrg,
    BillingDocument.DistributionChannel   as DistChannel,
    BillingDocument.Division              as Division,
    BillingDocument.BillingDocumentDate   as DocumentDate,    
    BillingDocument.SoldToParty           as SoldToParty,    
    BillingDocument.TransactionCurrency   as Currency,    
    SDDocumentCategory                    as OrderCategory,
    BillingDocumentCategory               as BillingCategory,
    BillingDocumentType                   as BillingType,
    BillingDocumentIsCancelled            as IsCancelled,
    @Semantics.amount.currencyCode: 'Currency'
    TotalNetAmount                        as InvoiceNetAmount,
    @Semantics.amount.currencyCode: 'Currency'
    TotalTaxAmount                        as InvoiceTaxAmount,
    @Semantics.amount.currencyCode: 'Currency'
    TotalNetAmount + TotalTaxAmount       as InvoiceGrossAmount, 
    VATRegistration                       as VATNumber,
 
    BillingDocument._ItemBasic.BillingDocumentItem        as DocumentItem,
    BillingDocument._ItemBasic.Material                   as Article,
    BillingDocument._ItemBasic.InternationalArticleNumber as GTIN,
    BillingDocument._ItemBasic._MaterialText[1: Language= $session.system_language].MaterialName as ArticleDescription,
    BillingDocument._ItemBasic.BillingQuantityUnit        as Unit,
    @Semantics.quantity.unitOfMeasure: 'Unit'
    BillingDocument._ItemBasic.BillingQuantity            as Quantity,
//  This is just a placeholder until I develop a funciton for checking the open quantity (invoice minus returns).
    @Semantics.quantity.unitOfMeasure: 'Unit'
    BillingDocument._ItemBasic.BillingQuantity            as OpenQuantity,

    @Semantics.amount.currencyCode: 'Currency'
    BillingDocument._ItemBasic.NetAmount                  as NetAmount,
    @Semantics.amount.currencyCode: 'Currency'
    BillingDocument._ItemBasic.TaxAmount                  as TaxAmount,
    @Semantics.amount.currencyCode: 'Currency'
    BillingDocument._ItemBasic.NetAmount + BillingDocument._ItemBasic.TaxAmount as GrossAmount,
    cast( 0 as abap.int2 )                                as InvoiceRowCount,                   
    

// - Partner Data
    Partner._DefaultAddress._Address.SearchTerm1          as Search1,
    Partner._DefaultAddress._Address.SearchTerm1          as Search2,
    Partner.BusinessPartnerFullName                       as Name,    

// - Address Data
    Partner._DefaultAddress._Address.Person               as PersonNumber,
    Partner._DefaultAddress._Address.AddressID,
    Partner._DefaultAddress._Address._DefaultEmailAddress.EmailAddress as EmailAddress,
    Partner._DefaultAddress._Address._DefaultMobilePhoneNumber.PhoneNumber as Mobile
//    Partner._DefaultAddress._Address._DefaultPhoneNumber.PhoneNumber as Phone
       
}












































