@ClientHandling.algorithm: #SESSION_VARIABLE
@ClientHandling.type: #CLIENT_DEPENDENT
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'TabFunc - Billing Document & Partner'
define table function ZPOSTF_BILLDOC_PARTNER
  with parameters
    @Environment.systemField: #CLIENT
    clnt :abap.clnt
returns
{
  client             : s_mandt;
  DocumentNumber     : vbeln;
  SalesOrg           : vkorg;
  DistChannel        : vtweg;
  Division           : spart;
  DocumentDate       : fkdat;
  SoldToParty        : kunnr;
  Currency           : waers;
  OrderCategory      : vbtyp;
  BillingCategory    : fktyp;
  BillingType        : fkart;
  IsCancelled        : fksto;
  @Semantics.amount.currencyCode: 'Currency'
  InvoiceNetAmount   : netwr;
  @Semantics.amount.currencyCode: 'Currency'
  InvoiceTaxAmount   : mwsbp;
  @Semantics.amount.currencyCode: 'Currency'
  InvoiceGrossAmount : netwr;
  VATNumber          : stceg;
  DocumentItem       : posnr;
  Article            : matnr;
  GTIN               : ean11;
  ArticleDescription : maktx;
  Unit               : vrkme;
  @Semantics.quantity.unitOfMeasure: 'Unit'
  Quantity           : fkimg;
  @Semantics.quantity.unitOfMeasure: 'Unit'
  OpenQuantity       : fkimg;
  @Semantics.amount.currencyCode: 'Currency'
  NetAmount          : netwr;
  @Semantics.amount.currencyCode: 'Currency'
  TaxAmount          : mwsbp;
  @Semantics.amount.currencyCode: 'Currency'
  GrossAmount        : netwr;
  InvoiceRowCount    : abap.int2;
  Search1            : ad_sort1;
  Search2            : ad_sort1;
  Name               : abap.char( 81 );
  PersonNumber       : ad_persnum;
  AddressID          : ad_addrnum;
  EmailAddress       : ad_smtpadr;
  Mobile             : ad_tlnmbr;

}

implemented by method
  ZCL_POS_SEARCH_RETURN=>invoice_search_tf;
