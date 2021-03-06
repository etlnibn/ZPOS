@AbapCatalog.sqlViewName: 'ZPOS_B2B_ORDINFO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'POS  - B2B Order Data (for Viking)'
define view ZPOS_I_ORDER_INFO as select from I_SalesOrder
{
    key SalesOrder,
    SalesOrderType,
    CreationDate,
    CreationTime,
    LastChangeDate,
    LastChangeDateTime,
    SenderBusinessSystemName,
    SalesOrganization,
    DistributionChannel,
    OrganizationDivision,
    SoldToParty,
    CustomerGroup,
    SalesOrderDate,
    ServicesRenderedDate,
    SDDocumentReason,
    PurchaseOrderByCustomer,
    PurchaseOrderByShipToParty,
    SDDocumentCollectiveNumber,
    CustomerPurchaseOrderType,
    CustomerPurchaseOrderDate,
    CustomerPurchaseOrderSuplmnt,
    SalesDistrict,
    StatisticsCurrency,
    ProductCatalog,
    NextCreditCheckDate,
    LastCustomerContactDate,
    TotalNetAmount,
    TransactionCurrency,
    PricingDate,
    RetailPromotion,
    SalesOrderCondition,
    SDPricingProcedure,
    CustomerPriceGroup,
    PriceListType,

    TaxDepartureCountry,
    VATRegistrationCountry,
    RequestedDeliveryDate,
    DeliveryDateTypeRule,
    ShippingType,
    ShippingCondition,
    IncotermsClassification,
    IncotermsTransferLocation,
    IncotermsLocation1,
    IncotermsLocation2,
    IncotermsVersion,
    CompleteDeliveryIsDefined,
    DeliveryBlockReason,
    FashionCancelDate,
    SalesOrderApprovalReason,
    BillingCompanyCode,
    HeaderBillingBlockReason,
    CustomerPaymentTerms,
    PaymentMethod,
    FixedValueDate,
    FiscalYear,
    FiscalPeriod,
    ExchangeRateDate,
    ExchangeRateType,
    AccountingExchangeRate,
    BusinessArea,
    CustomerAccountAssignmentGroup,
    CostCenterBusinessArea,
    CostCenter,
    ControllingArea,
    OrderID,
    ControllingObject,
    AssignmentReference,
    PaymentPlan,
    ReferenceSDDocument,
    ReferenceSDDocumentCategory,
    CorrespncExternalReference,
    AccountingDocExternalReference,
    BusinessSolutionOrder,
    OverallSDProcessStatus,
    OverallPurchaseConfStatus,
    OverallSDDocumentRejectionSts,
    TotalBlockStatus,
    OverallDelivConfStatus,
    OverallTotalDeliveryStatus,
    OverallDeliveryStatus,
    OverallDeliveryBlockStatus,
    OverallOrdReltdBillgStatus,
    OverallBillingBlockStatus,
    OverallTotalSDDocRefStatus,
    OverallSDDocReferenceStatus,
    TotalCreditCheckStatus,
    MaxDocValueCreditCheckStatus,
    PaymentTermCreditCheckStatus,
    FinDocCreditCheckStatus,
    ExprtInsurCreditCheckStatus,
    PaytAuthsnCreditCheckSts,
    CentralCreditCheckStatus,
    CentralCreditChkTechErrSts,
    HdrGeneralIncompletionStatus,
    OverallPricingIncompletionSts,
    HeaderDelivIncompletionStatus,
    HeaderBillgIncompletionStatus,
    OvrlItmGeneralIncompletionSts,
    OvrlItmBillingIncompletionSts,
    OvrlItmDelivIncompletionSts,
    OverallChmlCmplncStatus,
    OverallDangerousGoodsStatus,
    OverallSafetyDataSheetStatus,
    SalesDocApprovalStatus,
    _Item.SalesOrderItem,
    _Item.Material,
    _Item.OrderQuantity,
    
    /* Associations */

    _BillingCompanyCode,
    _BusinessArea,
    _BusinessAreaText,
    _CentralCreditCheckStatus,
    _CentralCreditChkTechErrSts,
    _ControllingArea,
    _ControllingObject,
    _CostCenter,
    _CostCenterBusinessArea,
    _CostCenterBusinessAreaText,
    _CustomerAccountAssgmtGroup,
    _CustomerGroup,
    _CustomerPaymentTerms,
    _CustomerPriceGroup,
    _CustomerPurchaseOrderType,
    _DeliveryBlockReason,
    _DeliveryDateTypeRule,
    _DistributionChannel,
    _EngagementProjectItem,
    _ExchangeRateType,
    _ExprtInsurCreditCheckStatus,
    _FinDocCreditCheckStatus,
    _HdrGeneralIncompletionStatus,
    _HeaderBillgIncompletionStatus,
    _HeaderBillingBlockReason,
    _HeaderDelivIncompletionStatus,
    _IncotermsClassification,
    _IncotermsVersion,
    _Item,
    _MaxDocValueCreditCheckStatus,
    _OrganizationDivision,
    _OverallBillingBlockStatus,
    _OverallChmlCmplncStatus,
    _OverallDangerousGoodsStatus,
    _OverallDelivConfStatus,
    _OverallDeliveryBlockStatus,
    _OverallDeliveryStatus,
    _OverallOrdReltdBillgStatus,
    _OverallPricingIncompletionSts,
    _OverallPurchaseConfStatus,
    _OverallSDDocReferenceStatus,
    _OverallSDDocumentRejectionSts,
    _OverallSDProcessStatus,
    _OverallTotalDeliveryStatus,
    _OverallTotalSDDocRefStatus,
    _OvrlItmBillingIncompletionSts,
    _OvrlItmDelivIncompletionSts,
    _OvrlItmGeneralIncompletionSts,
    _OvrlSftyDataSheetSts,
    _Partner,
    _PaymentTermCreditCheckStatus,
    _PaytAuthsnCreditCheckSts,
    _PriceListType,
    _PricingElement,
    _ReferenceSDDocumentCategory,
    _SalesDistrict,
    _SalesDocApprovalStatus,
    _SalesOrderApprovalReason,
    _SalesOrderType,
    _SalesOrganization,
    _SDDocumentReason,
    _SDPricingProcedure,
    _ShippingCondition,
    _ShippingType,
    _SoldToParty,
    _SolutionOrder,
    _StandardPartner,
    _StatisticsCurrency,
    _TotalBlockStatus,
    _TotalCreditCheckStatus,
    _TransactionCurrency
        
}
