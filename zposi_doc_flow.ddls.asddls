@ClientHandling.algorithm: #SESSION_VARIABLE
@ObjectModel.representativeKey: 'DocRelationshipUUID'
@ObjectModel.usageType.dataClass: #TRANSACTIONAL
@ObjectModel.usageType.serviceQuality: #B
@ObjectModel.usageType.sizeCategory: #XL
@Metadata.ignorePropagatedAnnotations:true
@EndUserText.label: 'SD Document Multi Level Process Flow'
@Analytics:{dataCategory:#DIMENSION}
@VDM.viewType: #BASIC
@AccessControl.authorizationCheck: #NOT_REQUIRED
@AbapCatalog.sqlViewName: 'ZPOSI_DOC_FLOW'
@AbapCatalog.preserveKey:true
@AbapCatalog.compiler.compareFilter:true
@Metadata.allowExtensions:true


define view ZPOSI_SDDocumentFlow
  as select from vbfa
  association [0..1] to I_SalesDocumentBasic     as _SalesOrder         on  $projection.SubsequentDocument = _SalesOrder.SalesDocument
  association [0..1] to I_SalesDocumentItemBasic as _ItemBasic          on  $projection.SubsequentDocument     = _ItemBasic.SalesDocument
                                                                        and $projection.SubsequentDocumentItem = _ItemBasic.SalesDocumentItem
  association [0..1] to I_UnitOfMeasure          as _BaseUnit           on  $projection.BaseUnit = _BaseUnit.UnitOfMeasure
  association [0..1] to I_UnitOfMeasure          as _OrderQuantityUnit  on  $projection.OrderQuantityUnit = _OrderQuantityUnit.UnitOfMeasure
  association [0..1] to I_Currency               as _StatisticsCurrency on  $projection.StatisticsCurrency = _StatisticsCurrency.Currency

{
      //Key
  key vbfa.ruuid                    as DocRelationshipUUID,

      //Preceding
      vbfa.vbelv                    as PrecedingDocument,
      vbfa.posnv                    as PrecedingDocumentItem,
      vbfa.vbtyp_v                  as PrecedingDocumentCategory,

      //Subsequent
      vbfa.vbeln                    as SubsequentDocument,
      vbfa.posnn                    as SubsequentDocumentItem,
      vbfa.vbtyp_n                  as SubsequentDocumentCategory,

      //Process Flow Level
      vbfa.stufe                    as ProcessFlowLevel,

      //Admin
//      @Semantics.systemDate.createdAt: true
      //      vbfa.erdat                                                   as  CreationDate,
      //      vbfa.erzet                                                   as  CreationTime,
//      @Semantics.systemDate.lastChangedAt: true
      //      vbfa.aedat                                                   as  LastChangeDate,

      //Quantity of subsequent document
      //      @DefaultAggregation: #SUM
      //      @Semantics.quantity.unitOfMeasure: 'BaseUnit'
      //       vbfa.rfmng                                                   as  QuantityInBaseUnit,
      //      @DefaultAggregation: #SUM
      //      @Semantics.quantity.unitOfMeasure: 'OrderQuantityUnit'
      //      vbfa.rfmng_flo                                               as  RefQuantityInOrdQtyUnitAsFloat,
      //      @DefaultAggregation: #SUM
      //      @Semantics.quantity.unitOfMeasure: 'BaseUnit'
      //      vbfa.rfmng_flt                                               as  RefQuantityInBaseUnitAsFloat,
      @Semantics.unitOfMeasure: true
      @ObjectModel.foreignKey.association: '_BaseUnit'
      vbfa.meins                    as BaseUnit,
      @Semantics.unitOfMeasure: true
      @ObjectModel.foreignKey.association: '_OrderQuantityUnit'
      vbfa.vrkme                    as OrderQuantityUnit,
      //      vbfa.plmin                                                   as  SDFulfillmentCalculationRule,

      //Pricing of subsequent document
      //      @DefaultAggregation: #SUM
      //      @Semantics.amount.currencyCode: 'StatisticsCurrency'
      //      vbfa.rfwrt                                                   as  NetAmount,
      @Semantics.currencyCode: true
      @ObjectModel.foreignKey.association: '_StatisticsCurrency'
      vbfa.waers                    as StatisticsCurrency,

      //Pick Pack Load
      //      vbfa.taqui                                                   as  TransferOrderInWrhsMgmtIsConfd,

      // Delivery related fields
      //      vbfa.lgnum                                                   as  WarehouseNumber,
      //      vbfa.mjahr                                                   as  MaterialDocumentYear,

      // Billing Plan related fields
      //      vbfa.fplnr                                                   as  BillingPlan,
      //      vbfa.fpltr                                                   as  BillingPlanItem,

      _SalesOrder.SalesDocumentType as SalesDocumentType,
      //      _ItemBasic.IsReturnsItem                                     as IsReturnsItem,
      @DefaultAggregation: #SUM
      @Semantics.quantity.unitOfMeasure: 'OrderQuantityUnit'
      _ItemBasic.OrderQuantity      as OrderQuantity,
      _BaseUnit,
      _OrderQuantityUnit,
      _StatisticsCurrency


} 

