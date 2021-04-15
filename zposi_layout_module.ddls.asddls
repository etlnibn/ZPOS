@ClientHandling.algorithm: #SESSION_VARIABLE
@AbapCatalog.sqlViewName: 'ZPOS_LAYOUT'

@ClientHandling.type: #CLIENT_DEPENDENT
@VDM: {
  viewType: #BASIC
 }
@AbapCatalog.viewEnhancementCategory: #NONE
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MASTER
}

@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@EndUserText.label: 'View for Retrieving Layout Module'
define view ZPOSI_LAYOUT_MODULE
  as select from    wlk1 as listing
    left outer join wrsz as assortment on listing.filia = assortment.asort
{

  key listing.filia    as Assortment,
  key listing.artnr    as Article,
  key listing.vrkme    as UoM,
  key listing.datbi    as ValidTo,
      //    key listing.lfdnr as Lfdnr,
      //    key assortment.asort as Asort,
      //    key assortment.lfdnr as Lfdnr,
      listing.datab    as ValidFrom,
      listing.ursac    as ListingModule,
      listing.quell    as Source,
      //    listing.pflkn as Pflkn,
      //    listing.anzal as Anzal,
      //    listing.datae as Datae,
      //    listing.negat as Negat,
      //    listing.aktio as Aktio,
      //    listing.thema as Thema,
      //    listing.strli as Strli,
      listing.sstat    as Status,
      //    listing.lifnr as Lifnr,
      //    listing.strnr as Strnr,
      assortment.locnr as Location,
      //    assortment.kunnr as Kunnr,
      assortment.vkorg as SalesOrg,
      assortment.vtweg as DistributionChannel
      //    assortment.spart as Spart
      //    assortment.datab as Datab,
      //    assortment.datbi as Datbi,
      //    assortment.rangf as Rangf,
      //    assortment.ligrd as Ligrd,
      //    assortment.ernam as Ernam,
      //    assortment.erzet as Erzet,
      //    assortment.aenam as Aenam,
      //    assortment.aedat as Aedat,
      //    assortment.aezet as Aezet

}

