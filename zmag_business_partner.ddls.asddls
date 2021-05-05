@AbapCatalog.sqlViewName: 'ZMAG_BUS_PARTNER'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Magento Business Partner View'
define view ZMAG_BUSINESS_PARTNER
  as select from I_BusinessPartner

  association [1..*] to I_BuPaIdentificationTP     as _Identification on $projection.BusinessPartner = _Identification.BusinessPartner
  association [1..*] to I_Businesspartnertaxnumber as _TaxNumber      on $projection.BusinessPartner = _TaxNumber.BusinessPartner

{

  key BusinessPartner,
      BusinessPartnerCategory,
      BusinessPartnerGrouping,
      //    BusinessPartnerFullName,
      //    BusinessPartnerIsBlocked,
      //    FirstName,
      //    LastName,
      PersonFullName,
      OrganizationBPName1,
      OrganizationBPName2,

      IndependentAddressID,
      _CurrentDefaultAddress._StandardAddress.StreetName,
      _CurrentDefaultAddress._StandardAddress.StreetSuffixName,
      _CurrentDefaultAddress._StandardAddress.CityName,
      _CurrentDefaultAddress._StandardAddress.PostalCode,
      _CurrentDefaultAddress._StandardAddress.Country,
      _CurrentDefaultAddress._StandardAddress._DefaultEmailAddress.EmailAddress,
      _CurrentDefaultAddress._StandardAddress._DefaultPhoneNumber.PhoneNumber,
      _Identification.BPIdentificationType,
      _Identification.BPIdentificationNumber,
      _TaxNumber.BPTaxType,
      _TaxNumber.BPTaxNumber,
      _DefaultAddress
}
