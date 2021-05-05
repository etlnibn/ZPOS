@AbapCatalog.sqlViewName: 'ZMAG_INFODETAIL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Magento Account Info'
define view ZMAGI_CUSTOMER_INFO_DETAIL
  as select from I_BusinessPartner

  association [1..*] to I_BuPaIdentificationTP     as _Identification on $projection.BusinessPartner = _Identification.BusinessPartner
  association [1..*] to I_Businesspartnertaxnumber as _TaxNumber      on $projection.BusinessPartner = _TaxNumber.BusinessPartner
  association [1..*] to zmd_b2bcard                as _EmployeeCard   on $projection.BusinessPartner = _EmployeeCard.contact
    
{

  key BusinessPartner,
      BusinessPartnerCategory,
      BusinessPartnerGrouping,
      //    BusinessPartnerFullName,
      //    BusinessPartnerIsBlocked,
      FirstName,
      LastName,
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
      _EmployeeCard.cardid as CardNumber,
      _EmployeeCard.expiration as ExpirationDate,
      _EmployeeCard.cardstatus as Status  
      
//      _DefaultAddress
}
