@AbapCatalog.sqlViewName: 'ZMAG_CARDDETAIL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Magento B2B Card Details'
define view ZMAGI_CUSTOMER_CARD_DETAIL
  as select from zmd_b2bcard

  association [1..*] to I_BusinessPartner      as _Contact        on $projection.Contact = _Contact.BusinessPartner
  association [1..*] to I_BuPaIdentificationTP as _Identification on $projection.Contact = _Identification.BusinessPartner
  
{

  key company                      as Company,
  key contact                      as Contact,
      cpstatus                     as Cpstatus,
      cardid                       as Cardid,

      case   when cardstatus = 'X'
              then 'Active'
              else 'Inactive' 
       end                         as Status,
      cardstatus                   as Cardstatus,
      expiration                   as Expiration,
      b2bcomment                   as B2bcomment,
      _Contact.FirstName,
      _Contact.LastName,
      _Contact.BusinessPartnerName as Fullname,
      _Contact.IndependentAddressID,
      _Contact._CurrentDefaultAddress._StandardAddress._DefaultEmailAddress.EmailAddress,
      _Contact._CurrentDefaultAddress._StandardAddress._DefaultPhoneNumber.PhoneNumber,
      _Identification.BPIdentificationType,      
      _Identification.BPIdentificationNumber

}
