class ZXI_CL_SI_IS_MAGENTO_BUSINESS definition
  public
  create public .

public section.

  interfaces ZXI_II_SI_IS_MAGENTO_BUSINESS .
*
*TYPES: begin of types ty_itab.
*TYPES:                     <fld1> type <ty1>.
*TYPES:                     <fld2> type <ty2>.
*                     include type <struc>.
*TYPES: End of types ty_itab
  PROTECTED SECTION.
private section.

  data MR_CONFIG_MAP type ref to ZCL_POS_CONFIG_TAB_HANLDER .

  methods B2B_PARTNER_MAPPING
    importing
      !IS_PAYLOAD type ZMAG_BUS_PARTNER_STY
    returning
      value(RS_DATA) type CVIS_EI_EXTERN
    raising
      CX_UUID_ERROR .
  methods CONTACT_PARTNER_MAPPING
    importing
      !IS_PAYLOAD type ZMAG_BUS_PARTNER_STY
    returning
      value(RS_DATA) type CVIS_EI_EXTERN
    raising
      CX_UUID_ERROR .
  methods CREATE_BUSINESS_PARTNER
    importing
      !IS_DATA type CVIS_EI_EXTERN
    returning
      value(RV_PARTNER) type BU_PARTNER .
  methods CREATE_CREDIT_DATA
    importing
      !IS_PAYLOAD type ZMAG_BUS_PARTNER_STY
      !IV_PARTNER_GUID type BU_PARTNER_GUID
    raising
      ZCX_POS_EXCEPTION .
  methods ATTACH_DOCUMENT_BP
    importing
      !IT_FILES type ZXI_ZMAG_FILE_DATA_STY_TAB
      !IV_PARTNER_GUID type BU_PARTNER_GUID
      !IV_OBJID type BDS_TYPEID
    raising
      ZCX_POS_EXCEPTION .
ENDCLASS.



CLASS ZXI_CL_SI_IS_MAGENTO_BUSINESS IMPLEMENTATION.


  METHOD attach_document_bp.

    DATA: ls_return2         TYPE bapiret2,
          ls_bo              TYPE sibflporb,
          ls_prop            TYPE sdokpropty,
          lt_prop            TYPE sdokproptys,
          lt_properties_attr TYPE crmt_attr_name_value_t,
          ls_file_info       TYPE sdokfilaci,
          lt_file_info       TYPE sdokfilacis,
          lt_file_content    TYPE sdokcntbins,
          lv_length          TYPE i,
          lv_xstring         TYPE xstring,
          ls_loio            TYPE skwf_io,
          ls_phio            TYPE skwf_io,
          ls_error           TYPE skwf_error,
          ls_textid          TYPE scx_t100key.

    CONSTANTS: lc_mime_type   TYPE w3conttype VALUE 'application/pdf',
               lc_bo_catid    TYPE string VALUE 'BO',
               lc_language    TYPE string VALUE 'LANGUAGE',
               lc_description TYPE string VALUE 'DESCRIPTION',
               lc_url         TYPE string VALUE 'KW_RELATIVE_URL',
               lc_pdf         TYPE string VALUE 'pdf'.

    LOOP AT it_files INTO DATA(ls_file).

      CLEAR: ls_return2, ls_bo, ls_prop, lt_prop, lt_properties_attr,
             ls_file_info, lt_file_info, lt_file_content, lv_length,
             lv_xstring, ls_loio, ls_phio, ls_error.

      CALL METHOD cl_http_utility=>if_http_utility~decode_x_base64
        EXPORTING
          encoded = ls_file-encoded_bytes
        RECEIVING
          decoded = lv_xstring.

      ls_prop-name = lc_description.
      ls_prop-value = ls_file-filename.
      APPEND ls_prop TO lt_prop.

      ls_prop-name = lc_url.
      ls_prop-value = ls_file-filename.
      APPEND ls_prop TO lt_prop.

      ls_prop-name = lc_language.
      ls_prop-value = sy-langu.
      APPEND ls_prop TO lt_prop.

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer        = lv_xstring
        IMPORTING
          output_length = lv_length
        TABLES
          binary_tab    = lt_file_content.

      ls_file_info-binary_flg = 'X'.
      ls_file_info-file_name = ls_file-filename.
      ls_file_info-file_size = lv_length.
      ls_file_info-mimetype = lc_mime_type.
      ls_file_info-property = lc_pdf.
      APPEND ls_file_info TO lt_file_info.

      ls_bo-instid = iv_partner_guid.
      ls_bo-typeid = crmkw_bot-bor_business_partner.
      ls_bo-catid = lc_bo_catid.

      TRY.
          CALL METHOD cl_crm_documents=>create_with_table
            EXPORTING
              business_object     = ls_bo
              properties          = lt_prop
              properties_attr     = lt_properties_attr
              file_access_info    = lt_file_info
              file_content_binary = lt_file_content
              raw_mode            = 'X'
            IMPORTING
              loio                = ls_loio
              phio                = ls_phio
              error               = ls_error. " evaluate if there is anything wrong during creation

* return error as BAPIRET2 (with message text)
          IF ls_error IS NOT INITIAL.
            CALL FUNCTION 'BALW_BAPIRETURN_GET2'
              EXPORTING
                type   = ls_error-type
                cl     = ls_error-id
                number = ls_error-no
                par1   = ls_error-v1
                par2   = ls_error-v2
                par3   = ls_error-v3
                par4   = ls_error-v4
              IMPORTING
                return = ls_return2.

            DATA(lx_exception) = NEW zcx_pos_exception( gt_return = VALUE #( ( ls_return2 ) ) msgty = zcl_pos_util=>co_msgty-error ).
            RAISE EXCEPTION lx_exception.

          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.


            CALL FUNCTION 'BDS_CONNECTION_CREATE'
              EXPORTING
*               CLIENT                         = SY-MANDT
                loio_id                        = ls_loio-objid
                loio_class                     = ls_loio-class
                classname                      = 'BUS1006'
                classtype                      = 'BO'
*               LOGICAL_SYSTEM                 =
                object_key                     = iv_objid
              EXCEPTIONS
                nothing_found                  = 1
                parameter_error                = 2
                not_allowed                    = 3
                error_kpro                     = 4
                internal_error                 = 5
                not_authorized                 = 6
                own_logical_system_not_defined = 7
                OTHERS                         = 8.
            IF sy-subrc <> 0.
* Implement suitable error handling here
            ENDIF.

          ENDIF.

        CATCH cx_root INTO DATA(lx_error).

          CLEAR ls_textid.
          ls_textid-msgid = zcl_pos_util=>co_pos_msgid.
          ls_textid-msgno = '000'.
          ls_textid-attr1 = lx_error->get_text( ).
          lx_exception = NEW zcx_pos_exception( textid = ls_textid msgty = zcl_pos_util=>co_msgty-error ).
          RAISE EXCEPTION lx_exception.

      ENDTRY.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

    ENDLOOP.

  ENDMETHOD.


  METHOD b2b_partner_mapping.

************************Data Declarations**************************

    DATA: ls_role     TYPE bus_ei_bupa_roles,
          ls_relation TYPE burs_ei_extern,
          ls_company  TYPE cmds_ei_company,
          ls_addr     TYPE bus_ei_bupa_address,
          lt_phone    TYPE bus_ei_bupa_telephone_t,
          lt_email    TYPE bus_ei_bupa_smtp_t,
          lt_tax      TYPE bus_ei_bupa_taxnumber_t,
          lt_ident    TYPE bus_ei_bupa_identification_t,
          lt_tax1     TYPE cmds_ei_tax_ind_t.

    DATA: lv_partner LIKE rs_data-partner-header-object_instance-bpartner.

    DATA: lt_role_cat     TYPE fsbp_tb003a_tty,
          lt_cvi_role_cat TYPE cvis_role_category_t,
          ls_cvi_role_cat TYPE cvis_role_category.

    DATA: ls_functions_st    TYPE cmds_ei_functions,
          ls_functions_t     TYPE cmds_ei_functions_t,
          ls_functions       TYPE cmds_ei_cmd_functions,
          ls_sales_data_st   TYPE cmds_ei_sales,
          ls_sales_data      TYPE cmds_ei_cmd_sales,
          ls_customer        TYPE cmds_ei_extern,
          ls_customers       TYPE cmds_ei_main,
          ls_company_code_st TYPE cmds_ei_company,
          ls_company_code    TYPE cmds_ei_cmd_company.

******************************Payload******************************

******************************Guid*********************************
    DATA: lv_guid    TYPE guid_32.

    CALL METHOD cl_system_uuid=>if_system_uuid_static~create_uuid_c32
      RECEIVING
        uuid = lv_guid.

******************************Create customer**********************
    rs_data-partner-header-object_task = 'I'.
    rs_data-partner-header-object_instance-bpartnerguid = lv_guid.

***************** Partner / Central data / common *****************
    rs_data-partner-central_data-common-data-bp_control-category = '2'.             "3=group 2=Organization 1=Person
    rs_data-partner-central_data-common-data-bp_control-grouping = 'ZB2B'.          "Grouping
    rs_data-partner-central_data-common-data-bp_organization-name1 = is_payload-companyname1.
    rs_data-partner-central_data-common-datax-bp_organization-name1 = abap_true.

    rs_data-partner-central_data-common-data-bp_organization-name2 = is_payload-companyname2  && '-' && sy-datum && sy-uzeit.
    rs_data-partner-central_data-common-datax-bp_organization-name2 = abap_true.


*****************Partner / Central data / Address *****************
***-------------------Telephone number--------------------------***
    APPEND INITIAL LINE TO  lt_phone ASSIGNING FIELD-SYMBOL(<ls_phone>).
    <ls_phone>-contact-task = 'I'.
    <ls_phone>-contact-data-telephone = is_payload-telephone.
    <ls_phone>-contact-data-country  = is_payload-country.
    <ls_phone>-contact-data-countryiso = is_payload-country.
    <ls_phone>-contact-data-r_3_user = ' '.
    <ls_phone>-contact-data-consnumber = '001'.

    <ls_phone>-contact-datax-country = abap_true.
    <ls_phone>-contact-datax-telephone = abap_true.
    <ls_phone>-contact-datax-countryiso = abap_true.
    <ls_phone>-contact-datax-r_3_user = abap_true.
    <ls_phone>-contact-datax-consnumber = abap_true.
    <ls_phone>-currently_valid = abap_true.
    ls_addr-data-communication-phone-phone = lt_phone.

***--------------------------Email------------------------***
    APPEND INITIAL LINE TO lt_email ASSIGNING FIELD-SYMBOL(<ls_email>).
    <ls_email>-contact-task = 'I'.
    <ls_email>-contact-data-e_mail = is_payload-emailinvoice.
    <ls_email>-contact-data-consnumber = '001'.

    <ls_email>-contact-datax-e_mail = abap_true.
    <ls_email>-contact-datax-consnumber = abap_true.
    <ls_email>-currently_valid = abap_true.
    ls_addr-data-communication-smtp-smtp = lt_email.


***--------------------------Street Address-----------------------***
    ls_addr-task = 'I'.
    ls_addr-data_key-operation          = 'XXDFLT'.
    ls_addr-data-postal-data-city       = is_payload-postarea.
    ls_addr-data-postal-data-postl_cod1 = is_payload-postcode.
    ls_addr-data-postal-data-street     = is_payload-addressline1.
    ls_addr-data-postal-data-str_suppl3 = is_payload-addressline2.
    ls_addr-data-postal-data-country    = is_payload-country.
    ls_addr-data-postal-data-countryiso = is_payload-country.
*        ls_addr-data-postal-data-region = is_payload-region.
    TRY.
        ls_addr-data-postal-data-langu      = zcl_pos_util=>get_language_by_country( is_payload-country ).

      CATCH zcx_pos_exception INTO DATA(lx_error).
*TODO:  Do something with the exception

    ENDTRY.

    ls_addr-data-postal-datax-city = abap_true.
    ls_addr-data-postal-datax-postl_cod1 = abap_true.
    ls_addr-data-postal-datax-street = abap_true.
    ls_addr-data-postal-datax-country = abap_true.
    ls_addr-data-postal-datax-countryiso = abap_true.
*        ls_addr-data-postal-datax-region = abap_true.
    ls_addr-data-postal-datax-langu = abap_true.
    ls_addr-currently_valid = abap_true.

    APPEND ls_addr TO rs_data-partner-central_data-address-addresses.


**--------------------------Tax----------------------------***
    APPEND INITIAL LINE TO lt_tax ASSIGNING FIELD-SYMBOL(<ls_tax>).
    <ls_tax>-task = 'I'.
    <ls_tax>-data_key-taxtype   = 'SE2'.       "is_payload-taxcategory. "'IN2'.
    <ls_tax>-data_key-taxnumber = is_payload-vatnumber.  "'12345678956'.

    rs_data-partner-central_data-taxnumber-taxnumbers = lt_tax.

**--------------------------Identification------------------------***
    APPEND INITIAL LINE TO lt_ident ASSIGNING FIELD-SYMBOL(<ls_ident>).
    <ls_ident>-task = 'I'.
    <ls_ident>-data_key-identificationcategory  = 'IBS001'.
    <ls_ident>-data_key-identificationnumber    = is_payload-ssn.

    rs_data-partner-central_data-ident_number-ident_numbers = lt_ident.



********************* Partner / Central data / role****************
    ls_role-task = 'I'.
    ls_role-data_key = 'FLCU01'.
    ls_role-data-rolecategory = 'FLCU01'.
    ls_role-data-valid_from = sy-datum.
    ls_role-data-valid_to = '99991231'.
    ls_role-currently_valid = abap_true.

    ls_role-datax-valid_from = abap_true.
    ls_role-datax-valid_to = abap_true.

    APPEND ls_role TO rs_data-partner-central_data-role-roles.
    rs_data-partner-central_data-role-current_state = abap_true.



********************* Partner / Credit Management / Role****************
    ls_role-task = 'I'.
    ls_role-data_key = 'UKM000'.
    ls_role-data-rolecategory = 'UKM000'.
    ls_role-data-valid_from = sy-datum.
    ls_role-data-valid_to = '99991231'.
    ls_role-currently_valid = abap_true.

    ls_role-datax-valid_from = abap_true.
    ls_role-datax-valid_to = abap_true.

    APPEND ls_role TO rs_data-partner-central_data-role-roles.
    rs_data-partner-central_data-role-current_state = abap_true.



*********************Partner / Central data / role*****************
    ls_role-task = 'I'.
    ls_role-data_key = 'FLCU00'.
    ls_role-data-rolecategory = 'FLCU00'.
    ls_role-data-valid_from = sy-datum.
    ls_role-data-valid_to = '99991231'.
    ls_role-currently_valid = abap_true.

    ls_role-datax-valid_from = abap_true.
    ls_role-datax-valid_to = abap_true.

    APPEND ls_role TO rs_data-partner-central_data-role-roles.
    rs_data-partner-central_data-role-current_state = abap_true.

*******************Partner relation / header **********************
    ls_relation-header-object_instance-partner1-identificationcategory = 'FLCU01'.
    APPEND ls_relation TO rs_data-partner_relation.

    ls_relation-header-object_task = 'I'.

******************** Customer / Company data **********************

    ls_company-task = 'I'.
    ls_company-data_key = '9700' . " company code
    ls_company-data-akont = '0000140000'.
    ls_company-data-zterm = '9730'. " terms of payment
    ls_company-datax-zterm = 'X'.

    ls_company-datax-akont = abap_true.
    ls_company-datax-zterm = abap_true.
    APPEND ls_company TO rs_data-customer-company_data-company.

************************* Customer / Header ***********************
    rs_data-customer-header-object_task = 'I'.
    rs_data-ensure_create-create_customer = abap_true.

    ls_cvi_role_cat-category = 'FLCU01'.
    APPEND ls_cvi_role_cat TO lt_cvi_role_cat.


*************SALES ORG AND DIST & DIVISION MANDATORY FIELDS********

    ls_sales_data_st-task = 'I'.
    ls_sales_data_st-data_key-vkorg = '9700'.   "''ZTM1'.
    ls_sales_data_st-data_key-vtweg = '20'.  "'TM'.
    ls_sales_data_st-data_key-spart = '10'.
    ls_sales_data_st-data-waers = 'SEK'.  "'USD'.
    ls_sales_data_st-data-inco1 = 'CIF'.
    ls_sales_data_st-data-inco2 = is_payload-postarea.  "'Costs,Insurance&Freight'.
    ls_sales_data_st-data-kzazu = 'X'.

    ls_sales_data_st-datax-waers = abap_true.
    ls_sales_data_st-datax-inco1 = abap_true.
    ls_sales_data_st-datax-inco2 = abap_true.
    ls_sales_data_st-datax-kzazu = abap_true.

    APPEND ls_sales_data_st TO ls_sales_data-sales.
    rs_data-customer-sales_data = ls_sales_data.

********************** GL Account ledger *************************
    ls_company_code_st-task = 'I'.
    APPEND ls_company_code_st TO ls_company_code-company.

**********************Account Group Assignemnt ******************

    APPEND INITIAL LINE TO lt_tax1 ASSIGNING FIELD-SYMBOL(<fs_tax1>).
    <fs_tax1>-task = 'I'.
    <fs_tax1>-data_key-aland = 'SE'.
    <fs_tax1>-data_key-tatyp = 'TTX1'.
    <fs_tax1>-data-taxkd = '1'.
    <fs_tax1>-datax-taxkd = abap_true.
    rs_data-customer-central_data-tax_ind-tax_ind = lt_tax1.

*        ls_customer-central_data-central-data-ktokd = 'CU01'.
*        ls_customer-header-object_instance-kunnr = ' '.
*        ls_customer-header-object_task = 'I'.
*        ls_customer-company_data = ls_company_code.
*        APPEND ls_customer TO ls_customers-customers.

*********************** End of sales data *************************


  ENDMETHOD.


  METHOD contact_partner_mapping.

************************Data Declarations**************************

    DATA: ls_role     TYPE bus_ei_bupa_roles,
          ls_relation TYPE burs_ei_extern,
          ls_addr     TYPE bus_ei_bupa_address,
          lt_phone    TYPE bus_ei_bupa_telephone_t,
          lt_email    TYPE bus_ei_bupa_smtp_t,
          lt_ident    TYPE bus_ei_bupa_identification_t.

    DATA: lv_partner LIKE rs_data-partner-header-object_instance-bpartner.

    DATA: lt_role_cat     TYPE fsbp_tb003a_tty,
          lt_cvi_role_cat TYPE cvis_role_category_t,
          ls_cvi_role_cat TYPE cvis_role_category.

    DATA: ls_functions_st TYPE cmds_ei_functions,
          ls_functions_t  TYPE cmds_ei_functions_t,
          ls_functions    TYPE cmds_ei_cmd_functions,
          ls_customer     TYPE cmds_ei_extern,
          ls_customers    TYPE cmds_ei_main.

******************************Payload******************************

******************************Create customer**********************
    rs_data-partner-header-object_task = 'I'.
    rs_data-partner-header-object_instance-bpartnerguid = is_payload-partner_guid.

***************** Partner / Central data / common *****************
    rs_data-partner-central_data-common-data-bp_control-category = '1'.             "3=group 2=Organization 1=Person
    rs_data-partner-central_data-common-data-bp_control-grouping = 'ZB2B'.          "Grouping

    rs_data-partner-central_data-common-data-bp_person-fullname = is_payload-attname.
    rs_data-partner-central_data-common-datax-bp_person-fullname = abap_true.

    SPLIT is_payload-attname AT ' ' INTO rs_data-partner-central_data-common-data-bp_person-firstname rs_data-partner-central_data-common-data-bp_person-lastname.
    rs_data-partner-central_data-common-datax-bp_person-firstname = abap_true.
    rs_data-partner-central_data-common-datax-bp_person-lastname = abap_true.

    rs_data-partner-central_data-common-data-bp_person-correspondlanguage = 'E'.
    rs_data-partner-central_data-common-datax-bp_person-correspondlanguage = abap_true.

    rs_data-partner-central_data-common-data-bp_person-correspondlanguageiso = 'EN'.
    rs_data-partner-central_data-common-datax-bp_person-correspondlanguageiso = abap_true.

    rs_data-partner-central_data-common-data-bp_centraldata-searchterm1  = is_payload-ssn.
    rs_data-partner-central_data-common-datax-bp_centraldata-searchterm1 = abap_true.



*****************Partner / Central data / Address *****************
***-------------------Telephone number--------------------------***
    APPEND INITIAL LINE TO  lt_phone ASSIGNING FIELD-SYMBOL(<ls_phone>).
    <ls_phone>-contact-task = 'I'.
    <ls_phone>-contact-data-telephone = is_payload-telephone.
    <ls_phone>-contact-data-country  = is_payload-country.
    <ls_phone>-contact-data-countryiso = is_payload-country.
    <ls_phone>-contact-data-r_3_user = ' '.
    <ls_phone>-contact-data-consnumber = '001'.

    <ls_phone>-contact-datax-country = abap_true.
    <ls_phone>-contact-datax-telephone = abap_true.
    <ls_phone>-contact-datax-countryiso = abap_true.
    <ls_phone>-contact-datax-r_3_user = abap_true.
    <ls_phone>-contact-datax-consnumber = abap_true.
    <ls_phone>-currently_valid = abap_true.
    ls_addr-data-communication-phone-phone = lt_phone.

***--------------------------Email------------------------***
    APPEND INITIAL LINE TO lt_email ASSIGNING FIELD-SYMBOL(<ls_email>).
    <ls_email>-contact-task = 'I'.
    <ls_email>-contact-data-e_mail = is_payload-email.
    <ls_email>-contact-data-consnumber = '001'.

    <ls_email>-contact-datax-e_mail = abap_true.
    <ls_email>-contact-datax-consnumber = abap_true.
    <ls_email>-currently_valid = abap_true.
    ls_addr-data-communication-smtp-smtp = lt_email.


***--------------------------Street Address-----------------------***
    ls_addr-task = 'I'.
    ls_addr-data_key-operation          = 'XXDFLT'.
*    ls_addr-data-postal-data-city       = is_payload-postarea.
*    ls_addr-data-postal-data-postl_cod1 = is_payload-postcode.
*    ls_addr-data-postal-data-street     = is_payload-addressline1.
*    ls_addr-data-postal-data-str_suppl3 = is_payload-addressline2.
    ls_addr-data-postal-data-country    = is_payload-country.
    ls_addr-data-postal-data-countryiso = is_payload-country.
**        ls_addr-data-postal-data-region = is_payload-region.
    ls_addr-data-postal-data-langu      = 'E'.              "Needs to be corrected
*
*
*    ls_addr-data-postal-datax-city = abap_true.
*    ls_addr-data-postal-datax-postl_cod1 = abap_true.
*    ls_addr-data-postal-datax-street = abap_true.
    ls_addr-data-postal-datax-country = abap_true.
    ls_addr-data-postal-datax-countryiso = abap_true.
**        ls_addr-data-postal-datax-region = abap_true.
    ls_addr-data-postal-datax-langu = abap_true.
    ls_addr-currently_valid = abap_true.

    APPEND ls_addr TO rs_data-partner-central_data-address-addresses.

**--------------------------Identification------------------------***
    APPEND INITIAL LINE TO lt_ident ASSIGNING FIELD-SYMBOL(<ls_ident>).
    <ls_ident>-task = 'I'.
    <ls_ident>-data_key-identificationcategory  = 'IBS001'.
    <ls_ident>-data_key-identificationnumber    = is_payload-ssn.

    rs_data-partner-central_data-ident_number-ident_numbers = lt_ident.

********************* Partner / Central data / role****************
    ls_role-task = 'I'.
    ls_role-data_key = 'BUP001'.
    ls_role-data-rolecategory = 'BUP001'.
    ls_role-data-valid_from = sy-datum.
    ls_role-data-valid_to = '99991231'.
    ls_role-currently_valid = abap_true.

    ls_role-datax-valid_from = abap_true.
    ls_role-datax-valid_to = abap_true.

    APPEND ls_role TO rs_data-partner-central_data-role-roles.
    rs_data-partner-central_data-role-current_state = abap_true.


*******************Partner relation / header **********************
*    ls_relation-header-object_instance-partner1-identificationcategory = 'BUP001'.
*    ls_relation-header-object_task = 'I'.
*    APPEND ls_relation TO rs_data-partner_relation.



  ENDMETHOD.


  METHOD create_business_partner.

************************Data Declarations**************************
    DATA: lt_return_map TYPE mdg_bs_bp_msgmap_t,
          lt_data       TYPE cvis_ei_extern_t,
          lt_return     TYPE bapiretm.
*
******************************Guid*********************************
    TRY.

        APPEND is_data TO lt_data.

************************** validating BP data ********************

        CALL METHOD cl_md_bp_maintain=>validate_single
          EXPORTING
            i_data        = is_data "lt_data
          IMPORTING
            et_return_map = lt_return_map. "lt_return.

*****************************Creating BP **************************
        IF lt_return_map IS  INITIAL.

          CALL METHOD cl_md_bp_maintain=>maintain
            EXPORTING
              i_data   = lt_data
            IMPORTING
              e_return = lt_return.

***************************** Commit BAPI *************************
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.                " Use of Command `COMMIT AND WAIT`

          IMPORT lv_partner TO rv_partner FROM MEMORY ID 'BUP_MEMORY_PARTNER'.

        ELSE.
* Fault Payload
        ENDIF.

      CATCH cx_uuid_error INTO DATA(lx_error).

*        er_entity-companyname1 = lx_error->get_text( ).

    ENDTRY.

  ENDMETHOD.


  METHOD create_credit_data.


    DATA: lv_status    TYPE char1,
          ls_bp_struct TYPE  bus_ei_extern.

* General Data
    ls_bp_struct-header-object_task                  = 'I'.
    ls_bp_struct-header-object_instance-bpartnerguid = iv_partner_guid.
    ls_bp_struct-ukmbp_data-segments-current_state   = abap_true.

* Profile Data
    ls_bp_struct-ukmbp_data-profile-data-limit_rule = 'STANDARD'.
    ls_bp_struct-ukmbp_data-profile-datax-limit_rule = abap_true.

    ls_bp_struct-ukmbp_data-profile-data-check_rule = '01'.
    ls_bp_struct-ukmbp_data-profile-datax-check_rule = abap_true.

* Segement Data
    APPEND INITIAL LINE TO ls_bp_struct-ukmbp_data-segments-segments ASSIGNING FIELD-SYMBOL(<ls_segment>).
    <ls_segment>-task                  = 'I'.
    <ls_segment>-data_key-partner      = is_payload-partnerid.
    <ls_segment>-data_key-credit_sgmnt = '9700'.      " Just for Testing

    <ls_segment>-data-credit_limit     = is_payload-creditlimit..
    <ls_segment>-data-limit_valid_date = '99991231'.
    <ls_segment>-data-limit_chg_date   = sy-datum.

    <ls_segment>-datax-credit_limit     = abap_true.
    <ls_segment>-datax-limit_valid_date = abap_true.
    <ls_segment>-datax-limit_chg_date   = abap_true.

    CALL FUNCTION 'UKMBP_INBOUND_MAIN'
      EXPORTING
        iv_x_save   = 'X'
      IMPORTING
        status      = lv_status
*     TABLES
*       ET_RETURN   =
      CHANGING
        c_bp_struct = ls_bp_struct.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.


    DATA: lt_bp1012 TYPE STANDARD TABLE OF bp1012.
*    DATA: lv_credit_sgmnt TYPE ukm_credit_sgmnt VALUE '0000'.
    DATA: lt_return2 TYPE bapiret2_t.

*    DATA(lo_facade) = cl_ukm_facade=>create( i_activity = cl_ukm_cnst_eventing=>bp_maintenance ).
*    DATA(lo_bupa_factory) = lo_facade->get_bupa_factory( ).
*    DATA(lo_partner) = lo_bupa_factory->get_business_partner( is_payload-partnerid ).

* Get Credit Profile Data
*    lo_partner->get_bp_cms( IMPORTING es_bp_cms = DATA(ls_bp_cms) ).

* Set Credit Profile Data
*    ls_bp_cms-limit_rule = 'STANDARD'.
*    ls_bp_cms-check_rule = '01'.

*    lo_partner->set_bp_cms( EXPORTING is_bp_cms = ls_bp_cms ).
*
*    DATA(lo_account) = lo_bupa_factory->get_credit_account(
*        i_partner         = is_payload-partnerid
*        i_credit_sgmnt    = lv_credit_sgmnt ).
*
*    lo_account->get_bp_cms_sgm( IMPORTING es_bp_cms_sgm = DATA(ls_bp_credit_sgm) ).
*    ls_bp_credit_sgm-credit_limit   = is_payload-creditlimit.
*    ls_bp_credit_sgm-limit_valid_date = '99991231'.
*    ls_bp_credit_sgm-limit_chg_date = sy-datum.
*    lo_account->set_bp_cms_sgm( EXPORTING is_bp_cms_sgm = ls_bp_credit_sgm ).
*
** Update the credit segment and credit profile
*    DATA(lt_ukm_return) = lo_bupa_factory->save_all( EXPORTING i_upd_task = abap_true ).

*    IF lt_ukm_return[] IS  INITIAL.
*      COMMIT WORK AND WAIT.

    APPEND INITIAL LINE TO lt_bp1012 ASSIGNING FIELD-SYMBOL(<ls_bp1012>).
    <ls_bp1012>-partner      = is_payload-partnerid.
    <ls_bp1012>-date_to      = '20991231'.
    <ls_bp1012>-date_from    = sy-datum.
    <ls_bp1012>-flg_permit   = abap_true.
    <ls_bp1012>-grade_method = 'Z_BISNODE'.
    <ls_bp1012>-grade        = is_payload-rating.
    <ls_bp1012>-flg_default  = abap_true.
    <ls_bp1012>-tendency     = 'O'.
    <ls_bp1012>-text         = 'Magneto-B2B'.

    CALL FUNCTION 'FS_API_BP1012_ADD'
      EXPORTING
        iv_partner = is_payload-partnerid
      TABLES
        et_return  = lt_return2
        it_bp1012  = lt_bp1012.

    IF lt_return2[] IS NOT INITIAL.
      DATA(lx_exception) = NEW zcx_pos_exception( gt_return = lt_return2 msgty = zcl_pos_util=>co_msgty-error ).
      RAISE EXCEPTION lx_exception.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.

*  ELSE.
*    CLEAR lt_return2[].
*    LOOP AT lt_ukm_return INTO DATA(ls_ukm_return).
*      APPEND INITIAL LINE TO lt_return2 ASSIGNING FIELD-SYMBOL(<ls_return2>).
*      <ls_return2>-type = ls_ukm_return-msgty.
*      <ls_return2>-id = ls_ukm_return-msgid.
*      <ls_return2>-number = ls_ukm_return-msgno.
*      <ls_return2>-message = ls_ukm_return-message.
*      <ls_return2>-message_v1 = ls_ukm_return-msgv1.
*      <ls_return2>-message_v2 = ls_ukm_return-msgv2.
*      <ls_return2>-message_v3 = ls_ukm_return-msgv3.
*      <ls_return2>-message_v4 = ls_ukm_return-msgv4.
*    ENDLOOP.
*
*      lx_exception = NEW zcx_pos_exception( gt_return = lt_return2 msgty = zcl_pos_util=>co_msgty-error ).
*      RAISE EXCEPTION lx_exception.
*
*    ENDIF.

  ENDMETHOD.


  METHOD zxi_ii_si_is_magento_business~si_is_magento_business_partner.

************************Data Declarations**************************
    DATA: ls_payload    TYPE zmag_bus_partner_sty,
          lt_return_map TYPE mdg_bs_bp_msgmap_t,
          lt_data       TYPE cvis_ei_extern_t,
          lt_return     TYPE bapiret2_t,
          lx_uuid_error TYPE REF TO cx_uuid_error.


******************************Guid*********************************

******************************Guid*********************************
    TRY.
        CALL METHOD cl_system_uuid=>if_system_uuid_static~create_uuid_c32
          RECEIVING
            uuid = ls_payload-partner_guid.

      CATCH cx_uuid_error INTO lx_uuid_error.

    ENDTRY.

    ls_payload-partnerid    = input-businesspartner-partnerid.
    ls_payload-addressline1 = input-businesspartner-addressline1.
    ls_payload-addressline2 = input-businesspartner-addressline2.
    ls_payload-attname      = input-businesspartner-attname.
    ls_payload-country      = input-businesspartner-country.
    ls_payload-companyname1 = input-businesspartner-companyname1.
    ls_payload-companyname2 = input-businesspartner-companyname2.
    ls_payload-creditlimit  = input-businesspartner-creditlimit.
    ls_payload-email        = input-businesspartner-email.
    ls_payload-emailinvoice = input-businesspartner-emailinvoice.
    ls_payload-postarea     = input-businesspartner-postarea.
    ls_payload-postcode     = input-businesspartner-postcode.
    ls_payload-rating       = input-businesspartner-rating.
    ls_payload-ssn          = input-businesspartner-ssn.
    ls_payload-shopid       = input-businesspartner-shopid.
    ls_payload-telephone    = input-businesspartner-telephone.
    ls_payload-vatnumber    = input-businesspartner-vatnumber.

    IF mr_config_map IS INITIAL.
      TRY.
          mr_config_map = NEW #( iv_country =  ls_payload-country ).

        CATCH zcx_pos_exception INTO DATA(lx_error).
* Do something with the exception

      ENDTRY.
    ENDIF.


    LOOP AT input-businesspartner-files-item INTO DATA(ls_file).
      APPEND INITIAL LINE TO ls_payload-files ASSIGNING FIELD-SYMBOL(<ls_file>).
      <ls_file>-filename      = ls_file-filename.
      <ls_file>-encoded_bytes = ls_file-encoded_bytes.
    ENDLOOP.

    DATA(ls_data)         = b2b_partner_mapping( ls_payload ).
    DATA(lv_organization) = create_business_partner( ls_data ).      " B2B Organization

    IF NOT lv_organization IS INITIAL.
      ls_payload-partnerid = lv_organization.

      SELECT SINGLE partner_guid FROM but000 INTO @DATA(lv_guid) WHERE partner = @lv_organization.

      CALL METHOD create_credit_data( is_payload = ls_payload iv_partner_guid = lv_guid ).
      CALL METHOD attach_document_bp(
          it_files        = input-businesspartner-files-item
          iv_partner_guid = lv_guid
          iv_objid        = CONV #( lv_organization ) ).

      output-businesspartner = input-businesspartner.
      output-businesspartner-partnerid = lv_organization.

      TRY.

          CALL METHOD cl_system_uuid=>if_system_uuid_static~create_uuid_c32
            RECEIVING
              uuid = ls_payload-partner_guid.

        CATCH cx_uuid_error INTO lx_uuid_error.

      ENDTRY.

      ls_data          = contact_partner_mapping( ls_payload ).
      DATA(lv_contact) = create_business_partner( ls_data ).          " Contact Person

      IF NOT lv_contact IS INITIAL.

        CALL FUNCTION 'BAPI_BUPR_RELATIONSHIP_CREATE'
          EXPORTING
            businesspartner1     = lv_organization
            businesspartner2     = lv_contact
            relationshipcategory = 'BUR001'
            validfromdate        = sy-datlo
            validuntildate       = '99991231'
          TABLES
            return               = lt_return.

* TODO: Error Handling

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.                " Use of Command `COMMIT AND WAIT`

      ENDIF.

    ENDIF.




  ENDMETHOD.
ENDCLASS.
