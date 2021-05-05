class ZCL_POS_MAGENTO_REST_API definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_s_report_data,
        vkorg        TYPE vkorg,
        werks        TYPE werks_d,
        matnr        TYPE matnr,
        description  TYPE maktx,
        uom          TYPE vrkme,
        changed_on   TYPE hdb_timestamp,
        success      TYPE boole_d,
        is_bundle    TYPE boole_d,
        http_status  TYPE zpos_http_status,
        http_message TYPE zpos_http_message,
        color        TYPE lvc_t_scol,               " ALV OO type
        style        TYPE lvc_t_styl,
      END OF ty_s_report_data .
  types:
    ty_t_report_data TYPE STANDARD TABLE OF ty_s_report_data .

  class-data:
    BEGIN OF co_endpoint,
        product  TYPE char1 VALUE 'P',
        bundle   TYPE char1 VALUE 'B',
        websites TYPE char1 VALUE 'W',
        shipment TYPE char1 VALUE 'S',
        invoice  type char1 value 'I',
      END OF co_endpoint .
  class-data:
    BEGIN OF co_http_operation,
        post   TYPE char4 VALUE 'POST',
        put    TYPE char4 VALUE 'PUT',
        get    TYPE char4 VALUE 'GET',
        delete TYPE char4 VALUE 'DEL',
      END OF co_http_operation .

  class-methods REST_OPERATION
    importing
      !IV_BODY type STRING optional
      !IV_ENDPOINT type STRING
      !IV_OPERATION type CHAR4
    exporting
      !EV_HTTP_STATUS type ZPOS_HTTP_STATUS
      !EV_HTTP_MESSAGE type ZPOS_HTTP_MESSAGE
      !EV_BODY type STRING .
  class-methods BUILD_ENDPOINT_STRING
    importing
      !IV_SUFFIX type STRING optional
      !IV_TYPE type CHAR1
      !IV_DATA type STRING
      !IV_VKORG type VKORG optional
    returning
      value(RV_URL) type STRING
    raising
      ZCX_POS_EXCEPTION .
  PROTECTED SECTION.
private section.

  constants:
    BEGIN OF co_alv_color,
        yellow TYPE lvc_col VALUE '3', "
        green  TYPE lvc_col VALUE '5', "
        red    TYPE lvc_col VALUE '6', "
      END OF co_alv_color .
  class-data MO_HTTP_CLIENT type ref to IF_HTTP_CLIENT .
  class-data MO_RESPONSE type ref to IF_REST_ENTITY .
  class-data MO_REST_CLIENT type ref to CL_REST_HTTP_CLIENT .
  class-data MR_OVERFLOW type ref to CX_SY_ARITHMETIC_OVERFLOW .
  class-data MR_ZERODIVIDE type ref to CX_SY_ZERODIVIDE .
  class-data MS_COLOR type LVC_S_SCOL .
  class-data:
    BEGIN OF ms_error_response,
        message TYPE string,
      END OF ms_error_response .
  class-data MS_SELTAB type RSPARAMS .
  class-data MS_TEXT_ID type SCX_T100KEY .
  class-data MT_HEX type SOLIX_TAB .
  class-data MV_TOKEN type STRING value 'Bearer j86uwiz4vlul7ifmo6c8ja0xx62762q4' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_POS_MAGENTO_REST_API IMPLEMENTATION.


  METHOD build_endpoint_string.

    DATA: lv_country   TYPE string,
          ls_textid    TYPE scx_t100key,
          lx_exception TYPE REF TO zcx_pos_exception.

* There are two country specific endpoints "no" and "sv"
    IF iv_vkorg IS SUPPLIED.
      CASE iv_vkorg.
        WHEN 9700.
          lv_country = 'sv'.
        WHEN 7400.
          lv_country = 'no'.
        WHEN OTHERS.
          CLEAR ls_textid.
          ls_textid-msgid = zcl_pos_util=>co_pos_msgid.
          ls_textid-msgno = '065'.
          ls_textid-attr1 = iv_vkorg.
          lx_exception = NEW zcx_pos_exception( textid = ls_textid msgty = zcl_pos_util=>co_msgty-error ).
          RAISE EXCEPTION lx_exception.
      ENDCASE.
    ENDIF.

    CASE iv_type.
      WHEN co_endpoint-product.                                                       " Product
*        /rest/sv/V1/products/999666K
        IF NOT lv_country IS INITIAL.                                                 " Product - Country scope - Normally the PUT
          CONCATENATE lv_country '/V1/products/' iv_data INTO rv_url.
        ELSE.                                                                         " Product - Global scope - Normally the GET
          CONCATENATE 'V1/products/' iv_data INTO rv_url.
        ENDIF.
      WHEN co_endpoint-bundle.                                                      " Bundle Options
        CONCATENATE 'V1/bundle-products/' iv_data iv_suffix INTO rv_url.
      WHEN co_endpoint-websites.                                                    " Website - Proxy for Article Existence Check
        CONCATENATE 'V1/products/' iv_data '/websites' INTO rv_url.
      WHEN co_endpoint-shipment.                                                    " Shipping Notification
        CONCATENATE 'V1/order/' iv_data '/ship' INTO rv_url.
      WHEN co_endpoint-invoice.                                                     " Invoice & Capture
        CONCATENATE 'V1/order/' iv_data '/invoice' INTO rv_url.

    ENDCASE.

  ENDMETHOD.


  METHOD rest_operation.

    DATA: BEGIN OF ls_error_response,
            message TYPE string,
          END OF ls_error_response.

    cl_http_client=>create_by_destination(
     EXPORTING
       destination              = 'MAGENTO'    " Logical destination (specified in function call)
     IMPORTING
       client                   = mo_http_client    " HTTP Client Abstraction
     EXCEPTIONS
       argument_not_found       = 1
       destination_not_found    = 2
       destination_no_authority = 3
       plugin_not_active        = 4
       internal_error           = 5
       OTHERS                   = 6
    ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

* Create REST client instance
    mo_rest_client = NEW #( mo_http_client ).

* In SM59 the API prefix is /rest/V1/ and here we just add the variable part
    cl_http_utility=>set_request_uri(
      EXPORTING
        request = mo_http_client->request    " HTTP Framework (iHTTP) HTTP Request
        uri     = iv_endpoint                      " URI String (in the Form of /path?query-string)
    ).

* Set HTTP version
*lo_http_client->request->set_version( if_http_request=>co_protocol_version_1_1 ).

* Set Authorization Header Token
    mo_rest_client->if_rest_client~set_request_header( EXPORTING iv_name  = 'Authorization' iv_value = mv_token ).

    mo_http_client->request->set_content_type( EXPORTING content_type = if_rest_media_type=>gc_appl_json ).

    DATA(lo_request_entity) = mo_rest_client->if_rest_client~create_request_entity( ).

    lo_request_entity->set_string_data( iv_body ).

* HTTP PUT
    CASE iv_operation.
      WHEN co_http_operation-put.
        mo_rest_client->if_rest_client~put( lo_request_entity ).
      WHEN co_http_operation-post.
        mo_rest_client->if_rest_client~post( lo_request_entity ).
      WHEN co_http_operation-get.
        mo_rest_client->if_rest_client~get( ).
      WHEN co_http_operation-delete.
        mo_rest_client->if_rest_client~delete( ).
      WHEN OTHERS.
        mo_rest_client->if_rest_client~put( lo_request_entity ).
    ENDCASE.

    mo_response = mo_rest_client->if_rest_client~get_response_entity( ).

    ev_http_status   = mo_response->get_header_field( '~status_code' ).

    ev_body = mo_response->get_string_data( ).

    IF cl_rest_status_code=>is_success( CONV #( ev_http_status ) ) = abap_true.
*      DATA(out) = cl_demo_output=>new( )->begin_section( 'Put - Response Code : ' && ev_http_status ).
*      out->write_json( lv_json_response ).
*      out->display( ).
    ELSE.
      CALL METHOD /ui2/cl_json=>deserialize( EXPORTING json = ev_body CHANGING data = ls_error_response ).
      ev_http_message = ls_error_response-message.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
