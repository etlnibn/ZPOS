*&---------------------------------------------------------------------*
*& Report ZPOS_API_TEST1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpos_magento_article_put.

DATA: lt_article TYPE STANDARD TABLE OF zpos_article.


SELECTION-SCREEN: BEGIN OF BLOCK screen.

  SELECTION-SCREEN: BEGIN OF BLOCK main WITH FRAME TITLE TEXT-tf0.
    SELECTION-SCREEN:    BEGIN OF LINE,
    COMMENT 1(33) TEXT-t00 FOR FIELD p_art.
    PARAMETERS: p_art TYPE matnr OBLIGATORY DEFAULT '2000001'.
    SELECTION-SCREEN: END OF LINE.

  SELECTION-SCREEN: END OF BLOCK main.

SELECTION-SCREEN: END OF BLOCK screen.


DATA: lo_http_client TYPE REF TO if_http_client,
      lo_rest_client TYPE REF TO cl_rest_http_client,
      lv_url         TYPE        string,
      lv_body        TYPE        string,
      lv_token       TYPE        string VALUE 'Bearer j86uwiz4vlul7ifmo6c8ja0xx62762q4',
      lo_response    TYPE REF TO     if_rest_entity.

*
*lt_article = VALUE #( ( matnr = '1111122' ean11 = '123456789-87654321'  description = 'Article Description' ) ).
*
*
*DATA(lo_json_writer) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
*CALL TRANSFORMATION id SOURCE source = lt_article RESULT XML lo_json_writer.
*DATA(lv_json) = cl_abap_codepage=>convert_from( lo_json_writer->get_output( ) ).
*
*DATA(out) = cl_demo_output=>new( )->begin_section( 'Article JSON' ).
**out->next_section( 'JSON' ).
*out->write_json( lv_json ).
*out->display( ).

lv_body =

    `   { ` &&
    `     "product": { ` &&
    `       "sku": "2000001", ` &&
    `       "price": 125.75, ` &&
    `       "name": "New Product ` && sy-uzeit && `", ` &&
    `       "attribute_set_id": 4,  ` &&
    `       "extension_attributes": { ` &&
    `           "website_ids": [ ` &&
    `               1, 3 ` &&
    `           ], ` &&
    `           "stock_item": { ` &&
    `               "stock_id": 1, ` &&
    `               "qty": 100 ` &&
    `                 } ` &&
    `               } ` &&
    `           } ` &&
    `   } `.


* Good for reference!
*lo_rest_client->if_rest_client~set_request_header( exporting iv_name = 'X-CSRF-Token' iv_value = 'Fetch' ).

* Create HTTP intance using RFC restination created
* The destination needs to exist in SM59 - Type G
* Make sure that in STRUST the certification has been loaded

cl_http_client=>create_by_destination(
 EXPORTING
   destination              = 'MAGENTO'    " Logical destination (specified in function call)
 IMPORTING
   client                   = lo_http_client    " HTTP Client Abstraction
 EXCEPTIONS
   argument_not_found       = 1
   destination_not_found    = 2
   destination_no_authority = 3
   plugin_not_active        = 4
   internal_error           = 5
   OTHERS                   = 6
).

* Create REST client instance
CREATE OBJECT lo_rest_client
  EXPORTING
    io_http_client = lo_http_client.


lv_url = p_art.
SHIFT lv_url LEFT DELETING LEADING '0'.

* In SM59 right now part of the API prefix is included - which might not be ideal
* but then we can adjust here
* the prefix is /rest/V1/products/ and here we just add the variable p_art to the URI
cl_http_utility=>set_request_uri(
  EXPORTING
    request = lo_http_client->request    " HTTP Framework (iHTTP) HTTP Request
    uri     = lv_url                      " URI String (in the Form of /path?query-string)
).

* Set HTTP version
*lo_http_client->request->set_version( if_http_request=>co_protocol_version_1_1 ).

* Set request header if any
CALL METHOD lo_rest_client->if_rest_client~set_request_header
  EXPORTING
    iv_name  = 'Authorization'
    iv_value = lv_token.

CALL METHOD lo_http_client->request->set_content_type(
  EXPORTING
    content_type = if_rest_media_type=>gc_appl_json ).

DATA(lo_request_entity) = lo_rest_client->if_rest_client~create_request_entity( ).

lo_request_entity->set_string_data( lv_body ).

* HTTP PUT
lo_rest_client->if_rest_client~put( lo_request_entity ).

* HTTP response
lo_response = lo_rest_client->if_rest_client~get_response_entity( ).

* HTTP return status
DATA(http_status)   = lo_response->get_header_field( '~status_code' ).

* HTTP JSON return string
DATA(lv_json_response) = lo_response->get_string_data( ).

DATA(out) = cl_demo_output=>new( )->begin_section( 'Put - Response Code : ' && http_status ).
*out->next_section( 'JSON' ).
out->write_json( lv_json_response ).
out->display( ).


*cl_abap_browser=>show_html(
**  EXPORTING
**    html         =                            " HTML Table, Line Width 255 Characters
*    title        =  'Magento Article Data'             " Window Title
**    size         = cl_abap_browser=>medium    " Size (S,M.L,XL)
**    modal        = abap_true                  " Dialog box is modal (else modeless)
*    html_string  = lv_json_response                  " HTML String
**    printing     = abap_false                 " Key for printing
**    buttons      = navigate_off               " Navigation Keys navigate_...
**    format       = cl_abap_browser=>landscape " Landscape/portrait format
**    position     = cl_abap_browser=>topleft   " Position
**    data_table   =                            " External data
**    anchor       =                            " Goto Point
**    context_menu = abap_false                 " Display context menu in browser
**    html_xstring =                            " HTML Binary String
**    check_html   = abap_true                  " Test of HTML File
**    container    =                            " Container for display
**    dialog       = abap_true                  " Display in dialog box
**  IMPORTING
**    html_errors  =                            " Error List from Test
*).


* Class to convert the JSON to an ABAP sttructure
*    DATA lr_json_deserializer TYPE REF TO cl_trex_json_deserializer.
*    CREATE OBJECT lr_json_deserializer.
*    lr_json_deserializer->deserialize( EXPORTING json = json_response IMPORTING abap = abap_response ).
