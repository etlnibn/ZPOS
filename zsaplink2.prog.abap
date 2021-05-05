**/---------------------------------------------------------------------\
**|   This file is part of SAPlink.                                     |
**|                                                                     |
**|   SAPlink is free software; you can redistribute it and/or modify   |
**|   it under the terms of the GNU General Public License as published |
**|   by the Free Software Foundation; either version 2 of the License, |
**|   or (at your option) any later version.                            |
**|                                                                     |
**|   SAPlink is distributed in the hope that it will be useful,        |
**|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
**|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
**|   GNU General Public License for more details.                      |
**|                                                                     |
**|   You should have received a copy of the GNU General Public License |
**|   along with SAPlink; if not, write to the                          |
**|   Free Software Foundation, Inc.,                                   |
**|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
**\---------------------------------------------------------------------/
**/---------------------------------------------------------------------\
**| /  __ \           | |      (_) |         | |                        |
**| | /  \/ ___  _ __ | |_ _ __ _| |__  _   _| |_ ___  _ __ ___         |
**| | |    / _ \| '_ \| __| '__| | '_ \| | | | __/ _ \| '__/ __|        |
**| | \__/\ (_) | | | | |_| |  | | |_) | |_| | || (_) | |  \__ \        |
**|  \____/\___/|_| |_|\__|_|  |_|_.__/ \__,_|\__\___/|_|  |___/        |
**|---------------------------------------------------------------------|
**| Lead Developers : ed herrmann                                       |
**|                        ewherrmann+saplinkcred@gmail.com             |
**|                   dan mcweeney                                      |
**|                        daniel.mcweeney+saplinkcred@gmail.com        |
**|---------------------------------------------------------------------|
**| For a full list of contributors visit:                              |
**|                                                                     |
**| project homepage: http://saplink.org                                |
**| discussion group: http://groups.google.com/group/saplink            |
**| project wiki:     https://wiki.sdn.sap.com/wiki/display/HOME/SAPlink|
**\---------------------------------------------------------------------/
REPORT  zsaplink.

**/------------------------DATA----------------------------\
*TABLES: sscrfields, e071, e07t.
*
*TYPE-POOLS: icon, slis, sabc, stms, trwbo.
*
*TYPES: BEGIN OF t_plugin,
*         object TYPE ko100-object,
*         text   TYPE ko100-text,
*       END OF t_plugin.
*
*TYPES: BEGIN OF t_objecttable,
*         classname TYPE string,
*         object    TYPE ko100-object,
*         text      TYPE ko100-text,
*       END OF t_objecttable.
*
*TYPES: BEGIN OF t_nuggetobject,
*         objtype TYPE string,
*         objname TYPE string,
*         exists  TYPE flag,
*       END OF t_nuggetobject.
**addition of package data
*****   Read all objects of the package
*TYPES: BEGIN OF t_objects_package,
*         select     TYPE char1,
*         object     TYPE tadir-object,
*         object_txt TYPE string,
*         obj_name   TYPE tadir-obj_name,
*         srcsystem  TYPE tadir-srcsystem,
*         down_flag  TYPE char1,
*         status     TYPE char1,
*         msg        TYPE string,
*       END OF t_objects_package.
*
*TYPES: BEGIN OF ty_objname,
*         object   TYPE trobjtype,
*         obj_name TYPE sobj_name,
*       END OF ty_objname.
*DATA: lt_objnames TYPE STANDARD TABLE OF ty_objname.
*DATA: l_objname TYPE ty_objname.
*
*FIELD-SYMBOLS: <ls_objname> TYPE ty_objname.
*
*DATA ls_r_author TYPE RANGE OF syst_uname.
*DATA lv_text TYPE string.
*DATA result TYPE abap_bool.
*DATA rc LIKE sy-subrc.
*DATA objects_package TYPE TABLE OF t_objects_package.
*DATA packageline TYPE t_objects_package.
*FIELD-SYMBOLS: <obj> LIKE LINE OF objects_package.
*DATA tabletypeline TYPE ko105.
*DATA tabletypesin TYPE TABLE OF ko105.
*DATA tabletypesout TYPE tr_object_texts.
*DATA tabletypeoutline TYPE ko100.
*DATA lt_fieldcat  TYPE          slis_t_fieldcat_alv.
*DATA ls_fieldcat  LIKE LINE OF  lt_fieldcat.
*DATA ls_layout    TYPE          slis_layout_alv.
*DATA lv_count TYPE i.
*DATA lv_pers  TYPE i.
*
**end of data addition for packages
**addition of Transport data
*TYPES: BEGIN OF t_requestobject,
*         object   TYPE e071-object,
*         obj_name TYPE e071-obj_name,
*       END OF t_requestobject.
*
*TYPES: tt_requestobject TYPE TABLE OF t_requestobject.
*
*DATA it_requestobject TYPE TABLE OF t_requestobject.
*DATA wa_requestobject TYPE t_requestobject.
*
**end of data addition for transport
*
*DATA pluginline TYPE t_plugin.
*DATA pluginlist TYPE TABLE OF t_plugin.
*DATA hidid(3) TYPE c.
*DATA currenttab TYPE string.
*DATA isslinkee(1) TYPE c VALUE ' '.
*DATA objecttable TYPE TABLE OF t_objecttable.
*DATA objectline TYPE t_objecttable.
*DATA _objname TYPE string.
*DATA _objtype TYPE string.
*DATA nuggetname TYPE string.
*DATA targetobject TYPE REF TO zsaplink.
*DATA xml TYPE string.
*DATA excclass TYPE REF TO zcx_saplink.
*DATA errormsg TYPE string.
*DATA statusmsg TYPE string.
*DATA _pluginexists TYPE flag.
*DATA _objectexists TYPE flag.
*DATA _flag TYPE flag.
*
*DATA errorflag TYPE flag.
*DATA it_nuggetobject TYPE TABLE OF t_nuggetobject.
*DATA wa_nuggetobject TYPE t_nuggetobject.
*
*DATA deffilename TYPE string.
*DATA retfilename TYPE string.
*DATA retpath TYPE string.
*DATA defaultpath TYPE string VALUE 'c:\sap\code_backup\' .
*DATA retfullpath TYPE string.
*DATA retuseract TYPE i.
*DATA retfiletable TYPE filetable.
*DATA retrc TYPE sysubrc.
*DATA retuseraction TYPE i.
*
*DATA nugg TYPE REF TO zsaplink_nugget.
*DATA stemp TYPE string.
*DATA anxmldoc TYPE REF TO if_ixml_document.
*DATA ixmldocument TYPE REF TO if_ixml_document.
*DATA lv_devclass TYPE devclass VALUE 'ZPOS'.
*
*DATA foo TYPE REF TO data.
*DATA len TYPE i.
*
*DATA: l_marker       TYPE i,
*      l_offset       TYPE i,
*      l_total_offset TYPE i.
*
*DATA:
*  es_selected_request TYPE trwbo_request_header,
*  es_selected_task    TYPE trwbo_request_header,
*  iv_organizer_type   TYPE trwbo_calling_organizer,
*  is_selection        TYPE trwbo_selection.
*
**\--------------------------------------------------------------------/
*
*
**/------------------------SELECTION SCREEN----------------------------\
*
**Slinkee tab
*SELECTION-SCREEN BEGIN OF TABBED BLOCK tabb FOR 20 LINES.
*  SELECTION-SCREEN TAB (17) TEXT-tb1 USER-COMMAND obj
*    DEFAULT SCREEN 110.
*SELECTION-SCREEN END OF BLOCK tabb.
*
**Slinkee tab
*SELECTION-SCREEN BEGIN OF SCREEN 110 AS SUBSCREEN.
*  SELECTION-SCREEN ULINE.
*  SELECTION-SCREEN COMMENT /10(75) TEXT-001.
*  SELECTION-SCREEN ULINE.
*  SELECTION-SCREEN COMMENT /10(75) TEXT-002.
*  SELECTION-SCREEN ULINE.
*  SELECTION-SCREEN BEGIN OF BLOCK main WITH FRAME.
*    SELECTION-SCREEN BEGIN OF BLOCK splk WITH FRAME TITLE TEXT-slk.
*
*      SELECTION-SCREEN:
*      BEGIN OF LINE,
*      COMMENT 1(62) TEXT-c10 FOR FIELD p_me.
*      PARAMETERS: p_me AS CHECKBOX DEFAULT 'X' .
*      SELECTION-SCREEN: END OF LINE.
*
*      SELECTION-SCREEN:
*      BEGIN OF LINE,
*      COMMENT 1(62) TEXT-c01 FOR FIELD p_cls.
*      PARAMETERS: p_cls AS CHECKBOX DEFAULT 'X' .
*      SELECTION-SCREEN: END OF LINE.
*
*      SELECTION-SCREEN:
*      BEGIN OF LINE,
*      COMMENT 1(62) TEXT-c00 FOR FIELD p_fun.
*      PARAMETERS: p_fun AS CHECKBOX DEFAULT 'X' .
*      SELECTION-SCREEN: END OF LINE.
*
*      SELECTION-SCREEN:
*      BEGIN OF LINE,
*      COMMENT 1(62) TEXT-c04 FOR FIELD p_prg.
*      PARAMETERS: p_prg AS CHECKBOX DEFAULT 'X' .
*      SELECTION-SCREEN: END OF LINE.
*
*      SELECTION-SCREEN:
*      BEGIN OF LINE,
*      COMMENT 1(62) TEXT-c02 FOR FIELD p_tbl.
*      PARAMETERS: p_tbl AS CHECKBOX DEFAULT 'X' .
*      SELECTION-SCREEN: END OF LINE.
*
*      SELECTION-SCREEN:
*      BEGIN OF LINE,
*      COMMENT 1(62) TEXT-c03 FOR FIELD p_tty.
*      PARAMETERS: p_tty AS CHECKBOX DEFAULT 'X' .
*      SELECTION-SCREEN: END OF LINE.
*
*      SELECTION-SCREEN:
*      BEGIN OF LINE,
*      COMMENT 1(62) TEXT-c05 FOR FIELD p_dom.
*      PARAMETERS: p_dom AS CHECKBOX DEFAULT 'X' .
*      SELECTION-SCREEN: END OF LINE.
*
*
*      SELECTION-SCREEN:
*      BEGIN OF LINE,
*      COMMENT 1(62) TEXT-c06 FOR FIELD p_oth.
*      PARAMETERS: p_oth AS CHECKBOX .
*      SELECTION-SCREEN: END OF LINE.
*
*      SELECTION-SCREEN:
*      BEGIN OF LINE,
*      COMMENT 1(62) TEXT-c50 FOR FIELD p_cat.
*      PARAMETERS: p_cat AS CHECKBOX DEFAULT 'X' .
*      SELECTION-SCREEN: END OF LINE.
*
*      PARAMETERS import TYPE c NO-DISPLAY .
*
*      PARAMETERS export TYPE c DEFAULT 'X' NO-DISPLAY.
*    SELECTION-SCREEN END OF BLOCK splk.
*
*    SELECTION-SCREEN BEGIN OF BLOCK opt WITH FRAME TITLE TEXT-opt.
*
*      PARAMETERS overwr TYPE c AS CHECKBOX MODIF ID did.
*      PARAMETERS filename(300) LOWER CASE TYPE c MODIF ID did.
*      PARAMETERS plugin TYPE ko100-object DEFAULT 'CLAS' NO-DISPLAY.
*      PARAMETERS objname(40) TYPE c DEFAULT 'ZCL_POS_TRANS_BASE' NO-DISPLAY.
*    SELECTION-SCREEN END OF BLOCK opt.
*  SELECTION-SCREEN END OF BLOCK main.
*SELECTION-SCREEN END OF SCREEN 110.
*
**\--------------------------------------------------------------------/
*
*
**/----------------------selection screen events-----------------------\
*INITIALIZATION.
*  CHECK sy-uname ='BRENNANN' .
*
*  CALL METHOD zsaplink=>getplugins(
*    CHANGING
*      objecttable = objecttable ).
*
*  IMPORT isslinkee FROM MEMORY ID 'ISSLNK'.
*
*  tabb-dynnr = 110.
*  tabb-activetab = 'OBJ'.
*
*AT SELECTION-SCREEN.
*  isslinkee = 'X'.
*  EXPORT isslinkee TO MEMORY ID 'ISSLNK'.
*
*AT SELECTION-SCREEN OUTPUT.
**** hide/show fields according to current selection
*  IF import = 'X'.
*    hidid = 'UID'.
*  ELSEIF export = 'X'.
*    hidid = 'DID'.
*  ENDIF.
*
*  LOOP AT SCREEN.
*    IF screen-group1 = hidid.
*      screen-active = '0'.
*      screen-invisible = '1'.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.
*
*  LOOP AT SCREEN.
*
*  ENDLOOP.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR filename.
*  CALL METHOD cl_gui_frontend_services=>file_open_dialog
*    EXPORTING
*      multiselection    = abap_false
*      file_filter       = '*.slnk'
*      default_extension = 'slnk'
*    CHANGING
*      file_table        = retfiletable
*      rc                = retrc
*      user_action       = retuseraction.
*  READ TABLE retfiletable INTO filename INDEX 1.
*
**
*
**\--------------------------------------------------------------------/
*
**/----------------------main------------------------------------------\
*START-OF-SELECTION.
*
*  IF p_me = abap_true.
*    APPEND INITIAL LINE TO ls_r_author ASSIGNING FIELD-SYMBOL(<ls_r_author>).
*    <ls_r_author>-sign   = 'I'.
*    <ls_r_author>-option = 'EQ'.
*    <ls_r_author>-low    = sy-uname.
*  ELSE.
*    APPEND INITIAL LINE TO ls_r_author ASSIGNING <ls_r_author>.
*    <ls_r_author>-sign   = 'I'.
*    <ls_r_author>-option = 'CP'.
*    <ls_r_author>-low    = '*'.
*
*  ENDIF.
*
*  CLEAR: errormsg, statusmsg.
*  PERFORM createfolder.
*  IF p_fun = 'X'.
*    IF p_cat = 'X'.
*      SELECT object obj_name FROM tadir INTO TABLE lt_objnames WHERE object = 'FUGR' AND author IN ls_r_author AND obj_name LIKE 'Z%' AND devclass = lv_devclass AND delflag NE 'X'.
*    ELSE.
*      SELECT object obj_name FROM tadir INTO TABLE lt_objnames WHERE object = 'FUGR' AND author IN ls_r_author AND obj_name LIKE 'Z%' AND delflag NE 'X'.
*    ENDIF.
*  ENDIF.
*
*  IF p_cls = 'X'.
*    IF p_cat = 'X'.
*      SELECT object obj_name FROM tadir INTO TABLE lt_objnames WHERE object = 'CLAS' AND author IN ls_r_author AND obj_name LIKE 'Z%' AND devclass = lv_devclass AND delflag NE 'X'.
*    ELSE.
*      SELECT object obj_name FROM tadir INTO TABLE lt_objnames WHERE object = 'CLAS' AND author IN ls_r_author AND obj_name LIKE 'Z%' AND delflag NE 'X'.
*    ENDIF.
*  ENDIF.
*
*  IF p_prg = 'X'.
*    IF p_cat = 'X'.
**      SELECT object obj_name FROM tadir APPENDING TABLE lt_objnames WHERE object = 'PROG' AND obj_name LIKE 'Z%' AND devclass = lv_devclass AND delflag NE 'X'.
*      SELECT object obj_name FROM tadir APPENDING TABLE lt_objnames WHERE object = 'PROG' AND author IN ls_r_author AND devclass = lv_devclass AND delflag NE 'X'.
*    ELSE.
*      SELECT object obj_name FROM tadir APPENDING TABLE lt_objnames WHERE object = 'PROG' AND author IN ls_r_author AND obj_name LIKE 'Z%' AND delflag NE 'X'.
*    ENDIF.
*  ENDIF.
*
*  IF p_tbl = 'X'.
*    IF p_cat = 'X'.
*
*      SELECT object obj_name FROM tadir APPENDING TABLE lt_objnames WHERE object = 'TABL' AND author IN ls_r_author AND obj_name LIKE 'Z%' AND devclass = lv_devclass AND delflag NE 'X'.
*    ELSE.
*      SELECT object obj_name FROM tadir APPENDING TABLE lt_objnames WHERE object = 'TABL' AND author IN ls_r_author AND obj_name LIKE 'Z%' AND delflag NE 'X'.
*    ENDIF.
*  ENDIF.
*
*  IF p_tty = 'X'.
*    IF p_cat = 'X'.
*      SELECT object obj_name FROM tadir APPENDING TABLE lt_objnames WHERE object = 'TTYP' AND author IN ls_r_author AND obj_name LIKE 'Z%' AND devclass = lv_devclass AND delflag NE 'X'.
*    ELSE.
*      SELECT object obj_name FROM tadir APPENDING TABLE lt_objnames WHERE object = 'TTYP' AND author IN ls_r_author AND obj_name LIKE 'Z%' AND delflag NE 'X'.
*    ENDIF.
*  ENDIF.
*
*
*  IF p_dom = 'X'.
*    IF p_cat = 'X'.
*      SELECT object obj_name FROM tadir APPENDING TABLE lt_objnames WHERE ( object = 'DOMA' OR object = 'DTEL' ) AND author IN ls_r_author AND obj_name LIKE 'Z%' AND devclass = lv_devclass AND delflag NE 'X'.
*
*    ELSE.
*      SELECT object obj_name FROM tadir APPENDING TABLE lt_objnames WHERE ( object = 'DOMA' OR object = 'DTEL' ) AND author IN ls_r_author AND obj_name LIKE 'Z%' AND delflag NE 'X'.
*    ENDIF.
*  ENDIF.
*
*
*  IF p_oth = 'X'.
*    IF p_cat = 'X'.
*      SELECT object obj_name FROM tadir APPENDING TABLE lt_objnames WHERE devclass = lv_devclass AND author IN ls_r_author AND delflag NE 'X'.
*
*    ELSE.
*      SELECT object obj_name FROM tadir APPENDING TABLE lt_objnames WHERE devclass LIKE 'Z%' AND author IN ls_r_author AND delflag NE 'X'.
*    ENDIF.
*  ENDIF.
*
*  SORT lt_objnames BY object obj_name. DELETE ADJACENT DUPLICATES FROM lt_objnames COMPARING ALL FIELDS.
*
*
*
************** S L I N K E E *************
*  LOOP AT lt_objnames INTO l_objname.
*    CLEAR: errorflag, errormsg.
*
*    IF isslinkee IS NOT INITIAL.
*
*      plugin = l_objname-object.
*      _objname = l_objname-obj_name.
*
**   Export slinkee
*
*      IF export = 'X'.
*        IF plugin IS INITIAL.
*          MESSAGE s208(00) WITH 'object type required'.
*          EXIT.
*        ELSEIF _objname IS INITIAL.
*          MESSAGE s208(00) WITH 'object name required'.
*          EXIT.
*        ENDIF.
*        READ TABLE objecttable INTO objectline WITH KEY object = plugin.
*        IF sy-subrc <> 0.
*          CONCATENATE 'Plugin for object type' plugin
*            'is not installed on this system' INTO errormsg
*            SEPARATED BY space.
*          PERFORM writemessage USING 'E' errormsg.
*          EXIT.
*        ENDIF.
*        CREATE OBJECT targetobject TYPE (objectline-classname)
*          EXPORTING
*            name = _objname.
*        TRY.
*            ixmldocument = targetobject->createixmldocfromobject( ).
*          CATCH zcx_saplink INTO excclass.
*            errormsg = excclass->get_text( ).
*            CONCATENATE errormsg ' - ' plugin ' - ' _objname INTO errormsg RESPECTING BLANKS.
*            PERFORM writemessage USING 'E' errormsg.
*        ENDTRY.
*        IF errormsg IS INITIAL.
*
*          xml = zsaplink=>convertixmldoctostring( ixmldocument ).
*
*          CONCATENATE plugin '_' _objname '.slnk' INTO deffilename.
*          CLEAR errorflag.
*
*          CONCATENATE retpath deffilename INTO retfullpath.
*          CONCATENATE 'Downloading Class:- ' _objname INTO lv_text.
*
*          CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*            EXPORTING       "PERCENTAGE = 50
*              text   = lv_text
*            EXCEPTIONS
*              OTHERS = 1.
*          PERFORM putonmachine USING retfullpath xml.
*
**      perform downloadXMLtoLM using defFilename xml
**      changing errorflag.
*          IF errorflag IS NOT INITIAL.
*            MESSAGE s208(00) WITH 'Action cancelled'.
*            EXIT.
*          ENDIF.
*        ENDIF.
**      perform displayXMLOnScreen using xml.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
**\--------------------------------------------------------------------/
*
**/----------------------displayXMLOnScreen----------------------------\
*FORM displayxmlonscreen USING xmlstring TYPE string.
*  DATA printxmldoc TYPE REF TO cl_xml_document.
*  DATA rc TYPE sysubrc.
*
*  CREATE OBJECT printxmldoc.
*  rc = printxmldoc->parse_string( xmlstring ).
*  CALL METHOD printxmldoc->display( ).
*
*ENDFORM.                    "displayXMLOnScreen
**\--------------------------------------------------------------------/
*
**/----------------------downloadXMLToLM-------------------------------\
*FORM downloadxmltolm USING   deffilename TYPE string
*                             xmlstring TYPE string
*                    CHANGING _errorflag TYPE flag.
*
*  DATA retfilename TYPE string.
*  DATA retpath TYPE string.
*  DATA retfullpath TYPE string.
*  DATA retuseract TYPE i.
*
*  CLEAR _errorflag.
*
*  CALL METHOD cl_gui_frontend_services=>file_save_dialog
*    EXPORTING
*      default_file_name = deffilename
*    CHANGING
*      filename          = retfilename
*      path              = retpath
*      fullpath          = retfullpath
*      user_action       = retuseract.
*
*  IF retuseract <> 0.
*    _errorflag = 'X'.
*  ELSE.
*    PERFORM putonmachine USING retfullpath xmlstring.
*  ENDIF.
*
*
*ENDFORM.                    "downloadXMLToLM
**\--------------------------------------------------------------------/
*
*
**/------------------------putOnMachine--------------------------------\
*FORM putonmachine USING fullpath TYPE string xmlstring TYPE string.
*
**rrq: issue 43--> replace binary with char table
**old code removed, use subversion for recovery
**types: begin of t_char,
**        maxChar(65535) type C,
**       end of t_char.
*
**data: tempTable_char type table of t_char,
*  DATA: temptable_char TYPE table_of_strings,
*        tempstring     TYPE string.
*
*  IF retuseract = 0.
*
*    SPLIT xmlstring AT cl_abap_char_utilities=>newline
*    INTO TABLE temptable_char.
*
*    CALL METHOD cl_gui_frontend_services=>gui_download
*      EXPORTING
*        filename = fullpath
*        filetype = 'DAT'
*      CHANGING
*        data_tab = temptable_char.
*
*  ENDIF.
**<--rrq: issue 43
*ENDFORM.                    "putOnMachine
**\--------------------------------------------------------------------/
*
*
**/----------------------uploadXMLFromLM-------------------------------\
*FORM uploadxmlfromlm USING p_filename xmlstring TYPE string .
*  DATA retfiletable TYPE filetable.
*  DATA retrc TYPE sysubrc.
*  DATA retuseraction TYPE i.
*  DATA temptable TYPE table_of_strings.
*  DATA temptable_bin TYPE TABLE  OF xstring.
*  DATA l_filename TYPE string.
*
*  l_filename = p_filename.
*  CALL METHOD cl_gui_frontend_services=>gui_upload
*    EXPORTING
*      filename                = l_filename
*    CHANGING
*      data_tab                = temptable
*    EXCEPTIONS
*      file_open_error         = 1
*      file_read_error         = 2
*      no_batch                = 3
*      gui_refuse_filetransfer = 4
*      invalid_type            = 5
*      no_authority            = 6
*      unknown_error           = 7
*      bad_data_format         = 8
*      header_not_allowed      = 9
*      separator_not_allowed   = 10
*      header_too_long         = 11
*      unknown_dp_error        = 12
*      access_denied           = 13
*      dp_out_of_memory        = 14
*      disk_full               = 15
*      dp_timeout              = 16
*      not_supported_by_gui    = 17
*      error_no_gui            = 18
*      OTHERS                  = 19.
*  IF sy-subrc <> 0.
*    CASE sy-subrc.
*      WHEN '1'.
*        PERFORM writemessage USING 'E' 'File Open Error'.
*      WHEN OTHERS.
*        PERFORM writemessage USING 'E' 'Unknown Error occured'.
*    ENDCASE.
*  ENDIF.
*
**  call method CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD
**        exporting
**          FILENAME = l_fileName
**        changing
**          data_tab = tempTable.
*  PERFORM createstring USING temptable CHANGING xmlstring.
*
*ENDFORM.                    "uploadXMLFromLM
**\--------------------------------------------------------------------/
*
**/----------------------createString----------------------------------\
*FORM createstring
*      USING
*        temptable TYPE table_of_strings
*      CHANGING
*        bigstring TYPE string.
*  DATA stemp TYPE string.
*  LOOP AT temptable INTO stemp.
*    CONCATENATE bigstring stemp cl_abap_char_utilities=>newline
*      INTO bigstring.
*  ENDLOOP.
*
*ENDFORM.                    "createString
**\--------------------------------------------------------------------/
*
**/----------------------installObject---------------------------------\
*FORM installobject USING l_ixmldocument TYPE REF TO if_ixml_document
*                         l_overwriteflag TYPE flag
*                CHANGING l_errorflag TYPE flag
*                         l_message TYPE string.
*
*  DATA l_objname TYPE string.
*  DATA l_objtype TYPE string.
*  DATA l_objtable TYPE TABLE OF t_objecttable.
*  DATA l_objline TYPE t_objecttable.
*  DATA l_targetobject TYPE REF TO zsaplink.
*  DATA l_installobject TYPE string.
*  DATA l_excclass TYPE REF TO zcx_saplink.
*
*  CLEAR l_errorflag.
*  CALL METHOD zsaplink=>getobjectinfofromixmldoc
*    EXPORTING
*      ixmldocument = l_ixmldocument
*    IMPORTING
*      objtypename  = l_objtype
*      objname      = l_objname.
*
*  CALL METHOD zsaplink=>getplugins( CHANGING objecttable = l_objtable ).
*
*  READ TABLE l_objtable INTO l_objline WITH KEY object = l_objtype.
*
*  IF sy-subrc <> 0.
*    CONCATENATE 'There is no installed SAPlink plugin for object type'
*      l_objtype l_objline-text INTO l_message SEPARATED BY space.
*    l_errorflag = 'X'.
*  ELSE.
*    CREATE OBJECT l_targetobject TYPE (l_objline-classname)
*      EXPORTING
*        name = l_objname.
*
*    TRY.
*        l_installobject = l_targetobject->createobjectfromixmldoc(
*                                        ixmldocument = l_ixmldocument
*                                        overwrite = l_overwriteflag ).
**    bad times
*      CATCH zcx_saplink INTO l_excclass.
*        l_message = l_excclass->get_text( ).
*        l_errorflag = 'X'.
*    ENDTRY.
**   good times
*    IF l_installobject IS NOT INITIAL.
*      CONCATENATE 'Installed: ' l_objtype '-' l_installobject
*       INTO l_message SEPARATED BY space.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.                    "installObject
**\--------------------------------------------------------------------/
*
**/----------------------confirmOverwrite------------------------------\
*FORM confirmoverwrite USING l_objinfo TYPE string
*                   CHANGING l_answer TYPE flag.
*
*  DATA l_message TYPE string.
*  DATA l_title TYPE string.
*
*  CLEAR l_answer.
*  l_title = 'Overwrite confirm. Proceed with CAUTION!'.
*
*  CONCATENATE 'You have selected to overwrite originals.'
*    l_objinfo 'will be overwritten. Are you sure?'
*    INTO l_message SEPARATED BY space.
*
*  CALL FUNCTION 'POPUP_TO_CONFIRM'
*    EXPORTING
*      titlebar              = l_title
*      text_question         = l_message
*      text_button_1         = 'Yes'
*      text_button_2         = 'Yes to all'
*      default_button        = '1'
*      display_cancel_button = 'X'
*    IMPORTING
*      answer                = l_answer.
*ENDFORM.                    "confirmOverwrite
**\--------------------------------------------------------------------/
*
**/----------------------checkObject-----------------------------------\
*FORM checkobject USING l_ixmldocument TYPE REF TO if_ixml_document
*              CHANGING l_objtype TYPE string
*                       l_objname TYPE string
*                       l_pluginexists TYPE flag
*                       l_objectexists TYPE flag.
*
*  DATA l_objtable TYPE TABLE OF t_objecttable.
*  DATA l_objline TYPE t_objecttable.
*  DATA l_targetobject TYPE REF TO zsaplink.
*
*  CLEAR: l_objtype, l_objname, l_pluginexists, l_objectexists.
*  CALL METHOD zsaplink=>getobjectinfofromixmldoc
*    EXPORTING
*      ixmldocument = l_ixmldocument
*    IMPORTING
*      objtypename  = l_objtype
*      objname      = l_objname.
*
*  CALL METHOD zsaplink=>getplugins( CHANGING objecttable = l_objtable ).
*
*  READ TABLE l_objtable INTO l_objline WITH KEY object = l_objtype.
*
*  IF sy-subrc = 0.
*    l_pluginexists = 'X'.
*    CREATE OBJECT l_targetobject TYPE (l_objline-classname)
*      EXPORTING
*        name = l_objname.
*
*    l_objectexists = l_targetobject->checkexists( ).
*  ENDIF.
*
*ENDFORM.                    "checkObject
**\--------------------------------------------------------------------/
*
**/---------------------get_current_screen_value-----------------------\
*FORM get_current_screen_value  USING    l_screen_field
*                                        l_screen_number
*                               CHANGING l_screen_value.
*
*  DATA it_dynpfields TYPE STANDARD TABLE OF dynpread.
*  DATA wa_dynpfields TYPE dynpread.
*
*
*  wa_dynpfields-fieldname = l_screen_field.
*  APPEND wa_dynpfields TO it_dynpfields.
*
*
*  CALL FUNCTION 'DYNP_VALUES_READ'
*    EXPORTING
*      dyname               = sy-cprog
*      dynumb               = l_screen_number
*      translate_to_upper   = 'X'
**     REQUEST              = ' '
**     PERFORM_CONVERSION_EXITS = ' '
**     PERFORM_INPUT_CONVERSION = ' '
**     DETERMINE_LOOP_INDEX = ' '
*    TABLES
*      dynpfields           = it_dynpfields
*    EXCEPTIONS
*      invalid_abapworkarea = 1
*      invalid_dynprofield  = 2
*      invalid_dynproname   = 3
*      invalid_dynpronummer = 4
*      invalid_request      = 5
*      no_fielddescription  = 6
*      invalid_parameter    = 7
*      undefind_error       = 8
*      double_conversion    = 9
*      stepl_not_found      = 10
*      OTHERS               = 11.
*  IF sy-subrc <> 0.
**  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ELSE.
*    READ TABLE it_dynpfields INTO wa_dynpfields
*      WITH KEY fieldname = l_screen_field.
*    IF sy-subrc = 0.
*      l_screen_value = wa_dynpfields-fieldvalue.
*    ENDIF.
*  ENDIF.
*
*
*ENDFORM.                    " get_current_screen_value
**/---------------------writeMessage-----------------------\
*FORM writemessage USING VALUE(p_type) TYPE sy-msgty
*                        VALUE(p_msg).
*  CASE p_type.
*    WHEN 'E' OR 'A' OR 'X'.
*      WRITE / icon_led_red AS ICON.
*    WHEN 'W'.
*      WRITE / icon_led_yellow AS ICON.
*    WHEN OTHERS.
*      WRITE / icon_led_green AS ICON.
*  ENDCASE.
*
*  WRITE p_msg.
*ENDFORM.                    "WriteMessage
*
**/-------------------------pf_status_set-------------------\
*FORM pf_status_set USING rt_extab TYPE slis_t_extab.
*
*  SET PF-STATUS 'SELOBJ' EXCLUDING rt_extab.
*
*ENDFORM.                    "pf_status_set
**/-------------------------user_command_user-------------------\
*FORM user_command_user USING r_ucomm LIKE sy-ucomm
*                  rs_selfield TYPE slis_selfield.
*  CASE r_ucomm.
*    WHEN 'TAKE'.
*      rs_selfield-exit = 'X'.
*  ENDCASE.
*ENDFORM.                    "user_command_user
*
**---------------build_fieldCatalog---------------------------------*
*FORM build_fieldcatalog .
**** Display list to select the objects for downloading
*  ls_fieldcat-fieldname = 'OBJECT'.
*  ls_fieldcat-seltext_l = 'Object/Plugin'.
*  APPEND ls_fieldcat TO lt_fieldcat.
*
*  ls_fieldcat-fieldname = 'OBJECT_TXT'.
*  ls_fieldcat-seltext_l = 'Object/Plugin'.
*  APPEND ls_fieldcat TO lt_fieldcat.
*
*  ls_fieldcat-fieldname = 'OBJ_NAME'.
*  ls_fieldcat-seltext_l = 'Object name'.
*  APPEND ls_fieldcat TO lt_fieldcat.
*
*  ls_fieldcat-fieldname = 'DOWN_FLAG'.
*  ls_fieldcat-seltext_s = 'Plugin'.
*  ls_fieldcat-seltext_l =
*  'Plugin available'.
*  APPEND ls_fieldcat TO lt_fieldcat.
*
*  ls_fieldcat-fieldname = 'MSG'.
*  ls_fieldcat-seltext_s = 'Message'.
*  ls_fieldcat-seltext_l =
*  'Status Message'.
*  APPEND ls_fieldcat TO lt_fieldcat.
*
*  ls_layout-box_fieldname     = 'SELECT'.
*  ls_layout-f2code            = 'MYPICK' .
*  ls_layout-colwidth_optimize = 'X'.
*  ls_layout-lights_fieldname  = 'STATUS'.
*ENDFORM.                    " build_fieldCatalog
**&--------------------------------------------------------------------*
**&      Form  ShowInitialGrid
*FORM showinitialgrid  TABLES   p_objects.
*
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
*      i_callback_program       = 'ZSAPLINK'
*      i_callback_pf_status_set = 'PF_STATUS_SET'
*      i_callback_user_command  = 'USER_COMMAND_USER'
*      i_grid_title             = 'Select objects'
*      it_fieldcat              = lt_fieldcat
*      is_layout                = ls_layout
*    TABLES
*      t_outtab                 = p_objects
*    EXCEPTIONS
*      OTHERS                   = 0.
*
*ENDFORM.                    " ShowInitialGrid
**&---------------------------------------------------------------------*
**&      Form  showResultsGrid
*FORM showresultsgrid  TABLES   p_objects.
**    ** Display results
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
*      i_callback_program      = 'ZSAPLINK'
*      i_callback_user_command = 'USER_COMMAND_USER'
*      it_fieldcat             = lt_fieldcat
*      i_grid_title            = 'Download results'
*      is_layout               = ls_layout
*    TABLES
*      t_outtab                = p_objects
*    EXCEPTIONS
*      OTHERS                  = 0.
*
*ENDFORM.                    " showResultsGrid
**&---------------------------------------------------------------------*
**&      Form  check_objects
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM check_objects .
*  LOOP AT objects_package ASSIGNING <obj>.
**     Check what can be downloaded and what can not.
*    READ TABLE objecttable INTO objectline
*        WITH KEY object = <obj>-object.
*    IF sy-subrc = 0.
**        Plug-in exists... set flag and make selected by default
*      <obj>-down_flag = 'X'.
*      <obj>-select = 'X'.
*    ELSE.
*      <obj>-msg = 'No Plugin Available'.
*      <obj>-down_flag = ' '.
*    ENDIF.
**     get texts
*    REFRESH tabletypesin.
*    tabletypeline-object = <obj>-object.
*    APPEND tabletypeline TO tabletypesin.
*
*    CALL FUNCTION 'TRINT_OBJECT_TABLE'
*      TABLES
*        tt_types_in  = tabletypesin
*        tt_types_out = tabletypesout.
*
*    LOOP AT tabletypesout INTO tabletypeoutline.
*      <obj>-object      = tabletypeoutline-object.
*      <obj>-object_txt = tabletypeoutline-text.
*    ENDLOOP.
*
*  ENDLOOP.
*  SORT objects_package BY down_flag DESCENDING object ASCENDING.
*
*ENDFORM.                    " check_objects
**&---------------------------------------------------------------------*
**&      Form  CreateEmptyNugget
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM createemptynugget USING p_nuggname.
*
*  ixmldocument = zsaplink_nugget=>createemptyxml(
*    nuggetname = p_nuggname ).
*  xml = zsaplink=>convertixmldoctostring( ixmldocument ).
*  CONCATENATE 'NUGG_' p_nuggname '.nugg' INTO stemp.
*  CLEAR errorflag.
*  PERFORM downloadxmltolm USING stemp xml
*                          CHANGING errorflag.
*  IF errorflag IS NOT INITIAL.
*    EXIT.
*  ENDIF.
*
*ENDFORM.                    " CreateEmptyNugget
*
**rrq: enhancement 3 & 42-->
**&---------------------------------------------------------------------*
**&      Form  addObjectstoNugget
**&---------------------------------------------------------------------*
*FORM addobjectstonugget .
*
*  PERFORM check_objects.
*  PERFORM build_fieldcatalog.
*
*  PERFORM showinitialgrid TABLES objects_package.
*
*  IF sy-ucomm <> 'TAKE'.
*    RETURN.
*  ENDIF .
*
**  Downloading
*  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*    EXPORTING
*      percentage = 1
*      text       = 'Upload file'.
*
**  PERFORM uploadxmlfromlm USING nugfile xml.
*
*  ixmldocument = zsaplink=>convertstringtoixmldoc( xml ).
*
**  CREATE OBJECT nugg
**    EXPORTING
**      ixmldocument = ixmldocument.
*
*  DESCRIBE TABLE objects_package LINES lv_count.
*  LOOP AT objects_package ASSIGNING <obj>
*  WHERE down_flag = 'X' AND select = 'X'.
*    lv_pers = sy-tabix * 100 / lv_count .
*    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*      EXPORTING
*        percentage = lv_pers
*        text       = <obj>-obj_name.
*
*    _objname = <obj>-obj_name. "nobjNam.
*    stemp = <obj>-object.      "nplugin.
*    TRY.
*        nugg->addobjecttonugget(
*        objname = _objname objtype = stemp ).
*      CATCH zcx_saplink INTO excclass.
*        errormsg = excclass->get_text( ).
**        perform writeMessage using 'E' errorMsg.
*        <obj>-msg = errormsg.
*        <obj>-status = 1.
*        CONTINUE.
*    ENDTRY.
*    <obj>-msg = 'Added to nugget'.
*    <obj>-status = 3.
*  ENDLOOP.
*
*  READ TABLE objects_package INTO packageline
*    WITH KEY status = 3. "ewH:do not download if none added
*
*  IF sy-subrc = 0.
*    ixmldocument = nugg->createixmldocfromnugget( ).
*    xml = zsaplink=>convertixmldoctostring( ixmldocument ).
**    stemp = nugfile.
*    PERFORM putonmachine USING stemp xml.
*  ENDIF.
*
*  PERFORM showresultsgrid TABLES objects_package.
*
*ENDFORM.                    " addObjectstoNugget
*
**&---------------------------------------------------------------------*
**&      Form  createFolder
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*FORM createfolder.
*
*
*  CONCATENATE  defaultpath sy-sysid '\' sy-datum '\' sy-uzeit '\' INTO retpath.
*  CALL METHOD cl_gui_frontend_services=>directory_exist
*    EXPORTING
*      directory            = retpath
*    RECEIVING
*      result               = result
*    EXCEPTIONS
*      cntl_error           = 1
*      error_no_gui         = 2
*      wrong_parameter      = 3
*      not_supported_by_gui = 4
*      OTHERS               = 5.
*
*  IF NOT result = abap_true.
*    CALL METHOD cl_gui_frontend_services=>directory_create
*      EXPORTING
*        directory                = retpath
*      CHANGING
*        rc                       = rc
*      EXCEPTIONS
*        directory_create_failed  = 1
*        cntl_error               = 2
*        error_no_gui             = 3
*        directory_access_denied  = 4
*        directory_already_exists = 5
*        path_not_found           = 6
*        unknown_error            = 7
*        not_supported_by_gui     = 8
*        wrong_parameter          = 9
*        OTHERS                   = 10.
*
*  ENDIF.
*
*ENDFORM.                    "createFolder
*  <--rrq: enhancement 3 & 42
