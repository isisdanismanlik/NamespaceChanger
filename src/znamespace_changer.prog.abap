*&---------------------------------------------------------------------*
*& Report ZNAMESPACE_CHANGER
*&---------------------------------------------------------------------*
*& Developer mkysoft
*& Homepage https://github.com/larshp/NamespaceChanger.git
*&---------------------------------------------------------------------*
REPORT znamespace_changer.

TYPES:
  BEGIN OF ty_replace,
    find TYPE string,
    with TYPE string,
  END OF ty_replace .

DATA: lo_repo     TYPE REF TO zcl_abapgit_repo_offline,
      ls_data     TYPE zif_abapgit_persistence=>ty_repo,
      li_popups   TYPE REF TO zif_abapgit_popups,
      lt_srcfiles TYPE zif_abapgit_definitions=>ty_files_item_tt,
      lt_dstfiles TYPE zif_abapgit_definitions=>ty_files_item_tt,
      lo_log      TYPE REF TO zif_abapgit_log,
      lt_replaces TYPE STANDARD TABLE OF ty_replace WITH DEFAULT KEY,
      lt_finds    TYPE TABLE OF string,
      lt_withs    TYPE TABLE OF string,
      lv_xfind    TYPE xstring,
      lv_find     TYPE string,
      lv_xwith    TYPE xstring,
      lv_with     TYPE string,
      lv_spackage TYPE devclass,
      lv_dpackage TYPE devclass.

FIELD-SYMBOLS: <ls_file>    LIKE LINE OF lt_srcfiles,
               <ls_replace> TYPE ty_replace.

PARAMETERS: p_spckg TYPE devclass DEFAULT '/SITIST/DEMO',
            p_find  TYPE string LOWER CASE DEFAULT 'SITIST sitist Sitist SitIst',
            p_dpckg TYPE devclass DEFAULT 'ZMKYSOFT',
            p_with  TYPE string LOWER CASE DEFAULT 'MKYSOFT mkysoft Mkysoft MkySoft'.

INITIALIZATION.

  ls_data-key = 'DUMMY'.
  ls_data-dot_abapgit = zcl_abapgit_dot_abapgit=>build_default( )->get_data( ).

START-OF-SELECTION.

* replace find/with
  SPLIT p_find AT space INTO TABLE lt_finds.
  SPLIT p_with AT space INTO TABLE lt_withs.
  CHECK lines( lt_finds ) EQ lines( lt_withs ).

  lv_find = p_spckg.
  lv_with = p_dpckg.
  APPEND VALUE #( find = lv_find with = lv_with ) TO lt_replaces.
  TRANSLATE lv_find USING '/#'.
  TRANSLATE lv_with USING '/#'.
  APPEND VALUE #( find = lv_find with = lv_with ) TO lt_replaces.
  TRANSLATE lv_find TO LOWER CASE.
  TRANSLATE lv_with TO LOWER CASE.
  APPEND VALUE #( find = lv_find with = lv_with ) TO lt_replaces.
  LOOP AT lt_finds INTO lv_find.
    lv_with = lt_withs[ sy-tabix ].
    APPEND VALUE #( find = lv_find with = lv_with ) TO lt_replaces.
  ENDLOOP.

*  lt_replaces = VALUE #( ( find = 'FORIBA' with = 'FTR' )
*                         ( find = 'foriba' with = 'ftr' )
*                         ( find = 'Foriba' with = 'ISIS' ) ).

*read package

  ls_data-package                  = p_spckg.
  ls_data-dot_abapgit-folder_logic = zif_abapgit_dot_abapgit=>c_folder_logic-full.

  CREATE OBJECT lo_repo
    EXPORTING
      is_data = ls_data.

  CREATE OBJECT lo_log TYPE zcl_abapgit_log.

  lt_srcfiles = lo_repo->get_files_local( ii_log = lo_log ).
  IF lo_log->count( ) > 0.
    lo_log->show( ).
  ENDIF.

* replace
  LOOP AT lt_srcfiles ASSIGNING <ls_file>.
    LOOP AT lt_replaces ASSIGNING <ls_replace>.
      REPLACE ALL OCCURRENCES OF <ls_replace>-find IN <ls_file>-file-path     WITH <ls_replace>-with.
      REPLACE ALL OCCURRENCES OF <ls_replace>-find IN <ls_file>-file-filename WITH <ls_replace>-with.

      CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
        EXPORTING
          text   = <ls_replace>-find
        IMPORTING
          buffer = lv_xfind
        EXCEPTIONS
          OTHERS = 0.
      CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
        EXPORTING
          text   = <ls_replace>-with
        IMPORTING
          buffer = lv_xwith
        EXCEPTIONS
          OTHERS = 0.
      REPLACE ALL OCCURRENCES OF lv_xfind IN <ls_file>-file-data WITH lv_xwith IN BYTE MODE.
    ENDLOOP.
    IF <ls_file>-item-obj_type EQ 'SHI3'.
      PERFORM replacemenuguid CHANGING <ls_file>-file-data.
    ENDIF.
    <ls_file>-file-sha1 = zcl_abapgit_hash=>sha1(
        iv_type = zif_abapgit_definitions=>c_type-blob
        iv_data = <ls_file>-file-data ).
    APPEND <ls_file> TO lt_dstfiles.
    WRITE: / <ls_file>-file-path, <ls_file>-file-filename.
  ENDLOOP.

  lv_spackage = lo_repo->get_package( ).
  TRANSLATE lv_spackage USING '/#'.
  lv_dpackage = p_dpckg.
  TRANSLATE lv_dpackage USING '/#'.
  REPLACE ALL OCCURRENCES OF lv_spackage IN lv_dpackage WITH lv_dpackage.
*  LOOP AT lt_replaces ASSIGNING <ls_replace>.
*    REPLACE ALL OCCURRENCES OF <ls_replace>-find IN lv_package WITH <ls_replace>-with.
*  ENDLOOP.

  zcl_abapgit_gui_router=>file_download( iv_package = lv_dpackage
                                         iv_xstr    = zcl_abapgit_zip=>encode_files( lt_dstfiles ) ).
*&---------------------------------------------------------------------*
*& Form REPLACEMENUGUID
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- lc_data
*&---------------------------------------------------------------------*
FORM replacemenuguid
            CHANGING pc_data TYPE xstring.
  DATA: lo_document TYPE REF TO if_ixml_document,
        lo_elements TYPE REF TO if_ixml_node_collection.

  CALL FUNCTION 'SDIXML_XML_TO_DOM'
    EXPORTING
      xml           = pc_data
    IMPORTING
      document      = lo_document
    EXCEPTIONS
      invalid_input = 1
      OTHERS        = 2.

  lo_elements = lo_document->get_elements_by_tag_name( name = 'NODE_ID' ).
  PERFORM checkandreplaceguid CHANGING lo_elements.
  lo_elements = lo_document->get_elements_by_tag_name( name = 'PARENT_ID' ).
  PERFORM checkandreplaceguid CHANGING lo_elements.
  lo_elements = lo_document->get_elements_by_tag_name( name = 'BROTHER_ID' ).
  PERFORM checkandreplaceguid CHANGING lo_elements.
  lo_elements = lo_document->get_elements_by_tag_name( name = 'COPY_NODE' ).
  PERFORM checkandreplaceguid CHANGING lo_elements.
  lo_elements = lo_document->get_elements_by_tag_name( name = 'COPNODFRST' ).
  PERFORM checkandreplaceguid CHANGING lo_elements.
  CALL FUNCTION 'SDIXML_DOM_TO_XML'
    EXPORTING
      document      = lo_document
      pretty_print  = 'X'
    IMPORTING
      xml_as_string = pc_data
    EXCEPTIONS
      no_document   = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    MESSAGE e298(00) WITH 'Xml document to string error' '' '' ''.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECKANDREPLACEGUŞD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LO_ELEMENTS
*&---------------------------------------------------------------------*
FORM checkandreplaceguid
                CHANGING po_elements TYPE REF TO if_ixml_node_collection.
  DATA: lo_iterator TYPE REF TO if_ixml_node_iterator,
        lo_node     TYPE REF TO if_ixml_node,
        lv_string   TYPE string,
        lv_guid     TYPE c LENGTH 32.

  CHECK po_elements IS NOT INITIAL.
  lo_iterator = po_elements->create_iterator( ).
  lo_node = lo_iterator->get_next( ).
  WHILE lo_node IS NOT INITIAL.
    IF lo_node->get_value( ) IS NOT INITIAL  AND
       strlen( lo_node->get_value( ) ) EQ 32 AND
       lo_node->get_value( ) CO '0123456789ABCDEF'.
      lv_guid = lo_node->get_value( ).
      CASE lv_guid+0(1).
        WHEN '0'. lv_guid+0(1) = '1'.
        WHEN '1'. lv_guid+0(1) = '2'.
        WHEN '2'. lv_guid+0(1) = '3'.
        WHEN '3'. lv_guid+0(1) = '4'.
        WHEN '4'. lv_guid+0(1) = '5'.
        WHEN '5'. lv_guid+0(1) = '6'.
        WHEN '6'. lv_guid+0(1) = '7'.
        WHEN '7'. lv_guid+0(1) = '8'.
        WHEN '8'. lv_guid+0(1) = '9'.
        WHEN '9'. lv_guid+0(1) = 'A'.
        WHEN 'A'. lv_guid+0(1) = 'B'.
        WHEN 'B'. lv_guid+0(1) = 'C'.
        WHEN 'C'. lv_guid+0(1) = 'D'.
        WHEN 'D'. lv_guid+0(1) = 'E'.
        WHEN 'E'. lv_guid+0(1) = 'F'.
        WHEN 'F'. lv_guid+0(1) = '0'.
      ENDCASE.
      lv_string = lv_guid.
      lo_node->set_value( lv_string ).
    ENDIF.
    lo_node = lo_iterator->get_next( ).
  ENDWHILE.
ENDFORM.
