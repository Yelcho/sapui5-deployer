*&---------------------------------------------------------------------*
*&  Include           /UI5/LUI5_REPOSITORY_LOADD01
*&---------------------------------------------------------------------*
************************************************************************
************************************************************************
*                                                                      *
*                Local Classes Definition                              *
*                                                                      *
************************************************************************
************************************************************************


************************************************************************
* Class LCL_FUNCTION
*
* provides some helper functions.
************************************************************************
CLASS lcl_function DEFINITION.
  PUBLIC SECTION.

    TYPES:          x255_table                   TYPE STANDARD TABLE OF raw255.
    CLASS-METHODS:  conv_unix_style_line_endings IMPORTING iv_string        TYPE string
                                                 RETURNING VALUE(rv_string) TYPE string,
      get_md5_hash_for_string      IMPORTING iv_string          TYPE string
                                   RETURNING VALUE(rv_md5_hash) TYPE md5_fields-hash,
      get_hash_for_xstring         IMPORTING iv_xstring     TYPE xstring
                                   RETURNING VALUE(rv_hash) TYPE string,
      max                          IMPORTING a             TYPE i DEFAULT -999999
                                             b             TYPE i DEFAULT -999999
                                             c             TYPE i DEFAULT -999999
                                             d             TYPE i DEFAULT -999999
                                             e             TYPE i DEFAULT -999999
                                             f             TYPE i DEFAULT -999999
                                   RETURNING VALUE(rv_max) TYPE i,
      set_return_msg               IMPORTING iv_success   TYPE char1
                                             iv_test_mode TYPE abap_bool
                                   CHANGING  cv_msg_id    LIKE sy-msgid
                                             cv_msg_no    LIKE sy-msgno,
      string_to_file               IMPORTING iv_file_content   TYPE string
                                             iv_code_page      TYPE string
                                             iv_file_path      TYPE string
                                   RETURNING VALUE(rv_success) TYPE abap_bool,
      text_matches_pattern         IMPORTING iv_text                        TYPE string
                                             iv_pattern_list                TYPE string_table
                                   RETURNING VALUE(rv_text_matches_pattern) TYPE abap_bool,
      text_to_abap_bool            IMPORTING iv_text             TYPE string
                                   RETURNING VALUE(rv_abap_bool) TYPE abap_bool,
      xstring_to_file              IMPORTING iv_file_content   TYPE xstring
                                             iv_file_path      TYPE string
                                   RETURNING VALUE(rv_success) TYPE abap_bool,
      xstring2xtable               IMPORTING iv_xstring TYPE xstring
                                   EXPORTING ev_xtable  TYPE x255_table
                                             ev_size    TYPE int4.

ENDCLASS.                    "lcl_function DEFINITION


************************************************************************
* Class LCL_OPERATION
*
* represents an upload operation to be performed.
************************************************************************
CLASS lcl_operation DEFINITION.

  PUBLIC SECTION.

    DATA: full_path      TYPE string,
          message        TYPE string,
          object_type    TYPE string,
          object_name    TYPE string,
          operation      TYPE string,
          relative_path  TYPE string,
          hash_value     TYPE md5_fields-hash,
          code_page      TYPE string,
          eol_conversion TYPE abap_bool,
          to_update      TYPE abap_bool.
    METHODS:       create_log_message          RETURNING VALUE(rv_message) TYPE string,
      get_file_extension          RETURNING VALUE(rv_file_extension) TYPE string,
      is_binary                   RETURNING VALUE(rv_is_binary) TYPE abap_bool.
    CLASS-DATA: create_folder           TYPE string VALUE 'CREATE FOLDER',
                delete_folder           TYPE string VALUE 'DELETE FOLDER',
                delete_file             TYPE string VALUE 'DELETE FILE',
                ignore_file             TYPE string VALUE 'IGNORE_FILE',
                ignore_folder           TYPE string VALUE 'IGNORE_FOLDER',
                upload_file             TYPE string VALUE 'UPLOAD FILE',
                object_type_none        TYPE string VALUE '',
                object_type_binary_file TYPE string VALUE 'BINARY_FILE',
                object_type_file        TYPE string VALUE 'FILE',
                object_type_text_file   TYPE string VALUE 'TEXT_FILE',
                object_type_folder      TYPE string VALUE 'FOLDER'.

ENDCLASS.                    "LCL_OPERATION

************************************************************************
* Class LCL_BADI
*
************************************************************************
CLASS lcl_badi DEFINITION.

  PUBLIC SECTION.
    DATA:
      implementation              TYPE REF TO /ui5/badi_prevent_deletion,
      never_delete_mime_resources TYPE abap_bool,
      application_name            TYPE string,
      workbench_request           TYPE trkorr,
      test_mode                   TYPE abap_bool,
      error_occurred              TYPE abap_bool,
      error_text                  TYPE string.

    METHODS:      constructor                    IMPORTING iv_test_mode               TYPE abap_bool
                                                           iv_sapui5_application_name TYPE string
                                                           iv_workbench_request       TYPE trkorr,
      disable_deletion_for           IMPORTING iv_operation     TYPE REF TO lcl_operation
                                     RETURNING VALUE(rv_result) TYPE abap_bool.

ENDCLASS.

************************************************************************
* Class LCL_SAPUI5_ZIP_ARCHIVE
*
* represents a zip file containing a SAPUI5 application
*
* Remarks:
* - Access to log messages is passed as reference to constructor:
*   1) Changing and table parameters are not allowed here;
*   2) Using a field symbol as an input parameter does not work either:
*      Write operation is not supported then.
*
************************************************************************
CLASS lcl_sapui5_zip_archive DEFINITION FINAL.

  PUBLIC SECTION.

    TYPES:       	 operations                     TYPE TABLE OF REF TO lcl_operation.
    METHODS:       constructor                    IMPORTING iv_url          TYPE string
                                                            iv_zip_file     TYPE xstring OPTIONAL
                                                            ir_log_messages TYPE REF TO string_table,
      determine_upload_operations    IMPORTING iv_delta_mode            TYPE abap_bool
                                               iv_accept_unix_style_eol TYPE abap_bool
                                               iv_code_page             TYPE string
                                     CHANGING  cv_load_operations       TYPE operations
                                               cv_success               TYPE char1,
      is_ok                          RETURNING VALUE(rv_is_ok) TYPE boolean,
      is_binary_file                 IMPORTING iv_file_path             TYPE string
                                     RETURNING VALUE(rv_is_binary_file) TYPE abap_bool,
      is_file_to_be_ignored          IMPORTING iv_file_path     TYPE string
                                     RETURNING VALUE(rv_ignore) TYPE abap_bool,
      is_text_file                   IMPORTING iv_file_path           TYPE string
                                     RETURNING VALUE(rv_is_text_file) TYPE abap_bool,
      read_file_to_table             IMPORTING iv_path         TYPE string
                                               iv_code_page    TYPE string
                                     RETURNING VALUE(rt_lines) TYPE string_table,
      read_file                      IMPORTING iv_path           TYPE string
                                     RETURNING VALUE(rv_content) TYPE xstring,
      get_upload_parameter           IMPORTING iv_key                     TYPE string
                                               iv_return_if_not_specified TYPE string DEFAULT ''
                                     RETURNING VALUE(rv_value)            TYPE string.
    DATA: binary_file_patterns TYPE string_table,
          text_file_patterns   TYPE string_table,
          upload_ignores       TYPE string_table,
          upload_parameters    TYPE slin_key_value_hash.

  PRIVATE SECTION.

    METHODS:       add_default_bin_file_patterns  CHANGING cv_file_patterns TYPE string_table,
      add_default_ignore_patterns    CHANGING cv_file_patterns TYPE string_table,
      add_default_text_file_patterns CHANGING cv_file_patterns TYPE string_table,
      add_import_upload_parameters   CHANGING cv_file_patterns TYPE slin_key_value_hash,
      create_folders_for_path        IMPORTING iv_path            TYPE string
                                     CHANGING  cv_folders_created TYPE string
                                               cv_load_operations TYPE operations,
      determine_binary_file_patterns IMPORTING iv_code_page TYPE string,
      determine_text_file_patterns   IMPORTING iv_code_page TYPE string,
      determine_upload_ignores       IMPORTING iv_code_page TYPE string,
      determine_upload_parameters    IMPORTING iv_code_page TYPE string.
    DATA: error_message     TYPE string,
          http_client       TYPE REF TO if_http_client,
          log_messages_ref  TYPE REF TO string_table,
          ok                TYPE boolean,
          upload_operations TYPE TABLE OF REF TO lcl_operation,
          url               TYPE string,
          zip_archive       TYPE REF TO cl_abap_zip.

ENDCLASS.                    "lcl_sapui5_zip_archive


************************************************************************
* Class LCL_EXCEPTION
*
* is the exception to handle errors in this report.
************************************************************************
CLASS lcx_exception DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.                    "lcx_exception DEFINITION


************************************************************************
* Class LCL_CANCELED
*
* indicates processing has been canceled by the user.
************************************************************************
CLASS lcx_canceled DEFINITION INHERITING FROM cx_dynamic_check.
ENDCLASS.                    "lcx_canceled DEFINITION



************************************************************************
* Class lcl_external_code_page
*
* supports conversion of external code page into abap code page.
************************************************************************
CLASS lcl_external_code_page DEFINITION.

  PUBLIC SECTION.

    TYPE-POOLS     abap.
    CLASS-DATA:    no_code_page                TYPE cpcodepage VALUE '0'.
    CLASS-METHODS: create                      IMPORTING iv_code_page_name            TYPE string
                                               RETURNING VALUE(rv_external_code_page) TYPE REF TO lcl_external_code_page
                                               RAISING   lcx_exception,
      for_sapgui_installation     RETURNING VALUE(rv_external_code_page) TYPE REF TO lcl_external_code_page
                                  RAISING   lcx_exception.
    METHODS:       get_abap_encoding           RETURNING VALUE(rv_abap_encoding) TYPE cpcodepage
                                               RAISING   lcx_exception,
      get_java_encoding           RETURNING VALUE(rv_java_encoding) TYPE string
                                  RAISING   lcx_exception.
    DATA: name TYPE string READ-ONLY,
          kind TYPE cpattrkind READ-ONLY.

ENDCLASS.                    "lcl_external_code_page DEFINITION


************************************************************************
* Class LCL_UI5_REPOSITORY
*
* represents a UI5 Repository and the UI5 application it contains.
************************************************************************
CLASS lcl_ui5_repository DEFINITION FINAL CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-DATA: message                    TYPE string,
                debug_conversions_for_file TYPE string VALUE ''.
    TYPES: BEGIN OF upload_parameters,
             description       TYPE string,
             package           TYPE devclass,
             transport_request TYPE trkorr,
             code_page_ui      TYPE string,
             code_page_abap    TYPE cpcodepage,
             code_page_java    TYPE string,
           END OF upload_parameters,
           upload_operations TYPE TABLE OF REF TO lcl_operation.
    DATA: already_exists                 TYPE abap_bool,
          api                            TYPE REF TO /ui5/if_ui5_rep_dt,
          binary_file_identification_msg TYPE string,
          binary_file_patterns           TYPE TABLE OF string,
          name                           TYPE string,
          text_file_identification_msg   TYPE string,
          text_file_patterns             TYPE TABLE OF string,
          ignores                        TYPE TABLE OF string,
          ignores_identification_msg     TYPE string.
    CLASS-METHODS: class_constructor,
      get_instance                   IMPORTING ir_log_messages TYPE REF TO string_table
                                     RETURNING VALUE(rv_self)  TYPE REF TO lcl_ui5_repository,
      evaluate_authorization         RETURNING  VALUE(rv_message) TYPE string.
    METHODS:       conciliate_upload_operations   IMPORTING iv_path              TYPE string DEFAULT ''
                                                            iv_delta_mode        TYPE abap_bool
                                                            iv_badi              TYPE REF TO lcl_badi
                                                            iv_archive           TYPE REF TO lcl_sapui5_zip_archive
                                                  CHANGING  cv_upload_operations TYPE upload_operations,
      get_mime_type_for_upload       IMPORTING iv_operation        TYPE REF TO lcl_operation
                                     RETURNING VALUE(rv_mime_type) TYPE string,
      set_name                       IMPORTING iv_name TYPE string.

  PRIVATE SECTION.
*    METHODS:
*                   is_binary_file                 IMPORTING iv_file_path TYPE string
*                                                  RETURNING value(rv_is_binary_file) TYPE abap_bool,
*                   is_file_to_be_ignored          IMPORTING iv_file_path TYPE string
*                                                  RETURNING value(rv_ignore) TYPE abap_bool,
*                   is_text_file                   IMPORTING iv_file_path TYPE string
*                                                  RETURNING value(rv_is_text_file) TYPE abap_bool.
    CLASS-DATA:    self                           TYPE REF TO lcl_ui5_repository.
    DATA:    log_messages_ref               TYPE REF TO string_table.

ENDCLASS.                    "lcl_ui5_repository DEFINITION
