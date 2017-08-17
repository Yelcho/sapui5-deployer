*&---------------------------------------------------------------------*
*&  Include           /UI5/LUI5_REPOSITORY_LOADP01
*&---------------------------------------------------------------------*
************************************************************************
************************************************************************
*                                                                      *
*                Local Classes Implementation                          *
*                                                                      *
************************************************************************
************************************************************************


************************************************************************
* Class LCL_FUNCTION
*
* provides some helper functions.
************************************************************************
CLASS lcl_function IMPLEMENTATION.

  METHOD max.
    DATA: max TYPE i.
    max = a.
    IF ( b > a ). max = b . ENDIF.
    IF ( c > max ). max = c . ENDIF.
    IF ( d > max ). max = d . ENDIF.
    IF ( e > max ). max = e . ENDIF.
    IF ( f > max ). max = f . ENDIF.
    rv_max = max.
  ENDMETHOD.                    "max

  METHOD set_return_msg.

    cv_msg_id = '/UI5/UI5_REP_LOAD'.

*   Test mode off
    IF iv_test_mode = abap_false.
      IF iv_success = status_success.
        cv_msg_no = '001'.
      ELSEIF iv_success = status_warning.
        cv_msg_no = '002'.
      ELSEIF iv_success = status_error.
        cv_msg_no = '003'.
      ENDIF.
    ENDIF.

*   Test mode on
    IF iv_test_mode = abap_true.
      IF iv_success = status_success.
        cv_msg_no = '004'.
      ELSEIF iv_success = status_warning.
        cv_msg_no = '005'.
      ELSEIF iv_success = status_error.
        cv_msg_no = '006'.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "set_return_msg

  METHOD text_matches_pattern.

*   Initialize
    rv_text_matches_pattern = abap_false.

*   Check if string matches a pattern of the list passed on entry
    DATA: pattern                  TYPE string,
          regular_expression_check TYPE abap_bool,
          match_count              TYPE i.
    LOOP AT iv_pattern_list INTO pattern.

*     Skip empty lines
      IF strlen( pattern ) = 0. CONTINUE. ENDIF.

*     Determine if pattern to be treated as a regular expression or a substring
      DATA: first_char       TYPE c, last_char TYPE c, last_char_offset TYPE i.
      last_char_offset = strlen( pattern ) - 1.
      first_char = pattern(1).
      last_char = pattern+last_char_offset(1).
      DATA: is_regular_expression TYPE abap_bool. is_regular_expression = abap_false.
      IF first_char = '^' AND last_char = '$'. is_regular_expression = abap_true. ENDIF.

*     Check
      IF is_regular_expression = abap_true.
        TRY.
            FIND REGEX pattern IN iv_text IGNORING CASE MATCH COUNT match_count.
          CATCH cx_root.
        ENDTRY.
      ELSE.
        FIND pattern IN iv_text IGNORING CASE MATCH COUNT match_count.
      ENDIF.
      IF match_count > 0. rv_text_matches_pattern = abap_true. RETURN. ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "text_matches_pattern

* Converts a text into an abap_bool expression
  METHOD text_to_abap_bool.

*   Initialize
    rv_abap_bool = abap_true.
    DATA: text TYPE string. text = iv_text.
    CONDENSE text. TRANSLATE text TO LOWER CASE.

*   ABAP_FALSE
    IF text IS INITIAL. rv_abap_bool = abap_false. RETURN. ENDIF.
    IF text = 'false'. rv_abap_bool = abap_false. RETURN. ENDIF. "#EC NOTEXT
    IF text = 'no'. rv_abap_bool = abap_false. RETURN. ENDIF. "#EC NOTEXT
    IF text = '0'. rv_abap_bool = abap_false. RETURN. ENDIF. "#EC NOTEXT

*   ABAP_TRUE
    IF text = 'x'. rv_abap_bool = abap_true. RETURN. ENDIF. "#EC NOTEXT
    IF text = 'true'. rv_abap_bool = abap_true. RETURN. ENDIF. "#EC NOTEXT
    IF text = 'yes'. rv_abap_bool = abap_false. RETURN. ENDIF. "#EC NOTEXT
    IF text = '1'. rv_abap_bool = abap_true. RETURN. ENDIF. "#EC NOTEXT

*   ABAP_UNDEFINED
    IF text = 'undefined'. rv_abap_bool = abap_undefined. RETURN. ENDIF. "#EC NOTEXT
    IF text = '-'. rv_abap_bool = abap_undefined. RETURN. ENDIF. "#EC NOTEXT
    IF text = '?'. rv_abap_bool = abap_undefined. RETURN. ENDIF. "#EC NOTEXT

  ENDMETHOD.                    "text_to_abap_bool

* Converts xstring into table of xstrings
  METHOD xstring2xtable.

*   Calculate size
    IF ev_size IS SUPPLIED.
      ev_size = xstrlen( iv_xstring ).
    ENDIF.

*   Split xstring into lines
    DATA: xline   TYPE x255,
          xstring TYPE xstring.
    FREE ev_xtable.
    xstring = iv_xstring.
    WHILE xstring IS NOT INITIAL.
      xline = xstring.
      APPEND xline TO ev_xtable.
      SHIFT xstring LEFT BY 255 PLACES IN BYTE MODE.
    ENDWHILE.

*   Clean up
    CLEAR xstring.

  ENDMETHOD.                    "xstring2table


* Converts unix style line endings (LF) to windows style (CR LF)
  METHOD conv_unix_style_line_endings.

    DATA: string             TYPE string,
          regular_expression TYPE string,
          replace_with       TYPE string.
    string = iv_string.
    regular_expression = '\r\n|\n\r|\n|\r'.
    replace_with = '\r\n'.
    REPLACE ALL OCCURRENCES OF REGEX regular_expression IN string WITH replace_with.

    rv_string = string.

  ENDMETHOD.                    "conv_unix_style_line_endings

* Outputs a sstring to a file
  METHOD string_to_file.

*   Initialize
    rv_success = abap_true.

*
    DATA: file_content_as_xstring TYPE xstring.
    TRY.
        file_content_as_xstring =
        /ui5/cl_ui5_rep_utility=>convert_string_2_xstring( iv_string = iv_file_content iv_code_page = iv_code_page ).
        xstring_to_file( iv_file_content = file_content_as_xstring iv_file_path = iv_file_path ).
      CATCH cx_root.
        rv_success = abap_false.
    ENDTRY.

  ENDMETHOD.                    "string_to_file

* Outputs an xstring to a file
  METHOD xstring_to_file.

*   Initialize
    rv_success = abap_true.

*   Prepare binary data for file download
*   ... Content of text files is passed in binary form as well
    DATA: file_size              TYPE int4,
          file_content_lines_bin TYPE lcl_function=>x255_table,
          file_length            TYPE int4.
    lcl_function=>xstring2xtable( EXPORTING iv_xstring = iv_file_content
                                  IMPORTING ev_size = file_size
                                            ev_xtable = file_content_lines_bin ).
*   Delegate
    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        bin_filesize            = file_size
        filename                = iv_file_path
        filetype                = 'BIN'
      IMPORTING
        filelength              = file_length        "virus_scan_profile = '/SCET/GUI_DOWNLOAD'
      CHANGING
        data_tab                = file_content_lines_bin
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        not_supported_by_gui    = 22
        error_no_gui            = 23
        OTHERS                  = 24.
    IF sy-subrc <> 0.
      rv_success = abap_false.
    ENDIF.

  ENDMETHOD.                    "xstring_to_file

* Calculates md5 hash value for string
* ... returns intial value in case of error
  METHOD get_md5_hash_for_string.

    DATA: string_length TYPE i. string_length = strlen( iv_string ).
    CALL FUNCTION 'MD5_CALCULATE_HASH_FOR_CHAR'
      EXPORTING
        data           = iv_string
        length         = string_length
      IMPORTING
        hash           = rv_md5_hash
      EXCEPTIONS
        no_data        = 1
        internal_error = 2
        OTHERS         = 3.
    IF sy-subrc > 0.
      CLEAR rv_md5_hash.
    ENDIF.

  ENDMETHOD.                    "get_md5_hash_for_string

* Calculates md5 hash value for string
* ... returns intial value in case of error
  METHOD get_hash_for_xstring.

    DATA: hash TYPE hash160.
    CALL FUNCTION 'CALCULATE_HASH_FOR_RAW'
      EXPORTING
*       ALG  = 'SHA1'
        data = iv_xstring
*       LENGTH               = 0
      IMPORTING
        hash = hash
*       HASHLEN              =
*       HASHX                = l_hashx
*       HASHXLEN             =
* EXCEPTIONS
*       UNKNOWN_ALG          = 1
*       PARAM_ERROR          = 2
*       INTERNAL_ERROR       = 3
*       OTHERS               = 4
      .

    rv_hash = hash.

    IF sy-subrc > 0.
      CLEAR rv_hash.
    ENDIF.

  ENDMETHOD.                    "get_md5_hash_for_xstring

ENDCLASS.                    "lcl_function IMPLEMENTATION


************************************************************************
* Class LCL_OPERATION
*
* represents an upload operation to be performed.
************************************************************************
CLASS lcl_operation IMPLEMENTATION.

*****************************************
  METHOD create_log_message.
*****************************************

*   Operation
    DATA: message        TYPE string,
          operation_(13) TYPE c.
    operation_ = me->operation.
    IF me->to_update = abap_false
       AND me->operation <> me->ignore_file
       AND me->operation <> me->ignore_folder.
      operation_ = 'NONE'.
    ENDIF.

    CONCATENATE operation_ ' : '  INTO message RESPECTING BLANKS.

*   Path
    IF me->operation = lcl_operation=>delete_folder
       OR me->operation = lcl_operation=>delete_file.
      CONCATENATE message me->relative_path INTO message.
    ELSE.
      CONCATENATE message me->full_path INTO message.
    ENDIF.

*   File type
    IF me->operation = lcl_operation=>upload_file.
      DATA: text_binary TYPE string.
      IF me->object_type = lcl_operation=>object_type_text_file.
        text_binary = TEXT-070.
        CONCATENATE message ' (' text_binary  ')' INTO message RESPECTING BLANKS.
      ELSEIF me->object_type = lcl_operation=>object_type_binary_file.
        text_binary = TEXT-071.
        CONCATENATE message ' (' text_binary  ')' INTO message RESPECTING BLANKS.
      ENDIF.
    ENDIF.
    IF me->message IS NOT INITIAL.
      CONCATENATE message ' (' me->message  ')' INTO message RESPECTING BLANKS.
    ENDIF.

    rv_message = message.

  ENDMETHOD.                    "create_log_message

*****************************************
  METHOD get_file_extension.
*****************************************

*   Initialize
    CLEAR rv_file_extension.

*   Return empty string if object behind is a folder
    IF object_type = lcl_operation=>object_type_folder. RETURN. ENDIF.

*   Split path
    DATA: parts     TYPE TABLE OF string,
          part      TYPE string,
          i         TYPE int4,
          extension TYPE string.
    SPLIT me->relative_path AT '/' INTO TABLE parts.
    i = lines( parts ).
    READ TABLE parts INTO part INDEX i.
*   Get file extension
    SPLIT part AT '.' INTO TABLE parts.
    i = lines( parts ).
    IF i > 1.
      READ TABLE parts INTO extension INDEX i.
      TRANSLATE extension TO UPPER CASE.                 "#EC TRANSLANG
    ENDIF.
*   Return
    IF extension IS INITIAL. RETURN. ENDIF.
    CONCATENATE '.' extension INTO extension.
    rv_file_extension = extension.

  ENDMETHOD.                    "get_file_extension

*****************************************
  METHOD is_binary.
*****************************************

*   Check file type
    rv_is_binary = abap_undefined.
    IF me->object_type = lcl_operation=>object_type_binary_file.
      rv_is_binary = abap_true.
    ELSEIF me->object_type = lcl_operation=>object_type_text_file.
      rv_is_binary = abap_false.
    ENDIF.

  ENDMETHOD.                    "is_binary

ENDCLASS.                    "lcl_operation

************************************************************************
* Class LCL_BADI
*
* represents the Business Add-In /UI5/BADI_PREVENT_DELETION
************************************************************************
CLASS lcl_badi IMPLEMENTATION.

*****************************************
  METHOD constructor.
*****************************************

*   Access business add-in
    TRY.

*       Retrieve BAdI implementation
        GET BADI me->implementation.
        IF me->implementation IS NOT INITIAL.
          IF cl_badi_query=>number_of_implementations( me->implementation ) = 0.
            CLEAR me->implementation.
          ENDIF.
        ENDIF.

*       Check if the deletion of any mime object is to be ommited
        me->never_delete_mime_resources = abap_false.
        IF me->implementation IS NOT INITIAL.
          CALL BADI me->implementation->disable_deletion_for_all
            EXPORTING
              iv_caller           = '/UI5/UI5_REPOSITORY_LOAD_HTTP'
            RECEIVING
              rv_prevent_deletion = me->never_delete_mime_resources.
        ENDIF.

*       Remember
        me->application_name = iv_sapui5_application_name.
        me->workbench_request = iv_workbench_request.
        me->test_mode = iv_test_mode.

      CATCH cx_badi_not_implemented.

        CLEAR me->implementation.

    ENDTRY.

  ENDMETHOD.                    "constructor

*****************************************
  METHOD disable_deletion_for.
*****************************************

*   Initialize
    rv_result = abap_false.

*   Return if no operation given
*       or if there's no BAdI
    IF iv_operation IS INITIAL
       OR me->implementation IS INITIAL.
      EXIT.
    ENDIF.

*   Return if text file or no delete operation
*   ... Note: The BAdI may hinder deletion if the file type is unknown
    IF iv_operation->object_type = lcl_operation=>object_type_text_file
       OR iv_operation->operation <> lcl_operation=>delete_file.
      EXIT.
    ENDIF.

*   Disable this deletion in case BAdI disables all
    IF me->never_delete_mime_resources = abap_true.
      rv_result = abap_true.
      EXIT.
    ENDIF.

*   Build path expression
    DATA: path TYPE string.
    CONCATENATE me->application_name '/' iv_operation->full_path INTO path.

*   Delegate to BAdI
    DATA: _error_occured TYPE abap_bool,
          _error_text    TYPE string.
    _error_occured = abap_false. _error_text = ''.
    CALL BADI me->implementation->disable_deletion_for_one
      EXPORTING
        iv_url              = path
        iv_rename           = abap_undefined
        iv_request          = me->workbench_request
        iv_test             = me->test_mode
        iv_caller           = '/UI5/CL_UI5_UPLOAD_TO_MIME'
      IMPORTING
        ev_prevent_deletion = rv_result
        ev_error            = _error_occured
        ev_text             = _error_text.

*   Remember last error
    IF _error_occured = abap_true.
      me->error_occurred = abap_true.
      me->error_text = _error_text.
    ENDIF.

  ENDMETHOD.                    "disable_deletion_for

ENDCLASS.                    "lcl_operation IMPLEMENTATION


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
CLASS lcl_sapui5_zip_archive IMPLEMENTATION.

*****************************************
  METHOD constructor.
*****************************************

*   Initialize
    me->ok = abap_true.
    CLEAR me->error_message.

*   Remember
    me->url = iv_url.
    me->log_messages_ref = ir_log_messages.

    IF iv_zip_file IS NOT SUPPLIED.
*   Access zip file via http

*   ... Confirm Url
      cl_http_client=>create_by_url( EXPORTING  url = me->url
                                     IMPORTING  client = me->http_client
                                     EXCEPTIONS argument_not_found = 1
                                        plugin_not_active  = 2
                                        internal_error     = 3
                                        OTHERS             = 4  ).
      IF sy-subrc <> 0.
        me->ok = abap_false. me->error_message = TEXT-011.
        REPLACE '%' IN me->error_message WITH me->url.
        APPEND me->error_message TO log_messages_ref->*.
        EXIT.
      ELSE.
        DATA: message TYPE string.
        message = TEXT-012. APPEND message TO me->log_messages_ref->*.
        CONCATENATE '. ' iv_url INTO message RESPECTING BLANKS.
        APPEND message TO log_messages_ref->*.
      ENDIF.

*  ... Grep binary content of archive
      DATA: return_code TYPE sysubrc.
      http_client->send( EXCEPTIONS OTHERS = 1 ).
      http_client->receive( EXCEPTIONS OTHERS = 1 ).

      http_client->get_last_error(
        IMPORTING
          code = return_code
          message = message
      ).
      IF return_code IS NOT INITIAL.
        me->ok = abap_false. me->error_message = TEXT-011.
        REPLACE '%' IN me->error_message WITH me->url.
        APPEND me->error_message TO me->log_messages_ref->*.
        EXIT.
      ENDIF.

      DATA: xstring TYPE xstring.
      xstring = http_client->response->get_data( ).

    ELSE.
      xstring = iv_zip_file.
    ENDIF.

*   ... Initialize zip access
    CREATE OBJECT zip_archive.
    zip_archive->load( EXPORTING zip = xstring
                    EXCEPTIONS OTHERS = 1 ).
*       Indicate if archive is empty or http status is not 200
    IF sy-subrc IS NOT INITIAL OR zip_archive->files[] IS INITIAL.

      me->ok = abap_false. me->error_message = TEXT-013.
      APPEND me->error_message TO me->log_messages_ref->*.

      DATA: http_status_code_response  TYPE i, http_status_code_response_ TYPE string, http_status_text_response TYPE string.
      http_client->response->get_status( IMPORTING code = http_status_code_response reason = http_status_text_response ).
      IF ( http_status_code_response <> 200 ).
        me->ok = abap_false. me->error_message = TEXT-109.
        http_status_code_response_ = http_status_code_response. CONDENSE http_status_code_response_ NO-GAPS.
        REPLACE '%1' IN me->error_message WITH http_status_code_response_.
        REPLACE '%2' IN me->error_message WITH http_status_text_response.
        APPEND me->error_message TO me->log_messages_ref->*.
      ENDIF.

      EXIT.

    ELSE.

*     Calculate number of files in archive
      " describe table zip_archive->files[] lines number -> Does not work as files[] is inconsistent
      DATA: count           TYPE i, number_of_files TYPE string, file LIKE LINE OF zip_archive->files[].
      CLEAR count.
      LOOP AT zip_archive->files[] INTO file.
        DATA: last_char_offset TYPE i, last_char TYPE c.
        last_char_offset = strlen( file-name ) - 1.
        last_char = file-name+last_char_offset(1).
        IF last_char NE '/'. count = count + 1. ENDIF.
      ENDLOOP.
      IF count = 0.
        me->ok = abap_false. me->error_message = TEXT-013.
        APPEND me->error_message TO me->log_messages_ref->*.
        EXIT.
      ENDIF.

*     Indicate
      number_of_files = count. CONDENSE number_of_files NO-GAPS.
      message = TEXT-014. ". % Files and Folders found in Archive.
      REPLACE '%' IN message WITH number_of_files.
      APPEND message TO log_messages_ref->*.
      APPEND '.' TO log_messages_ref->*. "Empty line

    ENDIF.

*   Header for Parameter section in log
    APPEND TEXT-019 TO log_messages_ref->*. "* Parameters *


*   Determine text and binary file patterns
*   ... encoding is expected to be UTF-8 compatible
    DATA: code_page_utf8 TYPE string VALUE 'UTF-8'.
    me->determine_text_file_patterns( code_page_utf8  ).
    me->determine_binary_file_patterns( code_page_utf8  ).
    me->determine_upload_ignores( code_page_utf8  ).

*   Determine upload parameters from file
*   ... encoding is expected to be UTF-8 compatible
    me->determine_upload_parameters( code_page_utf8 ).

  ENDMETHOD.                    "constructor

*****************************************
  METHOD is_ok.
*****************************************
    rv_is_ok = me->ok.
  ENDMETHOD.                    "is_ok


*****************************************
  METHOD determine_upload_operations.
*****************************************
*
* This method determines the operations needed to upload
* the archive content into a the SAPUI5 ABAP repository
* not yet containing the SAPUI5 application.

*   Initialize
    CLEAR cv_load_operations.

*   Access files in archive
    TYPES: BEGIN OF t_file,
             name TYPE string,
             date TYPE d,
             time TYPE t,
             size TYPE i,
           END OF t_file .
    TYPES: t_files TYPE TABLE OF t_file .
    DATA:  files TYPE t_files.
    files = me->zip_archive->files[].
    SORT files BY name AS TEXT.

*   For each file or folder in archive
    DATA: load_operation  TYPE REF TO lcl_operation,
          archive_entry   LIKE LINE OF me->zip_archive->files[],
          message         TYPE string,
          folders_created TYPE string.
    LOOP AT files INTO archive_entry.

*     Create load operation
      CREATE OBJECT load_operation.

      load_operation->full_path = archive_entry-name.
      load_operation->relative_path = load_operation->full_path.

      "if load_operation->full_path cs '.meta/i18n/'. break wegmann. endif.

*     Determine name of file or folder
      DATA: path_components TYPE string_table.
      SPLIT load_operation->full_path AT '/' INTO TABLE path_components.
      DATA: max_index TYPE i.
      DESCRIBE TABLE path_components LINES max_index.
      READ TABLE path_components INDEX max_index INTO load_operation->object_name.

*     Folder is to be created
      FIND REGEX '^.*/$' IN archive_entry-name.
      IF sy-subrc = 0.

*       Initialize
        load_operation->to_update = abap_true.

*       Set operation and object type
        load_operation->operation = lcl_operation=>create_folder.
        load_operation->object_type = lcl_operation=>object_type_folder.

*       Decide if folder is to be ignored
        DATA: is_folder_to_be_ignored TYPE abap_bool.
        is_folder_to_be_ignored = me->is_file_to_be_ignored( load_operation->full_path ).
        IF is_folder_to_be_ignored = abap_true.
          load_operation->operation = lcl_operation=>ignore_folder.
        ENDIF.

*       Build relative path
        DATA: first_char       TYPE c, last_char TYPE c, last_char_offset TYPE i.
        last_char_offset = strlen( load_operation->relative_path ) - 1.
        last_char = load_operation->relative_path+last_char_offset(1).
        IF last_char EQ '/'.
          REPLACE SECTION OFFSET last_char_offset OF load_operation->relative_path WITH ''.
        ENDIF.

*       Register folder
        CONCATENATE folders_created load_operation->full_path '; ' INTO folders_created RESPECTING BLANKS.

*     File is to be uploaded
      ELSE.

*       Initialize
        load_operation->to_update = abap_true.

*       Trigger operations to create folders for file
*       ... as me->zip_archive->files[] appears to be incomplete
        DATA: path TYPE string.
        path = load_operation->full_path.
        REPLACE load_operation->object_name IN path WITH ''.
        create_folders_for_path( EXPORTING iv_path = path
                                 CHANGING  cv_folders_created = folders_created
                                           cv_load_operations = cv_load_operations ).

*       Set operation
        load_operation->operation = lcl_operation=>upload_file.

*       Check if entry is to be ignored
        DATA: is_file_to_be_ignored TYPE abap_bool.
        is_file_to_be_ignored = me->is_file_to_be_ignored( load_operation->full_path ).
        IF is_file_to_be_ignored = abap_true.
          load_operation->operation = lcl_operation=>ignore_file.
        ENDIF.

*       Check if binary or text file
        load_operation->object_type = lcl_operation=>object_type_file.
        IF me->is_text_file( load_operation->full_path ) = abap_true.
          load_operation->object_type = lcl_operation=>object_type_text_file.
          load_operation->code_page = iv_code_page.
        ENDIF.
        IF me->is_binary_file( load_operation->full_path ) = abap_true.
          load_operation->object_type = lcl_operation=>object_type_binary_file.
          CLEAR load_operation->code_page.
        ENDIF.

      ENDIF.

*     ... Try to determine from mime type if not clear from previous check
      IF load_operation->object_type = lcl_operation=>object_type_file.
        DATA: extension TYPE string,
              ext       TYPE char20,
              mimetype  TYPE mimetypes-type.
        extension = load_operation->get_file_extension( ). ext = extension.
        CALL FUNCTION 'SDOK_MIMETYPE_GET'
          EXPORTING
            extension = ext
          IMPORTING
            mimetype  = mimetype.
        IF mimetype = 'image'.
          load_operation->object_type = lcl_operation=>object_type_binary_file.
        ELSE.
          load_operation->operation = lcl_operation=>ignore_file.
          load_operation->message = TEXT-072. "File type unknown
          cv_success = status_warning.
        ENDIF.
      ENDIF.

*     Read file and for text files check if end of line line markers are CR+LF (Windows)
      IF load_operation->object_type = lcl_operation=>object_type_text_file
         OR load_operation->object_type = lcl_operation=>object_type_binary_file.

        TRY.
            DATA: file_content           TYPE xstring,
                  file_content_as_string TYPE string.
            file_content = me->read_file( iv_path = archive_entry-name ).
            file_content_as_string =
               /ui5/cl_ui5_rep_utility=>convert_xstring_2_string( iv_xstring = file_content
                                                                  iv_code_page = iv_code_page ).
            load_operation->eol_conversion = abap_false.

            DATA: eol_conversion_needed TYPE abap_bool. eol_conversion_needed = abap_false.
            IF load_operation->object_type = lcl_operation=>object_type_text_file.
              DATA: file_content_as_string_windows TYPE string.
              file_content_as_string_windows =
                 lcl_function=>conv_unix_style_line_endings( iv_string = file_content_as_string ).

***** Debug conversion of Unix style files *****
              IF lcl_ui5_repository=>debug_conversions_for_file IS NOT INITIAL
                 AND load_operation->object_name EQ lcl_ui5_repository=>debug_conversions_for_file.

                DATA: file_path TYPE string.
                CONCATENATE lcl_ui5_repository=>debug_conversions_for_file
                            '.upload' INTO file_path.       "#EC NOTEXT
                lcl_function=>xstring_to_file( iv_file_content = file_content
                                               iv_file_path = file_path ).
                CONCATENATE lcl_ui5_repository=>debug_conversions_for_file
                            '.upload.as_string' INTO file_path. "#EC NOTEXT
                lcl_function=>string_to_file( iv_file_content = file_content_as_string
                                              iv_code_page = iv_code_page
                                              iv_file_path = file_path ).
                CONCATENATE lcl_ui5_repository=>debug_conversions_for_file
                            '.upload.as_string_windows' INTO file_path. "#EC NOTEXT
                lcl_function=>string_to_file( iv_file_content = file_content_as_string_windows
                                              iv_code_page = iv_code_page
                                              iv_file_path = file_path ).
              ENDIF.
************************************************

              IF file_content_as_string = file_content_as_string_windows.
                eol_conversion_needed = abap_false.
                load_operation->eol_conversion = abap_false.
              ELSE.
                eol_conversion_needed = abap_true.
                IF iv_accept_unix_style_eol = abap_true.
                  load_operation->eol_conversion = abap_true.
                ELSE.
                  "message = text-056. "* Warning: Text file % has unix style line delimeter. It is not uploaded *
                  "replace '%' in message with archive_entry-name.
                  "append message to me->log_messages_ref->*.
                  load_operation->operation = lcl_operation=>ignore_file.
                  load_operation->message = TEXT-076. "Warning: Unix style text file
                  load_operation->to_update = abap_false.
                  load_operation->eol_conversion = abap_false.
                  cv_success = status_warning.
                ENDIF.
              ENDIF.
            ENDIF.

*     Calculate md5 hash for file to compare with that in SAPUI5 ABAP repository
*     ... Needed only in delta mode
            IF iv_delta_mode EQ abap_true.
              TRY.
                  IF load_operation->eol_conversion = abap_true.
                    load_operation->hash_value = lcl_function=>get_md5_hash_for_string( file_content_as_string_windows ).
                  ELSE.
                    load_operation->hash_value = lcl_function=>get_md5_hash_for_string( file_content_as_string ).
                  ENDIF.
                  load_operation->code_page = iv_code_page.
                CATCH /ui5/cx_ui5_rep_dt.
                  message = TEXT-051. "* Warning: Unable to determine MD5 hash for file % *
                  REPLACE '%' IN message WITH load_operation->full_path .
                  APPEND message TO me->log_messages_ref->*.
                  cv_success = status_warning.
              ENDTRY.
            ENDIF.

          CATCH /ui5/cx_ui5_rep_dt.
            message = TEXT-050. "* Warning:  Content of file % could not be read *
            REPLACE '%' IN message WITH load_operation->full_path .
            APPEND message TO me->log_messages_ref->*.
            cv_success = status_warning.

        ENDTRY.

      ENDIF.


*     Register operation
      APPEND load_operation TO cv_load_operations.

    ENDLOOP.

  ENDMETHOD.                    "determine_upload_operations

*****************************************
  METHOD read_file.
*****************************************

*   Return if no path given
    IF iv_path IS INITIAL. RETURN. ENDIF.

*   Find file in archive
    DATA: index TYPE i.
    READ TABLE me->zip_archive->files TRANSPORTING NO FIELDS
                                      WITH KEY name = iv_path.
    IF sy-subrc <> 0. RETURN. ENDIF.
    index = sy-tabix.

    me->zip_archive->get( EXPORTING index = index IMPORTING content = rv_content ).

  ENDMETHOD.                   "read_file

*****************************************
  METHOD read_file_to_table.
*****************************************
*   Read as binary and convert to text
    DATA: file_content           TYPE xstring,
          file_content_as_string TYPE string,
          lines                  TYPE string_table.
    TRY.
        file_content = me->read_file( iv_path = iv_path ).
        file_content_as_string =
           /ui5/cl_ui5_rep_utility=>convert_xstring_2_string( iv_xstring = file_content
                                                              iv_code_page = iv_code_page ).
        file_content_as_string =
          lcl_function=>conv_unix_style_line_endings( iv_string = file_content_as_string ).
        lines = /ui5/cl_ui5_rep_utility=>code_string_2_code_tab( file_content_as_string ).
      CATCH /ui5/cx_ui5_rep_dt.
    ENDTRY.

*   Prepare response
    rt_lines = lines.

  ENDMETHOD.                   "read_file_to_table


*****************************************
  METHOD get_upload_parameter.
*****************************************

*   Return if no parameter given
    IF iv_key IS INITIAL. RETURN. ENDIF.

*   Find paramenter
    DATA: index            TYPE i,
          upload_parameter TYPE slin_key_value.

    READ TABLE me->upload_parameters INTO upload_parameter
                                     WITH KEY key = iv_key.
    IF sy-subrc <> 0.
      rv_value = iv_return_if_not_specified. RETURN.
    ENDIF.
    rv_value = upload_parameter-value.

  ENDMETHOD.                   "get_upload_paramenter


*****************************************
  METHOD add_default_bin_file_patterns.
*****************************************

*   ... Add default binary patterns
    DATA: binary_file_pattern TYPE string.
    binary_file_pattern = '.zip'.                     APPEND binary_file_pattern TO cv_file_patterns.
    binary_file_pattern = '.war'.                     APPEND binary_file_pattern TO cv_file_patterns.
    binary_file_pattern = '.xpi'.                     APPEND binary_file_pattern TO cv_file_patterns.
    binary_file_pattern = '.jpg'.                     APPEND binary_file_pattern TO cv_file_patterns.
    binary_file_pattern = '.gif'.                     APPEND binary_file_pattern TO cv_file_patterns.
    binary_file_pattern = '.png'.                     APPEND binary_file_pattern TO cv_file_patterns.
    binary_file_pattern = '.ico'.                     APPEND binary_file_pattern TO cv_file_patterns.
    binary_file_pattern = '^.*\.class$'.              APPEND binary_file_pattern TO cv_file_patterns.

  ENDMETHOD.                    "add_default_bin_file_patterns


*****************************************
  METHOD add_default_ignore_patterns.
*****************************************

*   ... Add default binary patterns
    DATA: ignore_file_pattern TYPE string.
    ignore_file_pattern = '.git'.                     APPEND ignore_file_pattern TO cv_file_patterns.
    ignore_file_pattern = '^.*[/|\\]build([/|\\].*)?$'.
    APPEND ignore_file_pattern TO cv_file_patterns.

  ENDMETHOD.                    "add_default_ignore_patterns


*****************************************
  METHOD add_import_upload_parameters.
*****************************************
* TODO:check if needed or for better solution
*   ... Add import parameter
    DATA: import_parameter TYPE slin_key_value.

    "    import_parameter-key    = 'SAPUI5ApplicationName'.
    "    import_parameter-value  = IV_SAPUI5_REPOSITORY.
    "    append import_parameter to cv_file_patterns.

    "    import_parameter-key     = 'TransportRequest'.
    "    import_parameter-value   = IV_WORKBENCH_REQUEST.
    "    append import_parameter to cv_file_patterns.

  ENDMETHOD.                    "add_import_upload_parameters


*****************************************
  METHOD add_default_text_file_patterns.
*****************************************

*   Add default text file patterns
    DATA: text_file_pattern TYPE string.
    text_file_pattern = '.txt'.                       APPEND text_file_pattern TO cv_file_patterns.
    text_file_pattern = '.html'.                      APPEND text_file_pattern TO cv_file_patterns.
    text_file_pattern = '.js'.                        APPEND text_file_pattern TO cv_file_patterns.
    text_file_pattern = '.json'.                      APPEND text_file_pattern TO cv_file_patterns.
    text_file_pattern = '.less'.                      APPEND text_file_pattern TO cv_file_patterns.
    text_file_pattern = '.css'.                       APPEND text_file_pattern TO cv_file_patterns.
    text_file_pattern = '.htm'.                       APPEND text_file_pattern TO cv_file_patterns.
    text_file_pattern = '.xml'.                       APPEND text_file_pattern TO cv_file_patterns.
    text_file_pattern = 'manifest.mf'.                APPEND text_file_pattern TO cv_file_patterns.
    text_file_pattern = '.classpath'.                 APPEND text_file_pattern TO cv_file_patterns.
    text_file_pattern = '.properties'.                APPEND text_file_pattern TO cv_file_patterns.
    text_file_pattern = '.project'.                   APPEND text_file_pattern TO cv_file_patterns.
    text_file_pattern = '.settings/'.                 APPEND text_file_pattern TO cv_file_patterns.
    text_file_pattern = '^.*\.control$'.              APPEND text_file_pattern TO cv_file_patterns.
    text_file_pattern = '^.*\.json$'.              APPEND text_file_pattern TO cv_file_patterns.
    text_file_pattern = '^.*\.less$'.              APPEND text_file_pattern TO cv_file_patterns.
    text_file_pattern = '^.*\.library$'.              APPEND text_file_pattern TO cv_file_patterns.
    text_file_pattern = '^.*\.theming$'.              APPEND text_file_pattern TO cv_file_patterns.
    text_file_pattern = '.Ui5RepositoryAppSetup'.     APPEND text_file_pattern TO cv_file_patterns.
    text_file_pattern = '.Ui5RepositoryIgnore'.       APPEND text_file_pattern TO cv_file_patterns.
    text_file_pattern = '.Ui5RepositoryBinaryFiles'.  APPEND text_file_pattern TO cv_file_patterns.
    text_file_pattern = '.Ui5RepositoryTextFiles'.    APPEND text_file_pattern TO cv_file_patterns.
    text_file_pattern = '.Ui5RepositoryUploadParameters'. APPEND text_file_pattern TO cv_file_patterns.

  ENDMETHOD.                    "add_default_text_file_patterns

*****************************************
  METHOD create_folders_for_path.
*****************************************

*   Adds operations to create folders for a path given as input
*   once

*   Check inputs
    IF iv_path IS INITIAL. RETURN. ENDIF.

*   Return if already done
    DATA: folder TYPE string.
    CONCATENATE iv_path '; ' INTO folder RESPECTING BLANKS.
    IF cv_folders_created CS folder. RETURN. ENDIF.

*   Parse path expression
    DATA: path_components TYPE string_table,
          max_index       TYPE i.
    SPLIT iv_path AT '/' INTO TABLE path_components.
    DESCRIBE TABLE path_components LINES max_index.

*   Create parent folder
    IF max_index > 1.
      DATA: parent_folder  TYPE string,
            path_component TYPE string.
      CLEAR parent_folder.
      LOOP AT path_components INTO path_component.
        IF sy-tabix = max_index. CLEAR path_component. ENDIF.
        IF path_component IS NOT INITIAL.
          CONCATENATE parent_folder path_component '/' INTO parent_folder RESPECTING BLANKS.
        ENDIF.
      ENDLOOP.
      create_folders_for_path( EXPORTING iv_path = parent_folder
                           CHANGING  cv_folders_created = cv_folders_created
                                     cv_load_operations = cv_load_operations ).
    ENDIF.

*   Prepare operation to create folder
    DATA: load_operation_ TYPE REF TO lcl_operation.
    CREATE OBJECT load_operation_.

*   ... Determine name of folder
    READ TABLE path_components INDEX max_index INTO load_operation_->object_name.
    load_operation_->full_path = iv_path.
    load_operation_->relative_path = iv_path.
    load_operation_->to_update = abap_true.

*   ... Set operation and object type
    load_operation_->operation = lcl_operation=>create_folder.
    load_operation_->object_type = lcl_operation=>object_type_folder.

*   ... Decide if folder is to be ignored
    DATA: is_folder_to_be_ignored TYPE abap_bool.
    is_folder_to_be_ignored = me->is_file_to_be_ignored( load_operation_->full_path ).
    IF is_folder_to_be_ignored = abap_true.
      load_operation_->operation = lcl_operation=>ignore_folder.
    ENDIF.

*   ...  Build relative path
    DATA: last_char_offset TYPE i,
          last_char        TYPE c.
    last_char_offset = strlen( load_operation_->relative_path ) - 1.
    last_char = load_operation_->relative_path+last_char_offset(1).
    IF last_char EQ '/'.
      REPLACE SECTION OFFSET last_char_offset OF load_operation_->relative_path WITH ''.
    ENDIF.

*   ... Register operation if needed
    CONCATENATE load_operation_->full_path '; ' INTO folder RESPECTING BLANKS.
    IF cv_folders_created NS folder.
      APPEND load_operation_ TO cv_load_operations.
      CONCATENATE cv_folders_created folder INTO cv_folders_created RESPECTING BLANKS.
    ENDIF.

  ENDMETHOD.                    "create_folder_operations

*****************************************
  METHOD determine_binary_file_patterns.
*****************************************

*   Build list of patterns identifying binary files

*   ... Standard text file patterns
    me->add_default_bin_file_patterns( CHANGING cv_file_patterns = me->binary_file_patterns ).

*   ... Patterns from '.Ui5RepositoryBinaryFiles' file
    DATA: line  TYPE string,
          lines TYPE string_table.
    lines = me->read_file_to_table( iv_path = '.Ui5RepositoryBinaryFiles' iv_code_page = iv_code_page ).
    LOOP AT lines INTO line.
      APPEND line TO me->binary_file_patterns.
    ENDLOOP.

*   ... Message
    IF lines IS INITIAL.
      APPEND TEXT-022 TO me->log_messages_ref->*. "* Binary files have been identified from standard settings *
    ELSE.
      APPEND TEXT-023 TO me->log_messages_ref->*. "* File '.Ui5RepositoryBinaryFiles' has been considered to identify binary files *
    ENDIF.

  ENDMETHOD.                    "determine_binary_file_patterns


*****************************************
  METHOD determine_text_file_patterns.
*****************************************

*   Build list of patterns identifying text files

*   ... Standard text file patterns
    me->add_default_text_file_patterns( CHANGING cv_file_patterns = me->text_file_patterns ).

*   ... Patterns from '.Ui5RepositoryTextFiles' file
    DATA: line  TYPE string,
          lines TYPE string_table.
    lines = me->read_file_to_table( iv_path = '.Ui5RepositoryTextFiles' iv_code_page = iv_code_page ).
    LOOP AT lines INTO line.
      APPEND line TO me->text_file_patterns.
    ENDLOOP.

*   ... Message
    IF lines IS INITIAL.
      APPEND TEXT-020 TO me->log_messages_ref->*.
    ELSE.
      APPEND TEXT-021 TO me->log_messages_ref->*.
    ENDIF.

  ENDMETHOD.                    "determine_text_file_patterns

*****************************************
  METHOD determine_upload_ignores.
*****************************************

* Determines patterns for files to be ignored during the upload of a UI5 application
*
* In case a .Ui5RepositoryIgnore file is found in the directory of the UI5 application
* its content is used to decide if a file is considered in the upload operation.
*
* Example content:
*
* .c#
* ^.*\.ttf$
* ^.*[/|\\]build([/|\\].*)?$
*
* In this case all files which contain the sub expression ".c#" in their full path
* string are ignored by the upload operation. Furthermore files with the extension
* ".ttf" are ignored as well. In this example "^.*\.ttf$" is detected as a regular
* expression because it starts with a "^" and ends with a "$".
* Finally the build directory and everything below gets ignored.
*
* If a .Ui5RepIgnore file is not found a standard list of ignore patterns is used:
* So there's no need for a user to think about this topic in a simple case.
*
* Remark: In Eclipse the ignore files are specified by a team provider setting.

*   Build list of files to ignore during upload

*   ... Either patterns from '.Ui5RepositoryIgnore' file
    me->upload_ignores = me->read_file_to_table( iv_path = '.Ui5RepositoryIgnore' iv_code_page = iv_code_page ).

*   ... Or standard ignore file patterns
    IF me->upload_ignores IS INITIAL.
      me->add_default_ignore_patterns( CHANGING cv_file_patterns = me->upload_ignores ).
      APPEND TEXT-024 TO me->log_messages_ref->*.
    ELSE.
      APPEND TEXT-025 TO me->log_messages_ref->*.
    ENDIF.

  ENDMETHOD.                    "determine_upload_ignores

*****************************************
  METHOD determine_upload_parameters.
*****************************************

* Determines file with parameters to the upload of a UI5 application in ABAP Repository
*
* In case a .Ui5RepositoryUploadParameters file is found in the directory of the UI5 application
* its content is used to decide which settings are used in the upload operation.
*
* Parameters:
*
* SAPUI5ApplicationName         Mandatory
* SAPUI5RepositoryDescription   Mandatory: Used for creation of SAPUI5 ABAP Repository, otherwise is an info
* SAPUI5RepositoryPackage       Mandatory
* TransportRequest              Mandatory: ABAP-Transportrequest
* ExternalCodePage              Mandatory: Codepage for the files
*

* If a .Ui5RepositoryUploadParameters file is not found an import parameter of this Function Module is used.

*   Build list of of upload paramenter

*   ... Upload parameters from '.Ui5RepositoryUploadParameters' file
    DATA: line             TYPE string,
          lines            TYPE string_table,
          upload_parameter TYPE slin_key_value.

    lines = me->read_file_to_table( iv_path = '.Ui5RepositoryUploadParameters' iv_code_page = iv_code_page ).
    LOOP AT lines INTO line.
      SPLIT line AT '=' INTO upload_parameter-key upload_parameter-value.
      INSERT upload_parameter INTO TABLE me->upload_parameters.
    ENDLOOP.

*   ... Or from import parameters
    IF me->upload_parameters IS INITIAL.
      "      APPEND text-027 TO me->log_messages_ref->*.
    ELSE.
      "      APPEND text-026 TO me->log_messages_ref->*.
    ENDIF.

  ENDMETHOD.                    "determine_upload_parameters

  METHOD is_binary_file.

    rv_is_binary_file = abap_undefined.
    IF ( lcl_function=>text_matches_pattern( iv_text = iv_file_path iv_pattern_list = me->binary_file_patterns ) = abap_true ).
      rv_is_binary_file = abap_true.
    ENDIF.

  ENDMETHOD.                    "is_binary_file

  METHOD is_file_to_be_ignored.

    rv_ignore = abap_false.
    IF ( lcl_function=>text_matches_pattern( iv_text = iv_file_path iv_pattern_list = me->upload_ignores ) = abap_true ).
      rv_ignore = abap_true.
    ENDIF.

  ENDMETHOD.                    "is_file_to_be_ignored

  METHOD is_text_file.

    rv_is_text_file = abap_undefined.
    IF ( lcl_function=>text_matches_pattern( iv_text = iv_file_path iv_pattern_list = me->text_file_patterns ) = abap_true ).
      rv_is_text_file = abap_true.
    ENDIF.

  ENDMETHOD.                    "is_text_file

ENDCLASS.                    "LCL_SAPUI5_ZIP_ARCHIVE




************************************************************************
* Class lcl_external_code_page
*
* supports conversion of external code page into abap code page.
************************************************************************
CLASS lcl_external_code_page IMPLEMENTATION.

  METHOD create.

*   Exception if no page name given
    IF iv_code_page_name IS INITIAL.
      RAISE EXCEPTION TYPE lcx_exception.
    ENDIF.

*   Create external code page instance and take over náme
    CREATE OBJECT rv_external_code_page.
    rv_external_code_page->name = iv_code_page_name.

*   Determine code page type from name
    DATA: name(10) TYPE c.
    name = iv_code_page_name.
    TRANSLATE name TO LOWER CASE.

    IF name(2) = 'cp'.
      rv_external_code_page->kind = 'J'.
    ELSEIF name(3) = 'iso'.
      rv_external_code_page->kind = 'H'.
    ELSEIF name(8) = 'us-ascii'.
      rv_external_code_page->kind = 'H'.
    ELSEIF name = 'utf-16be' OR
           name = 'utf-16le'.
      rv_external_code_page->kind = 'H'.
    ENDIF.

  ENDMETHOD.                    "create
  METHOD for_sapgui_installation.

*   Determine abap code page from sapgui installation
    DATA: code_page_abap TYPE abap_encoding,
          rc             TYPE i VALUE 0.
    CALL METHOD cl_gui_frontend_services=>get_saplogon_encoding
      CHANGING
        rc                            = rc
        file_encoding                 = code_page_abap
      EXCEPTIONS
        cntl_error                    = 1
        error_no_gui                  = 2
        not_supported_by_gui          = 3
        cannot_initialize_globalstate = 4
        OTHERS                        = 5.
    IF sy-subrc <> 0 OR code_page_abap = 0.
      RAISE EXCEPTION TYPE lcx_exception.
    ENDIF.

*   Determine corresponding java code page name
    DATA: java_code_page_name TYPE string,
          code_page_abap_     TYPE cpcodepage.
    code_page_abap_ = code_page_abap.
    CALL FUNCTION 'SCP_GET_JAVA_NAME'
      EXPORTING
        sap_codepage     = code_page_abap_
      IMPORTING
        name             = java_code_page_name
      EXCEPTIONS
        name_unknown     = 1
        invalid_codepage = 2
        OTHERS           = 3.
    IF sy-subrc <> 0. RAISE EXCEPTION TYPE lcx_exception. ENDIF.

*   Delegate for create
    rv_external_code_page = lcl_external_code_page=>create( iv_code_page_name = java_code_page_name ).

  ENDMETHOD.                    "create_for_abap_codepage

  METHOD get_abap_encoding.

*   Delegate ...
    IF me->kind IS NOT INITIAL.
      DATA: abap_encoding TYPE cpcodepage.
      CALL FUNCTION 'SCP_CODEPAGE_BY_EXTERNAL_NAME'
        EXPORTING
          external_name = me->name
          kind          = me->kind
        IMPORTING
          sap_codepage  = rv_abap_encoding
        EXCEPTIONS
          not_found     = 1.
    ELSE.
      "Default value for kind of code page.
      CALL FUNCTION 'SCP_CODEPAGE_BY_EXTERNAL_NAME'
        EXPORTING
          external_name = me->name
        IMPORTING
          sap_codepage  = rv_abap_encoding
        EXCEPTIONS
          not_found     = 1.
    ENDIF.
*   ... Raise exception if no abap code page found.
    IF sy-subrc <> 0. RAISE EXCEPTION TYPE lcx_exception. ENDIF.

  ENDMETHOD.                    "get_abap_encoding

  METHOD get_java_encoding.

*   Get ABAP codepage
    DATA: abap_encoding TYPE cpcodepage.
    abap_encoding = me->get_abap_encoding( ).

*   Delegate ...
    CALL FUNCTION 'SCP_GET_JAVA_NAME'
      EXPORTING
        sap_codepage     = abap_encoding
      IMPORTING
        name             = rv_java_encoding
      EXCEPTIONS
        name_unknown     = 1
        invalid_codepage = 2
        OTHERS           = 3.
    IF sy-subrc <> 0. RAISE EXCEPTION TYPE lcx_exception. ENDIF.

  ENDMETHOD.                    "get_java_encoding

ENDCLASS.                    "lcl_external_code_page



************************************************************************
* Class LCL_UI5_REPOSITORY
*
* represents a UI5 Repository and the UI5 application it contains.
************************************************************************
CLASS lcl_ui5_repository IMPLEMENTATION.

  METHOD class_constructor.
    CREATE OBJECT self.
  ENDMETHOD.                    "class_constructor

  METHOD get_mime_type_for_upload.

*   Check input
    CLEAR rv_mime_type.
    IF iv_operation IS INITIAL. RETURN. ENDIF.

*   Check if file is known to be text
    IF iv_operation->object_type = lcl_operation=>object_type_text_file.
      rv_mime_type = 'text/plain'.
      RETURN.
    ENDIF.

*   Check if file is known to be binary
    IF iv_operation->object_type = lcl_operation=>object_type_binary_file.
      rv_mime_type = 'image'. "Although this is not true neccessarily: This should trigger a binary upload
      RETURN.
    ENDIF.

*   Determine file extension
    DATA: extension TYPE string.
    extension = iv_operation->get_file_extension( ).

*   Determine mime type using file extension via SDOK_MIMETYPE_GET
    DATA: mimetype TYPE mimetypes-type,
          ext      TYPE char20.
    ext = extension.
    CALL FUNCTION 'SDOK_MIMETYPE_GET'
      EXPORTING
        extension = ext
      IMPORTING
        mimetype  = mimetype.
    .
    rv_mime_type = mimetype.

  ENDMETHOD.                    "get_mime_type_for_upload

*****************************************
  METHOD conciliate_upload_operations.
*****************************************
*
*   The method looks into each upload operation and changes it
*   if needed. This depends on the state of the corresponding
*   file in the repository.
*
*   Example 1:
*   If a file or folder does exist in the repository but is not
*   to be uploaded, then the object in the repository is to be
*   deleted.
*   There are two situations to be considered:
*   a) The object does not exist in the in the archive.
*   b) The object does exist but it is to be ignored
*      as it is in the ignore-list or it is a unix style text file
*      with no automatic conversion active.
*
*   Example 2:
*   If a file to be uploaded is identical with the one already
*   in the repository, then it is not to be uploaded if
*   delta mode has been activated.

*   Nothing to do if repository does not yet exist
    IF me->already_exists = abap_false. RETURN. ENDIF.

*   Walk through content of ui5 repository
    DATA: children TYPE string_table,
          child    TYPE string.
    TRY.
        children = me->api->get_folder_children( iv_path ).
      CATCH /ui5/cx_ui5_rep_dt.
    ENDTRY.
    LOOP AT children INTO child.

*     Debugging
      "if child cs 'meta.properties'. break wegmann. endif.

*     Check if child is intended for upload
*     ... and look for corresponding operation
      DATA: child_is_intended_for_upload TYPE abap_bool. child_is_intended_for_upload = abap_false.
      DATA: operation TYPE REF TO lcl_operation.
      CLEAR operation.
      DATA: operation_found TYPE abap_bool. operation_found = abap_false.
      LOOP AT cv_upload_operations INTO operation.
        IF operation->relative_path EQ child.
          operation_found = abap_true.
          IF NOT ( operation->operation = lcl_operation=>ignore_file
                    OR operation->operation = lcl_operation=>ignore_folder ).
            child_is_intended_for_upload = abap_true.
          ENDIF.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF operation_found = abap_false. CLEAR operation. ENDIF.

*     Check if it is a folder
      DATA: child_is_folder TYPE abap_bool.
      TRY.
          child_is_folder = me->api->check_is_folder( child ).
        CATCH /ui5/cx_ui5_rep_dt.
      ENDTRY.

*     Conciliate sub folder
      IF child_is_folder = abap_true.
*       Content
        conciliate_upload_operations( EXPORTING iv_delta_mode = iv_delta_mode
                                                iv_path = child
                                                iv_badi = iv_badi
                                                iv_archive = iv_archive
                                      CHANGING  cv_upload_operations = cv_upload_operations ).
*       Delete folder in repository not intended for upload
        IF child_is_intended_for_upload = abap_false.
          DATA: is_new_operation TYPE abap_bool.
          is_new_operation = abap_false. " There might be an operation of type IGRNORE_FILE already!
          IF operation IS INITIAL. CREATE OBJECT operation. is_new_operation = abap_true. ENDIF.
          operation->to_update = abap_true.
          operation->operation = lcl_operation=>delete_folder.
          operation->relative_path = child.
          operation->object_type = lcl_operation=>object_type_folder.
          IF is_new_operation = abap_true. APPEND operation TO cv_upload_operations. ENDIF.
        ELSE.
          IF iv_delta_mode = abap_true.
            operation->message = TEXT-075. "Folder already exists
            operation->to_update = abap_false.
          ENDIF.
        ENDIF.

*     Conciliate file
      ELSE.

*       Delete file not intended for upload
        IF child_is_intended_for_upload = abap_false.

*         Create operation if there's none yet
          IF operation IS INITIAL.
            CREATE OBJECT operation.
            is_new_operation = abap_true.
*            Determine object type using .Ui5RepositoryTextFiles
*            and .Ui5RepositoryBinaryFiles in the archive
            operation->object_type = lcl_operation=>object_type_file.
            IF iv_archive->is_binary_file( child ) = abap_true.
              operation->object_type = lcl_operation=>object_type_binary_file.
            ELSEIF iv_archive->is_text_file( child ) = abap_true.
              operation->object_type = lcl_operation=>object_type_text_file.
            ENDIF.
          ENDIF.
          operation->to_update = abap_true.
          operation->operation = lcl_operation=>delete_file.
          operation->relative_path = child.
          operation->full_path = child.

*         Omit deletion of mime resource if requested via BAdI /UI5/BADI_PREVENT_DELETION
          IF iv_badi->disable_deletion_for( operation ) = abap_true.
            operation->to_update = abap_false.
            operation->operation = lcl_operation=>ignore_file.
            operation->message = TEXT-065. "* Binary file has NOT been deleted in ABAP repository on BAdI request *
          ENDIF.
*
          operation->object_type = lcl_operation=>object_type_file.
          IF is_new_operation = abap_true. APPEND operation TO cv_upload_operations. ENDIF.

        ELSE.

*        Skip if nothing to do
*        ... this e.g. is a unix style text file
          IF operation->to_update = abap_false. CONTINUE. ENDIF.

*         Consider delta mode
          IF iv_delta_mode EQ abap_true.
            "Try to get a file content, hash of them and compare with a hash form zip
            DATA: lv_file_content           TYPE xstring,
                  lv_file_content_as_string TYPE string,
                  lv_hash                   TYPE md5_fields-hash.
            TRY.
                CLEAR lv_hash.
                me->api->get_file( EXPORTING  iv_path         = child
                                              iv_code_page    = operation->code_page
                                    IMPORTING ev_file_content = lv_file_content ).
                lv_file_content_as_string =
                   /ui5/cl_ui5_rep_utility=>convert_xstring_2_string( iv_xstring = lv_file_content
                                                                      iv_code_page = operation->code_page ).
                lv_hash = lcl_function=>get_md5_hash_for_string( lv_file_content_as_string ).

***** Debug conversion of Unix style files *****
                IF lcl_ui5_repository=>debug_conversions_for_file IS NOT INITIAL
                  AND operation->object_name EQ lcl_ui5_repository=>debug_conversions_for_file.

                  DATA: file_path TYPE string.
                  CONCATENATE lcl_ui5_repository=>debug_conversions_for_file
                              '.repository' INTO file_path. "#EC NOTEXT
                  lcl_function=>xstring_to_file( iv_file_content = lv_file_content
                                                 iv_file_path = file_path ).
                  CONCATENATE lcl_ui5_repository=>debug_conversions_for_file
                              '.repository.as_string' INTO file_path. "#EC NOTEXT
                  lcl_function=>string_to_file( iv_file_content = lv_file_content_as_string
                                                iv_code_page = operation->code_page
                                                iv_file_path = file_path ).
                ENDIF.
************************************************

              CATCH /ui5/cx_ui5_rep_dt.
                "File does not exist: It is to be uploaded
            ENDTRY.
*           Compare hash value from content of repository and to upload files
            IF operation->hash_value EQ lv_hash.
              operation->message   = TEXT-074. "Already up-to-date
              operation->to_update = abap_false.
            ELSE.
              operation->to_update = abap_true.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "conciliate_upload_operations

  METHOD get_instance.
    self->log_messages_ref = ir_log_messages.
    rv_self = self.
  ENDMETHOD.                    "get_instance

* Evaluate authorization available
  METHOD evaluate_authorization.

    rv_message = ''.
    DATA: may_be_insufficient TYPE abap_bool. may_be_insufficient = abap_false.
    DATA: affected TYPE string. affected = ''.

*   S_DEVELOP

    AUTHORITY-CHECK OBJECT 'S_DEVELOP'
             ID 'DEVCLASS' DUMMY
             ID 'OBJTYPE' FIELD 'WAPA'
             ID 'OBJNAME' DUMMY
             ID 'P_GROUP' DUMMY
             ID 'ACTVT' FIELD '02'.
    IF sy-subrc > 0.
      may_be_insufficient = abap_true.
      IF affected IS NOT INITIAL. CONCATENATE affected ', ' INTO affected RESPECTING BLANKS. ENDIF.
      CONCATENATE affected 'S_DEVELOP' INTO affected.
    ENDIF.

* S_ICF_ADM

    AUTHORITY-CHECK OBJECT 'S_ICF_ADM'
             ID 'ICF_TYPE' FIELD 'NODE'
             ID 'ACTVT' FIELD '02'
             ID 'ICF_HOST' DUMMY
             ID 'ICF_NODE' DUMMY
             .
    IF sy-subrc > 0.
      may_be_insufficient = abap_true.
      IF affected IS NOT INITIAL. CONCATENATE affected ', ' INTO affected RESPECTING BLANKS. ENDIF.
      CONCATENATE affected 'S_ICF_ADM' INTO affected.
    ENDIF.

* S_TRANSPRT

    AUTHORITY-CHECK OBJECT 'S_TRANSPRT'
             ID 'TTYPE' FIELD 'TASK'
             ID 'ACTVT' FIELD '02'.

    IF sy-subrc > 0.
      may_be_insufficient = abap_true.
      IF affected IS NOT INITIAL. CONCATENATE affected ', ' INTO affected RESPECTING BLANKS. ENDIF.
      CONCATENATE affected 'S_TRANSPRT' INTO affected.
    ENDIF.


* S_TCODE

    AUTHORITY-CHECK OBJECT 'S_TCODE'
             ID 'TCD' FIELD '*'.
    IF sy-subrc > 4.
      may_be_insufficient = abap_true.
      IF affected IS NOT INITIAL. CONCATENATE affected ', ' INTO affected RESPECTING BLANKS. ENDIF.
      CONCATENATE affected 'S_TCODE' INTO affected.
    ENDIF.

* S_CTS_ADMI

    AUTHORITY-CHECK OBJECT 'S_CTS_ADMI'
             ID 'CTS_ADMFCT' FIELD 'TABL'.

    IF sy-subrc > 4.
      may_be_insufficient = abap_true.
      IF affected IS NOT INITIAL. CONCATENATE affected ', ' INTO affected RESPECTING BLANKS. ENDIF.
      CONCATENATE affected 'S_CTS_ADMI' INTO affected.
    ENDIF.

*   S_CTS_SADM
    IF may_be_insufficient = abap_true.
      AUTHORITY-CHECK OBJECT 'S_CTS_SADM'
               ID 'DOMAIN' DUMMY
               ID 'DESTSYS' DUMMY
               ID 'CTS_ADMFCT' FIELD 'TABL'.
      IF sy-subrc >= 12.
        DATA: and TYPE string.
        and = 'and'(108). CONDENSE and.
        CONCATENATE affected ' ' and ' S_CTS_ADMI' INTO affected RESPECTING BLANKS.
      ENDIF.
    ENDIF.

*   Prepare authorization message
*   ... in case authorization appears to be insufficient
    IF may_be_insufficient = abap_true.
      rv_message = 'Warning : Authorization may be missing for'(102).
      CONCATENATE '* ' rv_message ' ' affected ' *' INTO rv_message RESPECTING BLANKS.
      IF strlen( rv_message ) > 120.
        rv_message = 'Warning : Authorizations may be missing.'(107).
        CONCATENATE '* ' rv_message ' *' INTO rv_message RESPECTING BLANKS.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "evaluate_authorization
*  METHOD is_binary_file.
*
*    rv_is_binary_file = abap_undefined.
*    IF ( lcl_function=>text_matches_pattern( iv_text = iv_file_path iv_pattern_list = me->binary_file_patterns ) = abap_true ).
*      rv_is_binary_file = abap_true.
*    ENDIF.
*
*  ENDMETHOD.                    "is_binary_file
*
*  METHOD is_file_to_be_ignored.
*
*    rv_ignore = abap_false.
*    IF ( lcl_function=>text_matches_pattern( iv_text = iv_file_path iv_pattern_list = me->ignores ) = abap_true ).
*      rv_ignore = abap_true.
*    ENDIF.
*
*  ENDMETHOD.                    "is_file_to_be_ignored
*
*  METHOD is_text_file.
*
*    rv_is_text_file = abap_undefined.
*    IF ( lcl_function=>text_matches_pattern( iv_text = iv_file_path iv_pattern_list = me->text_file_patterns ) = abap_true ).
*      rv_is_text_file = abap_true.
*    ENDIF.
*
*  ENDMETHOD.                    "is_text_file

  METHOD set_name.

    DATA: message TYPE string.

*   Remember name
    me->name = iv_name.

*   Check if UI5 Repository already exists. If yes retrieve API.
    TRY.
        me->api = /ui5/cl_ui5_rep_dt=>/ui5/if_ui5_rep_dt~get_api( iv_name = me->name ).
        me->already_exists  = abap_true.
        message = TEXT-029.

      CATCH cx_root.
        CLEAR me->api.
        me->already_exists = abap_false.
        message = TEXT-028.
    ENDTRY.
    CONCATENATE message  ' : ' iv_name INTO message.
    "    APPEND message TO me->log_messages_ref->*.

*   Check that name is valid
    DATA: p_application_name TYPE o2applname,
          p_application_ext  TYPE o2applext.
    p_application_name = iv_name.
    p_application_ext =  p_application_name.
    CALL METHOD cl_o2_helper=>check_application_name_valid
      EXPORTING
        p_application_name = p_application_name
        p_application_ext  = p_application_ext
      EXCEPTIONS
        invalid            = 1.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid
              TYPE 'S'
              NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
      CONCATENATE '* ' message INTO message RESPECTING BLANKS.
      APPEND message TO me->log_messages_ref->*.
      CLEAR  me->name.
      EXIT.
    ENDIF.

  ENDMETHOD.                    "set_name


ENDCLASS.                    "lcl_ui5_repository IMPLEMENTATION
