FUNCTION yui5_repository_load_http.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_URL) TYPE  STRING
*"     VALUE(IV_SAPUI5_APPLICATION_NAME) TYPE  STRING DEFAULT ''
*"     VALUE(IV_SAPUI5_APPLICATION_DESC) TYPE  STRING DEFAULT ''
*"     VALUE(IV_PACKAGE) TYPE  DEVCLASS DEFAULT ''
*"     VALUE(IV_WORKBENCH_REQUEST) TYPE  TRKORR DEFAULT ''
*"     VALUE(IV_EXTERNAL_CODE_PAGE) TYPE  STRING DEFAULT ''
*"     VALUE(IV_ACCEPT_UNIX_STYLE_EOL) TYPE  BOOLEAN DEFAULT '-'
*"     VALUE(IV_DELTA_MODE) TYPE  BOOLEAN DEFAULT '-'
*"     VALUE(IV_TEST_MODE) TYPE  BOOLEAN DEFAULT ''
*"     VALUE(IV_ZIP_FILE) TYPE  XSTRING OPTIONAL
*"  EXPORTING
*"     VALUE(EV_SUCCESS) TYPE  CHAR1
*"     VALUE(EV_LOG_MESSAGES) TYPE  STRING_TABLE
*"     VALUE(EV_MSG_ID) TYPE  SY-MSGID
*"     VALUE(EV_MSG_NO) TYPE  SY-MSGNO
*"----------------------------------------------------------------------
************************************************************************
*--------------------------------------------------------------------*
* This function module is pretty much a copy of SAP provided function
* module /UI5/UI5_REPOSITORY_LOAD_HTTP with minor changes to support
* input of a .zip file as an alternative to a URL.
*
* The doco for /UI5/UI5_REPOSITORY_LOAD_HTTP follows which describes
* the standard input parameters and how this function module works
* and how to use the .Ui5 parameter files to influence the behaviour.
*
*--------------------------------------------------------------------*
*
* The function module /UI5/UI5_REPOSITORY_LOAD_HTTP loads a SAPUI5
* application accessible at IV_URL into a SAPUI5 ABAP repository.
*
* Remarks:
*
* > As a mandatory input parameter the source url (IV_URL) has to be
*   passed. It points to a ZIP archive containing the SAPUI5 application
*   to be loaded into the SAPUI5 ABAP repository.
*
* > The parameter IV_TEST mode triggers a test run in which no upload
*   takes place. The export parameter EV_LOG_MESSAGES holds a detailed
*   log then.
*
*   Use the test mode to investigate behavior of the function module
*   you did not expect.
*
* > Further parameters are taken preferably from the function module's
*   import parameters or from the file .Ui5RepositoryUploadParameters
*   located in the archive.
*
*   As an advantage the function module can be started easily with a
*   set of documented standard parameters
*   e.g. automatically in the context of a Maven build process.
*
* > If you specify a parameter in the parameter file, you need
*   to place ist value in a line having the form
*   <parameter name> = <parameter value>
*
*   Examples: SAPUI5ApplicationName=UI5_HCM_LR_APV
*             AcceptUnixStyleEol=True               or X or Yes or 1
*
* > Overview of input parameters:
*
*   ---------------------------------------------------------------------------------------------------------------------
*   Parameter Name                Name                        Mandatory  Default     Remark
*   for use in                    in signature
*   parameter file                of function module
*   ---------------------------------------------------------------------------------------------------------------------
*   %                             IV_URL                      Yes        %           Url needs to point to valid
*                                                                                    zip archive.
*   SAPUI5ApplicationName         IV_SAPUI5_APPLICATION_NAME  Yes        %           App may be created or updated
*   SAPUI5ApplicationDescription  IV_SAPUI5_APPLICATION_DESC  No                     Used when app is to be created
*                                                                                    in the SAPUI5 ABAP repository.
*   SAPUI5ApplicationPackage      IV_PACKAGE                  No         '$TMP'
*   WorkbenchRequest              IV_WORKBENCH_REQUEST        Depends                Needed in case package is not $TMP.
*   ExternalCodePage              IV_EXTERNAL_CODE_PAGE       Depends    Codepage    Needed in case SAPGUI not installed.
*                                                                        of SAPGUI   E.g. "Cp1252" for Windows Standard
*   AcceptUnixStyleEol            IV_ACCEPT_UNIX_STYLE_EOL    No         ABAP_TRUE   Triggers automatic conversion
*                                                                                    of unix style text files to the
*                                                                                    Windows format expected by the
*                                                                                    repository.
*   DeltaMode                     IV_DELTA_MODE               No         ABAP_FALSE  Only changes to current state in
*                                                                                    the repository get registered
*                                                                                    in workbench request.
*   %                             IV_TEST_MODE                No         ABAP_FALSE  Upload operation is skipped then.
*   ---------------------------------------------------------------------------------------------------------------------
*   Legend: '%' means 'None"
*   ---------------------------------------------------------------------------------------------------------------------
*
* > Use the export variable EV_SUCCESS to check if the operation
*   was successful ('S'), a warning was indicated ('W') or an error
*   happened ('E').
*
*   In addition EV_MSG_ID and EV_MSG_NO give access to a message
*   with the outcome of the function call.
*
*   Have a look into the output EV_LOG_MESSAGES for the details.
*
* > In order to further control the upload operation you may place the
*   text files '.Ui5RepositoryIgnore', '.Ui5RepositoryTextFiles' or
*   '.Ui5RepositoryTextFiles' in the archive.
*
*   Each line in '.Ui5RepositoryIgnore' describes a file pattern
*   indicating which files should be ignored during upload. The line
*   may contain a substring of the file path or a regular expression
*   starting with '^' and ending with '$'. This setting overwrites
*   the built-in default.
*
*   Create or change the files '.Ui5RepositoryTextFiles' or
*   '.Ui5RepositoryTextFiles' used to identify text and binary content
*   in addition to the built-in default.
*
* > You may use the reports /UI5/UI5_REPOSITORY_LOAD_HTTP and
*   /UI5/UI5_REPOSITORY_LOAD_HTTPN to operate this function module
*   interactivly or to schedule its operation.
*
*   /UI5/UI5_REPOSITORY_LOAD_HTTP is for the upload of a single app.
*   /UI5/UI5_REPOSITORY_LOAD_HTTPN may handle more than one app.
*
*   A jenkins plugin which allows integration in a Maven build
*   process is being developed.
*
* > In order to up- and download SAPUI5 applications to and from the
*   file system you may use the report /UI5/UI5_REPOSITORY_LOAD.
*
* > Find SAP internal information here:
*   http://vesapui5.dhcp.wdf.sap.corp:1080/trac/sapui5.tools/wiki/SAPUI5Tools/UI5RepositoryLoadFaq

************************************************************************
* Check inputs
************************************************************************

* Initialize
  ev_success = status_success.
*
  DATA: message    TYPE string,
        empty_line TYPE string,
        parameter  TYPE string.
  empty_line = '.'.
  APPEND TEXT-001 TO ev_log_messages. "***** Upload of SAPUI5 Application from ZIP-Archive into SAPUI5 ABAP Repository *****
  APPEND empty_line TO ev_log_messages.

* Debugging conversions for a specific file
* ... In case you would like to debug:
*     Specify resource name here -  e.g. 'utils.js'
  lcl_ui5_repository=>debug_conversions_for_file = ''.
  "lcl_ui5_repository=>debug_conversions_for_file = 'utils.js'.

* Indicate test mode
  IF iv_test_mode = abap_true.
    APPEND TEXT-002 TO ev_log_messages. "* Running in test mode: Upload operations are skipped, Detailed Log *
    APPEND empty_line TO ev_log_messages.
  ELSE.
    APPEND TEXT-003 TO ev_log_messages. "* Running in regular mode: Brief Log *
    APPEND empty_line TO ev_log_messages.
  ENDIF.

* Check Url to SAPUI5 application

* ... Is not empty
  IF iv_url IS INITIAL AND iv_zip_file IS INITIAL.
    ev_success = status_error.
    APPEND TEXT-010 TO ev_log_messages. "* Upload canceled: Url to Zip File containing SAPUI5 App is missing. *
    lcl_function=>set_return_msg( EXPORTING iv_success = ev_success iv_test_mode = iv_test_mode
                                  CHANGING  cv_msg_id = ev_msg_id cv_msg_no = ev_msg_no ).
    EXIT.
  ENDIF.

* ... Refers to zip file
  "  IF iv_url NS '.zip'.
  "    ev_success = abap_false.
  "    message = text-010. APPEND message TO ev_log_messages.
  "    EXIT.
  "  ENDIF.

* ... Points to valid archive
  DATA: sapui5_archive   TYPE REF TO lcl_sapui5_zip_archive,
        log_messages_ref TYPE REF TO string_table.
  GET REFERENCE OF ev_log_messages INTO log_messages_ref.
  CREATE OBJECT sapui5_archive
    EXPORTING
      iv_url          = iv_url
      iv_zip_file     = iv_zip_file
      ir_log_messages = log_messages_ref.
  IF ( sapui5_archive->is_ok( ) = abap_false ).
    ev_success = status_error.
    lcl_function=>set_return_msg( EXPORTING iv_success = ev_success iv_test_mode = iv_test_mode
                                  CHANGING  cv_msg_id = ev_msg_id cv_msg_no = ev_msg_no ).
    EXIT.
  ENDIF.

* Determine and check upload parameters

* ... Name of SAPUI5 Repositiory
*     Take from import parameter or from zip archive file .Ui5RepositoryUploadParameters
*     Exit if no name given
  DATA: sapui5_application_name TYPE string,
        sapui5_application_desc TYPE string.
  sapui5_application_name = iv_sapui5_application_name.
  IF sapui5_application_name IS INITIAL.
    sapui5_application_name = sapui5_archive->get_upload_parameter('SAPUI5ApplicationName').
    message = TEXT-026. ". %1 "%2" has been determined from the '.Ui5RepositoryUploadParameters' file
  ELSE.
    message = TEXT-027. ". %1 "%2" has been determined from the corresponding import parameter
  ENDIF.
  REPLACE '%1' IN message WITH TEXT-100. "Name of SAPUI5 Application
  REPLACE '%2' IN message WITH sapui5_application_name.

  IF sapui5_application_name IS NOT INITIAL.
    APPEND message TO ev_log_messages.
  ELSE.
    ev_success = status_error.
    APPEND TEXT-015 TO ev_log_messages. "Specify SAPUI5 Application Name
    lcl_function=>set_return_msg( EXPORTING iv_success = ev_success iv_test_mode = iv_test_mode
                                  CHANGING  cv_msg_id = ev_msg_id cv_msg_no = ev_msg_no ).
    EXIT.
  ENDIF.

* ... Access SAPUI5 ABAP Repository
  DATA: ui5_repository         TYPE REF TO lcl_ui5_repository.
  ui5_repository = lcl_ui5_repository=>get_instance( ir_log_messages = log_messages_ref ).
  ui5_repository->set_name( sapui5_application_name ).
  IF ui5_repository->name IS INITIAL. "this indicates an invalid name
    ev_success = status_error.
    APPEND TEXT-090 TO ev_log_messages. "* Name of SAPUI5 Application appears to be invalid: Same rules as for BSP applications apply
    lcl_function=>set_return_msg( EXPORTING iv_success = ev_success iv_test_mode = iv_test_mode
                                  CHANGING  cv_msg_id = ev_msg_id cv_msg_no = ev_msg_no ).
    EXIT.
  ENDIF.

* ... Description of SAPUI5 repository
*     is used if repository does not yet exist.
  IF ui5_repository->already_exists = abap_false.
    sapui5_application_desc = iv_sapui5_application_desc.
    IF sapui5_application_desc IS NOT INITIAL.
      message = TEXT-027.
    ENDIF.
    IF sapui5_application_desc IS INITIAL.
      sapui5_application_desc = sapui5_archive->get_upload_parameter('SAPUI5ApplicationDescription').
      IF sapui5_application_desc IS NOT INITIAL.
        message = TEXT-026.
      ENDIF.
    ENDIF.
    IF sapui5_application_desc IS NOT INITIAL.
      REPLACE '%1' IN message WITH TEXT-101. "Description of SAPUI5 Application
      REPLACE '"%2" ' IN message WITH ''.
      CONDENSE message. "Remove double spaces
      APPEND message TO ev_log_messages.
    ENDIF.
  ENDIF.

* ... Package for SAPUI5 application
*     is defaulted to $TMP if not specified elsewhere
  DATA: application_package TYPE string.
  application_package = iv_package.
  IF application_package IS NOT INITIAL.
    message = TEXT-027. ". %1 "%2" has been determined from the corresponding import parameter.
  ENDIF.
  IF application_package IS INITIAL.
    application_package = sapui5_archive->get_upload_parameter('SAPUI5ApplicationPackage').
    IF application_package IS NOT INITIAL.
      message = TEXT-026. ". %1 "%2" has been determined from the '.Ui5RepositoryUploadParameters' file.
    ENDIF.
  ENDIF.
  IF application_package IS NOT INITIAL.
    REPLACE '%1' IN message WITH TEXT-103. "Package of SAPUI5 Application
    REPLACE '%2' IN message WITH application_package.
    APPEND message TO ev_log_messages.
  ENDIF.
  IF application_package IS INITIAL.
    application_package = '$TMP'.
    message = TEXT-036.
    APPEND message TO ev_log_messages.
  ENDIF.

* Package needs to be specified if repository already exists.
* ... should not happen as package is defaulted
  IF ui5_repository->already_exists = abap_false
     AND application_package IS INITIAL.
    ev_success = status_error.
    message = TEXT-017. APPEND message TO ev_log_messages. "* Repository Package is not definded *
    lcl_function=>set_return_msg( EXPORTING iv_success = ev_success iv_test_mode = iv_test_mode
                                  CHANGING  cv_msg_id = ev_msg_id cv_msg_no = ev_msg_no ).
    EXIT.
  ENDIF.

* Workbench request
* ... may be empty if package is $TMP (local)
  DATA: repository_workbench_request TYPE string.
  repository_workbench_request = iv_workbench_request.
  IF repository_workbench_request IS NOT INITIAL.
    message = TEXT-027.
  ENDIF.
  IF repository_workbench_request IS INITIAL.
    repository_workbench_request = sapui5_archive->get_upload_parameter('WorkbenchRequest').
    IF repository_workbench_request IS NOT INITIAL.
      message = TEXT-026.
    ENDIF.
  ENDIF.
  IF repository_workbench_request IS NOT INITIAL.
    REPLACE '%1' IN message WITH TEXT-104. "Workbench Request
    REPLACE '%2' IN message WITH repository_workbench_request.
    APPEND message TO ev_log_messages.
  ELSE.
    IF application_package NE '$TMP'.
      ev_success = status_error.
      message = TEXT-016. "* Upload canceled: ABAP workbench request for SAPUI5 ABAP Repository not specified. *
      APPEND message TO ev_log_messages.
      lcl_function=>set_return_msg( EXPORTING iv_success = ev_success iv_test_mode = iv_test_mode
                                    CHANGING  cv_msg_id = ev_msg_id cv_msg_no = ev_msg_no ).
      EXIT.
    ENDIF.
  ENDIF.

* External code page of text files in archive
  DATA: external_code_page_name TYPE string.
  external_code_page_name = iv_external_code_page.
  IF external_code_page_name IS NOT INITIAL.
    message = TEXT-027.
  ENDIF.
  IF external_code_page_name IS INITIAL.
    external_code_page_name = sapui5_archive->get_upload_parameter('ExternalCodePage').
    IF external_code_page_name IS NOT INITIAL.
      message = TEXT-026.
    ENDIF.
  ENDIF.
  IF external_code_page_name IS NOT INITIAL.
    REPLACE '%1' IN message WITH TEXT-105. "External Code Page
    REPLACE '%2' IN message WITH external_code_page_name.
    APPEND message TO ev_log_messages.
  ELSE.
    DATA: lv_ex TYPE REF TO cx_root.
    TRY.
        DATA: ecp TYPE REF TO lcl_external_code_page.
        ecp = lcl_external_code_page=>for_sapgui_installation( ).
        external_code_page_name = ecp->get_java_encoding( ).
        message = TEXT-018. "* Code page has been set to % *
        REPLACE '%' IN message WITH external_code_page_name.
        APPEND message TO ev_log_messages.
      CATCH lcx_exception INTO lv_ex.
        ev_success = status_error.
        message = TEXT-091. "* Upload canceled: External code page has not been specified. *
        APPEND message TO ev_log_messages.
        lcl_function=>set_return_msg( EXPORTING iv_success = ev_success iv_test_mode = iv_test_mode
                                      CHANGING  cv_msg_id = ev_msg_id cv_msg_no = ev_msg_no ).
        EXIT.
    ENDTRY.
  ENDIF.

* Accept unix style line endings
* ... default to ABAP_TRUE
  DATA: accept_unix_style_line_endings TYPE abap_bool.
  accept_unix_style_line_endings = iv_accept_unix_style_eol.
  IF accept_unix_style_line_endings NE abap_undefined.
    message = TEXT-027. ". %1 "%2" has been determined from the corresponding import parameter.
  ELSE.
    parameter =
      sapui5_archive->get_upload_parameter( iv_key = 'AcceptUnixStyleEol'
                                            iv_return_if_not_specified = '-' ).
    accept_unix_style_line_endings = lcl_function=>text_to_abap_bool( parameter ).
    IF accept_unix_style_line_endings NE abap_undefined.
      message = TEXT-026. ". %1 "%2" has been determined from the '.Ui5RepositoryUploadParameters' file.
    ENDIF.
  ENDIF.
  IF accept_unix_style_line_endings NE abap_undefined.
    REPLACE '%1' IN message WITH TEXT-106. "Acceptance of Unix style line delimeters in text files
    REPLACE '"%2" ' IN message WITH ''. CONDENSE message.
    APPEND message TO ev_log_messages.
  ELSE.
    accept_unix_style_line_endings = abap_true.
  ENDIF.
* ... Indicate
  IF accept_unix_style_line_endings = abap_true.
    message = TEXT-037. "Unix style end of line markers in text files get accepted.
  ELSE.
    message = TEXT-038. "Unix style end of line markers in text files get rejected.
  ENDIF.
  APPEND message TO ev_log_messages.

* Delta mode
  DATA: delta_mode TYPE abap_bool.
* ... Determine from function parameter or upload parameter file
  delta_mode = iv_delta_mode.
  IF delta_mode = abap_undefined.
    parameter =
       sapui5_archive->get_upload_parameter( iv_key = 'DeltaMode'
                                             iv_return_if_not_specified = '-' ).
    delta_mode = lcl_function=>text_to_abap_bool( parameter ).
  ENDIF.
* ... Delta-mode off if not specified.
  IF delta_mode = abap_undefined.
    delta_mode = abap_false.
  ENDIF.
* ... Indicate Delta Mode
  IF delta_mode = abap_true.
    message = TEXT-034. ". The delta mode has been turned on.
  ELSE.
    message = TEXT-035. ". The delta mode has been turned off.
  ENDIF.
  APPEND message TO ev_log_messages.

* ... Indicate if BAdI /UI5/BADI_PREVENT_DELETION to prevent the deletion of mime objects
*     has been implemented: This SAP internal BAdI may cause inconsistencies between
*     the app beeing uploaded and the SAPUI5 ABAP repository
  DATA: badi_to_prevent_deletion TYPE REF TO lcl_badi,
        workbench_request        TYPE trkorr.
  workbench_request = repository_workbench_request.
  CREATE OBJECT badi_to_prevent_deletion
    EXPORTING
      iv_test_mode               = iv_test_mode
      iv_sapui5_application_name = sapui5_application_name
      iv_workbench_request       = workbench_request.
  IF badi_to_prevent_deletion->implementation IS NOT INITIAL.
    APPEND TEXT-039 TO ev_log_messages. ". Attention: A BAdI implementation for /UI5/BADI_PREVENT_DELETION is active
  ENDIF.

*
  APPEND empty_line TO ev_log_messages.


************************************************************************
* Determine upload operations
************************************************************************

  "message = text-041. "* Analyzing Archive *
  "append message to ev_log_messages.

  TYPES: operations TYPE TABLE OF REF TO lcl_operation.
  DATA: load_operations TYPE operations.
  sapui5_archive->determine_upload_operations( EXPORTING iv_delta_mode = delta_mode
                                                         iv_code_page = external_code_page_name
                                                         iv_accept_unix_style_eol = accept_unix_style_line_endings
                                               CHANGING  cv_load_operations = load_operations
                                                         cv_success = ev_success ).

************************************************************************
* Explore SAPUI5 application to upload and conciliate operations
************************************************************************

* Conciliate
  workbench_request = repository_workbench_request.
  ui5_repository->conciliate_upload_operations( EXPORTING iv_delta_mode = delta_mode
                                                          iv_badi = badi_to_prevent_deletion
                                                          iv_archive = sapui5_archive
                                                CHANGING  cv_upload_operations = load_operations ).

* Estimate authorization
  DATA: authorization_warning TYPE string.
  authorization_warning = ui5_repository->evaluate_authorization( ).
  IF authorization_warning IS NOT INITIAL.
    APPEND authorization_warning TO ev_log_messages.
    APPEND empty_line TO ev_log_messages.
  ENDIF.

* Return if in test mode or in case an error occurred
  DATA: upload_operation TYPE REF TO lcl_operation.
  IF iv_test_mode = abap_true OR badi_to_prevent_deletion->error_occurred = abap_true.

*   List operations
    message = TEXT-040. "* Operations % *
    APPEND message TO ev_log_messages.
    LOOP AT load_operations INTO upload_operation.
      message = upload_operation->create_log_message( ).
      IF message IS INITIAL. CONTINUE. ENDIF.
      CONCATENATE '. ' message INTO message RESPECTING BLANKS.
      APPEND message TO ev_log_messages.
    ENDLOOP.

*   Inidicate BAdI error if occurred
    IF badi_to_prevent_deletion->error_occurred = abap_true.
      ev_success = status_error.
      APPEND empty_line TO ev_log_messages.
      APPEND TEXT-093 TO ev_log_messages. "* Upload canceled: A BAdI error has occurred. *
      CONCATENATE '. ' badi_to_prevent_deletion->error_text INTO message RESPECTING BLANKS.
      APPEND message TO ev_log_messages.
    ENDIF.

*   Return message
    lcl_function=>set_return_msg( EXPORTING iv_success = ev_success iv_test_mode = iv_test_mode
                                  CHANGING  cv_msg_id = ev_msg_id cv_msg_no = ev_msg_no ).
    EXIT.
  ENDIF.


************************************************************************
* Upload application
************************************************************************

***** Prepare upload parameters ****************************************

* Description
  DATA: upload_parameters TYPE lcl_ui5_repository=>upload_parameters.
  upload_parameters-description = sapui5_application_desc.

* Package
  upload_parameters-package = application_package.

* Transport_request
  upload_parameters-transport_request = repository_workbench_request.

* Code page
  upload_parameters-code_page_ui = external_code_page_name.
  TRY.
      ecp = lcl_external_code_page=>create( upload_parameters-code_page_ui ).
      upload_parameters-code_page_abap = ecp->get_abap_encoding( ).
      upload_parameters-code_page_java = ecp->get_java_encoding( ).

*     Confirm valid code page has been entered.
      IF upload_parameters-code_page_abap IS INITIAL.
        ev_success = status_error.
        APPEND TEXT-032 TO ev_log_messages. "* Upload canceled: External Code Page is invalid. You may use e.g. 'Cp1252' *
        lcl_function=>set_return_msg( EXPORTING iv_success = ev_success iv_test_mode = iv_test_mode
                                      CHANGING  cv_msg_id = ev_msg_id cv_msg_no = ev_msg_no ).
        EXIT.
      ENDIF.

    CATCH cx_root.

*     General code page problem
      ev_success = status_error.
      APPEND TEXT-033 TO ev_log_messages. "* Upload canceled: Code Page is invalid. *
      lcl_function=>set_return_msg( EXPORTING iv_success = ev_success iv_test_mode = iv_test_mode
                                    CHANGING  cv_msg_id = ev_msg_id cv_msg_no = ev_msg_no ).
      EXIT.

  ENDTRY.

***** Create SAPUI5 ABAP repository if neccessary *********************

  IF ui5_repository->already_exists = abap_false.

*   Message
    message = TEXT-028. "* Creating new SAPUI5 application % *
    REPLACE '%' IN message WITH ui5_repository->name.
    APPEND message TO ev_log_messages.

*   Delegate ...
    DATA: lv_ex_rep_dt TYPE REF TO /ui5/cx_ui5_rep_dt.
    TRY.
        ui5_repository->api =
           /ui5/cl_ui5_rep_dt=>/ui5/if_ui5_rep_dt~create_repository(
                iv_name        = ui5_repository->name
                iv_description = upload_parameters-description
                iv_devclass    =  upload_parameters-package
                iv_transport_request = upload_parameters-transport_request
                iv_dialog_mode = space ).

      CATCH /ui5/cx_ui5_rep_dt INTO lv_ex_rep_dt.
*       Cleanup
        IF ui5_repository->api IS NOT INITIAL. CLEAR ui5_repository->api. ENDIF.

*       If any: write error text on log
        ev_success = status_error.
        DATA: text TYPE string.
        text = lv_ex_rep_dt->get_text( ).
        IF text IS NOT INITIAL.
          CONCATENATE '* ' text ' *' INTO text RESPECTING BLANKS.
          APPEND text TO ev_log_messages.
        ENDIF.
        APPEND TEXT-030 TO ev_log_messages. "* SAPUI5 application has not been created (successfully) *
        lcl_function=>set_return_msg( EXPORTING iv_success = ev_success iv_test_mode = iv_test_mode
                                      CHANGING  cv_msg_id = ev_msg_id cv_msg_no = ev_msg_no ).
        EXIT.
    ENDTRY.
  ENDIF.

* Update message if repository does already exist
  IF ui5_repository->already_exists = abap_true.
    message = TEXT-029. "* Updating existing SAPUI5 application % *
    REPLACE '%' IN message WITH ui5_repository->name.
    APPEND message TO ev_log_messages.
  ENDIF.

***** Perform upload ***************************************************

* Perform upload operation and write log entry
  TRY.
      ui5_repository->api->lock( ).
    CATCH /ui5/cx_ui5_rep_dt.
      ev_success = status_error.
      APPEND TEXT-092 TO ev_log_messages. "* Upload canceled: Unable to lock SAPUI5 repository *
      lcl_function=>set_return_msg( EXPORTING iv_success = ev_success iv_test_mode = iv_test_mode
                                    CHANGING  cv_msg_id = ev_msg_id cv_msg_no = ev_msg_no ).
      EXIT.
  ENDTRY.

***** Loop at each operation *****
*     and if requested ...
  DATA: lv_message TYPE string.
  LOOP AT load_operations INTO upload_operation.

*   Continue if nothing to do
    IF delta_mode EQ abap_true
       AND upload_operation->to_update NE abap_true.
      CONTINUE.
    ENDIF.

    CASE upload_operation->operation.

***** Create folder *****

      WHEN lcl_operation=>create_folder.

        TRY.
            ui5_repository->api->create_folder( iv_path = upload_operation->relative_path
                                                iv_transport_request = upload_parameters-transport_request ).
          CATCH /ui5/cx_ui5_rep_dt.
            lv_message = '* Warning: Folder % has not been created *'(054).
            REPLACE '%' IN lv_message WITH upload_operation->relative_path.
            APPEND lv_message TO ev_log_messages.
        ENDTRY.

***** Upload text or binary file ****

      WHEN lcl_operation=>upload_file.
        IF ( upload_operation->object_type = lcl_operation=>object_type_text_file
          OR upload_operation->object_type = lcl_operation=>object_type_binary_file ).
          TRY.

*             Read file from archive
              DATA: file_content TYPE xstring.
              DATA: upload_operation_is_binary TYPE abap_bool.
              upload_operation_is_binary = upload_operation->is_binary( ).
              file_content = sapui5_archive->read_file( iv_path = upload_operation->full_path ).
              "file_content = file_system->read_file( iv_file_path = upload_operation->full_path
              "                                       iv_file_is_binary = upload_operation_is_binary
              "                                       iv_code_page_abap = upload_parameters-code_page_abap ).

*             Convert line endings of text files from unix to windows format
*             ... if needed and requested
              IF upload_operation->object_type = lcl_operation=>object_type_text_file
                 AND upload_operation->eol_conversion = abap_true
                 AND accept_unix_style_line_endings = abap_true.
                DATA: file_content_as_is             TYPE string,
                      file_content_as_string         TYPE string,
                      file_content_as_string_windows TYPE string.
                TRY.
                    file_content_as_is = file_content.
                    file_content_as_string =
                       /ui5/cl_ui5_rep_utility=>convert_xstring_2_string( iv_xstring = file_content
                                                                          iv_code_page = upload_operation->code_page ).
                    file_content_as_string_windows =
                       lcl_function=>conv_unix_style_line_endings( iv_string = file_content_as_string ).
                    file_content =
                       /ui5/cl_ui5_rep_utility=>convert_string_2_xstring( iv_string = file_content_as_string_windows
                                                                          iv_code_page = upload_operation->code_page ).
                  CATCH /ui5/cx_ui5_rep_dt.
                  CATCH cx_sy_conversion_error.
                    ev_success = status_error.
                    file_content = file_content_as_is.
                    lv_message = '* Error: Unable to convert Unix style text file % *'(057).
                    REPLACE '%' IN lv_message WITH upload_operation->relative_path.
                    APPEND lv_message TO ev_log_messages.
                ENDTRY.
              ENDIF.

*             Upload content into ui5 repository
              DATA: mime_type TYPE string.
              mime_type = ui5_repository->get_mime_type_for_upload( upload_operation ).
              upload_operation_is_binary = upload_operation->is_binary( ).
              TRY.
                  CALL METHOD ui5_repository->api->put_file
                    EXPORTING
                      iv_path              = upload_operation->relative_path
                      iv_transport_request = upload_parameters-transport_request
                      iv_file_content      = file_content
                      iv_mime_type         = mime_type     "ui5_repository->get_mime_type_for_upload( upload_operation )
                      iv_code_page         = upload_parameters-code_page_java
                      iv_is_binary         = upload_operation_is_binary.   "upload_operation->is_binary( ).
                  lv_message = upload_operation->create_log_message( ).
                  APPEND lv_message TO ev_log_messages.

*                 Put content of version.json as log message:
*                 No more than 15 lines with a maximum length of 120 characters
                  IF upload_operation->full_path CS 'version.json'
                     AND upload_operation_is_binary = abap_false. "#EC NOTEXT
                    TRY.
*                       Convert to windows format
                        file_content_as_is = file_content.
                        file_content_as_string =
                           /ui5/cl_ui5_rep_utility=>convert_xstring_2_string( iv_xstring = file_content
                                                                              iv_code_page = upload_operation->code_page ).
                        file_content_as_string_windows =
                           lcl_function=>conv_unix_style_line_endings( iv_string = file_content_as_string ).
*                       Retrieve lines
                        DATA: lines TYPE o2pageline_table.
                        lines = /ui5/cl_ui5_rep_utility=>code_string_2_code_tab( iv_src_string = file_content_as_string_windows ).
*                       Prepare and write lines
                        IF lines( lines ) < 15.
                          LOOP AT lines INTO lv_message.
                            "Mini escaping and beautification
                            REPLACE ALL OCCURRENCES OF '<'      IN lv_message WITH '&lt;'. "#EC NOTEXT
                            REPLACE ALL OCCURRENCES OF '>'      IN lv_message WITH '&gt;'. "#EC NOTEXT
                            REPLACE ALL OCCURRENCES OF '"'      IN lv_message WITH ''. "#EC NOTEXT
                            REPLACE ALL OCCURRENCES OF '&quot;' IN lv_message WITH ''. "#EC NOTEXT
                            IF strlen( lv_message ) > 120.
                              lv_message = lv_message(120).
                              CONCATENATE lv_message ' ...' INTO lv_message RESPECTING BLANKS. "#EC NOTEXT
                            ENDIF.
                            CONCATENATE '. ' lv_message INTO lv_message RESPECTING BLANKS. "#EC NOTEXT
                            APPEND lv_message TO ev_log_messages.
                          ENDLOOP.
                        ENDIF.
                      CATCH cx_root .
                    ENDTRY.
                  ENDIF.

                CATCH /ui5/cx_ui5_rep_dt INTO lv_ex_rep_dt.
*                 Write error message for UI5 repository problems
                  text = lv_ex_rep_dt->get_text( ).
                  CONCATENATE '* ' text ' *' INTO lv_message RESPECTING BLANKS.
                  APPEND lv_message TO ev_log_messages.
                  RAISE EXCEPTION TYPE lcx_exception.
              ENDTRY.

            CATCH lcx_exception.
*             Write error message for UI5 repository problems
              IF upload_operation->is_binary( ) = abap_true.
                lv_message = '* Warning: Text file % has not been uploaded *'(055).
              ELSEIF upload_operation->is_binary( ) = abap_false.
                lv_message = '* Warning: Binary file % has not been uploaded *'(064).
              ENDIF.
              REPLACE '%' IN lv_message WITH upload_operation->relative_path.
              APPEND lv_message TO ev_log_messages.
              IF ev_success = status_success. "Keep status error
                ev_success = status_warning.
              ENDIF.
          ENDTRY.
        ENDIF.

*     Delete file/folder in UI5 repository
      WHEN lcl_operation=>delete_file OR
            lcl_operation=>delete_folder.
        TRY.
            ui5_repository->api->delete( iv_path = upload_operation->relative_path
                                         iv_transport_request = upload_parameters-transport_request ).
            lv_message = upload_operation->create_log_message( ). "
            APPEND lv_message TO ev_log_messages. "
          CATCH /ui5/cx_ui5_rep_dt INTO lv_ex_rep_dt.
*           Write error message for UI5 repository problems
            text = lv_ex_rep_dt->get_text( ).
            CONCATENATE '* ' text ' *' INTO lv_message RESPECTING BLANKS.
            APPEND lv_message TO ev_log_messages.
        ENDTRY.

***** Indicate files and folders being ignored *****

*     Ignore file
*     Ignore folder
      WHEN lcl_operation=>ignore_file OR
           lcl_operation=>ignore_folder.
        lv_message = upload_operation->create_log_message( ).
        APPEND lv_message TO ev_log_messages.

    ENDCASE.
  ENDLOOP.

* Unlock SAPUI5 repository
  TRY.
      ui5_repository->api->unlock( ).
    CATCH /ui5/cx_ui5_rep_dt.
  ENDTRY.

************* Recalculate application index and write log messages*********************************

  IF iv_test_mode NE abap_true.

    DATA: lr_app_index  TYPE REF TO /ui5/cl_ui5_app_index.
    DATA: lv_application TYPE o2applname.
    DATA: lt_messages    TYPE bapiret2_t.
    DATA: lv_log_msg     TYPE string.

    FIELD-SYMBOLS: <ls_message> TYPE bapiret2.

    lr_app_index = /ui5/cl_ui5_app_index=>get_instance( ).

    lv_application = sapui5_application_name.

    CALL METHOD lr_app_index->recalculate_app
      EXPORTING
        iv_application = lv_application
      IMPORTING
*       es_result      =
        et_messages    = lt_messages.

*
    APPEND empty_line TO ev_log_messages.
    APPEND TEXT-111 TO ev_log_messages.

    LOOP AT lt_messages ASSIGNING <ls_message>.
      CONCATENATE TEXT-110 <ls_message>-message INTO lv_log_msg SEPARATED BY space.
      APPEND  lv_log_msg TO ev_log_messages.
    ENDLOOP.

    IF lt_messages IS NOT INITIAL.
      APPEND TEXT-112 TO ev_log_messages.
    ENDIF.

  ENDIF.
*******************************************************************************************


* Upload done
  APPEND empty_line TO ev_log_messages.
  APPEND TEXT-043 TO ev_log_messages.  "* Done *

* Set return message
  lcl_function=>set_return_msg( EXPORTING iv_success = ev_success iv_test_mode = iv_test_mode
                                CHANGING  cv_msg_id = ev_msg_id cv_msg_no = ev_msg_no ).


ENDFUNCTION.
