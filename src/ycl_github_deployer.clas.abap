*----------------------------------------------------------------------*
*       CLASS YCL_GITHUB_DEPLOYER DEFINITION
*----------------------------------------------------------------------*
* Documentation is at http://yelcho.github.io/sapui5-deployer/
*----------------------------------------------------------------------*
CLASS ycl_github_deployer DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS deploy
      IMPORTING
        !repository  TYPE string
        !branch      TYPE string
        !github_user TYPE string OPTIONAL
        !password    TYPE string OPTIONAL
        !transport   TYPE trkorr OPTIONAL
        !test_only   TYPE boolean OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF submodule_info_type,
        folder     TYPE string,
        repository TYPE string,
        branch     TYPE string,
      END OF submodule_info_type .
    TYPES:
      submodule_info_ttype TYPE STANDARD TABLE OF submodule_info_type .

    DATA repository TYPE string .
    DATA branch TYPE string .
    DATA transport TYPE trkorr .
    DATA test_only TYPE boolean .
    DATA github_user TYPE string .
    DATA password TYPE string .
    DATA minify_file_count TYPE i .
    DATA minify_byte_count TYPE i .
    DATA enable_minify TYPE boolean VALUE abap_true ##NO_TEXT.

    METHODS constructor
      IMPORTING
        !repository  TYPE string
        !branch      TYPE string
        !github_user TYPE string OPTIONAL
        !password    TYPE string OPTIONAL
        !transport   TYPE trkorr OPTIONAL
        !test_only   TYPE boolean OPTIONAL
      RAISING
        cx_sy_create_data_error .
    METHODS add_module_to_archive
      IMPORTING
        !info          TYPE submodule_info_type
        VALUE(archive) TYPE REF TO cl_abap_zip .
    METHODS cleanup_stringdata
      IMPORTING
        !data_in        TYPE string
        !nogaps         TYPE boolean OPTIONAL
      RETURNING
        VALUE(data_out) TYPE string .
    METHODS cleanup_xmldata
      IMPORTING
        !data_in        TYPE string
      RETURNING
        VALUE(data_out) TYPE string .
    METHODS get_file_type
      IMPORTING
        !iv_filename       TYPE string
      RETURNING
        VALUE(rv_filetype) TYPE string .
    METHODS get_css_minify_params
      RETURNING
        VALUE(rt_params) TYPE tihttpnvp .
    METHODS get_js_minify_params
      RETURNING
        VALUE(rt_params) TYPE tihttpnvp .
    METHODS get_module_archive
      IMPORTING
        !url           TYPE string
        !user          TYPE string
        !password      TYPE string
      RETURNING
        VALUE(archive) TYPE REF TO cl_abap_zip .
    METHODS get_submodule_info
      IMPORTING
        !iv_submodule_data       TYPE xstring
      EXPORTING
        VALUE(et_submodule_info) TYPE submodule_info_ttype .
    METHODS minify
      IMPORTING
        !data_in        TYPE xstring
        !file_type      TYPE string
      RETURNING
        VALUE(data_out) TYPE xstring .
    METHODS minify_js
      IMPORTING
        !data_in        TYPE xstring
      RETURNING
        VALUE(data_out) TYPE xstring .
    METHODS minify_css
      IMPORTING
        !data_in        TYPE xstring
      RETURNING
        VALUE(data_out) TYPE xstring .
    METHODS remove_cover_folder
      IMPORTING
        !archive           TYPE REF TO cl_abap_zip
      RETURNING
        VALUE(new_archive) TYPE REF TO cl_abap_zip .
    METHODS save_archive_file
      IMPORTING
        !archive TYPE REF TO cl_abap_zip .
    METHODS update_cachebuster .
    METHODS upload_archive_into_icf
      IMPORTING
        VALUE(archive) TYPE REF TO cl_abap_zip
      RAISING
        cx_sy_create_data_error .
    METHODS xstring_to_string
      IMPORTING
        !in        TYPE xstring
      RETURNING
        VALUE(out) TYPE string .
    METHODS string_to_xstring
      IMPORTING
        !in        TYPE string
      RETURNING
        VALUE(out) TYPE xstring .
ENDCLASS.



CLASS YCL_GITHUB_DEPLOYER IMPLEMENTATION.


  METHOD add_module_to_archive.

    DATA: lo_archive           TYPE REF TO cl_abap_zip,
          ls_file              LIKE LINE OF archive->files,
          lv_content           TYPE xstring,
          lv_cdata             TYPE string,
          lv_preload_namespace TYPE string,
          lt_preload_modules   TYPE tihttpnvp,
          lr_preload_module    TYPE REF TO ihttpnvp,
          lv_component         TYPE string,
          lv_junk              TYPE string ##needed.

    WRITE:/, `Adding module`, info-repository, info-branch, 'to archive'.

    lo_archive = remove_cover_folder( get_module_archive(
        url      = |https://github.com/{ info-repository }/zipball/{ info-branch }|
        user     = me->github_user
        password = me->password ) ).

    IF lo_archive IS NOT BOUND.
      WRITE:/, `Error with module`, info-repository, info-branch COLOR COL_NEGATIVE.
      RETURN.
    ENDIF.

* Get Component-preload namespace
    READ TABLE lo_archive->files INTO ls_file WITH KEY name = 'Component.js'.
    IF sy-subrc = 0.
      lo_archive->get(
        EXPORTING
          name = ls_file-name
        IMPORTING
          content = lv_content
        EXCEPTIONS
          zip_index_error = 1
          zip_decompression_error = 2
          OTHERS = 3 ).
      IF sy-subrc = 0.
        lv_component = xstring_to_string( lv_content ).
        SPLIT lv_component AT `.extend(` INTO lv_junk lv_component.
        IF sy-subrc = 0.
          SHIFT lv_component LEFT DELETING LEADING: `'`, `"`.
          SPLIT lv_component AT `.Component` INTO lv_preload_namespace lv_junk.
          REPLACE ALL OCCURRENCES OF `.` IN lv_preload_namespace WITH `/`.
          WRITE: /, 'Preload component namespace is', lv_preload_namespace.
        ENDIF.
      ENDIF.
    ENDIF.

    LOOP AT lo_archive->files INTO ls_file.
      CLEAR lv_content.
      lo_archive->get(
        EXPORTING
          name = ls_file-name
        IMPORTING
          content = lv_content
        EXCEPTIONS
          zip_index_error = 1
          zip_decompression_error = 2
          OTHERS = 3 ).

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        RETURN.
      ENDIF.

* Minification
      IF enable_minify = abap_true.

        lv_content = minify(
            data_in   = lv_content
            file_type = get_file_type( ls_file-name ) ).

* Collect details for Component.preload - not much point doing this unless minification works
        IF lv_preload_namespace IS NOT INITIAL.
          CASE get_file_type( ls_file-name ).
            WHEN 'JS'.
              APPEND INITIAL LINE TO lt_preload_modules REFERENCE INTO lr_preload_module.
              lr_preload_module->name = |{ lv_preload_namespace }/{ ls_file-name }|.
              lr_preload_module->value = xstring_to_string( lv_content ).
            WHEN 'XML'.
              IF ls_file-name CS '.view.xml'.
                APPEND INITIAL LINE TO lt_preload_modules REFERENCE INTO lr_preload_module.
                lr_preload_module->name = |{ lv_preload_namespace }/{ ls_file-name }|.
                lr_preload_module->value =  me->cleanup_xmldata( xstring_to_string( lv_content ) ).
              ENDIF.
            WHEN 'PROPERTIES'.
              APPEND INITIAL LINE TO lt_preload_modules REFERENCE INTO lr_preload_module.
              lr_preload_module->name = |{ lv_preload_namespace }/{ ls_file-name }|.
              lr_preload_module->value =  me->cleanup_xmldata( xstring_to_string( lv_content ) ).
            WHEN OTHERS.
          ENDCASE.
        ENDIF.
      ENDIF.

      IF info-folder IS NOT INITIAL.
        ls_file-name = |{ info-folder }/{ ls_file-name }|.
      ENDIF.

* Process any Git submodules
      DATA: lt_submodule_info TYPE submodule_info_ttype,
            lr_submodule_info TYPE REF TO submodule_info_type.
      IF ls_file-name = '.gitmodules'.
        get_submodule_info(
          EXPORTING
            iv_submodule_data = lv_content
          IMPORTING
            et_submodule_info = lt_submodule_info ).

        LOOP AT lt_submodule_info REFERENCE INTO lr_submodule_info.
          add_module_to_archive(
            info    = lr_submodule_info->*
            archive = archive ).
        ENDLOOP.
      ENDIF.

      archive->add(
        EXPORTING
          name = ls_file-name
          content = lv_content ).
    ENDLOOP.

* Build Component.preload and add to archive
    IF lv_preload_namespace IS NOT INITIAL AND enable_minify = abap_true.
      LOOP AT lt_preload_modules REFERENCE INTO lr_preload_module.
        REPLACE ALL OCCURRENCES OF `"` IN lr_preload_module->value WITH `\"`.
        IF lv_cdata IS INITIAL.
          lv_cdata = |"{ lr_preload_module->name }":"{ lr_preload_module->value }"|.
        ELSE.
          lv_cdata = |{ lv_cdata },"{ lr_preload_module->name }":"{ lr_preload_module->value }"|.
        ENDIF.
      ENDLOOP.
      lv_cdata =
        |jQuery.sap.registerPreloadedModules(\{| &&
        |"version": "2.0",| &&
        |"name": "{ lv_preload_namespace }/Component-preload",| &&
        |"modules": \{| &&
        lv_cdata &&
        |\}| &&
        |\});|.
      archive->add(
        EXPORTING
          name = 'Component-preload.js'
          content = string_to_xstring( lv_cdata ) ).
    ENDIF.

  ENDMETHOD.                    "add_module_to_archive


  METHOD cleanup_stringdata.

    CONSTANTS: lf  TYPE x LENGTH 4 VALUE '0A',
               cr  TYPE x LENGTH 4 VALUE '0D',
               tab TYPE x LENGTH 4 VALUE '09',
               spc TYPE x LENGTH 4 VALUE '20'.

    FIELD-SYMBOLS: <lf>    TYPE char1,
                   <cr>    TYPE char1,
                   <tab>   TYPE char1,
                   <space> TYPE char1.

    ASSIGN lf   TO <lf>     CASTING.
    ASSIGN cr   TO <cr>     CASTING.
    ASSIGN tab  TO <tab>    CASTING.
    ASSIGN spc  TO <space>  CASTING.

    data_out = data_in.

* Remove line feeds
    REPLACE ALL OCCURRENCES OF <lf> IN data_out WITH ''.
* Remove carriage returns
    REPLACE ALL OCCURRENCES OF <cr> IN data_out WITH ''.
* Replace tabs with space
    REPLACE ALL OCCURRENCES OF <tab> IN data_out WITH <space>.

    IF nogaps = abap_true.
      CONDENSE data_out NO-GAPS.
    ELSE.
      CONDENSE data_out.
    ENDIF.

  ENDMETHOD.                    "minify


  METHOD cleanup_xmldata.

    CONSTANTS: lf  TYPE x LENGTH 4 VALUE '0A',
               cr  TYPE x LENGTH 4 VALUE '0D',
               tab TYPE x LENGTH 4 VALUE '09',
               spc TYPE x LENGTH 4 VALUE '20'.

    FIELD-SYMBOLS: <lf>    TYPE char1,
                   <cr>    TYPE char1,
                   <tab>   TYPE char1,
                   <space> TYPE char1.

    ASSIGN lf   TO <lf>     CASTING.
    ASSIGN cr   TO <cr>     CASTING.
    ASSIGN tab  TO <tab>    CASTING.
    ASSIGN spc  TO <space>  CASTING.

    data_out = data_in.

* Replace tabs with \t
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN data_out WITH '\t'.
* Replace newlines with \n
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN data_out WITH '\n'.
* Remove all spaces that immediately follow a new line - hope there are none inside XML data ;)
    DO.
      REPLACE ALL OCCURRENCES OF `\n ` IN data_out WITH `\n`.
      IF sy-subrc NE 0 OR sy-index > 500. EXIT. ENDIF.
    ENDDO.

* Remove line feeds - should be none left
    REPLACE ALL OCCURRENCES OF <lf> IN data_out WITH ''.
* Remove carriage returns
    REPLACE ALL OCCURRENCES OF <cr> IN data_out WITH ''.

  ENDMETHOD.                    "minify


  METHOD constructor.

    me->repository = repository.
    me->branch = branch.
    me->github_user = github_user.
    me->password = password.
    me->transport = transport.
    me->test_only = test_only.

    DATA: lo_archive TYPE REF TO cl_abap_zip,
          ls_module  TYPE submodule_info_type.

    ls_module-repository = me->repository.
    ls_module-branch = me->branch.

    CREATE OBJECT lo_archive.

    add_module_to_archive(
      info    = ls_module
      archive = lo_archive ).

    save_archive_file( lo_archive ).

    upload_archive_into_icf( lo_archive ).

    update_cachebuster( ).

  ENDMETHOD.                    "constructor


  METHOD deploy.
    DATA: deployer TYPE REF TO ycl_github_deployer.

    CREATE OBJECT deployer
      EXPORTING
        repository  = repository
        branch      = branch
        github_user = github_user
        password    = password
        transport   = transport
        test_only   = test_only.

  ENDMETHOD.                    "deploy


  METHOD get_css_minify_params.

    DATA: lr_param TYPE REF TO ihttpnvp.

    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'type'.
    lr_param->value = 'css'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[advanced]'.
    lr_param->value = 'true'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[aggressiveMerging]'.
    lr_param->value = 'true'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[rebase]'.
    lr_param->value = 'true'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[processImport]'.
    lr_param->value = 'true'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[benchmark]'.
    lr_param->value = 'false'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[compatibility]'.
    lr_param->value = 'false'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[keepBreaks]'.
    lr_param->value = 'false'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[debug]'.
    lr_param->value = 'false'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[inliner]'.
    lr_param->value = ''.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[relativeTo]'.
    lr_param->value = ''.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[root]'.
    lr_param->value = ''.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[target]'.
    lr_param->value = ''.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[keepSpecialComments]'.
    lr_param->value = '*'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[roundingPrecision]'.
    lr_param->value = '2'.

  ENDMETHOD.                    "get_css_minify_params


  METHOD get_file_type.
    DATA: lt_strings   TYPE stringtab,
          lr_extension TYPE REF TO string.

    SPLIT iv_filename AT '.' INTO TABLE lt_strings.

    LOOP AT lt_strings REFERENCE INTO lr_extension. ENDLOOP.
    CHECK sy-subrc = 0.

    TRANSLATE lr_extension->* TO UPPER CASE.

    rv_filetype = lr_extension->*.

  ENDMETHOD.                    "GET_FILE_TYPE


  METHOD get_js_minify_params.
    DATA: lr_param TYPE REF TO ihttpnvp.

    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'type'.
    lr_param->value = 'javascript'.

    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[sequences]'.
    lr_param->value = 'true'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[properties]'.
    lr_param->value = 'true'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[dead_code]'.
    lr_param->value = 'true'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[drop_debugger]'.
    lr_param->value = 'true'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[conditionals]'.
    lr_param->value = 'true'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[comparisons]'.
    lr_param->value = 'true'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[evaluate]'.
    lr_param->value = 'true'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[booleans]'.
    lr_param->value = 'true'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[loops]'.
    lr_param->value = 'true'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[unused]'.
    lr_param->value = 'true'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[hoist_funs]'.
    lr_param->value = 'true'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[if_return]'.
    lr_param->value = 'true'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[join_vars]'.
    lr_param->value = 'true'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[cascade]'.
    lr_param->value = 'true'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[warnings]'.
    lr_param->value = 'true'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[negate_iife]'.
    lr_param->value = 'true'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[unsafe]'.
    lr_param->value = 'false'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[hoist_vars]'.
    lr_param->value = 'false'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[pure_getters]'.
    lr_param->value = 'false'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[drop_console]'.
    lr_param->value = 'false'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[keep_fargs]'.
    lr_param->value = 'false'.
    APPEND INITIAL LINE TO rt_params REFERENCE INTO lr_param.
    lr_param->name = 'options[pure_funcs]'.
    lr_param->value = ''.

  ENDMETHOD.                    "GET_JS_MINIFY_PARAMS


  METHOD get_module_archive.

    DATA: lr_client  TYPE REF TO if_http_client,
          lv_code    TYPE i,
          lv_message TYPE string.

    WRITE:/, `Retrieving archive from`, url.

    cl_http_client=>create_by_url(
      EXPORTING
        url = url
        ssl_id        = 'ANONYM'
      IMPORTING
        client = lr_client
      EXCEPTIONS
        OTHERS = 1 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    IF user IS NOT INITIAL.
      lr_client->authenticate( username = user password = password ).
      lr_client->propertytype_accept_cookie = if_http_client=>co_enabled.
    ENDIF.

    lr_client->send( EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc <> 0.
      lv_message = |http send error|.
      WRITE:/, lv_message COLOR COL_NEGATIVE.
      MESSAGE lv_message TYPE 'E' DISPLAY LIKE 'I'.
      RETURN.
    ENDIF.
    lr_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          " make sure:
          " a) SSL is setup properly in STRUST
          " b) no firewalls
          " check trace file in transaction SMICM
          lv_message = 'HTTP Communication Failure'.        "#EC NOTEXT
        WHEN 2.
          lv_message = 'HTTP Invalid State'.                "#EC NOTEXT
        WHEN 3.
          lv_message = 'HTTP Processing failed'.            "#EC NOTEXT
        WHEN OTHERS.
          lv_message = 'Another error occured'.             "#EC NOTEXT
      ENDCASE.
      WRITE:/, lv_message COLOR COL_NEGATIVE.
      MESSAGE lv_message TYPE 'E' DISPLAY LIKE 'I'.
      RETURN.
    ENDIF.

    lr_client->response->get_status( IMPORTING code = lv_code ).

    IF lv_code NE 200.
      IF lv_message IS INITIAL.
        lv_message = |HTTP Return Code is { lv_code }|.
      ENDIF.
      MESSAGE lv_message TYPE 'E' DISPLAY LIKE 'I'.
      RETURN.
    ENDIF.

    CREATE OBJECT archive.

    archive->load(
      EXPORTING
            zip = lr_client->response->get_data( )
      EXCEPTIONS
        zip_parse_error = 1
        OTHERS = 2 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

  ENDMETHOD.                    "get_archive


  METHOD get_submodule_info.

    DATA: lv_gitmodules TYPE string,
          lt_gitmodules TYPE stringtab,
          lr_string     TYPE REF TO string,
          lv_junk1      TYPE string ##needed,
          lv_junk2      TYPE string ##needed,
          lr_url        TYPE REF TO string,
          lr_path       TYPE REF TO string ##needed,
          ls_submodule  TYPE submodule_info_type.

    CONSTANTS: lf  TYPE x LENGTH 4 VALUE '0A00'.
    FIELD-SYMBOLS: <lf> TYPE char1.
    ASSIGN lf TO <lf> CASTING.

    lv_gitmodules = xstring_to_string( iv_submodule_data ).

    SPLIT lv_gitmodules AT <lf> INTO TABLE lt_gitmodules.

    LOOP AT lt_gitmodules REFERENCE INTO lr_string.
      lr_string->* = cleanup_stringdata( data_in = lr_string->* nogaps = abap_true ).
      CHECK lr_string->* CS 'submodule'.
      CLEAR ls_submodule.
      SPLIT lr_string->* AT '"' INTO lv_junk1 ls_submodule-folder lv_junk2.
      READ TABLE lt_gitmodules REFERENCE INTO lr_path INDEX ( sy-tabix + 1 ).
      CHECK sy-subrc = 0.
      ls_submodule-branch = 'master'. "For submodules we always grab master branch
      READ TABLE lt_gitmodules REFERENCE INTO lr_url INDEX ( sy-tabix + 1 ).
      CHECK sy-subrc = 0.
      SPLIT lr_url->* AT `url = https://github.com/` INTO lv_junk1 ls_submodule-repository.
      APPEND ls_submodule TO et_submodule_info.
    ENDLOOP.

  ENDMETHOD.                    "GET_SUBMODULE_INFO


  METHOD minify.

    CASE file_type.
      WHEN 'JS'.
        data_out = minify_js( data_in ).
      WHEN 'CSS'.
        data_out = minify_css( data_in ).
      WHEN OTHERS.
    ENDCASE.

    IF data_out IS INITIAL. "If minification failed just return unchanged data
      data_out = data_in.
    ENDIF.

* Increment counters
    ADD 1 TO minify_file_count.
    minify_byte_count = minify_byte_count + xstrlen( data_in ) - xstrlen( data_out ).

  ENDMETHOD.                    "minify


  METHOD minify_css.

    DATA: lr_client  TYPE REF TO if_http_client,
          lv_code    TYPE i,
          lt_ffields TYPE tihttpnvp,
          lr_ffield  TYPE REF TO ihttpnvp.

    DATA: BEGIN OF json_response,
            code TYPE string,
            map  TYPE string,
          END OF json_response.

    cl_http_client=>create_by_url(
      EXPORTING
        url = 'http://refresh-sf.herokuapp.com/css/' "See http://refresh-sf.com/ for details
        ssl_id = 'ANONYM'
      IMPORTING
        client = lr_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active = 2
        internal_error = 3
        OTHERS = 4 ).

    IF sy-subrc = 0.
      lt_ffields = get_css_minify_params( ).

      APPEND INITIAL LINE TO lt_ffields REFERENCE INTO lr_ffield.
      lr_ffield->name = 'code'.
      lr_ffield->value = xstring_to_string( data_in ).

      lr_client->request->set_form_fields( lt_ffields ).
      lr_client->request->set_method( if_http_request=>co_request_method_post ).

      lr_client->send(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          http_invalid_timeout       = 4
          OTHERS                     = 5 ).
      IF sy-subrc <> 0.
        enable_minify = abap_false.
      ELSE.
        lr_client->receive(
          EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state         = 2
            http_processing_failed     = 3
            OTHERS                     = 4 ).
        IF sy-subrc <> 0.
          enable_minify = abap_false.
        ELSE.
          lr_client->response->get_status( IMPORTING code = lv_code ).
        ENDIF.
      ENDIF.

      IF lv_code EQ 200.
        /ui2/cl_json=>deserialize(
          EXPORTING json = lr_client->response->get_cdata( )
            pretty_name = /ui2/cl_json=>pretty_mode-camel_case
          CHANGING
            data =  json_response ).
        data_out = string_to_xstring( json_response-code ).
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "minify


  METHOD minify_js.

    DATA: lr_client  TYPE REF TO if_http_client,
          lv_code    TYPE i,
          lt_ffields TYPE tihttpnvp,
          lr_ffield  TYPE REF TO ihttpnvp.

    DATA:
      BEGIN OF json_response,
        code TYPE string,
        map  TYPE string,
      END OF json_response.

    cl_http_client=>create_by_url(
      EXPORTING
        url = 'http://refresh-sf.herokuapp.com/javascript/' "See http://refresh-sf.com/ for details
        ssl_id        = 'ANONYM'
      IMPORTING
        client = lr_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active = 2
        internal_error = 3
        OTHERS = 4 ).

    IF sy-subrc = 0.
      lt_ffields = get_js_minify_params( ).

      APPEND INITIAL LINE TO lt_ffields REFERENCE INTO lr_ffield.
      lr_ffield->name = 'code'.
      lr_ffield->value = xstring_to_string( data_in ).

      lr_client->request->set_form_fields( lt_ffields ).
      lr_client->request->set_method( if_http_request=>co_request_method_post ).

      lr_client->send(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          http_invalid_timeout       = 4
          OTHERS                     = 5 ).
      IF sy-subrc <> 0.
        enable_minify = abap_false.
      ELSE.
        lr_client->receive(
          EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state         = 2
            http_processing_failed     = 3
            OTHERS                     = 4 ).
        IF sy-subrc <> 0.
          enable_minify = abap_false.
        ELSE.
          lr_client->response->get_status( IMPORTING code = lv_code ).
        ENDIF.
      ENDIF.

      IF lv_code EQ 200.
        /ui2/cl_json=>deserialize(
          EXPORTING json = lr_client->response->get_cdata( )
            pretty_name = /ui2/cl_json=>pretty_mode-camel_case
          CHANGING
            data =  json_response ).
        data_out = string_to_xstring( json_response-code ).
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "minify


  METHOD remove_cover_folder.

    DATA: ls_file    LIKE LINE OF archive->files,
          lv_content TYPE xstring,
          lv_dummy   TYPE string ##needed.

    CHECK archive IS BOUND.

    IF lines( archive->files ) = 0.
      WRITE:/, 'No files in archive'.
      RETURN.
    ENDIF.

    CREATE OBJECT new_archive.

    LOOP AT archive->files INTO ls_file.
      CLEAR lv_content.
      archive->get(
        EXPORTING
          name = ls_file-name
        IMPORTING
          content = lv_content
        EXCEPTIONS
          zip_index_error = 1
          zip_decompression_error = 2
          OTHERS = 3 ).

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        RETURN.
      ENDIF.

      IF ls_file-name CA '/'.
        SPLIT ls_file-name AT '/' INTO lv_dummy ls_file-name.
      ELSE.
        SPLIT ls_file-name AT '\' INTO lv_dummy ls_file-name.
      ENDIF.
      IF ls_file-name IS INITIAL.
        CONTINUE.
      ENDIF.
      new_archive->add(
        EXPORTING
          name = ls_file-name
          content = lv_content ).
    ENDLOOP.

  ENDMETHOD.                    "remove_cover_folder


  METHOD save_archive_file.

    DATA: lv_content  TYPE xstring,
          fullpath    TYPE string,
          filename    TYPE string,
          path        TYPE string,
          user_action TYPE i,
          encoding    TYPE abap_encoding,
          length      TYPE i ##needed.

    DATA: BEGIN OF line_bin,
            data(1024) TYPE x,
          END OF line_bin.
    DATA: data_tab_bin LIKE STANDARD TABLE OF line_bin.

    CHECK test_only = abap_true.

    lv_content = archive->save( ).

* Binary download table
    WHILE xstrlen( lv_content ) > 0.
      line_bin-data = lv_content.
      APPEND line_bin TO data_tab_bin.
      SHIFT lv_content LEFT BY 1024 PLACES IN BYTE MODE.
    ENDWHILE.

* Get filename
    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        window_title         = 'Download Archive'
        with_encoding        = 'X'
        initial_directory    = 'C:\'
      CHANGING
        filename             = filename
        path                 = path
        fullpath             = fullpath
        user_action          = user_action
        file_encoding        = encoding
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF user_action <> cl_gui_frontend_services=>action_ok.
      RETURN.
    ENDIF.

* Binary download
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = fullpath
        filetype                = 'BIN'
      IMPORTING
        filelength              = length
      TABLES
        data_tab                = data_tab_bin
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
        OTHERS                  = 22 ##FM_SUBRC_OK.

  ENDMETHOD.                    "save_zip_file


  METHOD string_to_xstring.

*    CALL FUNCTION 'HR_KR_STRING_TO_XSTRING'
*      EXPORTING
**       CODEPAGE_TO      = '8500'
*        unicode_string   = in
**       OUT_LEN          = OUT_LEN
*      IMPORTING
*        xstring_stream   = out
*      EXCEPTIONS
*        invalid_codepage = 1
*        invalid_string   = 2
*        OTHERS           = 3 ##FM_SUBRC_OK.


    DATA(lv_conv) = cl_abap_conv_out_ce=>create( ).

    CALL METHOD lv_conv->write
      EXPORTING
        n    = -1
        data = in
      IMPORTING
        len  = DATA(lv_length).


    CALL METHOD lv_conv->get_buffer
      RECEIVING
        buffer = out.

  ENDMETHOD.                    "STRING_TO_XSTRING


  METHOD update_cachebuster.

    DATA: lv_jobnumber TYPE          tbtcjob-jobcount,
          lv_jobname   TYPE          tbtcjob-jobname VALUE 'UI5_CACHEBUSTER'.

    CHECK test_only NE abap_true.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = lv_jobname
      IMPORTING
        jobcount         = lv_jobnumber
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.

    CHECK sy-subrc = 0.

    SUBMIT /ui5/app_index_calculate "/ui5/update_cachebuster
      VIA JOB lv_jobname NUMBER lv_jobnumber AND RETURN.

    CHECK sy-subrc = 0.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = lv_jobnumber
        jobname              = lv_jobname
        strtimmed            = 'X'
      EXCEPTIONS
        cant_start_immediate = 1
        invalid_startdate    = 2
        jobname_missing      = 3
        job_close_failed     = 4
        job_nosteps          = 5
        job_notex            = 6
        lock_failed          = 7
        OTHERS               = 8 ##FM_SUBRC_OK.


  ENDMETHOD.


  METHOD upload_archive_into_icf.

    DATA: lv_zip_file TYPE xstring.
* Messages
    DATA: lv_success      TYPE char1,
          lt_log_messages TYPE string_table,
          lv_message      TYPE string,
          lv_msg_id       TYPE sy-msgid,
          lv_msg_no       TYPE sy-msgno.

    CHECK archive->files IS NOT INITIAL.

    lv_message = |Archive contains { lines( archive->files ) } folders & files|.
    WRITE:/, lv_message.
    IF minify_file_count > 0.
      lv_message = |{ minify_file_count } files minified - { minify_byte_count } bytes removed|.
      WRITE:/, lv_message.
    ENDIF.

    WRITE:/, `Loading archive.....`.

    lv_zip_file = archive->save( ).

    CALL FUNCTION 'YUI5_REPOSITORY_LOAD_HTTP'
      EXPORTING
        iv_url               = ''
*       IV_SAPUI5_APPLICATION_NAME = ''
*       IV_SAPUI5_APPLICATION_DESC = ''
*       IV_PACKAGE           = ''
        iv_workbench_request = me->transport
*       IV_EXTERNAL_CODE_PAGE      = ''
*       IV_ACCEPT_UNIX_STYLE_EOL   = 'X'
*       IV_DELTA_MODE        = '-'
        iv_test_mode         = me->test_only
        iv_zip_file          = lv_zip_file
      IMPORTING
        ev_success           = lv_success
        ev_log_messages      = lt_log_messages
        ev_msg_id            = lv_msg_id
        ev_msg_no            = lv_msg_no.

    MESSAGE ID lv_msg_id TYPE lv_success NUMBER lv_msg_no INTO lv_message.
    LOOP AT lt_log_messages INTO lv_message.
      WRITE:/, lv_message.
    ENDLOOP.
    CASE lv_success.
      WHEN 'S'.
        WRITE:/, 'Success' COLOR COL_POSITIVE, lv_message COLOR COL_POSITIVE.
      WHEN 'W'.
        WRITE:/, 'Warning' COLOR COL_GROUP, lv_message COLOR COL_GROUP.
      WHEN OTHERS.
        WRITE:/, 'Error' COLOR COL_NEGATIVE, lv_message COLOR COL_NEGATIVE.
        RAISE EXCEPTION TYPE cx_sy_create_data_error "Just any old exception to signal failure
          EXPORTING
            textid = cx_sy_create_data_error=>generic_type
*           previous = previous
*           typename = typename
          .
    ENDCASE.

  ENDMETHOD.                    "load_archive


  METHOD xstring_to_string.

*    CALL FUNCTION 'HR_KR_XSTRING_TO_STRING'
*      EXPORTING
**       FROM_CODEPAGE = '8500'
*        in_xstring = in
**       OUT_LEN    = OUT_LEN
*      IMPORTING
*        out_string = out.

    DATA lo_conv TYPE REF TO cl_abap_conv_in_ce.

    CALL METHOD cl_abap_conv_in_ce=>create
      EXPORTING
        encoding    = 'UTF-8'
        endian      = 'L'
        ignore_cerr = 'X'
        replacement = '#'
        input       = in
      RECEIVING
        conv        = lo_conv.

    CALL METHOD lo_conv->read
      IMPORTING
        data = out.

  ENDMETHOD.                    "XSTRING_TO_STRING
ENDCLASS.
