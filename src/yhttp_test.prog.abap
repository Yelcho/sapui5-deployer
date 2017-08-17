*&---------------------------------------------------------------------*
*& Report YHTTP_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT yhttp_test.
*--------------------------------------------------------------------*
* Simple test to ensure we can talk to GitHub.
*--------------------------------------------------------------------*

DATA: lr_client  TYPE REF TO if_http_client.

cl_http_client=>create_by_url(
  EXPORTING
    url           = 'https://github.com/'
    ssl_id        = 'ANONYM'
  IMPORTING
    client        = lr_client
  EXCEPTIONS
    argument_not_found = 1
    plugin_not_active = 2
    internal_error = 3
    OTHERS = 4 ).
IF sy-subrc <> 0.
  CASE sy-subrc.
    WHEN 1.
      " make sure:
      " a) SSL is setup properly in STRUST
      DATA(lv_message) = 'HTTPS ARGUMENT_NOT_FOUND | STRUST/SSL Setup correct?'. "#EC NOTEXT
    WHEN OTHERS.
      lv_message = 'While creating HTTP Client'.            "#EC NOTEXT

  ENDCASE.
  WRITE:/, lv_message COLOR COL_NEGATIVE.
  MESSAGE lv_message TYPE 'E' DISPLAY LIKE 'I'.
  RETURN.
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
      lv_message = 'HTTP Communication Failure'.            "#EC NOTEXT
    WHEN 2.
      lv_message = 'HTTP Invalid State'.                    "#EC NOTEXT
    WHEN 3.
      lv_message = 'HTTP Processing failed'.                "#EC NOTEXT
    WHEN OTHERS.
      lv_message = 'Another error occured'.                 "#EC NOTEXT
  ENDCASE.
  WRITE:/, lv_message COLOR COL_NEGATIVE.
  MESSAGE lv_message TYPE 'E' DISPLAY LIKE 'I'.
  RETURN.
ENDIF.

lr_client->response->get_status( IMPORTING code = DATA(lv_code) ).

IF lv_code NE 200.
  IF lv_message IS INITIAL.
    lv_message = |HTTP Return Code is { lv_code }|.         "#EC NOTEXT
  ENDIF.
  MESSAGE lv_message TYPE 'E' DISPLAY LIKE 'I'.
ELSE.
  MESSAGE |All okay| TYPE 'I' DISPLAY LIKE 'I'.
ENDIF.
