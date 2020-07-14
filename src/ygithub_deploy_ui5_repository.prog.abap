*&---------------------------------------------------------------------*
*& Report YGITHUB_DEPLOY_UI5_REPOSITORY
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ygithub_deploy_ui5_repository line-SIZE 120.

DATA: exc TYPE REF TO cx_root.

SELECTION-SCREEN BEGIN OF BLOCK blk_main.
PARAMETERS:
  git_repo TYPE string OBLIGATORY LOWER CASE,
  branch   TYPE string OBLIGATORY LOWER CASE DEFAULT `master`,
  username TYPE string VISIBLE LENGTH 15 LOWER CASE,
  password TYPE string VISIBLE LENGTH 15 LOWER CASE,
  tranport TYPE trkorr,
  test     TYPE boolean DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK blk_main.

START-OF-SELECTION.

  TRY.
      ycl_github_deployer=>deploy(
            repository  = git_repo
            branch      = branch
            github_user = username
            password    = password
            transport   = tranport
            test_only   = test ).
    CATCH cx_root INTO exc.
      MESSAGE exc TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
