*&---------------------------------------------------------------------*
*& Report YGITHUB_DEPLOY_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ygithub_deploy_test.

*--------------------------------------------------------------------*
* This program tests deployment of a GitHub SAPUI5 repository
*
* Check https://github.com/grahamrobbo/demojam to see the repo
* that will be deployted. This is a public repo so no user
* credentials are required.
*
* Note in the .Ui5RepositoryUploadParameters file where the settings
* for this app are defined.
*
* SAPUI5ApplicationName=ZDEMOJAM
* SAPUI5ApplicationPackage=$TMP
* SAPUI5ApplicationDescription=Simple SAPUI5 App
* ExternalCodePage=Cp1252
* AcceptUnixStyleEol=True
* DeltaMode=False
*
* If you want to change these parameters, for example to give the app
* a different name, clone the repo and make changes in your own copy.
*--------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK blk_main.
PARAMETERS:
  user     TYPE string LOWER CASE
    DEFAULT ``,
  password TYPE string LOWER CASE
    DEFAULT ``,
  repo     TYPE string OBLIGATORY LOWER CASE
    DEFAULT `grahamrobbo/demojam`,
  branch   TYPE string OBLIGATORY LOWER CASE
    DEFAULT `master`,
  test     TYPE boolean DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK blk_main.

START-OF-SELECTION.

  TRY.
      ycl_github_deployer=>deploy(
            github_user = user
            password    = password
            repository  = repo
            branch      = branch
            test_only   = test ).
    CATCH cx_root INTO DATA(exc).
      MESSAGE exc TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
