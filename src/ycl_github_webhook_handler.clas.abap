class YCL_GITHUB_WEBHOOK_HANDLER definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.

  types:
    BEGIN OF user_profile_plan_type,
          name TYPE string,
          space TYPE string,
          collaborators TYPE string,
          private_repos TYPE string,
        END OF user_profile_plan_type .
  types:
    BEGIN OF user_profile_type,
            login TYPE string,
            id TYPE string,
            avatar_url TYPE string,
            gravatar_id TYPE string,
            url TYPE string,
            html_url TYPE string,
            followers_url TYPE string,
            following_url TYPE string,
            gists_url TYPE string,
            starred_url TYPE string,
            subscriptions_url TYPE string,
            organizations_url TYPE string,
            repos_url TYPE string,
            events_url TYPE string,
            received_events_url TYPE string,
            type TYPE string,
            site_admin TYPE string,
            name TYPE string,
            company TYPE string,
            blog TYPE string,
            location TYPE string,
            email TYPE string,
            hireable TYPE string,
            bio TYPE string,
            public_repos TYPE string,
            public_gists TYPE string,
            followers TYPE string,
            following TYPE string,
            created_at TYPE string,
            updated_at TYPE string,
            private_gists TYPE string,
            total_private_repos TYPE string,
            owned_private_repos TYPE string,
            disk_usage TYPE string,
            collaborators TYPE string,
            plan TYPE user_profile_plan_type,
          END OF user_profile_type .
  types:
    BEGIN OF github_permissions_type,
          admin TYPE boolean,
          push TYPE boolean,
          pull TYPE boolean,
        END OF github_permissions_type .
  types:
    BEGIN OF github_owner_type,
          login TYPE string,
          id TYPE i,
          avatar_url TYPE string,
          gravatar_id TYPE string,
          url TYPE string,
          html_url TYPE string,
          followers_url TYPE string,
          following_url TYPE string,
          gists_url TYPE string,
          starred_url TYPE string,
          subscriptions_url TYPE string,
          organizations_url TYPE string,
          repos_url TYPE string,
          events_url TYPE string,
          received_events_url TYPE string,
          type TYPE string,
          site_admin TYPE boolean,
          name type string,
          email type string,
          username type string,
        END OF github_owner_type .
  types:
    BEGIN OF github_organization_type,
        login TYPE string,
        id TYPE i,
        description type string,
        avatar_url TYPE string,
        url TYPE string,
        html_url TYPE string,
        members_url type string,
        followers_url TYPE string,
        following_url TYPE string,
        public_members_url type string,
        gists_url TYPE string,
        starred_url TYPE string,
        subscriptions_url TYPE string,
        organizations_url TYPE string,
        repos_url TYPE string,
        events_url TYPE string,
        received_events_url TYPE string,
        type TYPE string,
        site_admin TYPE boolean,
      END OF github_organization_type .
  types:
    BEGIN OF github_repository_type,
          id TYPE i,
          name TYPE string,
          full_name TYPE string,
          owner TYPE github_owner_type,
          private TYPE boolean,
          html_url TYPE string,
          description TYPE string,
          fork TYPE boolean,
          url TYPE string,
          forks_url TYPE string,
          keys_url TYPE string,
          collaborators_url TYPE string,
          teams_url TYPE string,
          hooks_url TYPE string,
          issue_events_url TYPE string,
          events_url TYPE string,
          assignees_url TYPE string,
          branches_url TYPE string,
          tags_url TYPE string,
          blobs_url TYPE string,
          git_tags_url TYPE string,
          git_refs_url TYPE string,
          trees_url TYPE string,
          statuses_url TYPE string,
          languages_url TYPE string,
          stargazers_url TYPE string,
          contributors_url TYPE string,
          subscribers_url TYPE string,
          subscription_url TYPE string,
          commits_url TYPE string,
          git_commits_url TYPE string,
          comments_url TYPE string,
          issue_comment_url TYPE string,
          contents_url TYPE string,
          compare_url TYPE string,
          merges_url TYPE string,
          archive_url TYPE string,
          downloads_url TYPE string,
          issues_url TYPE string,
          pulls_url TYPE string,
          milestones_url TYPE string,
          notifications_url TYPE string,
          labels_url TYPE string,
          releases_url TYPE string,
          created_at TYPE string,
          updated_at TYPE string,
          pushed_at TYPE string,
          git_url TYPE string,
          ssh_url TYPE string,
          clone_url TYPE string,
          svn_url TYPE string,
          homepage TYPE string,
          size TYPE i,
          stargazers_count TYPE i,
          watchers_count TYPE i,
          language TYPE string,
          has_issues TYPE boolean,
          has_downloads TYPE boolean,
          has_wiki TYPE boolean,
          has_pages TYPE boolean,
          forks_count TYPE i,
          mirror_url TYPE string,
          open_issues_count TYPE i,
          forks TYPE i,
          open_issues TYPE i,
          watchers TYPE i,
          startgazers type i,
          default_branch TYPE string,
          master_branch type string,
          permissions TYPE github_permissions_type,
          organization TYPE github_organization_type,
          network_count TYPE i,
          subscribers_count TYPE i,
      END OF github_repository_type .
  types:
    github_repository_ttype TYPE TABLE OF github_repository_type .
  types:
    begin of github_commit_type,
        id type string,
        distinct type boolean,
        message type string,
        timestamp type string,
        url type string,
        author type github_owner_type,
        committer type github_owner_type,
        added type stringtab,
        removed type stringtab,
        modified type stringtab,
      end of github_commit_type .
  types:
    github_commit_ttype type table of github_commit_type .
  types:
    begin of github_push_type,
      ref type string,
      before type string,
      after type string,
      created type boolean,
      deleted type boolean,
      forced type boolean,
      base_ref type string,
      compare type string,
      "commits type table of github_commit_type,
      head_commit type github_commit_type,
      repository type github_repository_type,
      pusher type github_owner_type,
      organization type github_organization_type,
      sender type github_owner_type,
      end of github_push_type .

  data MT_GITHUB_FIELDS type TIHTTPNVP .

  methods READ_GITHUB_FIELDS
    importing
      !SERVER type ref to IF_HTTP_SERVER .
  methods GET_COMMIT_DETAILS
    importing
      !SERVER type ref to IF_HTTP_SERVER
    returning
      value(PUSH) type GITHUB_PUSH_TYPE .
ENDCLASS.



CLASS YCL_GITHUB_WEBHOOK_HANDLER IMPLEMENTATION.


  METHOD get_commit_details.
    DATA(payload) = server->request->get_cdata( ).
    SPLIT payload AT 'payload=' INTO DATA(junk) payload.

    /ui2/cl_json=>deserialize(
      EXPORTING json = cl_http_utility=>unescape_url( payload )
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data =  push ).

  ENDMETHOD.


  METHOD if_http_extension~handle_request.
    DATA: ls_push      TYPE github_push_type,
          lt_stringtab TYPE stringtab,
          ls_string    TYPE string,
          lt_rfcdest   TYPE STANDARD TABLE OF rfcdest,
          lr_rfcdest   TYPE REF TO rfcdest.

    ls_push = get_commit_details( server ).

    CASE ls_push-repository-full_name.
      WHEN 'grahamrobbo/demojam'.

      WHEN OTHERS.
        server->response->set_status( code = 404 reason = |Not found| ).
        RETURN.
    ENDCASE.

    SPLIT ls_push-ref AT '/' INTO TABLE lt_stringtab.
    LOOP AT lt_stringtab INTO ls_string. ENDLOOP.
    IF ls_string NE ls_push-repository-master_branch. "Only process master branch
      server->response->set_status( code = 403 reason = |Forbidden| ).
    ELSE.
      CALL FUNCTION 'ZGITHUBDEPLOY'
        EXPORTING
          repository            = ls_push-repository-full_name
          branch                = ls_push-repository-master_branch
*         github_user           = 'github_user'
*         password              = 'github_password'
        EXCEPTIONS
          system_failure        = 1
          communication_failure = 2
          OTHERS                = 3.
    ENDIF.

  ENDMETHOD.


  METHOD read_github_fields.

    DATA: lt_fields TYPE tihttpnvp,
          ls_fields LIKE LINE OF lt_fields,
          lv_json   TYPE xstring,
          lr_conv   TYPE REF TO cl_abap_conv_out_ce.

* Json Processing variables.
    DATA: lr_reader     TYPE REF TO if_sxml_reader,
          lr_node       TYPE REF TO if_sxml_node,
          lr_value_node TYPE REF TO if_sxml_value_node,
          lr_open_elem  TYPE REF TO if_sxml_open_element,
          lt_attribs    TYPE if_sxml_attribute=>attributes,
          ls_attrib     LIKE LINE OF lt_attribs.

    DATA: parse_error TYPE REF TO cx_sxml_parse_error.

    server->request->get_form_fields_cs(
* EXPORTING
* formfield_encoding = 0 " CO_FORMFIELD_ENCODING_RAW / _ENCODED
* search_option = 3 " CO_BODY_BEFORE_QUERY_STRING or others
      CHANGING
        fields = lt_fields ). " Form fields

    READ TABLE lt_fields INTO ls_fields INDEX 1.
* convert to xstring
    lr_conv = cl_abap_conv_out_ce=>create( ).
    lr_conv->convert(
      EXPORTING
        data = ls_fields-value " Field to Be Converted
* n = - 1 " Number of Units to Be Converted
      IMPORTING
        buffer = lv_json " Converted Data
* len = " Number of Converted Units
    ).
* CATCH cx_sy_codepage_converter_init. "
* CATCH cx_sy_conversion_codepage. "
* CATCH cx_parameter_invalid_type. "
    CLEAR me->mt_github_fields.

    IF lv_json IS INITIAL.
      lv_json = server->request->get_data( ).
    ENDIF.
* get the reader
    lr_reader = cl_sxml_string_reader=>create( lv_json ).
* Parse in a do loop.
    TRY.
        DO.
          CLEAR lr_node.
          lr_node = lr_reader->read_next_node( ).
          IF lr_node IS INITIAL.
            EXIT.
          ENDIF.
          CASE lr_node->type.
            WHEN if_sxml_node=>co_nt_element_open.
              CLEAR ls_fields.
              lr_open_elem ?= lr_node.
              ls_fields-name = lr_open_elem->qname-name.
              IF ls_fields-name EQ 'str'.
                " read the name attribute for getting the name of the json value
                lt_attribs = lr_open_elem->get_attributes( ).
                LOOP AT lt_attribs INTO ls_attrib.
                  IF ls_attrib->qname-name = 'name'.
                    ls_fields-name = ls_attrib->get_value( ).
                  ENDIF.
                ENDLOOP.
              ELSE.
                CLEAR ls_fields.
              ENDIF.
            WHEN if_sxml_node=>co_nt_value.
* node value.
              IF ls_fields IS INITIAL.
                CONTINUE.
              ENDIF.
              lr_value_node ?= lr_node.
              ls_fields-value = lr_value_node->get_value( ).
              APPEND ls_fields TO me->mt_github_fields.
            WHEN OTHERS.
          ENDCASE.
        ENDDO.
      CATCH cx_sxml_parse_error INTO parse_error.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
