"! <p class="shorttext synchronized">VSP RFC Service</p>
"! Enables dynamic RFC/BAPI calls via WebSocket.
"! Actions: call, search, getMetadata, ping, moveToPackage
CLASS zcl_vsp_rfc_service DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_vsp_service.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_param_info,
        parameter  TYPE rs38l_par_,
        kind       TYPE rs38l_kind,
        typ        TYPE string,
        dbfield    TYPE string,
        optional   TYPE rs38l_opti,
      END OF ty_param_info,
      tt_param_info TYPE STANDARD TABLE OF ty_param_info WITH KEY parameter.

    METHODS handle_call
      IMPORTING is_message         TYPE zif_vsp_service=>ty_message
      RETURNING VALUE(rs_response) TYPE zif_vsp_service=>ty_response.

    METHODS handle_search
      IMPORTING is_message         TYPE zif_vsp_service=>ty_message
      RETURNING VALUE(rs_response) TYPE zif_vsp_service=>ty_response.

    METHODS handle_get_metadata
      IMPORTING is_message         TYPE zif_vsp_service=>ty_message
      RETURNING VALUE(rs_response) TYPE zif_vsp_service=>ty_response.

    METHODS handle_ping
      IMPORTING is_message         TYPE zif_vsp_service=>ty_message
      RETURNING VALUE(rs_response) TYPE zif_vsp_service=>ty_response.

    METHODS handle_move_to_package
      IMPORTING is_message         TYPE zif_vsp_service=>ty_message
      RETURNING VALUE(rs_response) TYPE zif_vsp_service=>ty_response.

    METHODS get_func_interface
      IMPORTING iv_function       TYPE rs38l_fnam
      EXPORTING et_import         TYPE tt_param_info
                et_export         TYPE tt_param_info
                et_changing       TYPE tt_param_info
                et_tables         TYPE tt_param_info
      RETURNING VALUE(rv_success) TYPE abap_bool.

    METHODS create_param_data
      IMPORTING is_param       TYPE ty_param_info
      RETURNING VALUE(ro_data) TYPE REF TO data.

    METHODS create_table_data
      IMPORTING is_param       TYPE ty_param_info
      RETURNING VALUE(ro_data) TYPE REF TO data.

ENDCLASS.


CLASS zcl_vsp_rfc_service IMPLEMENTATION.

  METHOD zif_vsp_service~get_domain.
    rv_domain = 'rfc'.
  ENDMETHOD.

  METHOD zif_vsp_service~handle_message.
    CASE is_message-action.
      WHEN 'call'.
        rs_response = handle_call( is_message ).
      WHEN 'search'.
        rs_response = handle_search( is_message ).
      WHEN 'getMetadata'.
        rs_response = handle_get_metadata( is_message ).
      WHEN 'ping'.
        rs_response = handle_ping( is_message ).
      WHEN 'moveToPackage'.
        rs_response = handle_move_to_package( is_message ).
      WHEN OTHERS.
        rs_response = zcl_vsp_utils=>build_error(
          iv_id      = is_message-id
          iv_code    = 'UNKNOWN_ACTION'
          iv_message = 'Action not supported'
        ).
    ENDCASE.
  ENDMETHOD.

  METHOD zif_vsp_service~on_disconnect.
  ENDMETHOD.

  METHOD handle_move_to_package.
    " Extract parameters: pgmid, object, obj_name, new_package
    DATA(lv_pgmid) = zcl_vsp_utils=>extract_param( iv_params = is_message-params iv_name = 'pgmid' ).
    DATA(lv_object) = zcl_vsp_utils=>extract_param( iv_params = is_message-params iv_name = 'object' ).
    DATA(lv_obj_name) = zcl_vsp_utils=>extract_param( iv_params = is_message-params iv_name = 'obj_name' ).
    DATA(lv_new_pkg) = zcl_vsp_utils=>extract_param( iv_params = is_message-params iv_name = 'new_package' ).

    " Validate required params
    IF lv_object IS INITIAL.
      rs_response = zcl_vsp_utils=>build_error( iv_id = is_message-id iv_code = 'MISSING_PARAM' iv_message = 'Parameter object is required (e.g., CLAS, PROG, INTF, SAPC)' ).
      RETURN.
    ENDIF.
    IF lv_obj_name IS INITIAL.
      rs_response = zcl_vsp_utils=>build_error( iv_id = is_message-id iv_code = 'MISSING_PARAM' iv_message = 'Parameter obj_name is required' ).
      RETURN.
    ENDIF.
    IF lv_new_pkg IS INITIAL.
      rs_response = zcl_vsp_utils=>build_error( iv_id = is_message-id iv_code = 'MISSING_PARAM' iv_message = 'Parameter new_package is required' ).
      RETURN.
    ENDIF.

    " Default pgmid to R3TR
    IF lv_pgmid IS INITIAL.
      lv_pgmid = 'R3TR'.
    ENDIF.

    " Uppercase all values
    TRANSLATE lv_pgmid TO UPPER CASE.
    TRANSLATE lv_object TO UPPER CASE.
    TRANSLATE lv_obj_name TO UPPER CASE.
    TRANSLATE lv_new_pkg TO UPPER CASE.

    " Call ZADT_CL_TADIR_MOVE to perform the move
    DATA lv_result TYPE string.
    TRY.
        " Note: Use move_object (not move_object_and_commit) because
        " COMMIT WORK is not allowed in APC/WebSocket context.
        " The change persists via TR_TADIR_INTERFACE's internal handling.
        lv_result = zadt_cl_tadir_move=>move_object(
          iv_pgmid    = CONV #( lv_pgmid )
          iv_object   = CONV #( lv_object )
          iv_obj_name = CONV #( lv_obj_name )
          iv_new_pkg  = CONV #( lv_new_pkg )
        ).
      CATCH cx_root INTO DATA(lx_error).
        rs_response = zcl_vsp_utils=>build_error( iv_id = is_message-id iv_code = 'MOVE_ERROR' iv_message = lx_error->get_text( ) ).
        RETURN.
    ENDTRY.

    " Build response using json helpers
    DATA(lv_success_bool) = xsdbool( lv_result CP 'SUCCESS*' ).
    DATA(lv_data) = zcl_vsp_utils=>json_obj( zcl_vsp_utils=>json_join( VALUE #(
      ( zcl_vsp_utils=>json_bool( iv_key = 'success' iv_value = lv_success_bool ) )
      ( zcl_vsp_utils=>json_str( iv_key = 'pgmid' iv_value = lv_pgmid ) )
      ( zcl_vsp_utils=>json_str( iv_key = 'object' iv_value = lv_object ) )
      ( zcl_vsp_utils=>json_str( iv_key = 'obj_name' iv_value = lv_obj_name ) )
      ( zcl_vsp_utils=>json_str( iv_key = 'new_package' iv_value = lv_new_pkg ) )
      ( zcl_vsp_utils=>json_str( iv_key = 'message' iv_value = lv_result ) )
    ) ) ).

    rs_response = zcl_vsp_utils=>build_success( iv_id = is_message-id iv_data = lv_data ).
  ENDMETHOD.

  METHOD handle_call.
    DATA(lv_function) = zcl_vsp_utils=>extract_param( iv_params = is_message-params iv_name = 'function' ).
    IF lv_function IS INITIAL.
      rs_response = zcl_vsp_utils=>build_error( iv_id = is_message-id iv_code = 'MISSING_PARAM' iv_message = 'Parameter function is required' ).
      RETURN.
    ENDIF.
    TRANSLATE lv_function TO UPPER CASE.

    DATA lt_import TYPE tt_param_info.
    DATA lt_export TYPE tt_param_info.
    DATA lt_changing TYPE tt_param_info.
    DATA lt_tables TYPE tt_param_info.

    IF get_func_interface( EXPORTING iv_function = CONV #( lv_function )
                           IMPORTING et_import = lt_import et_export = lt_export
                                     et_changing = lt_changing et_tables = lt_tables ) = abap_false.
      rs_response = zcl_vsp_utils=>build_error( iv_id = is_message-id iv_code = 'FUNC_NOT_FOUND' iv_message = |Function { lv_function } not found| ).
      RETURN.
    ENDIF.

    DATA lt_ptab TYPE abap_func_parmbind_tab.
    DATA lt_etab TYPE abap_func_excpbind_tab.
    DATA ls_ptab TYPE abap_func_parmbind.
    DATA lo_data TYPE REF TO data.
    DATA lv_val TYPE string.
    FIELD-SYMBOLS <fs_val> TYPE any.

    " Function's IMPORT params: we EXPORT values TO the function
    LOOP AT lt_import INTO DATA(ls_imp).
      CLEAR: ls_ptab, lo_data, lv_val.
      lo_data = create_param_data( ls_imp ).
      IF lo_data IS BOUND.
        lv_val = zcl_vsp_utils=>extract_param( iv_params = is_message-params iv_name = CONV #( ls_imp-parameter ) ).
        IF lv_val IS NOT INITIAL.
          ASSIGN lo_data->* TO <fs_val>.
          IF sy-subrc = 0.
            TRY.
                <fs_val> = lv_val.
              CATCH cx_root.
            ENDTRY.
          ENDIF.
        ENDIF.
        ls_ptab-name = ls_imp-parameter.
        ls_ptab-kind = abap_func_exporting.
        ls_ptab-value = lo_data.
        INSERT ls_ptab INTO TABLE lt_ptab.
      ENDIF.
    ENDLOOP.

    " Function's EXPORT params: we IMPORT values FROM the function
    LOOP AT lt_export INTO DATA(ls_exp).
      CLEAR: ls_ptab, lo_data.
      lo_data = create_param_data( ls_exp ).
      IF lo_data IS BOUND.
        ls_ptab-name = ls_exp-parameter.
        ls_ptab-kind = abap_func_importing.
        ls_ptab-value = lo_data.
        INSERT ls_ptab INTO TABLE lt_ptab.
      ENDIF.
    ENDLOOP.

    " TABLES params: create internal tables
    LOOP AT lt_tables INTO DATA(ls_tbl).
      CLEAR: ls_ptab, lo_data.
      lo_data = create_table_data( ls_tbl ).
      IF lo_data IS BOUND.
        ls_ptab-name = ls_tbl-parameter.
        ls_ptab-kind = abap_func_tables.
        ls_ptab-value = lo_data.
        INSERT ls_ptab INTO TABLE lt_ptab.
      ENDIF.
    ENDLOOP.

    INSERT VALUE #( name = 'OTHERS' value = 99 ) INTO TABLE lt_etab.

    TRY.
        CALL FUNCTION lv_function
          PARAMETER-TABLE lt_ptab
          EXCEPTION-TABLE lt_etab.
      CATCH cx_root INTO DATA(lx_call).
        rs_response = zcl_vsp_utils=>build_error( iv_id = is_message-id iv_code = 'CALL_ERROR' iv_message = lx_call->get_text( ) ).
        RETURN.
    ENDTRY.

    DATA(lv_subrc) = sy-subrc.

    " Build exports JSON
    DATA lt_exports TYPE string_table.
    DATA lv_str TYPE string.
    LOOP AT lt_ptab INTO DATA(ls_out) WHERE kind = abap_func_importing.
      ASSIGN ls_out-value->* TO FIELD-SYMBOL(<fs_out>).
      IF sy-subrc = 0.
        DATA(lv_pname) = CONV string( ls_out-name ).
        CONDENSE lv_pname.
        DATA(lo_exp_type) = cl_abap_typedescr=>describe_by_data( <fs_out> ).
        IF lo_exp_type->kind = cl_abap_typedescr=>kind_elem.
          TRY.
              lv_str = <fs_out>.
            CATCH cx_root.
              lv_str = ''.
          ENDTRY.
          APPEND zcl_vsp_utils=>json_str( iv_key = lv_pname iv_value = lv_str ) TO lt_exports.
        ELSE.
          " Handle structure
          DATA(lt_struct_parts) = VALUE string_table( ).
          DATA(lo_exp_struc) = CAST cl_abap_structdescr( lo_exp_type ).
          LOOP AT lo_exp_struc->components INTO DATA(ls_exp_comp).
            ASSIGN COMPONENT ls_exp_comp-name OF STRUCTURE <fs_out> TO FIELD-SYMBOL(<fs_exp_comp>).
            IF sy-subrc = 0.
              DATA(lo_comp_type) = cl_abap_typedescr=>describe_by_data( <fs_exp_comp> ).
              IF lo_comp_type->kind = cl_abap_typedescr=>kind_elem.
                TRY.
                    lv_str = <fs_exp_comp>.
                  CATCH cx_root.
                    lv_str = ''.
                ENDTRY.
              ELSE.
                lv_str = '[complex]'.
              ENDIF.
              APPEND zcl_vsp_utils=>json_str( iv_key = CONV #( ls_exp_comp-name ) iv_value = lv_str ) TO lt_struct_parts.
            ENDIF.
          ENDLOOP.
          APPEND |"{ lv_pname }":{ zcl_vsp_utils=>json_obj( zcl_vsp_utils=>json_join( lt_struct_parts ) ) }| TO lt_exports.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " Build tables JSON
    DATA lt_tables_json TYPE string_table.
    LOOP AT lt_ptab INTO ls_out WHERE kind = abap_func_tables.
      ASSIGN ls_out-value->* TO FIELD-SYMBOL(<fs_tab>).
      IF sy-subrc = 0.
        lv_pname = CONV string( ls_out-name ).
        CONDENSE lv_pname.
        DATA(lt_rows) = VALUE string_table( ).
        TRY.
            LOOP AT <fs_tab> ASSIGNING FIELD-SYMBOL(<fs_row>).
              DATA(lt_row_parts) = VALUE string_table( ).
              DATA(lo_struc) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( <fs_row> ) ).
              LOOP AT lo_struc->components INTO DATA(ls_comp).
                ASSIGN COMPONENT ls_comp-name OF STRUCTURE <fs_row> TO FIELD-SYMBOL(<fs_comp>).
                IF sy-subrc = 0.
                  DATA(lo_type) = cl_abap_typedescr=>describe_by_data( <fs_comp> ).
                  IF lo_type->kind = cl_abap_typedescr=>kind_elem.
                    TRY.
                        lv_str = <fs_comp>.
                      CATCH cx_root.
                        lv_str = ''.
                    ENDTRY.
                  ELSE.
                    lv_str = '[complex]'.
                  ENDIF.
                  APPEND zcl_vsp_utils=>json_str( iv_key = CONV #( ls_comp-name ) iv_value = lv_str ) TO lt_row_parts.
                ENDIF.
              ENDLOOP.
              APPEND zcl_vsp_utils=>json_obj( zcl_vsp_utils=>json_join( lt_row_parts ) ) TO lt_rows.
            ENDLOOP.
          CATCH cx_root.
            APPEND zcl_vsp_utils=>json_obj( `"error":"serialization failed"` ) TO lt_rows.
        ENDTRY.
        APPEND |"{ lv_pname }":{ zcl_vsp_utils=>json_arr( zcl_vsp_utils=>json_join( lt_rows ) ) }| TO lt_tables_json.
      ENDIF.
    ENDLOOP.

    " Build final response
    DATA(lv_data) = zcl_vsp_utils=>json_obj( zcl_vsp_utils=>json_join( VALUE #(
      ( zcl_vsp_utils=>json_int( iv_key = 'subrc' iv_value = lv_subrc ) )
      ( |"exports":{ zcl_vsp_utils=>json_obj( zcl_vsp_utils=>json_join( lt_exports ) ) }| )
      ( |"tables":{ zcl_vsp_utils=>json_obj( zcl_vsp_utils=>json_join( lt_tables_json ) ) }| )
    ) ) ).

    rs_response = zcl_vsp_utils=>build_success( iv_id = is_message-id iv_data = lv_data ).
  ENDMETHOD.

  METHOD create_param_data.
    DATA lv_type TYPE string.

    IF strlen( is_param-typ ) > 0.
      lv_type = is_param-typ.
    ELSEIF strlen( is_param-dbfield ) > 0.
      lv_type = is_param-dbfield.
    ELSE.
      CREATE DATA ro_data TYPE string.
      RETURN.
    ENDIF.

    TRY.
        CREATE DATA ro_data TYPE (lv_type).
      CATCH cx_sy_create_data_error.
        TRY.
            CREATE DATA ro_data TYPE string.
          CATCH cx_root.
            CLEAR ro_data.
        ENDTRY.
    ENDTRY.
  ENDMETHOD.

  METHOD create_table_data.
    DATA lv_type TYPE string.

    IF strlen( is_param-typ ) > 0.
      lv_type = is_param-typ.
    ELSEIF strlen( is_param-dbfield ) > 0.
      lv_type = is_param-dbfield.
    ELSE.
      RETURN.
    ENDIF.

    TRY.
        CREATE DATA ro_data TYPE STANDARD TABLE OF (lv_type).
      CATCH cx_sy_create_data_error.
        CLEAR ro_data.
    ENDTRY.
  ENDMETHOD.

  METHOD handle_get_metadata.
    DATA(lv_function) = zcl_vsp_utils=>extract_param( iv_params = is_message-params iv_name = 'function' ).
    IF lv_function IS INITIAL.
      rs_response = zcl_vsp_utils=>build_error( iv_id = is_message-id iv_code = 'MISSING_PARAM' iv_message = 'Parameter function is required' ).
      RETURN.
    ENDIF.
    TRANSLATE lv_function TO UPPER CASE.

    DATA lt_import TYPE tt_param_info.
    DATA lt_export TYPE tt_param_info.
    DATA lt_changing TYPE tt_param_info.
    DATA lt_tables TYPE tt_param_info.

    IF get_func_interface( EXPORTING iv_function = CONV #( lv_function )
                           IMPORTING et_import = lt_import et_export = lt_export
                                     et_changing = lt_changing et_tables = lt_tables ) = abap_false.
      rs_response = zcl_vsp_utils=>build_error( iv_id = is_message-id iv_code = 'FUNC_NOT_FOUND' iv_message = |Function { lv_function } not found| ).
      RETURN.
    ENDIF.

    " Build parameters array
    DATA lt_params TYPE string_table.

    LOOP AT lt_import INTO DATA(ls_i).
      DATA(lv_typ) = COND string( WHEN strlen( ls_i-typ ) > 0 THEN ls_i-typ ELSE ls_i-dbfield ).
      DATA(lv_pname) = CONV string( ls_i-parameter ).
      CONDENSE lv_pname.
      APPEND zcl_vsp_utils=>json_obj( zcl_vsp_utils=>json_join( VALUE #(
        ( zcl_vsp_utils=>json_str( iv_key = 'name' iv_value = lv_pname ) )
        ( zcl_vsp_utils=>json_str( iv_key = 'kind' iv_value = 'importing' ) )
        ( zcl_vsp_utils=>json_str( iv_key = 'type' iv_value = lv_typ ) )
        ( zcl_vsp_utils=>json_bool( iv_key = 'optional' iv_value = ls_i-optional ) )
      ) ) ) TO lt_params.
    ENDLOOP.

    LOOP AT lt_export INTO DATA(ls_e).
      lv_typ = COND string( WHEN strlen( ls_e-typ ) > 0 THEN ls_e-typ ELSE ls_e-dbfield ).
      lv_pname = CONV string( ls_e-parameter ).
      CONDENSE lv_pname.
      APPEND zcl_vsp_utils=>json_obj( zcl_vsp_utils=>json_join( VALUE #(
        ( zcl_vsp_utils=>json_str( iv_key = 'name' iv_value = lv_pname ) )
        ( zcl_vsp_utils=>json_str( iv_key = 'kind' iv_value = 'exporting' ) )
        ( zcl_vsp_utils=>json_str( iv_key = 'type' iv_value = lv_typ ) )
        ( zcl_vsp_utils=>json_bool( iv_key = 'optional' iv_value = abap_true ) )
      ) ) ) TO lt_params.
    ENDLOOP.

    LOOP AT lt_changing INTO DATA(ls_ch).
      lv_typ = COND string( WHEN strlen( ls_ch-typ ) > 0 THEN ls_ch-typ ELSE ls_ch-dbfield ).
      lv_pname = CONV string( ls_ch-parameter ).
      CONDENSE lv_pname.
      APPEND zcl_vsp_utils=>json_obj( zcl_vsp_utils=>json_join( VALUE #(
        ( zcl_vsp_utils=>json_str( iv_key = 'name' iv_value = lv_pname ) )
        ( zcl_vsp_utils=>json_str( iv_key = 'kind' iv_value = 'changing' ) )
        ( zcl_vsp_utils=>json_str( iv_key = 'type' iv_value = lv_typ ) )
        ( zcl_vsp_utils=>json_bool( iv_key = 'optional' iv_value = abap_true ) )
      ) ) ) TO lt_params.
    ENDLOOP.

    LOOP AT lt_tables INTO DATA(ls_t).
      lv_typ = COND string( WHEN strlen( ls_t-typ ) > 0 THEN ls_t-typ ELSE ls_t-dbfield ).
      lv_pname = CONV string( ls_t-parameter ).
      CONDENSE lv_pname.
      APPEND zcl_vsp_utils=>json_obj( zcl_vsp_utils=>json_join( VALUE #(
        ( zcl_vsp_utils=>json_str( iv_key = 'name' iv_value = lv_pname ) )
        ( zcl_vsp_utils=>json_str( iv_key = 'kind' iv_value = 'tables' ) )
        ( zcl_vsp_utils=>json_str( iv_key = 'type' iv_value = lv_typ ) )
        ( zcl_vsp_utils=>json_bool( iv_key = 'optional' iv_value = abap_true ) )
      ) ) ) TO lt_params.
    ENDLOOP.

    DATA(lv_data) = zcl_vsp_utils=>json_obj( zcl_vsp_utils=>json_join( VALUE #(
      ( zcl_vsp_utils=>json_str( iv_key = 'function' iv_value = lv_function ) )
      ( |"parameters":{ zcl_vsp_utils=>json_arr( zcl_vsp_utils=>json_join( lt_params ) ) }| )
    ) ) ).

    rs_response = zcl_vsp_utils=>build_success( iv_id = is_message-id iv_data = lv_data ).
  ENDMETHOD.

  METHOD get_func_interface.
    rv_success = abap_false.
    CLEAR: et_import, et_export, et_changing, et_tables.

    DATA lt_import TYPE STANDARD TABLE OF rsimp.
    DATA lt_export TYPE STANDARD TABLE OF rsexp.
    DATA lt_changing TYPE STANDARD TABLE OF rscha.
    DATA lt_tables TYPE STANDARD TABLE OF rstbl.
    DATA lt_except TYPE STANDARD TABLE OF rsexc.

    CALL FUNCTION 'FUNCTION_IMPORT_INTERFACE'
      EXPORTING
        funcname           = iv_function
      TABLES
        import_parameter   = lt_import
        export_parameter   = lt_export
        changing_parameter = lt_changing
        tables_parameter   = lt_tables
        exception_list     = lt_except
      EXCEPTIONS
        error_message      = 1
        function_not_found = 2
        invalid_name       = 3
        OTHERS             = 4.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_import INTO DATA(ls_i).
      INSERT VALUE #( parameter = ls_i-parameter kind = 'I' typ = ls_i-typ dbfield = ls_i-dbfield optional = ls_i-optional ) INTO TABLE et_import.
    ENDLOOP.
    LOOP AT lt_export INTO DATA(ls_e).
      INSERT VALUE #( parameter = ls_e-parameter kind = 'E' typ = ls_e-typ dbfield = ls_e-dbfield ) INTO TABLE et_export.
    ENDLOOP.
    LOOP AT lt_changing INTO DATA(ls_ch).
      INSERT VALUE #( parameter = ls_ch-parameter kind = 'C' typ = ls_ch-typ dbfield = ls_ch-dbfield optional = ls_ch-optional ) INTO TABLE et_changing.
    ENDLOOP.
    LOOP AT lt_tables INTO DATA(ls_t).
      INSERT VALUE #( parameter = ls_t-parameter kind = 'T' typ = ls_t-typ dbfield = ls_t-dbstruct optional = ls_t-optional ) INTO TABLE et_tables.
    ENDLOOP.

    rv_success = abap_true.
  ENDMETHOD.

  METHOD handle_search.
    DATA lv_pattern TYPE string.
    lv_pattern = zcl_vsp_utils=>extract_param( iv_params = is_message-params iv_name = 'pattern' ).
    IF lv_pattern IS INITIAL.
      lv_pattern = '%'.
    ENDIF.
    TRANSLATE lv_pattern TO UPPER CASE.
    REPLACE ALL OCCURRENCES OF '*' IN lv_pattern WITH '%'.

    DATA lt_funcs TYPE STANDARD TABLE OF rs38l_fnam.
    SELECT funcname FROM tfdir INTO TABLE lt_funcs
      UP TO 100 ROWS
      WHERE funcname LIKE lv_pattern
      ORDER BY funcname.

    " Build array of function names
    DATA lt_items TYPE string_table.
    LOOP AT lt_funcs INTO DATA(lv_func).
      APPEND zcl_vsp_utils=>json_obj( zcl_vsp_utils=>json_str( iv_key = 'name' iv_value = CONV #( lv_func ) ) ) TO lt_items.
    ENDLOOP.

    rs_response = zcl_vsp_utils=>build_success(
      iv_id   = is_message-id
      iv_data = zcl_vsp_utils=>json_arr( zcl_vsp_utils=>json_join( lt_items ) )
    ).
  ENDMETHOD.

  METHOD handle_ping.
    DATA(lv_ts) = |{ sy-datum }{ sy-uzeit }|.
    DATA(lv_data) = zcl_vsp_utils=>json_obj( zcl_vsp_utils=>json_join( VALUE #(
      ( zcl_vsp_utils=>json_bool( iv_key = 'pong' iv_value = abap_true ) )
      ( zcl_vsp_utils=>json_str( iv_key = 'timestamp' iv_value = lv_ts ) )
    ) ) ).
    rs_response = zcl_vsp_utils=>build_success( iv_id = is_message-id iv_data = lv_data ).
  ENDMETHOD.

ENDCLASS.