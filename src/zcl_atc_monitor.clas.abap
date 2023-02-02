class ZCL_ATC_MONITOR definition
  public
  create public .

public section.

  types:
    tr_priority TYPE RANGE OF satc_ac_resultvt-priority .
  types:
    BEGIN OF ts_alv,
        package_name   TYPE satc_ac_resultvt-package_name,
        resp1          TYPE satc_ac_resultvt-resp1,
        error_obj_type TYPE satc_ac_resultvt-error_obj_type,
        error_obj_name TYPE satc_ac_resultvt-error_obj_name,
        user_name           TYPE sy-uname,
        err_obj        TYPE char30,
        err_obj_type   TYPE char4,
        err_line       TYPE i,
        priority       TYPE satc_ac_resultvt-priority,
        err_new        TYPE i,
        dayoff         TYPE i,
        found_datum    TYPE sy-datum,
        found_uzeit    TYPE sy-uzeit,
        check_title    TYPE char80,
        message_title  TYPE char80,
        korrnum        TYPE vrsd-korrnum,
      END OF ts_alv .
  types:
    tt_alv             TYPE STANDARD TABLE OF ts_alv .
  types:
    tr_run_series_name TYPE RANGE OF satc_ac_resulth-run_series_name .
  types:
    tr_resp1           TYPE RANGE OF satc_ac_resultvt-resp1 .
  types:
    tr_date            TYPE RANGE OF satc_ac_resulth-scheduled_on_ts .

  data MT_ATC_MONITOR_ALV type TT_ALV .

  methods FIND_ATC_DATA
    importing
      !IV_DATUM type SY-DATUM
      !IV_DELTA_DAY type T5A4A-DLYDY
      !IR_RUN_SERIES_NAME type TR_RUN_SERIES_NAME
      !IR_RESP1 type TR_RESP1
      !IR_PRIOR type TR_PRIORITY .
  methods DISPLAY .
protected section.
PRIVATE SECTION.

  METHODS _get_transp_query_for_incl
    IMPORTING
      !iv_datum       TYPE sy-datum
    CHANGING
      !ct_atc_monitor TYPE ztt_atc_monitor .
  METHODS _get_transp_query_for_meth
    IMPORTING
      !iv_datum       TYPE sy-datum
    CHANGING
      !ct_atc_monitor TYPE ztt_atc_monitor .
  METHODS _get_transp_query_for_null_obj
    IMPORTING
      !iv_datum       TYPE sy-datum
    CHANGING
      !ct_atc_monitor TYPE ztt_atc_monitor .
  METHODS _get_transp_query_for_other
    IMPORTING
      !iv_datum       TYPE sy-datum
    CHANGING
      !ct_atc_monitor TYPE ztt_atc_monitor .
  METHODS _get_errors_one_day
    IMPORTING
      !iv_datum           TYPE sy-datum
      !ir_run_series_name TYPE tr_run_series_name
      !ir_resp1           TYPE tr_resp1
      !ir_prior           TYPE tr_priority
    EXPORTING
      !et_atc_monitor     TYPE ztt_atc_monitor .
  METHODS _get_delta
    IMPORTING
      !it_atc_monitor_earl TYPE ztt_atc_monitor
      !it_atc_monitor_curr TYPE ztt_atc_monitor
    EXPORTING
      !et_atc_delta        TYPE ztt_atc_monitor .
  METHODS _db_get_result
    IMPORTING
      !ir_date            TYPE tr_date
      !ir_run_series_name TYPE tr_run_series_name
      !ir_resp1           TYPE tr_resp1
      !ir_prior           TYPE tr_priority
    EXPORTING
      !et_atc_monitor     TYPE ztt_atc_monitor .
  METHODS _get_error_obj
    CHANGING
      !ct_atc_monitor TYPE ztt_atc_monitor .
  METHODS _get_error_mes
    CHANGING
      !ct_atc_monitor TYPE ztt_atc_monitor .
  METHODS _get_mr_date
    IMPORTING
      !iv_datum      TYPE sy-datum
    RETURNING
      VALUE(rr_date) TYPE tr_date .
  METHODS _get_delta_day_earlier
    IMPORTING
      !iv_datum       TYPE sy-datum
      !iv_delta_day   TYPE t5a4a-dlydy
    RETURNING
      VALUE(rv_datum) TYPE sy-datum .
  METHODS _get_datum_time_dayoff
    IMPORTING
      !iv_datum       TYPE sy-datum
    CHANGING
      !ct_atc_monitor TYPE ztt_atc_monitor .
  METHODS _get_transp_query
    IMPORTING
      !iv_datum       TYPE sy-datum
    CHANGING
      !ct_atc_monitor TYPE ztt_atc_monitor .
ENDCLASS.



CLASS ZCL_ATC_MONITOR IMPLEMENTATION.


  method DISPLAY.

    DATA: lo_alv   TYPE REF TO cl_salv_table,
          lo_dis   TYPE REF TO cl_salv_display_settings,
          lo_col   TYPE REF TO cl_salv_column,
          lx_msg   TYPE REF TO cx_salv_msg,
          lv_title TYPE lvc_title.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = mt_atc_monitor_alv ).
      CATCH cx_salv_msg INTO lx_msg.
    ENDTRY.

    DATA(lo_func) = lo_alv->get_functions( ).
    lo_func->set_all( abap_true ).
    DATA(lo_cols) = lo_alv->get_columns( ).
    lo_cols->set_optimize(
        value = abap_true
    ).
    lo_alv->display( ).

  endmethod.


  METHOD find_atc_data.

    " Для получения ошибок АТС на указанную дату
    me->_get_errors_one_day(
      EXPORTING
        iv_datum           = iv_datum
        ir_run_series_name = ir_run_series_name
        ir_resp1           = ir_resp1
        ir_prior           = ir_prior
      IMPORTING
        et_atc_monitor     = DATA(lt_current_atc_monitor)
    ).

    IF iv_delta_day <= 0.
      DATA(lt_atc_monitor) = lt_current_atc_monitor.
    ELSE.
      " Для получения даты на DELTA дней ранее
      DATA(lv_datum_delta_early) = me->_get_delta_day_earlier(
                                 iv_datum     = iv_datum
                                 iv_delta_day = iv_delta_day
                             ).

      " Для получения ошибок АТС на DELTA дней ранее
      me->_get_errors_one_day(
        EXPORTING
          iv_datum           = lv_datum_delta_early
          ir_run_series_name = ir_run_series_name
          ir_resp1           = ir_resp1
          ir_prior           = ir_prior
        IMPORTING
          et_atc_monitor     = DATA(lt_earl_atc_monitor)
      ).

      " Для получения разницы
      _get_delta(
        EXPORTING
          it_atc_monitor_earl = lt_earl_atc_monitor
          it_atc_monitor_curr = lt_current_atc_monitor
        IMPORTING
          et_atc_delta        = lt_atc_monitor
      ).
    ENDIF.

    " Для получения даты времени и dayoff
    _get_datum_time_dayoff(
      EXPORTING
        iv_datum       = iv_datum
      CHANGING
        ct_atc_monitor = lt_atc_monitor
    ).

    " Для получения запроса
    _get_transp_query(
      EXPORTING
        iv_datum       = iv_datum    " Системное поле: текущая дата сервера приложения
      CHANGING
        ct_atc_monitor = lt_atc_monitor    " TT_ATC_MONITOR
    ).

    mt_atc_monitor_alv = CORRESPONDING #( lt_atc_monitor ).


  ENDMETHOD.


  METHOD _db_get_result.

    SELECT a~display_id
      , b~obj_name, b~obj_type, b~module_id, b~module_msg_key, b~checksum
      , a~scheduled_on_ts, a~run_series_name
      , b~package_name, b~resp1, b~error_obj_type, b~error_obj_name
      , b~priority, b~navigation_data, b~description_data, b~since
      INTO CORRESPONDING FIELDS OF TABLE @et_atc_monitor
      FROM satc_ac_resulth AS a
     INNER JOIN satc_ac_resultvt  AS b
        ON a~display_id = b~display_id
     WHERE a~scheduled_on_ts IN @ir_date
       AND a~is_complete = 'X'
       AND a~is_mass_test = 'X'
       AND a~is_central_run = 'X'
       AND a~run_series_name IN @ir_run_series_name
       AND b~resp1 IN @ir_resp1
       AND b~priority IN @ir_prior
    .

  ENDMETHOD.


  METHOD _GET_DATUM_TIME_DAYOFF.

    DATA: lv_since TYPE char14.
    LOOP AT ct_atc_monitor ASSIGNING FIELD-SYMBOL(<fs_atc_monitor>).
      lv_since = CONV char14( <fs_atc_monitor>-since ).
      <fs_atc_monitor>-found_datum = lv_since(8).
      <fs_atc_monitor>-found_uzeit = lv_since+8(6).
      <fs_atc_monitor>-dayoff = iv_datum - <fs_atc_monitor>-found_datum.
    ENDLOOP.

  ENDMETHOD.


  METHOD _get_delta.

    REFRESH et_atc_delta.
    DATA(lt_atc_monitor_curr) = it_atc_monitor_curr.
    DATA(lt_atc_monitor_earl) = it_atc_monitor_earl.

    SORT lt_atc_monitor_curr BY err_obj err_obj_type message_title check_title since.
    SORT lt_atc_monitor_earl BY err_obj err_obj_type message_title check_title since.

    LOOP AT lt_atc_monitor_curr ASSIGNING FIELD-SYMBOL(<fs_atc_cur>).
*      LOOP AT lt_atc_monitor_earl ASSIGNING FIELD-SYMBOL(<fs_atc_earl>) WHERE err_obj = <fs_atc_cur>-err_obj
*                                                                          AND err_obj_type = <fs_atc_cur>-err_obj_type
*                                                                          AND message_title = <fs_atc_cur>-message_title
*                                                                          AND check_title = <fs_atc_cur>-check_title
*                                                                          AND since = <fs_atc_cur>-since.
*        DELETE lt_atc_monitor_curr.
*        DELETE lt_atc_monitor_earl.
*        EXIT.
*      ENDLOOP.
      READ TABLE lt_atc_monitor_earl ASSIGNING FIELD-SYMBOL(<fs_atc_earl>) WITH KEY err_obj = <fs_atc_cur>-err_obj
                                                                                    err_obj_type = <fs_atc_cur>-err_obj_type
                                                                                    message_title = <fs_atc_cur>-message_title
                                                                                    check_title = <fs_atc_cur>-check_title
                                                                                    since = <fs_atc_cur>-since BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE lt_atc_monitor_earl INDEX sy-tabix.
        DELETE lt_atc_monitor_curr.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_atc_monitor_curr ASSIGNING <fs_atc_cur>.
      <fs_atc_cur>-err_new = 1.
    ENDLOOP.

    et_atc_delta = lt_atc_monitor_curr.

  ENDMETHOD.


  method _GET_DELTA_DAY_EARLIER.


    DATA lv_change TYPE p0001-begda.
    DATA months    TYPE t5a4a-dlymo.
    DATA signum    TYPE t5a4a-split VALUE '-'.
    DATA years     TYPE t5a4a-dlyyr.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = iv_datum
        days      = iv_delta_day
        months    = months
        signum    = signum
        years     = years
      IMPORTING
        calc_date = rv_datum.


  endmethod.


  METHOD _get_errors_one_day.

    " Для получения параметра date в виде range
    DATA(lr_date) = _get_mr_date( iv_datum = iv_datum  ).

    " Для получения заголовка и позиции прогона ATC
    _db_get_result(
      EXPORTING
        ir_date           = lr_date
        ir_run_series_name = ir_run_series_name
        ir_resp1           = ir_resp1
        ir_prior           = ir_prior
      IMPORTING
        et_atc_monitor     = et_atc_monitor
    ).

*    DATA: lt_object_table TYPE TABLE OF  ko100,
*          lv_obj_type     TYPE ko100-object.
*    CALL FUNCTION 'TR_OBJECT_TABLE'
*      TABLES
*        wt_object_text = lt_object_table.
*    SORT lt_object_table BY object.

    " Для получения подробной инфы про ошибочные объекты
    _get_error_obj(
      CHANGING
        ct_atc_monitor = et_atc_monitor
    ).
    " Для получения подробного описания сообщения ошибки
    _get_error_mes(
      CHANGING
        ct_atc_monitor = et_atc_monitor
    ).


  ENDMETHOD.


  method _GET_ERROR_MES.

    DATA: lo_memento_tool TYPE REF TO cl_satc_ac_memento.

    LOOP AT ct_atc_monitor ASSIGNING FIELD-SYMBOL(<fs_atc>).
      lo_memento_tool = NEW #( ).

      DATA(lo_description) = lo_memento_tool->new_text_handle_by_memento( <fs_atc>-description_data ).
      DATA(lt_descr) = lo_description->if_satc_ac_text_handle~get_text_lines( ).
      LOOP AT lt_descr ASSIGNING FIELD-SYMBOL(<fs_descr>).
        IF <fs_atc>-message_title IS INITIAL AND <fs_descr>-index = 1.
          <fs_atc>-message_title = <fs_descr>-text.
        ENDIF.
      ENDLOOP.

      DATA(check_info) = cl_satc_ac__ui_cache=>get_check_info( <fs_atc>-module_id ).
      <fs_atc>-check_title = check_info->get_title( ).

    ENDLOOP.

  endmethod.


  method _GET_ERROR_OBJ.

    DATA: lo_memento_tool TYPE REF TO cl_satc_ac_memento,
          lv_obj_type     TYPE ko100-object.

    LOOP AT ct_atc_monitor ASSIGNING FIELD-SYMBOL(<fs_atc>).
      lo_memento_tool = NEW #( ).

      DATA(lo_navigator) = lo_memento_tool->new_navigator_by_memento( i_memento = <fs_atc>-navigation_data ).
      DATA(lt_navigator) = lo_navigator->if_satc_ac_navigator~get_description( ).

      TRY.
          DATA(first_line) = lt_navigator[ 1 ].
          lv_obj_type = first_line-tag+3(4).
          TRANSLATE lv_obj_type TO UPPER CASE.
*          READ TABLE lt_object_table TRANSPORTING NO FIELDS WITH KEY object = lv_obj_type BINARY SEARCH.
*          IF sy-subrc = 0.

          <fs_atc>-err_obj_type = lv_obj_type.
          <fs_atc>-user_name = first_line-contact.
          <fs_atc>-err_obj = first_line-item.
*          ENDIF.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      TRY .
          DATA(second_line) = lt_navigator[ 2 ].
          <fs_atc>-err_line = second_line-item.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
    ENDLOOP.

  endmethod.


  method _GET_MR_DATE.

    rr_date = VALUE #(
   ( sign = 'I' option = 'BT' low = CONV #( |{ iv_datum }000000| )
                             high = CONV #( |{ iv_datum }235959| ) )
   ).

  endmethod.


  METHOD _get_transp_query.
    " Для получения трансп.запросов для методов
    _get_transp_query_for_meth(
      EXPORTING
        iv_datum       = iv_datum    " Системное поле: текущая дата сервера приложения
      CHANGING
        ct_atc_monitor = ct_atc_monitor
    ).

    " Для получения трансп.запросов для типа INCL
    _get_transp_query_for_incl(
      EXPORTING
        iv_datum       = iv_datum    " Системное поле: текущая дата сервера приложения
      CHANGING
        ct_atc_monitor = ct_atc_monitor
    ).

    " Для получения трансп.запросов остальных типов
    _get_transp_query_for_other(
      EXPORTING
        iv_datum       = iv_datum    " Системное поле: текущая дата сервера приложения
      CHANGING
        ct_atc_monitor = ct_atc_monitor
    ).

    " Для получения трансп.запросов для тех типов где нет под объектов (TABL меню и прочие)
    _get_transp_query_for_null_obj(
      EXPORTING
        iv_datum       = iv_datum    " Системное поле: текущая дата сервера приложения
      CHANGING
        ct_atc_monitor = ct_atc_monitor
    ).
  ENDMETHOD.


  METHOD _get_transp_query_for_incl.
    TYPES: BEGIN OF ts_methods,
             err_obj_type TYPE zts_atc_monitor-err_obj_type,
             err_obj      TYPE vrsd-objname,
             found_datum  TYPE sy-datum,
             found_uzeit  TYPE sy-uzeit,
           END OF ts_methods.
    DATA: lt_methods TYPE TABLE OF ts_methods.

    lt_methods = CORRESPONDING #( ct_atc_monitor ).
    DELETE lt_methods WHERE err_obj_type <> 'INCL'.

    SORT lt_methods BY err_obj.
    DELETE ADJACENT DUPLICATES FROM lt_methods COMPARING err_obj.

    IF lt_methods IS NOT INITIAL.
      SELECT objtype, objname, versno, korrnum, author, datum, zeit
        INTO TABLE @DATA(lt_vrsd_meth)
        FROM vrsd
         FOR ALL ENTRIES IN @lt_methods
       WHERE objtype = 'REPS'
         AND objname = @lt_methods-err_obj
         AND datum < @iv_datum.
    ENDIF.

    SORT lt_vrsd_meth BY objtype objname datum DESCENDING zeit DESCENDING.

    LOOP AT ct_atc_monitor ASSIGNING FIELD-SYMBOL(<fs_atc_monitor>) WHERE err_obj_type = 'INCL' AND korrnum IS INITIAL.
      READ TABLE lt_vrsd_meth ASSIGNING FIELD-SYMBOL(<fs_vrds_meth>) WITH KEY objname = <fs_atc_monitor>-err_obj BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_atc_monitor>-korrnum = <fs_vrds_meth>-korrnum.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD _get_transp_query_for_meth.
    TYPES: BEGIN OF ts_methods,
             error_obj_type TYPE satc_ac_resultvt-error_obj_type,
             error_obj_name TYPE satc_ac_resultvt-error_obj_name,
             err_obj_type   TYPE zts_atc_monitor-err_obj_type,
             err_obj        TYPE zts_atc_monitor-err_obj,
             name           TYPE vrsd-objname,
           END OF ts_methods.
    DATA: lt_methods TYPE TABLE OF ts_methods.

    lt_methods = CORRESPONDING #( ct_atc_monitor ).
    DELETE lt_methods WHERE err_obj_type <> 'METH'.

    LOOP AT lt_methods ASSIGNING FIELD-SYMBOL(<fs_methods>).
      <fs_methods>-name = |{ <fs_methods>-error_obj_name WIDTH = 30 ALIGN = LEFT }{ <fs_methods>-err_obj WIDTH = 30 }|.
    ENDLOOP.
    SORT lt_methods BY name.
    DELETE ADJACENT DUPLICATES FROM lt_methods COMPARING name.

    IF lt_methods IS NOT INITIAL.
      SELECT objtype, objname, versno, korrnum, author, datum, zeit
        INTO TABLE @DATA(lt_vrsd_meth)
        FROM vrsd
         FOR ALL ENTRIES IN @lt_methods
       WHERE objtype = 'METH'
         AND objname = @lt_methods-name
         AND datum < @iv_datum.
    ENDIF.
    SORT lt_vrsd_meth BY objtype objname datum DESCENDING zeit DESCENDING.

    LOOP AT ct_atc_monitor ASSIGNING FIELD-SYMBOL(<fs_atc_monitor>) WHERE err_obj_type = 'METH' AND korrnum IS INITIAL.
      DATA lv_name TYPE vrsd-objname.
      lv_name = |{ <fs_atc_monitor>-error_obj_name WIDTH = 30 ALIGN = LEFT }{ <fs_atc_monitor>-err_obj WIDTH = 30 }|.
      READ TABLE lt_vrsd_meth ASSIGNING FIELD-SYMBOL(<fs_vrds_meth>) WITH KEY objname = lv_name BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_atc_monitor>-korrnum = <fs_vrds_meth>-korrnum.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD _get_transp_query_for_null_obj.
    TYPES: BEGIN OF ts_objects,
             error_obj_name TYPE  vrsd-objname,
             err_obj_type   TYPE zts_atc_monitor-err_obj_type,
             err_obj        TYPE satc_ac_resultvt-error_obj_name,
           END OF ts_objects.
    DATA: lt_objects TYPE TABLE OF ts_objects.

    lt_objects = CORRESPONDING #( ct_atc_monitor ).
    DELETE lt_objects WHERE err_obj_type <> '' AND err_obj <> ''.

    SORT lt_objects BY  error_obj_name.
    DELETE ADJACENT DUPLICATES FROM lt_objects COMPARING  error_obj_name.

    IF lt_objects IS NOT INITIAL.
      SELECT objtype, objname, versno, korrnum, author, datum, zeit
        INTO TABLE @DATA(lt_vrsd)
        FROM vrsd
         FOR ALL ENTRIES IN @lt_objects
       WHERE objname = @lt_objects-error_obj_name
         AND datum < @iv_datum.
    ENDIF.

    SORT lt_vrsd BY objtype objname datum DESCENDING zeit DESCENDING.

    LOOP AT ct_atc_monitor ASSIGNING FIELD-SYMBOL(<fs_atc_monitor>) WHERE korrnum IS INITIAL.
      READ TABLE lt_vrsd ASSIGNING FIELD-SYMBOL(<fs_vrds_meth>) WITH KEY objname = <fs_atc_monitor>-error_obj_name BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_atc_monitor>-korrnum = <fs_vrds_meth>-korrnum.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD _get_transp_query_for_other.
    TYPES: BEGIN OF ts_objects,
             err_obj_type TYPE zts_atc_monitor-err_obj_type,
             err_obj      TYPE vrsd-objname,
           END OF ts_objects.
    DATA: lt_objects TYPE TABLE OF ts_objects.

    lt_objects = CORRESPONDING #( ct_atc_monitor ).
    DELETE lt_objects WHERE err_obj_type = 'INCL' AND err_obj_type = 'METH'.

    SORT lt_objects BY err_obj_type err_obj.
    DELETE ADJACENT DUPLICATES FROM lt_objects COMPARING err_obj_type err_obj.

    IF lt_objects IS NOT INITIAL.
      SELECT objtype, objname, versno, korrnum, author, datum, zeit
        INTO TABLE @DATA(lt_vrsd)
        FROM vrsd
         FOR ALL ENTRIES IN @lt_objects
       WHERE objtype = @lt_objects-err_obj_type
         AND objname = @lt_objects-err_obj
         AND datum < @iv_datum.
    ENDIF.

    SORT lt_vrsd BY objtype objname datum DESCENDING zeit DESCENDING.

    LOOP AT ct_atc_monitor ASSIGNING FIELD-SYMBOL(<fs_atc_monitor>) WHERE korrnum IS INITIAL.
      READ TABLE lt_vrsd ASSIGNING FIELD-SYMBOL(<fs_vrds_meth>) WITH KEY objtype = <fs_atc_monitor>-err_obj_type objname = <fs_atc_monitor>-err_obj BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_atc_monitor>-korrnum = <fs_vrds_meth>-korrnum.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
