*&---------------------------------------------------------------------*
*& Report:                ZCARGA_CONSUMO_MVER
*& Description:           CARGA DE CONSUMO DO TIPO MRP SEM AHD
*&
*& Creation Date:         2023-05-16
*& Author ABAP:           João Guilherme Ponciano
*& Functional Consultant: Gleziane Freitas Soares
*& Request Number:        D05K972850
*&---------------------------------------------------------------------*
*& Change History:
*& Date       Version   Description
*&----------- --------- -----------------------------------------------*
*& <date>     <version> <description>
*& <date>     <version> <description>
*&---------------------------------------------------------------------*

REPORT zcarga_consumo_mver.

TABLES: marc.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETER p_refsit TYPE mmsiteref-refsite OBLIGATORY.

SELECT-OPTIONS s_matnr FOR marc-matnr.

SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF ty_mmsiteref,
     werks TYPE mmsiteref-werks,
  END OF ty_mmsiteref,

  BEGIN OF ty_marc,
    matnr TYPE marc-matnr,
    werks TYPE marc-werks,
    maktx TYPE makt-maktx,
  END OF ty_marc,

  BEGIN OF ty_mver,
    matnr TYPE mver-matnr,
    werks TYPE mver-werks,
    gjahr TYPE mver-gjahr,
    zahlr TYPE mver-zahlr,
    mgv01 TYPE mver-mgv01,
    mgv02 TYPE mver-mgv02,
    mgv03 TYPE mver-mgv03,
    mgv04 TYPE mver-mgv04,
    mgv05 TYPE mver-mgv05,
    mgv06 TYPE mver-mgv06,
    mgv07 TYPE mver-mgv07,
    mgv08 TYPE mver-mgv08,
    mgv09 TYPE mver-mgv09,
    mgv10 TYPE mver-mgv10,
    mgv11 TYPE mver-mgv11,
    mgv12 TYPE mver-mgv12,
    mgv13 TYPE mver-mgv13,
  END OF ty_mver,

  BEGIN OF ty_log_alv,
    werks   TYPE mver-werks,
    matnr   TYPE mver-matnr,
    ertag   TYPE ertag,
    week    TYPE c LENGTH 2,
    kovbw   TYPE kovbw,
    status  TYPE zstt_cons,
    message TYPE zed_msg,
END OF ty_log_alv.

*----------------------------------------------------------------------*
* Internal Tables
*----------------------------------------------------------------------*
DATA:
  gt_mmsiteref   TYPE TABLE OF ty_mmsiteref,
  gt_marc        TYPE TABLE OF ty_marc,
  gt_val_param   TYPE TABLE OF zbtab_val_param,
  gt_dismm       TYPE TABLE OF dismm,
  gt_mver        TYPE TABLE OF ty_mver,
  gt_mveg_ueb    TYPE TABLE OF mveg_ueb,
  gt_mveu_ueb    TYPE TABLE OF mveu_ueb,
  gt_merrdat_f   TYPE TABLE OF merrdat_f,
  gt_log_consumo TYPE TABLE OF zlog_consumo_log,
  gt_log_alv     TYPE TABLE OF ty_log_alv.

*----------------------------------------------------------------------*
* Structures
*----------------------------------------------------------------------*
DATA:
  gs_val_param   TYPE zbtab_val_param,
  gs_dismm       TYPE dismm,
  gs_marc        TYPE ty_marc,
  gs_mver        TYPE ty_mver,
  gs_mveg_ueb    TYPE mveg_ueb,
  gs_merrdat_f   TYPE merrdat_f,
  gs_log_consumo TYPE zlog_consumo_log,
  gs_log_alv     TYPE ty_log_alv.

*----------------------------------------------------------------------*
* Ranges
*----------------------------------------------------------------------*
DATA:
  gr_dismm TYPE RANGE OF dismm,
  rs_dismm LIKE LINE OF gr_dismm,
  gr_gjahr TYPE RANGE OF gjahr,
  rs_gjahr LIKE LINE OF gr_gjahr.

*----------------------------------------------------------------------*
* Variables
*----------------------------------------------------------------------*
DATA:
  gv_week       TYPE scal-week,
  gv_week_pre   TYPE scal-week,
  gv_week_times TYPE n LENGTH 2,

  gv_gjahr      TYPE mver-gjahr,
  gv_ertag      TYPE datum,
  gv_zahlr      TYPE i,

  gv_posi       TYPE n LENGTH 2,
  gv_field      TYPE string,
  gv_total      TYPE mgvbr,
  gv_year       TYPE n LENGTH 4,
  gv_subseq     TYPE i,
  gv_subseq_dt  TYPE datum,
  gv_subseq_wk  TYPE scal-week,
  gv_first_week     TYPE n LENGTH 2,

  gv_posi_now   TYPE n LENGTH 2,
  gv_zahlr_now  TYPE i VALUE 1,

  gv_not_found  TYPE c,
  gv_status     TYPE c,
  gv_message    TYPE string.

*----------------------------------------------------------------------*
* Field Symbols
*----------------------------------------------------------------------*
FIELD-SYMBOLS:
  <fs_value>    TYPE mgvbr,
  <fs_mver>     TYPE ty_mver,
  <fs_mveg_ueb> TYPE mveg_ueb.

*----------------------------------------------------------------------*
* Objects
*----------------------------------------------------------------------*
DATA:
  go_table   TYPE REF TO cl_salv_table,
  go_funct   TYPE REF TO cl_salv_functions,
  go_columns TYPE REF TO cl_salv_columns_table,
  go_column  TYPE REF TO cl_salv_column_table.

*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  SELECT werks
    FROM mmsiteref
    INTO TABLE gt_mmsiteref
   WHERE refsite EQ p_refsit
     AND ass_to  GE sy-datum.

  IF sy-subrc EQ 0.

    SELECT SINGLE *
      FROM zbtab_val_param
      INTO gs_val_param
     WHERE programa EQ 'ZCARGA_CONSUMO_MVER'
       AND campo    EQ 'DISMM'.

    IF sy-subrc EQ 0.

      SPLIT gs_val_param-valor AT ';' INTO TABLE gt_dismm.

      LOOP AT gt_dismm INTO gs_dismm.
        rs_dismm-sign   = 'I'.
        rs_dismm-option = 'EQ'.
        rs_dismm-low    = gs_dismm.
        APPEND rs_dismm TO gr_dismm.
        CLEAR rs_dismm.
      ENDLOOP.

      SELECT rc~matnr rc~werks kt~maktx
        FROM marc AS rc INNER JOIN makt AS kt
          ON rc~matnr EQ kt~matnr
        INTO TABLE gt_marc
     FOR ALL ENTRIES IN gt_mmsiteref
       WHERE rc~matnr IN s_matnr
         AND rc~werks EQ gt_mmsiteref-werks
         AND rc~dismm IN gr_dismm
         AND kt~spras EQ 'PT'.

      IF sy-subrc EQ 0.

        SORT gt_marc BY matnr werks.

        "Identifica a semana no ano atual
        CALL FUNCTION 'DATE_GET_WEEK'
          EXPORTING
            date         = sy-datum
          IMPORTING
            week         = gv_week "yyyy.ww
          EXCEPTIONS
            date_invalid = 1
            OTHERS       = 2.

        IF gv_week+4(2) EQ 01.
          gv_year = gv_week(4) - 1.
          CONCATENATE gv_year '52' INTO gv_week_pre.
        ELSE.
          gv_week_pre = gv_week - 1.
        ENDIF.

        "Identifica a data da semanaa anterior para aualizar com 0,100
        CALL FUNCTION 'WEEK_GET_FIRST_DAY'
          EXPORTING
            week         = gv_week_pre
          IMPORTING
            date         = gv_ertag "yyyy.mm.dd
          EXCEPTIONS
            week_invalid = 1
            OTHERS       = 2.

        gv_week_times = gv_week_pre+4(2).

        "Se posiciona dentro dos registros subsequentes
        DO gv_week_times TIMES.
          ADD 1 TO gv_posi_now.

          IF gv_posi_now EQ 14.
            ADD 1 TO gv_zahlr_now.
            gv_posi_now = 01.
          ENDIF.
        ENDDO.

        IF gv_week+4(2) LT 13.
          rs_gjahr-option = 'BT'.
          rs_gjahr-low    = sy-datum(4) - 1.
          rs_gjahr-high   = gv_week(4).
        ELSE.
          rs_gjahr-option = 'EQ'.
          rs_gjahr-low    = gv_week(4).
        ENDIF.

        rs_gjahr-sign = 'I'.
        APPEND rs_gjahr TO gr_gjahr.
        CLEAR rs_gjahr.

        SELECT matnr werks gjahr zahlr mgv01 mgv02 mgv03 mgv04 mgv05
               mgv06 mgv07 mgv08 mgv09 mgv10 mgv11 mgv12 mgv13
          FROM mver
          INTO TABLE gt_mver
       FOR ALL ENTRIES IN gt_marc
         WHERE matnr EQ gt_marc-matnr
           AND werks EQ gt_marc-werks
           AND gjahr IN gr_gjahr
           AND perkz EQ 'W'.

        DELETE gt_mver WHERE gjahr NE sy-datum(4) AND zahlr NE 4.
        SORT gt_mver BY matnr werks zahlr gjahr.

        "Identifica os MATNR e WERKS não cadastrado na MVER
        LOOP AT gt_marc INTO gs_marc.

          gv_subseq = gv_zahlr_now.

          DO gv_zahlr_now TIMES.
            READ TABLE gt_mver TRANSPORTING NO FIELDS WITH KEY matnr = gs_marc-matnr
                                                               werks = gs_marc-werks
                                                               zahlr = gv_subseq BINARY SEARCH.
            IF sy-subrc NE 0.
              "Se posiciona na primeira semana de cada registro subsequente
              CASE gv_subseq.
                WHEN 1.
                  gv_first_week = 01.
                WHEN 2.
                  gv_first_week = 14.
                WHEN 3.
                  gv_first_week = 27.
                WHEN 4.
                  gv_first_week = 40.
              ENDCASE.

              CONCATENATE sy-datum(4) gv_first_week INTO gv_subseq_wk.

              CALL FUNCTION 'WEEK_GET_FIRST_DAY'
                EXPORTING
                  week         = gv_subseq_wk
                IMPORTING
                  date         = gv_subseq_dt "yyyy.mm.dd
                EXCEPTIONS
                  week_invalid = 1
                  OTHERS       = 2.

              gs_mveg_ueb-matnr = gs_marc-matnr.
              gs_mveg_ueb-werks = gs_marc-werks.
              gs_mveg_ueb-ertag = gv_subseq_dt.
              gs_mveg_ueb-kovbw = '0.100'.
              gs_mveg_ueb-kzexi = 'X'.
              APPEND gs_mveg_ueb TO gt_mveg_ueb.
              CLEAR gs_mveg_ueb.

            ENDIF.

            SUBTRACT 1 FROM gv_subseq.

          ENDDO.
        ENDLOOP.

        IF gt_mveg_ueb IS NOT INITIAL.

          "Cria os registros na MVER
          CALL FUNCTION 'MVER_MAINTAIN_DARK'
            TABLES
              amveg_ueb       = gt_mveg_ueb
              amveu_ueb       = gt_mveu_ueb
              amerrdat_f      = gt_merrdat_f
            EXCEPTIONS
              update_error    = 1
              internal_error  = 2
              too_many_errors = 3
              OTHERS          = 4.

          IF sy-subrc EQ 0.
            COMMIT WORK AND WAIT.

            "Zera os valores dos registros recém criados
            LOOP AT gt_mveg_ueb ASSIGNING <fs_mveg_ueb>.
              <fs_mveg_ueb>-kovbw = '0.000'.
            ENDLOOP.

            CALL FUNCTION 'MVER_MAINTAIN_DARK'
              TABLES
                amveg_ueb       = gt_mveg_ueb
                amveu_ueb       = gt_mveu_ueb
                amerrdat_f      = gt_merrdat_f
              EXCEPTIONS
                update_error    = 1
                internal_error  = 2
                too_many_errors = 3
                OTHERS          = 4.

            IF sy-subrc EQ 0.
              COMMIT WORK AND WAIT.

              SELECT matnr werks gjahr zahlr mgv01 mgv02 mgv03 mgv04 mgv05
                     mgv06 mgv07 mgv08 mgv09 mgv10 mgv11 mgv12 mgv13
                FROM mver
                INTO TABLE gt_mver
             FOR ALL ENTRIES IN gt_marc
               WHERE matnr EQ gt_marc-matnr
                 AND werks EQ gt_marc-werks
                 AND gjahr IN gr_gjahr
                 AND perkz EQ 'W'.

              IF sy-subrc EQ 0.

                DELETE gt_mver WHERE gjahr NE sy-datum(4) AND zahlr NE 4.
                SORT gt_mver BY matnr werks zahlr gjahr.

              ENDIF.
            ENDIF.
          ENDIF.

          REFRESH: gt_mveg_ueb.

        ENDIF.

        "MATNR / LOJA
        LOOP AT gt_marc INTO gs_marc.

          gv_posi  = gv_posi_now.
          gv_zahlr = gv_zahlr_now.

          READ TABLE gt_mver ASSIGNING <fs_mver> WITH KEY matnr = gs_marc-matnr
                                                          werks = gs_marc-werks
                                                          zahlr = gv_zahlr BINARY SEARCH.
          IF sy-subrc EQ 0.
            "Verifica se o consumo para as 12 semanas anteriores é igual a zero
            DO 12 TIMES.
              CONCATENATE 'MGV' gv_posi INTO gv_field.
              ASSIGN COMPONENT gv_field OF STRUCTURE <fs_mver> TO <fs_value>.

              gv_total = gv_total + <fs_value>.

              IF gv_posi EQ 01.
                IF gv_zahlr EQ 01.
                  gv_zahlr = 04.
                ELSE.
                  SUBTRACT 1 FROM gv_zahlr.
                ENDIF.

                READ TABLE gt_mver ASSIGNING <fs_mver> WITH KEY matnr = gs_marc-matnr
                                                                werks = gs_marc-werks
                                                                zahlr = gv_zahlr BINARY SEARCH.
                IF sy-subrc NE 0.
                  gv_not_found = 'X'.
                  EXIT.
                ELSE.
                  gv_posi = 14.
                ENDIF.
              ENDIF.

              SUBTRACT 1 FROM gv_posi.
            ENDDO.

            IF gv_total IS NOT INITIAL.
              CLEAR: gv_total.
              CONTINUE.
            ELSEIF gv_not_found IS NOT INITIAL.
              CLEAR: gv_not_found.
              CONTINUE.
            ENDIF.

            "MVER
            gs_mveg_ueb-matnr = <fs_mver>-matnr.
            gs_mveg_ueb-werks = <fs_mver>-werks.
            gs_mveg_ueb-ertag = gv_ertag.
            gs_mveg_ueb-kovbw = '0.100'.
            gs_mveg_ueb-kzexi = 'X'.
            APPEND gs_mveg_ueb TO gt_mveg_ueb.
            CLEAR gs_mveg_ueb.

          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
* End of Selection
*----------------------------------------------------------------------*
END-OF-SELECTION.

  IF gt_mveg_ueb IS NOT INITIAL.

    "Atualiza a tabela MVER
    CALL FUNCTION 'MVER_MAINTAIN_DARK'
      TABLES
        amveg_ueb       = gt_mveg_ueb
        amveu_ueb       = gt_mveu_ueb
        amerrdat_f      = gt_merrdat_f
      EXCEPTIONS
        update_error    = 1
        internal_error  = 2
        too_many_errors = 3
        OTHERS          = 4.

    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.

      SORT gt_merrdat_f BY matnr werks.

      LOOP AT gt_mveg_ueb INTO gs_mveg_ueb.

        READ TABLE gt_merrdat_f INTO gs_merrdat_f WITH KEY matnr = gs_mveg_ueb-matnr
                                                           werks = gs_mveg_ueb-werks BINARY SEARCH.
        IF sy-subrc EQ 0.

          CALL FUNCTION 'MESSAGE_TEXT_BUILD'
            EXPORTING
              msgid               = gs_merrdat_f-msgid
              msgnr               = gs_merrdat_f-msgno
              msgv1               = gs_merrdat_f-msgv1
              msgv2               = gs_merrdat_f-msgv2
              msgv3               = gs_merrdat_f-msgv3
              msgv4               = gs_merrdat_f-msgv4
            IMPORTING
              message_text_output = gv_message.

          gv_status = 'E'.
        ELSE.
          gv_status  = 'S'.
          gv_message = text-003.
        ENDIF.

        READ TABLE gt_marc INTO gs_marc WITH KEY matnr = gs_mveg_ueb-matnr
                                                 werks = gs_mveg_ueb-werks BINARY SEARCH.
        IF sy-subrc EQ 0.

          "Tabela de log
          gs_log_consumo-werks     = gs_marc-werks.
          gs_log_consumo-matnr     = gs_marc-matnr.
          gs_log_consumo-maktx     = gs_marc-maktx.
          gs_log_consumo-old_value = '0.000'.
          gs_log_consumo-new_value = '0.100'.
          gs_log_consumo-refsite   = p_refsit.
          gs_log_consumo-ertag     = gv_ertag.
          gs_log_consumo-status    = gv_status.
          gs_log_consumo-repid     = sy-repid.
          gs_log_consumo-uname     = sy-uname.
          gs_log_consumo-datum     = sy-datum.
          gs_log_consumo-uzeit     = sy-uzeit.
          APPEND gs_log_consumo TO gt_log_consumo.
          CLEAR gs_log_consumo.

          "Log em ALV
          gs_log_alv-werks   = gs_marc-werks.
          gs_log_alv-matnr   = gs_marc-matnr.
          gs_log_alv-ertag   = gv_ertag.
          gs_log_alv-week    = gv_week_pre+4(2).
          gs_log_alv-kovbw   = '0.100'.
          gs_log_alv-status  = gv_status.
          gs_log_alv-message = gv_message.
          APPEND gs_log_alv TO gt_log_alv.
          CLEAR gs_log_alv.

        ENDIF.
      ENDLOOP.

      IF gt_log_consumo IS NOT INITIAL.
        MODIFY zlog_consumo_log FROM TABLE gt_log_consumo.
        IF sy-subrc EQ 0.
          COMMIT WORK.
        ENDIF.

        TRY.
            CALL METHOD cl_salv_table=>factory
              IMPORTING
                r_salv_table = go_table
              CHANGING
                t_table      = gt_log_alv.
          CATCH cx_salv_msg.
        ENDTRY.

        go_funct = go_table->get_functions( ).
        go_funct->set_all( abap_true ).

        go_columns = go_table->get_columns( ).
        go_column ?= go_columns->get_column( 'STATUS' ).
        go_column->set_short_text( 'Status' ).
        go_column->set_medium_text( 'Status' ).
        go_column->set_long_text( 'Status' ).

        go_column ?= go_columns->get_column( 'WERKS' ).
        go_column->set_short_text( 'Centro' ).
        go_column->set_medium_text( 'Centro' ).
        go_column->set_long_text( 'Centro' ).

        go_column ?= go_columns->get_column( 'MATNR' ).
        go_column->set_short_text( 'Material' ).
        go_column->set_medium_text( 'Material' ).
        go_column->set_long_text( 'Material' ).

        go_column ?= go_columns->get_column( 'ERTAG' ).
        go_column->set_short_text( 'Período' ).
        go_column->set_medium_text( 'Período' ).
        go_column->set_long_text( 'Período' ).

        go_column ?= go_columns->get_column( 'WEEK' ).
        go_column->set_short_text( 'Semana' ).
        go_column->set_medium_text( 'Semana' ).
        go_column->set_long_text( 'Semana' ).

        go_column ?= go_columns->get_column( 'KOVBW' ).
        go_column->set_short_text( 'Valor' ).
        go_column->set_medium_text( 'Valor' ).
        go_column->set_long_text( 'Valor' ).

        go_column ?= go_columns->get_column( 'MESSAGE' ).
        go_column->set_short_text( 'Mensagem' ).
        go_column->set_medium_text( 'Mensagem' ).
        go_column->set_long_text( 'Mensagem' ).

        go_table->display( ).

      ENDIF.
    ENDIF.

  ELSE.
    "Nenhum consumo atualizado!
    MESSAGE text-002 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.