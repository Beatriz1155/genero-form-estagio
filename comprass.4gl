SCHEMA "db_estagio"

GLOBALS
   DEFINE gr_compra RECORD LIKE compra.*
   DEFINE ga_compra_l DYNAMIC ARRAY OF RECORD
        cod_artigo LIKE artigo.cod_artigo,
        cod_armazem LIKE armazem.cod_armazem,
        quantidade  LIKE compra_l.quantidade,
        preco_unitario LIKE compra_l.preco_unitario,

        id_artigo LIKE compra_l.id_artigo,
        id_armazem LIKE compra_l.id_armazem
   END RECORD
   DEFINE sr_compra_l RECORD
    cod_artigo      LIKE artigo.cod_artigo,
    cod_armazem     LIKE armazem.cod_armazem,
    quantidade      LIKE compra_l.quantidade,
    preco_unitario  LIKE compra_l.preco_unitario
END RECORD
   DEFINE g_flag_cursor BOOLEAN
END GLOBALS

MAIN
   CLOSE WINDOW SCREEN
   CONNECT TO "db_estagio" USER "admdecisor" USING "tek@nd"
   SET ISOLATION TO DIRTY READ
   OPEN WINDOW w_compra WITH FORM "compras"

   CALL ini_compra()

   DISPLAY "Código Compra:" TO label_cod_comp
   DISPLAY "Data:" TO label_data_comp
   DISPLAY "Ano:" TO label_ano_comp
   DISPLAY "Número:" TO label_num_comp
   DISPLAY "Código Fornecedor:" TO label_cod_forn_comp
   
   MENU
      ON ACTION inserir
         CALL ins_compra()

      ON ACTION consultar
         CALL cons_compra()

      ON ACTION seguinte
         CALL sapu_compra("S")

      ON ACTION anterior
         CALL sapu_compra("A")

      ON ACTION primeiro
         CALL sapu_compra("P")

      ON ACTION ultimo
         CALL sapu_compra("U")

      ON ACTION modificar
         CALL mod_compra()

      ON ACTION eliminar
         CALL eli_compra()

      ON ACTION QUIT
         EXIT MENU

      ON ACTION CLOSE
         EXIT MENU
   END MENU

   CLOSE WINDOW w_compra
   DISCONNECT CURRENT
END MAIN 

-------------------------------------------------------------------------------

FUNCTION ini_compra()
   LET g_flag_cursor = FALSE
   INITIALIZE gr_compra.* TO NULL
END FUNCTION

-------------------------------------------------------------------------------

FUNCTION ins_compra()
   DEFINE l_ok BOOLEAN
   DEFINE i SMALLINT

    BEGIN WORK

   INITIALIZE gr_compra.* TO NULL
   LET l_ok = inp_compra()

   IF l_ok THEN

      TRY
        LET gr_compra.id_compra = 0
        LET gr_compra.cod_compra = "G"
        
        INSERT INTO compra (
            cod_compra, ano_compra, num_compra, data_compra, id_fornecedor
        ) VALUES (
            gr_compra.cod_compra, gr_compra.ano_compra,
            gr_compra.num_compra, gr_compra.data_compra,
            gr_compra.id_fornecedor
        )
        CATCH
            LET l_ok = FALSE
        END TRY
    END IF

    IF l_ok THEN
      LET gr_compra.id_compra = SQLCA.sqlerrd[2]
    END IF

    IF l_ok THEN
        FOR i = 1 TO ga_compra_l.getLength()
            TRY
                INSERT INTO compra_l
                    (
                    ID_COMPRA_L,Id_compra,Id_artigo,Id_armazem,quantidade,preco_unitario
                    )
                    VALUES 
                        (
                        0, 
                        gr_compra.id_compra, 
                        ga_compra_l[i].id_artigo, 
                        ga_compra_l[i].id_armazem, 
                        ga_compra_l[i].quantidade, 
                        ga_compra_l[i].preco_unitario
                        )
            CATCH
                LET l_ok = FALSE
            END TRY
        END FOR
      END IF

      IF l_ok THEN
        COMMIT WORK
        MESSAGE "Compra registada com sucesso!"
    ELSE
        ROLLBACK WORK
        ERROR "Ação cancelada pelo utilizador ou dados inválidos."
    END IF
END FUNCTION

---------------------------------------------------------------------------------

FUNCTION inp_compra()
   DEFINE l_existe INTEGER
   DEFINE c_codFornecedor CHAR(20)
   DEFINE int_flag BOOLEAN

   LET int_flag = FALSE

   LET gr_compra.data_compra = TODAY
   LET gr_compra.ano_compra = YEAR(gr_compra.data_compra)
   
   DIALOG
      INPUT gr_compra.cod_compra, gr_compra.ano_compra, gr_compra.num_compra, gr_compra.data_compra, c_codFornecedor
         FROM cod_comp, ano_comp, num_comp, data_comp, cod_forn
         ATTRIBUTES(WITHOUT DEFAULTS)

         AFTER FIELD cod_comp
            IF gr_compra.cod_compra IS NULL THEN
               ERROR "Tem de introduzir o código da compra"
               NEXT FIELD CURRENT
            END IF

         AFTER FIELD cod_forn
            IF c_codFornecedor IS NULL THEN
               ERROR "Tem de introduzir o código do fornecedor"
               NEXT FIELD CURRENT
            END IF

            SELECT COUNT(*) INTO l_existe 
            FROM fornecedor 
            WHERE cod_fornecedor = c_codFornecedor

            IF l_existe != 1 THEN
               ERROR "O código do fornecedor não existe na base de dados"
               NEXT FIELD CURRENT
            ELSE
               SELECT id_fornecedor INTO gr_compra.id_fornecedor
               FROM fornecedor
               WHERE cod_fornecedor = c_codFornecedor
            END IF

         AFTER FIELD data_comp
            IF gr_compra.data_compra IS NOT NULL THEN
                LET gr_compra.ano_compra = YEAR(gr_compra.data_compra)
            END IF
      END INPUT

    INPUT ARRAY ga_compra_l FROM sr_compra_l.*
    ATTRIBUTES(WITHOUT DEFAULTS)
            BEFORE ROW
            -- validações
            IF ga_compra_l[arr_curr()].cod_artigo IS NOT NULL THEN
                SELECT id_artigo INTO ga_compra_l[arr_curr()].id_artigo
                FROM artigo
                WHERE cod_artigo = ga_compra_l[arr_curr()].cod_artigo

                IF SQLCA.SQLCODE != 0 THEN
                   ERROR "Artigo inválido"
                   NEXT FIELD sr_compra_l.cod_artigo
                END IF
            END IF
            
            IF ga_compra_l[arr_curr()].cod_armazem IS NOT NULL THEN
                SELECT id_armazem INTO ga_compra_l[arr_curr()].id_armazem
                FROM armazem
                WHERE cod_armazem = ga_compra_l[arr_curr()].cod_armazem

                IF SQLCA.SQLCODE != 0 THEN
                   ERROR "Armazém inválido"
                   NEXT FIELD sr_compra_l.cod_armazem
                END IF
            END IF
            
            AFTER ROW
                -- Validar dados (exemplo)
                IF ga_compra_l[arr_curr()].quantidade <= 0 THEN
                    ERROR "A quantidade deve ser maior que zero."
                    NEXT FIELD quantidade
                END IF
      END INPUT
      
      ON ACTION CLOSE
         LET int_flag = TRUE
         EXIT DIALOG

      ON ACTION CANCEL
         LET int_flag = TRUE
         EXIT DIALOG

      ON ACTION ACCEPT
         ACCEPT DIALOG

   END DIALOG

   -- Se a data_compra ainda não está preenchida, cancelar inserção
   IF gr_compra.data_compra IS NULL OR gr_compra.data_compra = "" THEN
      ERROR "Data inválida. Não pode continuar sem uma data válida."
      RETURN FALSE
   END IF

   RETURN (int_flag == FALSE)
END FUNCTION

----------------------------------------------------------------------------

FUNCTION cons_compra()
    DEFINE l_comando STRING

    LET l_comando =
        "SELECT compra.cod_compra, compra.ano_compra, compra.num_compra, compra.data_compra, " ||
        "fornecedor.cod_fornecedor, fornecedor.nome_fornecedor, fornecedor.morada_fornecedor, fornecedor.localidade_fornecedor " ||
        "FROM compra " ||
        "LEFT JOIN fornecedor ON compra.id_fornecedor = fornecedor.id_fornecedor"

    PREPARE com_compra FROM l_comando
    DECLARE cur_compra SCROLL CURSOR FOR com_compra
    OPEN cur_compra

    LET g_flag_cursor = TRUE
    CALL sapu_compra("P")
END FUNCTION

----------------------------------------------------------------------------

FUNCTION sapu_compra(p_tipo CHAR(1))
    DEFINE cod CHAR(10), ano SMALLINT, num SMALLINT, data_compra DATE
    DEFINE cod_f CHAR(10), nome_f CHAR(50), morada_f CHAR(100), local_f CHAR(50)

    IF g_flag_cursor THEN
        CASE p_tipo
            WHEN "P"
                FETCH FIRST cur_compra
                    INTO cod, ano, num, data_compra,
                         cod_f, nome_f, morada_f, local_f
            WHEN "U"
                FETCH LAST cur_compra
                    INTO cod, ano, num, data_compra,
                         cod_f, nome_f, morada_f, local_f
            WHEN "S"
                FETCH NEXT cur_compra
                    INTO cod, ano, num, data_compra,
                         cod_f, nome_f, morada_f, local_f
            WHEN "A"
                FETCH PREVIOUS cur_compra
                    INTO cod, ano, num, data_compra,
                         cod_f, nome_f, morada_f, local_f
        END CASE

        IF sqlca.sqlcode == 100 THEN
            ERROR "Não existem registos nestas condições"
        ELSE
            LET gr_compra.cod_compra = cod
            LET gr_compra.ano_compra = ano
            LET gr_compra.num_compra = num
            LET gr_compra.data_compra = data_compra
            LET gr_compra.id_fornecedor = cod_f  -- se existir este campo temporário

            DISPLAY gr_compra.cod_compra, gr_compra.ano_compra, gr_compra.num_compra, gr_compra.data_compra,
                    cod_f, nome_f, morada_f, local_f
                TO cod_compra_f, ano_compra_f, num_compra_f, data_compra_f,
                   cod_fornecedor_f, nome_fornecedor_f, morada_fornecedor_f, localidade_fornecedor_f
        END IF
    ELSE
        ERROR "Tem de consultar primeiro"
    END IF
END FUNCTION

------------------------------------------------------------------------------------------

FUNCTION mod_compra()
    DEFINE l_ok BOOLEAN
    DEFINE l_erro STRING
    DEFINE l_id_fornecedor INTEGER

    LET l_ok = TRUE
    LET l_erro = NULL

    -- Verificar se há registo no ecrã
    IF gr_compra.cod_compra IS NULL THEN
        ERROR "Tem de consultar ou inserir para modificar"
        RETURN
    END IF

    -- Iniciar transação
    BEGIN WORK
    LET g_flag_cursor = FALSE

    -- Tentar bloquear o registo
    TRY
        UPDATE compra
           SET cod_compra = cod_compra
         WHERE cod_compra = gr_compra.cod_compra
    CATCH
        LET l_ok = FALSE
        LET l_erro = "Este registo está a ser utilizado por outro utilizador ou não existe."
    END TRY

    -- Recarregar da base de dados
    IF l_ok THEN
        SELECT *
          INTO gr_compra.*
          FROM compra
         WHERE cod_compra = gr_compra.cod_compra

        IF sqlca.sqlcode == 100 THEN
            LET l_ok = FALSE
            LET l_erro = "A sua consulta está desatualizada"
        END IF
    END IF

    -- Input dos dados
    IF l_ok THEN
        LET l_ok = inp_compra()
        IF NOT l_ok THEN
            LET l_erro = "Ação interrompida pelo utilizador"
        END IF
    END IF

    -- Verificar se código de fornecedor inserido existe
    IF l_ok THEN
        SELECT id_fornecedor
          INTO l_id_fornecedor
          FROM fornecedor
         WHERE cod_fornecedor = gr_compra.id_fornecedor

        IF sqlca.sqlcode == 100 THEN
            LET l_ok = FALSE
            LET l_erro = "Código de fornecedor não encontrado"
        ELSE
            LET gr_compra.id_fornecedor = l_id_fornecedor
        END IF
    END IF

    -- Atualizar registo
    IF l_ok THEN
        UPDATE compra
           SET ano_compra = gr_compra.ano_compra,
               num_compra = gr_compra.num_compra,
               data_compra = gr_compra.data_compra,
               id_fornecedor = gr_compra.id_fornecedor
         WHERE cod_compra = gr_compra.cod_compra
    END IF

    -- Commit ou rollback
    IF l_ok THEN
        COMMIT WORK
        ERROR "Registo modificado com sucesso"
    ELSE
        ROLLBACK WORK
        ERROR l_erro
    END IF
END FUNCTION

-----------------------------------------------------------------------------------

FUNCTION eli_compra()
   DEFINE l_ok BOOLEAN
   DEFINE l_erro STRING
   DEFINE l_resposta STRING

   -- Verifica se existe registo no ecrã
   IF (gr_compra.id_compra IS NULL) THEN
      ERROR "Tem de consultar ou inserir para eliminar"
      RETURN
   END IF

   -- Abre transação
   BEGIN WORK
   LET g_flag_cursor = FALSE
   LET l_ok = TRUE
   LET l_erro = NULL

   -- Tenta bloquear o registo
   TRY
      UPDATE compras
         SET id_compra = id_compra
       WHERE id_compra = gr_compras.id_compra
   CATCH
      LET l_ok = FALSE
      LET l_erro = "Este registo está a ser utilizado por outro utilizador/Registo não existente"
   END TRY

   -- Carrega o registo novamente
   IF l_ok THEN
      SELECT *
        INTO gr_compra.*
        FROM compras
       WHERE id_compra = gr_compras.id_compra

      IF (SQLCA.sqlcode == 100) THEN
         LET l_ok = FALSE
         LET l_erro = "A sua consulta está desatualizada"
      END IF
   END IF

   -- Confirmar com o utilizador
   IF l_ok THEN
      LET l_resposta = fgl_winbutton(
         "ATENÇÃO",
         "Deseja eliminar o registo da compra?",
         "Não",
         "Sim|Não",
         "question", 0)

      LET l_ok = (l_resposta == "Sim")
   END IF

   -- Eliminar da base de dados
   IF l_ok THEN
      TRY
         DELETE FROM compras
          WHERE id_compra = gr_compras.id_compra
      CATCH
         LET l_ok = FALSE
         LET l_erro = "Erro ao eliminar o registo"
      END TRY
   END IF

   -- Finalizar transação
   IF l_ok THEN
      COMMIT WORK
      ERROR "Registo eliminado com sucesso"
      CALL ini_compra()  -- limpa estrutura
      CLEAR FORM
   ELSE
      ROLLBACK WORK
      ERROR l_erro
   END IF
END FUNCTION