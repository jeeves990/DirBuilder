CREATE PROCEDURE `insert_rows_to_books_ex` (
				IN DELIMITER CHAR(5),
				IN col_clause VARCHAR(2000), 
				IN val_clause VARCHAR(5000))
    
storedProc : BEGIN
	DECLARE COMMA CHAR(1) DEFAULT ',';
    DECLARE OPEN_PAREN CHAR(1) DEFAULT '(';
    DECLARE CLOSE_PAREN CHAR(1) DEFAULT ')';
    
    DECLARE message varchar(500);
    
    -- Error handling start here
	BEGIN
		DECLARE EXIT HANDLER FOR SQLEXCEPTION
		BEGIN
			GET DIAGNOSTICS CONDITION 1 @ERRNO = MYSQL_ERRNO, @MESSAGE_TEXT = MESSAGE_TEXT;
			SET MESSAGE = CONCAT(@MESSAGE_TEXT," : Erro code - ",@ERRNO);
			ROLLBACK; -- if any error occures it will rollback changes
		END;
	END;

	/*************************************************************************************
	 *	col_clause is a comma separated list of field/column names from books_ex which
     *  may come enclosed by parens.
     *************************************************************************************/
    SET @cols = Strip_parens(col_clause);
    SET @col_count = 0;
    
    /*************************************************************************************
	 * 	need a string of '?'s (question marks) separated by COMMA's for the insert_stmt
     * 	parameterized query created later in this script.
     *************************************************************************************/
    create_question_mark_string:
    BEGIN
		SET @col_count = Count_elements_in_string(COMMA, @cols);
		SET @question_mark_str = '';
		create_question_marks:
		WHILE @col_count > 0 DO
			SET @question_mark_str = CONCAT(@question_mark_str, '?,');
			SET @col_count = @col_count -1;
		END WHILE create_question_marks;
    
		IF RIGHT(@question_mark_str, 1) = COMMA THEN
			SET @question_mark_str = SUBSTRING(@question_mark_str, 1, length(@question_mark_str) -1);
		END IF;
    END create_question_mark_string;
    
    SELECT
    SET @insert_stmt = CONCAT('INSERT INTO books_ex ', col_clause, ' VALUES ', OPEN_PAREN, @question_mark_str, CLOSE_PAREN);
    SELECT @insert_stmt;
    
    /*
    PREPARE insert_stmt FROM @insert_stmt;
    SELECT 'AFTER PREPARE insert_stmt';
   
    SET @delim_len = LENGTH(delimiter);
    SET @val_set = '';

	-- /*************************************************************************************
    -- *  val_clause is "delimiter" separated list of column value strings. 
    -- *	These column value strings are (usually) enclosed in parens. The 
    -- *	The column value strings are a comma separated list of values.
	-- *************************************************************************************
    insert_to_books_iterator:
    WHILE LENGTH(val_clause) > 0 DO   
		SET @delim_pos = LOCATE(delimiter, val_clause);
        -- get the first set of column values in val_clause
        SET @val_set = SUBSTRING(val_clause, 1, @delim_pos -1);
        SELECT @val_set;
        
        whack_parens_comma:
        BEGIN
			SET @val_set = Strip_parens(@val_set);
			 
             IF RIGHT(@val_set, 1) = COMMA THEN
 				SET @val_set = SUBSTRING(@val_set, 1, LENGTH(@val_set) -1);
             END IF;
         END whack_parens_comma;
        
        place_values_in_question_mark_list:
        LOOP
			-- get first value set from @val_set
			SET @comma_pos_val_set = LOCATE(COMMA, @val_set);
            SET @aval_set = LEFT(@val_set, @comma_pos_val_set -1);
            SELECT @val_set;
		    EXECUTE insert_stmt USING @aval_set;
            
            -- remove first value set
            SET @val_set = SUBSTRING(@val_set, @comma_pos_val_set +1);
        END LOOP place_values_in_question_mark_list;
        
      
    
    END WHILE insert_to_books_iterator;

	DEALLOCATE PREPARE insert_stmt;
    */
END