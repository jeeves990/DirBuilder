CREATE DEFINER=`root`@`%` PROCEDURE `insert_rows_to_books_ex_TEST`(
				IN DELIMITER CHAR(5),
				IN col_clause VARCHAR(2000), 
				IN val_clause VARCHAR(5000))
storedProc : BEGIN
	DECLARE COMMA CHAR(1) DEFAULT ',';
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
    
	SET @OPEN_PAREN = '(';
    SET @CLOSE_PAREN = ')';
    SET @Q_MARK_STR = CONCATE(@OPEN_PAREN, @question_mark_str, @CLOSE_PAREN);
    SET @insert_stmt = CONCAT('INSERT INTO books_ex ', col_clause, ' VALUES ', @Q_MARK_STR);
    SELECT @insert_stmt;
    

END