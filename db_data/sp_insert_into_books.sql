CREATE  DEFINER=`sherril`@`%` PROCEDURE `Insert_into_books`(IN delimiter CHAR(5),
															IN key_word VARCHAR(25),
															IN col_clause VARCHAR(200), 
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
	
    SET @cols = col_clause;
    SET @col_count = 0;
    
    -- precondition: col_clause is a string of comma separated strings
    -- enclosed in parens
    BEGIN   -- strip away the parens
		IF LEFT(@cols, 1) = OPEN_PAREN THEN
			SET @cols = SUBSTRING(@cols, 2);
		END IF;
		IF RIGHT(@cols, 1) = CLOSE_PAREN THEN
			SET @cols = LEFT(@cols, LENGTH(@cols) -1);
		END IF;
    END;
    
    SET @question_str = '';
    count_cols:
    WHILE length(@cols) > 0 DO
		SET @pos = LOCATE(COMMA, @Cols);
        IF @pos = 0 THEN
			SET @col_count = @col_count +1;
            SET @question_str = CONCAT(@question_str, '?');
            LEAVE count_cols;
        END IF;
        
        SET @col_count = @col_count +1;
		SET @question_str = CONCAT(@question_str, '?,');
            
        SET @cols = LTRIM(SUBSTRING(@cols, @pos +1));
    END WHILE count_cols;
    
    -- SELECT @question_str;
    
    SET @check_stmt = CONCAT('SELECT 1 FROM books WHERE ', key_word , ' = ?'); 
    -- SELECT 'AFTER SET @check_stmt = ';
    SELECT @check_stmt;
    
    PREPARE check_stmt FROM @check_stmt;
    -- SELECT 'AFTER PREPARE check_stmt';
    
    SET @insert_stmt = CONCAT('INSERT INTO books ', col_clause, ' VALUES ', OPEN_PAREN, @question_str, CLOSE_PAREN);
    SELECT @insert_stmt;
    
    PREPARE insert_stmt FROM @insert_stmt;
    SELECT 'AFTER PREPARE insert_stmt';
    
    CALL Get_key_word_value(delimiter, key_word, col_clause, val_clause, @key_word_value);
    SELECT @key_word_value;
    
    SET @delim_len = LENGTH(delimiter);
	CREATE TEMPORARY TABLE IF NOT EXIST tmp_table(acol varchar(2000), id integer);
	
    SET @val_set = '';
	insert_iterator:
    WHILE LENGTH(val_clause) > 0 DO
		SET @delim_pos = LOCATE(delimiter, val_clause);
        SET @val_set = SUBSTRING(val_clause, 1, delimiter -1);
        SELECT @val_set;
        
        -- whack the parens and comma
        BEGIN
	   
			IF LEFT(@val_set, 1) = OPEN_PAREN THEN
 				SET @val_set = SUBSTRING(@val_set, 2);
 			END IF;
             
 			IF RIGHT(@val_set, 1) = CLOSE_PAREN THEN
 				SET @val_set = LEFT(@val_set, 1, LENGTH(@val_set) -1));
 			END IF;
             
             IF RIGHT(@val_set, 1) = COMMA THEN
 				SET @val_set = LEFT(@val_set, 1, LENGTH(@val_set) -1));
             END IF;
         END;
        
        DELETE FROM tmp_table;
        SET @i = 0;
        collect_values:
        LOOP
			SET @pos = LOCATE(COMMA, @val_set);
            SET @i = @i + 1;
            IF @pos = 0 THEN
				INSERT INTO tmp_table VALUES(@str, @i);
                LEAVE collect_values;
            END IF;
            SET @str = LEFT(@val_set, @pos -1);
            
		    INSERT INTO tmp_table VALUES(@str, @i);
            SET @val_set = SUBSTRING(@val_set, @pos +1);
        END LOOP collect_values;
        
        -- check if key_word is unique 
        SET @avalue = 0;
		BEGIN
			CALL Get_key_word_value(delimiter, key_word, col_clause, val_clause, @key_word_value);
        
			EXECUTE check_stmt USING @key_word_value;  --  INTO @bool;	
			IF NOT @avalue THEN
				EXECUTE insert_stmt USING @val_set;
                SELECT acol FROM tmp_table ORDER BY id;
			END IF;
		END;
        
        -- trim off used @val_set
        SET val_clause = SUBSTRING(val_clause, @delim_pos + @delim_len);
		SELECT val_clause;
    
    END WHILE insert_iterator;

   
   DEALLOCATE PREPARE check_stmt;
   DEALLOCATE PREPARE insert_stmt;
END