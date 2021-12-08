CREATE  PROCEDURE `Insert_into_books` (IN delimiter CHAR(5),
                           IN key_word VARCHAR(25),
                           IN col_clause VARCHAR(200), 
                           IN val_clause VARCHAR(5000),
                           OUT key_word_value VARCHAR(200))
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
    
    SET @delim_pos = LOCATE(key_word, col_clause);
    SET @substr = SUBSTRING(col_clause, 1, @delim_pos -1);
    
    SET @comma_count = 0;
    find_commas:
    LOOP
		SET @i = LOCATE(COMMA, @substr);
        -- IF none are found, LEAVE the loop
        -- this should not occur since it would mean the entire
        -- SUBSTRING was @iterated over without finding the key_word
        IF @i = 0 THEN
			LEAVE find_commas;
        END IF;
        -- a COMMA was found
        SET @comma_count = @comma_count +1;
        -- SELECT @comma_count;
		-- whack the first element
        SET @substr = SUBSTRING(@substr, @i+1);
        -- SELECT @substr;
    END LOOP;
	
	SET @substr = val_clause;
    
    BEGIN  -- whack enclosing parens
		IF SUBSTRING(@substr, 1, 1) = OPEN_PAREN THEN
			SET @substr = SUBSTRING(@substr, 2);
		END IF;
    SELECT @substr;
    SELECT RIGHT(@substr, 1);
		IF RIGHT(@substr, 1) = CLOSE_PAREN THEN
			SET @substr = SUBSTRING(@substr, 1, LENGTH(@substr) -1);
            SELECT 'INSIDE THE IF STATEMENT';
		END IF;
    SELECT @substr;
	END;
    
    -- add a stopper COMMA to @substr
	SET @substr = CONCAT(@substr, COMMA);
    
    SET @key_word_len = LENGTH(key_word);
    SET key_word = LOWER(key_word);
    
    SET @i = 1;
    -- find the value associated with key_word (a column) in val_clause
    eliminator:
    WHILE @i < @comma_count +1 do
		SET @delim_pos = LOCATE(COMMA, @substr);
        -- if this is the end of the val_clause (@substr) leave
        IF @delim_pos = 0 THEN
			SET key_word_value = '';
			LEAVE storedProc;
        END IF;
        SET @substr = SUBSTRING(@substr, @delim_pos +1);
        
		SELECT @substr;
        SET @i = @i +1;
	END WHILE eliminator;
    
    SET @delim_pos = LOCATE(COMMA, @substr);
        
    SET key_word_value = SUBSTRING(@substr, 1, @delim_pos -1);
    
END