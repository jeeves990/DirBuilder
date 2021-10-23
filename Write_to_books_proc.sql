CREATE DEFINER=`root`@`localhost` PROCEDURE `Write_to_books`(parm  TEXT)
BEGIN
    DECLARE sql_stmt TEXT;			-- the statement to be prepared
    DECLARE the_keys TEXT;			-- the comma separated list of keys to be inserted into sql_stmt
    DECLARE the_values TEXT;		-- the comma separated list of values to be inserted into sql_stmt
    DECLARE _len  INT;				-- work integer
    DECLARE equal_pos INT;			-- the position in the parsed key=value pair
    DECLARE tab_pos INT;			-- the position in parms of the next TAB delimiter
    DECLARE _pair TEXT;				-- the parsed key=value pair
    DECLARE _key VARCHAR(65);		-- the key parsed from the key=value pair
    DECLARE _value TEXT;			-- the value parsed from the key=value pair
    SET the_keys = '';
    SET the_values = '';
    SET _pair = '';
    SET _key = '';
    SET _value = '';
    
    SET sql_stmt = "INSERT INTO books ([the keys]) VALUES ([the values])";
    SET _len = LENGTH(parm);
/*
    aLoop : WHILE (_len > 0) DO
		SET tab_pos := LOCATE(x'0009', parm);
        SET _pair = TRIM(SUBSTR(parm, 1, tab_pos -1));
        
        SET _key = TRIM(SUBSTR(_pair, 1, equal_pos -1));
        SET the_keys = CONCAT(the_keys, _key, ',');
        
        SET _value = TRIM(SUBSTR(_pair, equal_pos +1));
        SET the_values = CONCAT(the_values, _value, ',');
        
        SET parm = SUBSTR(parm, tab_pos +1);
    
		SET _len = LENGTH(parm);
    END WHILE aLoop;
*/    
END