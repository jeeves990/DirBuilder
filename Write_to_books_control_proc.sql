CREATE DEFINER=`root`@`localhost` PROCEDURE `Write_to_books_control`(parm TEXT, eltDelim char(15), out outS TEXT)
proc_exit : BEGIN
	DECLARE aLine TEXT;
	DECLARE _len INT;
	SET outS := '';
	SET @pos := LOCATE(eltDelim, parm);
	SELECT parm + '; ' + eltDelim + '; ' + @pos;
	IF (@pos = 0) THEN		LEAVE proc_exit;	END IF;

	SET _len = LENGTH(parm);
	aLoop : WHILE _len > 0 DO

		SET aLine := TRIM(SUBSTRING(parm, 1, @pos));

		SET outS := CONCAT(outS, aLine, x'0013', x'0010');
		SET parm := SUBSTRING(parm, @pos +LENGTH(eltDelim) +1);
		SET @pos := LOCATE(eltDelim, parm);
		IF (@pos = 0) THEN  
		  LEAVE aLoop;
		END IF;
        
		SET _len = LENGTH(parm);
	END WHILE aLoop;
  
	SET _len = LENGTH(parm);

	IF _len > 0 THEN
		SET outS = CONCAT( outS, parm, x'0009');
	END IF;

	SELECT outS;


END