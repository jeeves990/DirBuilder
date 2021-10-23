CREATE DEFINER=`root`@`localhost` PROCEDURE `doiterate`(p1 INT)
BEGIN
	DECLARE end_value INT;
	label1: LOOP
		SET p1 = p1 + 1;
		SELECT p1;
		IF p1 < p1 + 10 THEN
		  ITERATE label1;
		END IF;
		LEAVE label1;
	END LOOP label1;
	SET @x = p1;
	SELECT @x;
END