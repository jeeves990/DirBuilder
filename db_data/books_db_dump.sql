-- MySQL dump 10.13  Distrib 8.0.27, for Win64 (x86_64)
--
-- Host: localhost    Database: books_db
-- ------------------------------------------------------
-- Server version	8.0.27

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!50503 SET NAMES utf8mb4 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Current Database: `books_db`
--

CREATE DATABASE /*!32312 IF NOT EXISTS*/ `books_db` /*!40100 DEFAULT CHARACTER SET latin1 */ /*!80016 DEFAULT ENCRYPTION='N' */;

USE `books_db`;

--
-- Table structure for table `books`
--

DROP TABLE IF EXISTS `books`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `books` (
  `id` int NOT NULL AUTO_INCREMENT,
  `msku` varchar(45) DEFAULT NULL,
  `title` varchar(300) NOT NULL,
  `asin` varchar(45) DEFAULT NULL,
  `list_price` decimal(10,4) DEFAULT NULL,
  `cost` decimal(10,4) DEFAULT NULL,
  `supplier` varchar(100) DEFAULT NULL,
  `date_listed` datetime DEFAULT NULL,
  `_rank_` int DEFAULT NULL,
  `_condition_` varchar(45) DEFAULT NULL,
  `notes` varchar(1000) DEFAULT NULL,
  `quantity` decimal(10,3) DEFAULT NULL,
  `min_price` decimal(10,4) DEFAULT NULL,
  `max_price` decimal(10,4) DEFAULT NULL,
  `qty` int DEFAULT NULL,
  `expDate` date DEFAULT NULL,
  `destination` varchar(45) DEFAULT NULL,
  `_date_` date DEFAULT NULL,
  `fnsku` varchar(25) DEFAULT NULL,
  `cost_unit` decimal(10,4) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `primary_ndx` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=14100 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `books`
--

LOCK TABLES `books` WRITE;
/*!40000 ALTER TABLE `books` DISABLE KEYS */;
/*!40000 ALTER TABLE `books` ENABLE KEYS */;
UNLOCK TABLES;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8mb4 */ ;
/*!50003 SET character_set_results = utf8mb4 */ ;
/*!50003 SET collation_connection  = utf8mb4_0900_ai_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'STRICT_TRANS_TABLES,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
/*!50003 CREATE*/ /*!50017 DEFINER=`sherril`@`%`*/ /*!50003 TRIGGER `books_BEFORE_INSERT` BEFORE INSERT ON `books` FOR EACH ROW BEGIN
	    
	DECLARE EXIT HANDLER FOR SQLEXCEPTION
        RESIGNAL;
    DECLARE EXIT HANDLER FOR SQLWARNING
        RESIGNAL;
    DECLARE EXIT HANDLER FOR NOT FOUND
        RESIGNAL; 
    
    IF (NEW.cost IS NULL) THEN
		SET NEW.cost = NEW.cost_unit;
        -- INSERT INTO LOG SELECT now(), concat('NEW.COST', CAST(NEW.COST AS VARCHAR);
	END IF;
	IF (NEW.quantity IS NULL) then
		SET NEW.quantity = NEW.qty;
	END IF;
	IF (NEW.supplier IS NULL) THEN
		SET NEW.supplier = 'EST';
	END IF;
    IF (NEW.date_listed IS NULL) THEN
    	 SET NEW.date_listed = CAST(NEW._date_ AS DATETIME);
	END IF;
    IF (NEW.supplier IS NULL) OR (NEW.supplier = '') THEN
		SET NEW.supplier = 'EST';
	END IF;
    
END */;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;

--
-- Table structure for table `books-listed-on`
--

DROP TABLE IF EXISTS `books-listed-on`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `books-listed-on` (
  `id` int NOT NULL AUTO_INCREMENT,
  `listing-entities_id` int NOT NULL,
  `books_id` int NOT NULL,
  PRIMARY KEY (`id`,`listing-entities_id`,`books_id`),
  KEY `fk_books-listed-on_listing-entities1_idx` (`listing-entities_id`),
  KEY `fk_books-listed-on_books1_idx` (`books_id`),
  CONSTRAINT `fk_books-listed-on_books1` FOREIGN KEY (`books_id`) REFERENCES `books` (`id`),
  CONSTRAINT `fk_books-listed-on_listing-entities1` FOREIGN KEY (`listing-entities_id`) REFERENCES `listing-entities` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `books-listed-on`
--

LOCK TABLES `books-listed-on` WRITE;
/*!40000 ALTER TABLE `books-listed-on` DISABLE KEYS */;
/*!40000 ALTER TABLE `books-listed-on` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `books-stored-at`
--

DROP TABLE IF EXISTS `books-stored-at`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `books-stored-at` (
  `id` int NOT NULL AUTO_INCREMENT,
  `books_id` int NOT NULL,
  `storage-locations_id` int NOT NULL,
  PRIMARY KEY (`id`,`books_id`,`storage-locations_id`),
  KEY `fk_books-stored-at_books1_idx` (`books_id`),
  KEY `fk_books-stored-at_storage-locations1_idx` (`storage-locations_id`),
  CONSTRAINT `fk_books-stored-at_books1` FOREIGN KEY (`books_id`) REFERENCES `books` (`id`),
  CONSTRAINT `fk_books-stored-at_storage-locations1` FOREIGN KEY (`storage-locations_id`) REFERENCES `storage-locations` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `books-stored-at`
--

LOCK TABLES `books-stored-at` WRITE;
/*!40000 ALTER TABLE `books-stored-at` DISABLE KEYS */;
/*!40000 ALTER TABLE `books-stored-at` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `books_ex`
--

DROP TABLE IF EXISTS `books_ex`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `books_ex` (
  `id` int DEFAULT NULL,
  `msku` varchar(45) DEFAULT NULL,
  `title` varchar(300) DEFAULT NULL,
  `asin` varchar(45) DEFAULT NULL,
  `list_price` decimal(10,4) DEFAULT NULL,
  `cost` decimal(10,4) DEFAULT NULL,
  `supplier` varchar(100) DEFAULT NULL,
  `date_listed` datetime DEFAULT NULL,
  `_rank_` int DEFAULT NULL,
  `_condition_` varchar(45) DEFAULT NULL,
  `notes` varchar(1000) DEFAULT NULL,
  `quantity` decimal(10,3) DEFAULT NULL,
  `min_price` decimal(10,4) DEFAULT NULL,
  `max_price` decimal(10,4) DEFAULT NULL,
  `qty` int DEFAULT NULL,
  `expDate` date DEFAULT NULL,
  `destination` varchar(45) DEFAULT NULL,
  `_date_` date DEFAULT NULL,
  `fnsku` varchar(25) DEFAULT NULL,
  `cost_unit` decimal(10,4) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `books_ex`
--

LOCK TABLES `books_ex` WRITE;
/*!40000 ALTER TABLE `books_ex` DISABLE KEYS */;
/*!40000 ALTER TABLE `books_ex` ENABLE KEYS */;
UNLOCK TABLES;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8mb4 */ ;
/*!50003 SET character_set_results = utf8mb4 */ ;
/*!50003 SET collation_connection  = utf8mb4_0900_ai_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'STRICT_TRANS_TABLES,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
/*!50003 CREATE*/ /*!50017 DEFINER=`root`@`%`*/ /*!50003 TRIGGER `books_ex_BEFORE_INSERT` BEFORE INSERT ON `books_ex` FOR EACH ROW BEGIN
	IF (NEW.asin = '') OR (NEW.asin IS NULL) THEN
		SET NEW.asin = uuid();
	END IF;
END */;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8mb4 */ ;
/*!50003 SET character_set_results = utf8mb4 */ ;
/*!50003 SET collation_connection  = utf8mb4_0900_ai_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'STRICT_TRANS_TABLES,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
/*!50003 CREATE*/ /*!50017 DEFINER=`root`@`%`*/ /*!50003 TRIGGER `books_ex_AFTER_INSERT` AFTER INSERT ON `books_ex` FOR EACH ROW BEGIN

 DECLARE message varchar(500);
    
 -- Error handling start here
	BEGIN
		DECLARE EXIT HANDLER FOR SQLEXCEPTION
		BEGIN
			
            GET DIAGNOSTICS CONDITION 1 @ERRNO = MYSQL_ERRNO, @MESSAGE_TEXT = MESSAGE_TEXT;
			SET MESSAGE = CONCAT(@MESSAGE_TEXT," : Error code - ", @ERRNO);
			
		END;
	END;

	IF NOT (SELECT COUNT(*) FROM books b WHERE b.asin = asin) THEN
		INSERT INTO books SELECT * FROM books_ex;
    END IF;

END */;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;

--
-- Table structure for table `books_log`
--

DROP TABLE IF EXISTS `books_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `books_log` (
  `id` int NOT NULL AUTO_INCREMENT,
  `dtime` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `message` varchar(450) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=7 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `books_log`
--

LOCK TABLES `books_log` WRITE;
/*!40000 ALTER TABLE `books_log` DISABLE KEYS */;
INSERT INTO `books_log` VALUES (1,'2021-12-09 12:44:31','HELLO, THERE, BIG BOY'),(2,'2021-12-09 12:54:26','HELLO, THERE, BIG BOY'),(3,'2021-12-09 13:07:55','HELLO, THERE, BIG BOY'),(4,'2021-12-09 13:09:07','HELLO, THERE, BIG BOY'),(5,'2021-12-09 13:10:29','HELLO, THERE, BIG BOY'),(6,'2021-12-09 13:28:59','HELLO, THERE, BIG BOY');
/*!40000 ALTER TABLE `books_log` ENABLE KEYS */;
UNLOCK TABLES;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8mb4 */ ;
/*!50003 SET character_set_results = utf8mb4 */ ;
/*!50003 SET collation_connection  = utf8mb4_0900_ai_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'STRICT_TRANS_TABLES,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
/*!50003 CREATE*/ /*!50017 DEFINER=`root`@`%`*/ /*!50003 TRIGGER `books_log_BEFORE_INSERT` BEFORE INSERT ON `books_log` FOR EACH ROW BEGIN
	SET NEW.dtime = NOW();
END */;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;

--
-- Table structure for table `inventory_lab_data`
--

DROP TABLE IF EXISTS `inventory_lab_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `inventory_lab_data` (
  `msku` varchar(45) DEFAULT NULL,
  `title` varchar(500) DEFAULT NULL,
  `fnsku` varchar(45) DEFAULT NULL,
  `asin` varchar(45) DEFAULT NULL,
  `list_price` decimal(10,4) DEFAULT NULL,
  `cost_unit` decimal(10,4) DEFAULT NULL,
  `supplier` varchar(45) DEFAULT NULL,
  `_date_` date DEFAULT NULL,
  `_rank_` int DEFAULT NULL,
  `_condition_` varchar(100) DEFAULT NULL,
  `notes` varchar(750) DEFAULT NULL,
  `destination` varchar(100) DEFAULT NULL,
  `qty` decimal(10,4) DEFAULT NULL,
  `expDate` date DEFAULT NULL,
  `min_price` decimal(10,4) DEFAULT NULL,
  `max_price` decimal(10,4) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT='table is a convenience for DirBuilder.exe. Insertions are to this table, thence the before insert trigger inserts them into the books table';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `inventory_lab_data`
--

LOCK TABLES `inventory_lab_data` WRITE;
/*!40000 ALTER TABLE `inventory_lab_data` DISABLE KEYS */;
/*!40000 ALTER TABLE `inventory_lab_data` ENABLE KEYS */;
UNLOCK TABLES;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8mb4 */ ;
/*!50003 SET character_set_results = utf8mb4 */ ;
/*!50003 SET collation_connection  = utf8mb4_0900_ai_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'STRICT_TRANS_TABLES,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
/*!50003 CREATE*/ /*!50017 DEFINER=`root`@`%`*/ /*!50003 TRIGGER `inventory_lab_data_BEFORE_INSERT` BEFORE INSERT ON `inventory_lab_data` FOR EACH ROW BEGIN
	DECLARE EXIT HANDLER FOR SQLEXCEPTION
        RESIGNAL;
    DECLARE EXIT HANDLER FOR SQLWARNING
        RESIGNAL;
    DECLARE EXIT HANDLER FOR NOT FOUND
        RESIGNAL; 
    
    IF NOT (SELECT COUNT(*) FROM books WHERE asin = NEW.asin AND _date_ = NEW._date_ AND NEW.title = title) 	THEN
		INSERT INTO books (
			asin, _condition_, _date_, _rank_, 
			cost_unit, destination, expDate, fnsku,
			list_price, max_price, min_price, msku, 	
			notes, qty, supplier, title)
		VALUES  (
			NEW.asin, NEW._condition_, NEW._date_, NEW._rank_,
			NEW.cost_unit, NEW.destination, NEW.expDate, NEW.fnsku,
			NEW.list_price, NEW.min_price, NEW.max_price, NEW.msku, 
			NEW.notes, NEW.qty, NEW.supplier, NEW.title);
	END IF;
    	
END */;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;

--
-- Table structure for table `listing-entities`
--

DROP TABLE IF EXISTS `listing-entities`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `listing-entities` (
  `id` int NOT NULL AUTO_INCREMENT,
  `entity-name` varchar(45) NOT NULL,
  `url` varchar(105) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `listing-entities`
--

LOCK TABLES `listing-entities` WRITE;
/*!40000 ALTER TABLE `listing-entities` DISABLE KEYS */;
/*!40000 ALTER TABLE `listing-entities` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `mappings`
--

DROP TABLE IF EXISTS `mappings`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `mappings` (
  `id` int NOT NULL AUTO_INCREMENT,
  `mapping` varchar(100) DEFAULT NULL,
  `source_reports_id` int NOT NULL,
  PRIMARY KEY (`id`,`source_reports_id`),
  UNIQUE KEY `unique_mapping` (`source_reports_id`,`mapping`),
  KEY `fk_mappings_source_reports1_idx` (`source_reports_id`),
  CONSTRAINT `fk_mappings_source_reports1` FOREIGN KEY (`source_reports_id`) REFERENCES `source_reports` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `mappings`
--

LOCK TABLES `mappings` WRITE;
/*!40000 ALTER TABLE `mappings` DISABLE KEYS */;
/*!40000 ALTER TABLE `mappings` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `source_reports`
--

DROP TABLE IF EXISTS `source_reports`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `source_reports` (
  `id` int NOT NULL AUTO_INCREMENT,
  `source_report` varchar(100) DEFAULT NULL COMMENT 'the report name',
  PRIMARY KEY (`id`),
  UNIQUE KEY `source_report_UNIQUE` (`source_report`),
  KEY `source_report_rpts` (`source_report`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `source_reports`
--

LOCK TABLES `source_reports` WRITE;
/*!40000 ALTER TABLE `source_reports` DISABLE KEYS */;
/*!40000 ALTER TABLE `source_reports` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `storage-locations`
--

DROP TABLE IF EXISTS `storage-locations`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `storage-locations` (
  `id` int NOT NULL AUTO_INCREMENT,
  `location` varchar(100) NOT NULL,
  `location-datum` varchar(1000) DEFAULT NULL,
  `active-location` tinyint DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `storage-locations`
--

LOCK TABLES `storage-locations` WRITE;
/*!40000 ALTER TABLE `storage-locations` DISABLE KEYS */;
/*!40000 ALTER TABLE `storage-locations` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2021-12-10 18:19:41
