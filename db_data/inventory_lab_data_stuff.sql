SELECT count(*) FROM books_db.books;
SELECT * FROM books_db.books;

SET SQL_SAFE_UPDATES = 0;
delete FROM books_db.books;

SELECT * FROM books_db.inventory_lab_data;

SET SQL_SAFE_UPDATES = 0;
delete FROM books_db.inventory_lab_data;