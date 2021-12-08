call books_db.Insert_into_books(',', 'asin', '(msku, title, asin, list_price)', '("hello msku", "this is my book", "1212331", 12.056)');
SELECT @key_word;

call books_db.Get_book_count(@bookcount);
select @bookcount;

/*
use books_db;
select * from books;

SET SQL_SAFE_UPDATES = 0; 
delete from books;
commit;

*/