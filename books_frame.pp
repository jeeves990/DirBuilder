unit books_frame;

{$mode ObjFPC}{$H+}

interface

uses
      Classes, SysUtils, DB, csvdataset, memds, Forms, Controls, DBCtrls,
			DBGrids, SQLDB, ExtCtrls,
      DirBuilder_dmod;

type

	{ TBooksFrame }

  TBooksFrame = class(TFrame)
		dstCSV : TCSVDataset;
		dtaSrc : TDataSource;
		grid : TDBGrid;
		dbNav : TDBNavigator;
		pnlNav : TPanel;
  private
    procedure write2CSV;
  public

  end;

implementation

{$R *.lfm}

{ TBooksFrame }

const
  READ_BOOKS = 'SELECT * FROM booksdb.books LIMIT 5';
procedure TBooksFrame.write2CSV;
var
  qry : TSQLQuery;
begin
  qry := TSQLQuery.Create(self);
  try
    qry.DataBase := DirBuilder_dataModule.BooksDbConn;
    qry.SQL.Add(READ_BOOKS);
	finally
    qry.Free;
	end;
end;

end.













