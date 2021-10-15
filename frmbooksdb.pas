unit frmBooksDb;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, DB, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Grids, DBGrids, ActnList, Menus, IniPropStorage, ComCtrls, PairSplitter,
  StdCtrls, mysql57conn, DirBuilder_dmod;

type

  { TfmBooksDb }

  TfmBooksDb = class(TForm)
    ActionGetBooks: TAction;
    ActionGetBooks1: TAction;
    ActionList: TActionList;
    booksPopup: TPopupMenu;
    fmBooksDbProps: TIniPropStorage;
    lblBooksStoredAt: TLabel;
    lblStorageLocations: TLabel;
    lblBooksListedOn: TLabel;
    lblEntities: TLabel;
    MenuItem1: TMenuItem;
    pnlStorageLocations: TPanel;
    pnlEntities: TPanel;
    pgctrlBooksDb: TPageControl;
    pnlBooks: TPanel;
    pnlBooksListedOn: TPanel;
    pnlBooksStoredAt: TPanel;
    pnlName: TPanel;
    qryBooks: TSQLQuery;
    sgridBooks: TStringGrid;
    sgridBooksListedOn: TStringGrid;
    sgridBooksStoredAt: TStringGrid;
    sgridStorageLocations: TStringGrid;
    sgridEntities: TStringGrid;
    Splitter1: TSplitter;
    spltrEntities: TSplitter;
    spltrMain: TSplitter;
    tabshEntity: TTabSheet;
    tabshLocal: TTabSheet;
    procedure ActionGetBooksExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    dmod : TDirBuilder_dataModule;
    dbConn : TMySQL57Connection;
    dbtx : TSQLTransaction;
  public

  end;

var
  fmBooksDb: TfmBooksDb;

implementation

{$R *.lfm}

{ TfmBooksDb }

procedure TfmBooksDb.ActionGetBooksExecute(Sender: TObject);
var
  i, idx : Integer;
  qry : TSQLQuery;
  ara : array of String;

  procedure addColumnTitles;
  begin
    i := 0;
    while i < qry.FieldDefs.Count do
    begin
      ara[i] := qry.FieldDefs[i].DisplayName;
      Inc(i);
    end;
  end;

  procedure buildRow;
  begin
    i := 0;
    while i < qry.FieldCount do
    begin
      ara[i] := qry.Fields[i].Value;
      Inc(i);
    end;
  end;

begin
  sgridBooks.Clear;
  sgridBooks.FixedRows := 1;
  qry := TSQLQuery.Create(self);
  try
    qry.DataBase := dbConn;
    qry.Transaction := dbtx;
    qry.SQL.Add('Select * from books');
    qry.Open;
    if qry.RecordCount = 0 then
       Exit;
    qry.First;
    SetLength(ara, qry.Fields.Count);
    addColumnTitles;

    if qry.RecordCount = 0 then
       Exit;

    while not qry.EOF do
    begin
      buildRow;
      idx := qry.RecNo;
      sgridBooks.InsertRowWithValues(idx, ara);
      qry.Next;
    end;
  finally
    qry.Free;
  end;
end;

procedure TfmBooksDb.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    fmBooksDbProps.Save;
    CloseAction := caFree;
end;

procedure TfmBooksDb.FormCreate(Sender: TObject);
begin
  dmod := DirBuilder_dataModule;
  dbConn := dmod.BooksDbConn;
  dbtx := dmod.BooksDbTx;
  fmBooksDbProps.Restore;
end;

end.








