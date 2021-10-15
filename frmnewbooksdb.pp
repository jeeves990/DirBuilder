unit frmNewBooksDB;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  ActnList, Menus, frame4Table, ECGrid, DirBuilder_dmod, SQLDB
  ;

type

  { TfmNewBooksDb }

  TfmNewBooksDb = class(TForm)
    ActionAddBook: TAction;
    ActionAddMapBook2StorageLocation: TAction;
    ActionDeleteMapBook2StorageLocation: TAction;
    ActionEditMapBook2StorageLocation: TAction;
    ActionAddBook2EntityMapping: TAction;
    ActionEditBook2EntityMapping: TAction;
    ActionDeleteBook2EntityMapping: TAction;
    ActionEditBook: TAction;
    ActionDeleteBook: TAction;
    ActionAddEntity: TAction;
    ActionEditEntity: TAction;
    ActionDeleteEntity: TAction;
    ActionAddStorageLocation: TAction;
    ActionEditStorageLocation: TAction;
    ActionDeleteStorageLocation: TAction;
    Actions: TActionList;
    frameBooksTable: TframeTable;
    frameStoredAt: TframeTable;
    frameStorageLocations: TframeTable;
    frameBooksListedOn: TframeTable;
    frameInternetEntities: TframeTable;
    ImageList1: TImageList;
    pgCtrl: TPageControl;
    booksDbPopupMnu: TPopupMenu;
    spltrMain: TSplitter;
    spltrBookStorage: TSplitter;
    SQLQuery1: TSQLQuery;
    tabshBookStorage: TTabSheet;
    tabshInternetEntities: TTabSheet;
    ToolButton3: TToolButton;
    procedure ActionAddBookExecute(Sender: TObject);
    procedure ActionAddMapBook2StorageLocationExecute(Sender: TObject);
    procedure booksDbPopupMnuPopup(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure frameBooksListedOnMouseEnter(Sender: TObject);
    procedure frameBooksTableMouseEnter(Sender: TObject);
    procedure frameInternetEntitiesMouseEnter(Sender: TObject);
    procedure frameStorageLocationsMouseEnter(Sender: TObject);
    procedure frameStoredAtMouseEnter(Sender: TObject);
  private
    FCurrentComponent : TComponent;
    FCurrentTag : Integer;
    FQry : TSQLQuery;
    dmod : TDirBuilder_dataModule;
    mnuItmAra : array of TMenuItem;
  public

  end;

var
  fmNewBooksDb: TfmNewBooksDb;

implementation

const
  BOOKS_TAG                    = 100000;
  BOOKS_STORAGE_MAP_TAG        = 100001;
  STORAGE_LOCATION_TAG         = 100002;
  BOOKS_TO_ENTITIES_TAG        = 100003;
  ENTITIES_TAG                 = 100004;

{$R *.lfm}

{ TfmNewBooksDb }

procedure TfmNewBooksDb.frameStoredAtMouseEnter(Sender: TObject);
begin
  FCurrentTag := BOOKS_STORAGE_MAP_TAG;
end;

procedure TfmNewBooksDb.frameBooksTableMouseEnter(Sender: TObject);
begin
  FCurrentTag := BOOKS_TAG;
end;

procedure TfmNewBooksDb.frameInternetEntitiesMouseEnter(Sender: TObject);
begin
  FCurrentTag := ENTITIES_TAG;
end;

procedure TfmNewBooksDb.frameStorageLocationsMouseEnter(Sender: TObject);
begin
  FCurrentTag := STORAGE_LOCATION_TAG;
end;

procedure TfmNewBooksDb.frameBooksListedOnMouseEnter(Sender: TObject);
begin
  FCurrentTag := BOOKS_TO_ENTITIES_TAG;
end;

procedure TfmNewBooksDb.booksDbPopupMnuPopup(Sender: TObject);
var
  mnu : TPopupMenu;
  i : Integer;
begin
  mnu := booksDbPopupMnu;
  if mnu.Items.Count <> 3 then
     raise Exception.Create('mnuitem array is not initialized');
  try
    case FCurrentTag of
      BOOKS_TAG             :
      begin
        mnuItmAra[0].Action := ActionAddBook;
        mnuItmAra[1].Action := ActionEditBook;
        mnuItmAra[2].Action := ActionDeleteBook;
      end;
      BOOKS_STORAGE_MAP_TAG :
      begin
        mnuItmAra[0].Action := ActionAddMapBook2StorageLocation;
        mnuItmAra[1].Action := ActionEditMapBook2StorageLocation;
        mnuItmAra[2].Action := ActionDeleteMapBook2StorageLocation;
      end;
      STORAGE_LOCATION_TAG  :
      begin
        mnuItmAra[0].Action := ActionAddStorageLocation;
        mnuItmAra[1].Action := ActionEditStorageLocation;
        mnuItmAra[2].Action := ActionDeleteStorageLocation;
      end;
      BOOKS_TO_ENTITIES_TAG :
      begin
        mnuItmAra[0].Action := ActionAddBook2EntityMapping;
        mnuItmAra[1].Action := ActionEditBook2EntityMapping;
        mnuItmAra[2].Action := ActionDeleteBook2EntityMapping;
      end;
      ENTITIES_TAG          :
      begin
        mnuItmAra[0].Action := ActionAddEntity;
        mnuItmAra[1].Action := ActionEditEntity;
        mnuItmAra[2].Action := ActionDeleteEntity;
      end;

    end;

  finally

  end;
end;

procedure TfmNewBooksDb.FormClose(Sender: TObject;
                                 var CloseAction: TCloseAction);
var
  i : Integer;
begin
  FQry.Free;
  i := 0;
  while i < Length(mnuItmAra) do
  begin
    if mnuItmAra[i] <> nil then
       mnuItmAra[i].Free;
    Inc(i);
  end;
  SetLength(mnuItmAra, 0);
end;

procedure TfmNewBooksDb.FormCreate(Sender: TObject);
var
  i : Integer;
begin
  dmod := DirBuilder_dataModule;
  FQry := TSQLQuery.Create(self);
  FQry.DataBase := dmod.BooksDbConn;
  FQry.Transaction := dmod.BooksDbConn.Transaction;
  SetLength(mnuItmAra, 3);
  i := 0;
  while i < Length(mnuItmAra) do
  begin
    mnuItmAra[i] := TMenuItem.Create(self);
    Inc(i);
  end;
  booksDbPopupMnu.Items.Add(mnuItmAra);
end;

procedure TfmNewBooksDb.ActionAddMapBook2StorageLocationExecute(Sender: TObject);
begin
  FQry.SQL.Clear;
end;

procedure TfmNewBooksDb.ActionAddBookExecute(Sender: TObject);
var
  col, row : Integer;
  grid : TECGrid;
const
  _sql = 'SELECT * FROM BOOKS';
begin
  FQry.SQL.Clear;
  FQry.SQL.Add(_sql);
  FQry.Open;
  grid := frameBooksTable.sGrid;
  //grid.Color:=clBlue;
  grid.ColCount := FQry.FieldDefs.Count +grid.FixedCols;
  for col := 0 to FQry.FieldDefs.Count -1 do
    grid.Columns[col +grid.FixedCols].Title := FQry.FieldDefs[col].DisplayName;

  row := 0;
  FQry.First;
  while not FQry.EOF do
  begin
    grid.RowCount := gridRowCount +1;
    row := gridRowCount -1;
    for col := 0 to FQry.Fields.Count -1 do
      grid.Cells[col, row] := FQry.Fields.Fields[col].AsString;
  end;
end;


end.



















