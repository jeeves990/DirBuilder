unit frmNewBooksDB;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
	ActnList, Menus, frame4Table, DirBuilder_dmod, SQLDB, grids, StdCtrls,
	XMLPropStorage, frmAddEdit,
  Mouse;


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
		booksFrame : TframeTable;
    ImageList1: TImageList;
    pgCtrl: TPageControl;
    booksDbPopupMnu: TPopupMenu;
    spltrMain: TSplitter;
		tbsStorage : TTabSheet;
		tbsListing : TTabSheet;
    ToolButton3: TToolButton;
		MainPropStorage : TXMLPropStorage;
    procedure ActionAddBookExecute(Sender: TObject);
    procedure ActionAddRowExecute(Sender: TObject);
    procedure ActionEditRowExecute(Sender: TObject);
    procedure booksDbPopupMnuPopup(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FaddEdDlg: TfmAddEdit;
    FCurrentComponent : TComponent;
    FCurrentTag : Integer;
    FQry : TSQLQuery;
    FDmod : TDirBuilder_dataModule;
    F_sql: String;
    mnuItmAra : array of TMenuItem;
    fmAddEdit : TfmAddEdit;
    function ComponentUnderMouse(_parent: TWinControl=nil): TWinControl;
		procedure DockMasterCreateControl(Sender : TObject; aName : string;
					var AControl : TControl; DoDisableAutoSizing : boolean);
  public
    constructor Create(aOwner : TComponent); override;
    property Dmod : TDirBuilder_dataModule read FDmod write FDmod;
    property _sql : String read F_sql write F_sql;
    property addEdDlg : TfmAddEdit read FaddEdDlg write FaddEdDlg;
  end;

var
  fmNewBooksDb: TfmNewBooksDb;

implementation

{$R *.lfm}

uses frmDirFromCSV;

const
  BOOKS_TAG                    = 100000;
  BOOKS_STORAGE_MAP_TAG        = 100001;
  STORAGE_LOCATION_TAG         = 100002;
  BOOKS_TO_ENTITIES_TAG        = 100003;
  ENTITIES_TAG                 = 100004;
  SQL4BOOKS                    = 'SELECT * FROM BOOKS';

{ TfmNewBooksDb }

function TfmNewBooksDb.ComponentUnderMouse(_parent: TWinControl): TWinControl;
var
  ctrl: TWinControl;
  pt: TPoint;
begin
  if not Assigned(_parent) then
    _parent := self;
  try
    pt.X := Mouse.GetMouseX;
    pt.Y := Mouse.GetMouseY;
    pt := _parent.ScreenToClient(pt);
    ctrl := TWinControl(_parent.ControlAtPos(pt, [capfRecursive, capfOnlyWinControls])); //, capfAllowWinControls]);
    if Assigned(ctrl) then
      Result := ctrl;
  except on e: Exception do
    ShowMessage(e.Message);
  end;
end;

procedure TfmNewBooksDb.DockMasterCreateControl(Sender : TObject;
			aName : string; var AControl : TControl; DoDisableAutoSizing : boolean);
begin

end;

constructor TfmNewBooksDb.Create(aOwner: TComponent);
begin
  try
    inherited Create(aOwner);
  except on e : Exception do
    ShowMessage(e.Message);
  end;
  fmAddEdit := TfmAddEdit.Create(self);
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
  pt : TPoint;
begin
  FQry.Free;
  if Assigned(fmAddEdit) then
     fmAddEdit.Free;
  i := 0;
  while i < Length(mnuItmAra) do
  begin
    if mnuItmAra[i] <> nil then
       mnuItmAra[i].Free;
    Inc(i);
  end;
  SetLength(mnuItmAra, 0);
  MainPropStorage.Save;
end;

{$DEFINE DEBUG}
procedure TfmNewBooksDb.FormCreate(Sender: TObject);
var
  i : Integer;
  pt : TPoint;
  ctrl : TWinControl;
begin
  ctrl := Parent;
  FDmod := frmFayesDirBuilder.dmod;
  FQry := TSQLQuery.Create(self);
  FQry.DataBase := FDmod.BooksDbConn;
  FQry.Transaction := FDmod.BooksDbConn.Transaction;
  MainPropStorage.Restore;

  {$IFDEF DEBUG}
  FQry.SQL.Add(SQL4BOOKS);
  {$ELSE}
  FQry.SQL.Add(F_sql);
  {$ENDIF}


  //DockMaster.MakeDockSite(Self,[akBottom],admrpChild);
  //DockMaster.OnCreateControl := @DockMasterCreateControl;
  //DockMaster.OnShowOptions:=@ShowAnchorDockOptions;

  SetBounds(100,50,600,80);
  //ViewSrcEditor1ToolButtonClick(Self);
  //ViewMessagesToolButtonClick(Self);
  //ViewOIToolButtonClick(Self);
  //ViewFPDocEditorToolButtonClick(Self);
  //fmAddEdit.Show;
  //exit;
  //SetLength(mnuItmAra, 3);
  //i := 0;
  //while i < Length(mnuItmAra) do
  //begin
  //  mnuItmAra[i] := TMenuItem.Create(self);
  //  Inc(i);
  //end;
  //booksDbPopupMnu.Items.Add(mnuItmAra);

end;

procedure TfmNewBooksDb.ActionAddRowExecute(Sender: TObject);
var
  ctrl : TComponent;
begin
  inherited;
  exit;
  // display a dialog to add the row
  ActionAddBook.Execute;
end;

procedure TfmNewBooksDb.ActionEditRowExecute(Sender: TObject);
begin
  ActionEditBook.Execute;
end;

procedure TfmNewBooksDb.ActionAddBookExecute(Sender: TObject);
var
  col, row : Integer;
  grid : TStringGrid;
  ctrl : TWinControl;
begin
  ctrl := ComponentUnderMouse;
  Exit;
  FQry.SQL.Clear;
  {$IFDEF DEBUG}
  FQry.SQL.Add(SQL4BOOKS);
  {$ELSE}
  FQry.SQL.Add(Fsql);

  {$ENDIF}
  //FQry.Open;
  //grid := self.frameBooksTable.sGrid;
  //grid.ColCount := FQry.FieldDefs.Count +grid.FixedCols;
  //for col := 0 to FQry.FieldDefs.Count -1 do
  //  grid.Columns[col +grid.FixedCols].Title.Caption := FQry.FieldDefs[col].DisplayName;
  //
  //row := 0;
  //FQry.First;
  //while not FQry.EOF do
  //begin
  //  grid.RowCount := grid.RowCount +1;
  //  row := grid.RowCount -1;
  //  for col := 0 to FQry.Fields.Count -1 do
  //    grid.Cells[col, row] := FQry.Fields.Fields[col].AsString;
  //end;
end;


end.



















