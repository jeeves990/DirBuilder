unit frmMap_to_xml;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, XMLConf, Forms, Controls, Graphics, Dialogs,
  Grids, Menus,
  StdCtrls, LCLIntf, LCLType,
	ActnList, ComCtrls, IniPropStorage, SQLDB, fgl,
  DirBuilder_dmod;

type
  TRecFieldData = class
    _field, _type, _null, _key, _default, _extra : String;
	end;
  TFldDataList = specialize TFPGObjectList<TRecFieldData>;

	{ TStringGrid }

  TStringGrid = class(Grids.TStringGrid)
  protected
    procedure DrawFocusRect(aCol,aRow: Integer; ARect: TRect); override;
  end;

	{ TFmMap_to_xml }

  TFmMap_to_xml = class(TForm)
				ActionGetReportType : TAction;
				ActionReadColumnFromXML : TAction;
				ActionWriteCol2XML : TAction;
				ActionAdd : TAction;
				ActionResizeGridColumns : TAction;
				ActionAddNewColumn : TAction;
		ActionAddColumnsFromBooks : TAction;
		Actions : TActionList;
		cboxReports : TComboBox;
		Grid : TStringGrid;
		IniPropStorage : TIniPropStorage;
		MainMenu : TMainMenu;
		MenuItem1 : TMenuItem;
		MenuItem2 : TMenuItem;
		MenuItem3 : TMenuItem;
		MenuItem4 : TMenuItem;
		MenuItem5 : TMenuItem;
		MenuItem6 : TMenuItem;
		MenuItem7 : TMenuItem;
		MenuItem8 : TMenuItem;
		N3 : TMenuItem;
		N2 : TMenuItem;
		N1 : TMenuItem;
		mnuAddColsFromBooks : TMenuItem;
		popup : TPopupMenu;
		StatusBar : TStatusBar;
  procedure ActionAddColumnsFromBooksExecute(Sender : TObject);
		procedure ActionAddNewColumnExecute(Sender : TObject);
		procedure ActionGetReportTypeExecute(Sender : TObject);
		procedure ActionReadColumnFromXMLExecute(Sender : TObject);
		procedure ActionResizeGridColumnsExecute(Sender : TObject);
		procedure ActionWriteCol2XMLExecute(Sender : TObject);
		procedure cboxReportsCloseUp(Sender : TObject);
    procedure FormClose(Sender : TObject; var CloseAction : TCloseAction);
    procedure FormCreate(Sender : TObject);
		procedure FormShow(Sender : TObject);
		procedure GridSelectEditor(Sender : TObject; aCol, aRow : Integer;
					var Editor : TWinControl);
    procedure GridShowHint(Sender : TObject; HintInfo : PHintInfo);
  private
	  FDmod : TDirBuilder_dataModule;
	  FFldList : TFldDataList;
  	FFormCreateFlag : Boolean;
    FReportTypes : TStringList;

	  procedure AddDBColumnNames;
		function getComboBoxWidth : Integer;
		function Get_report_type(rpt_type : String) : String;
		procedure SetReportTypes(AValue : TStringList);
  public
    //procedure DisplayHint(Sender : TObject; HintInfo : PHintInfo);
    property ReportTypes : TStringList read FReportTypes write SetReportTypes;
  end;

var
  FmMap_to_xml : TFmMap_to_xml;

implementation

uses stringGridHelper, frmSelReportType;

{$R *.lfm}

const
  ACTIVE_LISTINGS    = 'Active Listings';
  INVENTORY_RPT      =  'Inventory';
  OPEN_LISTINGS_LITE = 'Open Listings Lite';
  REPRICING_CENTRAL  = 'Repricing Central File';

{ TMyStringGrid }

procedure TStringGrid.DrawFocusRect(aCol, aRow : Integer; ARect : TRect);
var
  fldName : String;
begin
	inherited DrawFocusRect(aCol, aRow, ARect);
  if aCol = 0 then
  begin
    fldName := FmMap_to_xml.Grid.Cells[0, aRow];
    FmMap_to_xml.StatusBar.SimpleText := Format('Cell[%d, %d]', [aCol, aRow]);
	end;
end;

{ TFmMap_to_xml }

procedure TFmMap_to_xml.FormCreate(Sender : TObject);
begin
  FDmod := TDirBuilder_dataModule.Create(self);
  IniPropStorage.IniFileName := PROPSTORAGE_FILENAME;
  FFldList := TFldDataList.Create(True);
  FReportTypes := TStringList.Create;
  FFormCreateFlag := True;
  cboxReports.Items.Add(ACTIVE_LISTINGS);
  cboxReports.Items.Add(INVENTORY_RPT);
  cboxReports.Items.Add(OPEN_LISTINGS_LITE);
  cboxReports.Items.Add(REPRICING_CENTRAL);
  IniPropStorage.Restore;
end;

procedure TFmMap_to_xml.FormShow(Sender : TObject);
begin
  FFormCreateFlag := False;
end;

function TFmMap_to_xml.getComboBoxWidth : Integer;
var
  idx, sWidth : Integer;
begin
  Result := 0;
  sWidth := 0;
  for idx := 0 to cboxReports.Items.Count -1 do
  begin

	end;
end;

procedure TFmMap_to_xml.SetReportTypes(AValue : TStringList);
var
  i : Integer;
begin
	if FReportTypes.Count > 0 then Exit;
  for i := 0 to cboxReports.Items.Count -1 do
    FReportTypes.Add(cboxReports.Items[i]);
end;

procedure TFmMap_to_xml.GridSelectEditor(Sender : TObject; aCol,
			aRow : Integer; var Editor : TWinControl);
var
  cbox : TCustomComboBox;
begin
  if (aCol >= Grid.FixedCols) and (aRow = 0) then
  begin
    cboxReports.BoundsRect := Grid.CellRect(aCol,aRow);
    cboxReports.Text := Grid.Cells[Grid.Col,Grid.Row];
    Editor := cboxReports;
  end;
end;

procedure TFmMap_to_xml.GridShowHint(Sender : TObject; HintInfo : PHintInfo);
var
  pt : TPoint;
  rect : TRect;
  idx : Integer;
begin
  if Grid <> TStringGrid(Sender) then
    Exit;
  {  1. which cell are we over.  }
  pt := Mouse.CursorPos;
  pt := TStringGrid(Sender).ScreenToControl(pt);
  for idx := 1 to Grid.RowCount -1 do
  begin
	end;

  {  2. get the cells[..., ...] text  }
  {  3. use the text found to get the TRecFieldData record from FFldList  }
  {  first, which cell are we over.  }
end;

procedure TFmMap_to_xml.AddDBColumnNames;
var
  rowIdx, colIdx : Integer;
  qry : TSQLQuery;
  rec : TRecFieldData;
const
  SQL_COLUMNS = 'SHOW COLUMNS FROM books';
begin
  qry := FDmod.qryWork;
  try
    FFormCreateFlag := True;
    qry.SQL.Add(SQL_COLUMNS);
    qry.Open;
    Grid.Clear;
    Grid.ColCount := 1;
    Grid.RowCount := 1;
    while not qry.EOF do
    begin
      rec := TRecFieldData.Create;
      Grid.RowCount := Grid.RowCount +1;
      Grid.Cells[0, Grid.RowCount -1] := qry.FieldByName('field').AsString;

      rec._field := qry.FieldByName('field').AsString;
      rec._default := qry.FieldByName('default').AsString;
      rec._extra := qry.FieldByName('extra').AsString;
      rec._key := qry.FieldByName('key').AsString;
      rec._null := qry.FieldByName('null').AsString;
      rec._type := qry.FieldByName('type').AsString;

      FFldList.Add(rec);
      qry.Next;
		end;
	finally
    qry.Close;
    FFormCreateFlag := False;
  end;
end;

procedure {TFmMap_to_xml.}DisplayHint(Sender : TObject; HintInfo : PHintInfo);
var
  pt : TPoint;
begin
  {  1. which cell are we over.  }
  pt := Mouse.CursorPos;
  //ScreenToControl(pt);

  {  2. get the cells[..., ...] text  }
  {  3. use the text found to get the TRecFieldData record from FFldList  }
  {  first, which cell are we over.  }
end;

procedure TFmMap_to_xml.FormClose(Sender : TObject; var CloseAction : TCloseAction);
begin
  FDmod.Free;
  FFldList.Free;
  if Assigned(FReportTypes) then
    FReportTypes.Free;
  IniPropStorage.Save;
end;

procedure TFmMap_to_xml.ActionAddColumnsFromBooksExecute(Sender : TObject);
begin
  AddDBColumnNames;
end;

procedure TFmMap_to_xml.ActionAddNewColumnExecute(Sender : TObject);
var
  rowIdx, colIdx : Integer;
begin
  //if Grid.ColCount = 1 then
  //  Exit;
  colIdx := Grid.ColCount - 1;

  rowIdx := Grid.FixedRows;
  while rowIdx < Grid.RowCount do
  begin
    if Grid.Cells[colIdx, rowIdx] > EmptyStr then
      Break;
    Inc(rowIdx);
	end;
  {  if all cells in the last column are empty, then exit  }
  if rowIdx = Grid.RowCount then
    Exit;
  {  otherwise, add a column  }
  Grid.ColCount := Grid.ColCount +1;
  Grid.ColWidths[Grid.ColCount -1] := 150;
end;

procedure TFmMap_to_xml.ActionGetReportTypeExecute(Sender : TObject);
begin
  Get_report_type('another report');
end;

function TFmMap_to_xml.Get_report_type(rpt_type : String) : String;
var
  dlg : TfmSelectReportType;
  sLst : TStringList;
begin
  dlg := TfmSelectReportType.Create(self);
  try
    sLst := dlg.ReportList;
    dlg.ShowModal;
    if dlg.ModalResult > 0 then
    begin
      //Result := sLst.;
		end;
	finally
    dlg.Free;
	end;

end;

procedure TFmMap_to_xml.ActionReadColumnFromXMLExecute(Sender : TObject);
begin
//
end;

procedure TFmMap_to_xml.ActionResizeGridColumnsExecute(Sender : TObject);
var
  grHelper : TGridHelper;
  S : String;
begin
  if not Grid.Visible then
    Exit  ;
  Grid.Update;
	grHelper := TGridHelper.Create;
  try
    grHelper.maxSize := 250;
    grHelper.SGrid := Grid;
    grHelper.SetStringGridColumnWidths;
  finally
    grHelper.Free;
  end;
end;

procedure TFmMap_to_xml.ActionWriteCol2XMLExecute(Sender : TObject);
const
  fmt_str = '%s=%s';
var
  aCol, rowDx : Integer;
  aStr : String;
begin
  if Grid.Col = 0 then Exit;
  aCol := Grid.Col;
  for rowDx := 1 to Grid.RowCount -1 do
  begin
    aStr := Format(fmt_str, [Grid.Cells[0, rowDx], Grid.Cells[aCol, rowDx]]);
    //XMLConfig.;
	end;
end;


procedure TFmMap_to_xml.cboxReportsCloseUp(Sender : TObject);
var
  cbox : TComboBox;
begin
  Grid.Cells[Grid.Col, Grid.Row];
  cbox := TComboBox(Sender);
  try
    Grid.Cells[Grid.Col, Grid.Row] := cbox.SelText;
    Grid.Row := 1;
    Grid.SetFocus;
	finally
    ActionResizeGridColumns.Execute;
    cbox.Width := Grid.ColWidths[Grid.Col];
	end;

end;

end.




