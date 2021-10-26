unit frmMap_to_XML;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, XMLConf, Forms, Controls, Graphics, Dialogs,
  Grids, Menus,
  StdCtrls, LCLIntf, LCLType,
  ActnList, ComCtrls, IniPropStorage, SQLDB, fgl,
  DirBuilder_dmod, IniFileHandler;

type
  TRecFieldData = class
    _field, _type, _null, _key, _default, _extra: string;
  end;

  TFldDataList = specialize TFPGObjectList<TRecFieldData>;

  { TStringGrid }

  TStringGrid = class(Grids.TStringGrid)
  protected
    procedure DrawFocusRect(aCol, aRow: integer; ARect: TRect); override;
  end;

  { TFmMap_to_xml }

  TFmMap_to_xml = class(TForm)
    ActionRefreshIniHandler: TAction;
    ActionGetReportType: TAction;
    ActionReadColumnFromINI: TAction;
    ActionWriteCol2INI: TAction;
    ActionAdd: TAction;
    ActionResizeGridColumns: TAction;
    ActionAddNewColumn: TAction;
    ActionAddColumnsFromBooks: TAction;
    Actions: TActionList;
    cboxReports: TComboBox;
    Grid: TStringGrid;
    IniPropStorage: TIniPropStorage;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    mnuOther: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem9: TMenuItem;
    N4: TMenuItem;
    N2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    N3: TMenuItem;
    N1: TMenuItem;
    pgCtrl: TPageControl;
    popup: TPopupMenu;
    StatusBar: TStatusBar;
    tsMappingData: TTabSheet;
    procedure ActionAddColumnsFromBooksExecute(Sender: TObject);
    procedure ActionAddNewColumnExecute(Sender: TObject);
    procedure ActionGetReportTypeExecute(Sender: TObject);
    procedure ActionReadColumnFromINIExecute(Sender: TObject);
    procedure ActionRefreshIniHandlerExecute(Sender: TObject);
    procedure ActionResizeGridColumnsExecute(Sender: TObject);
    procedure ActionWriteCol2INIExecute(Sender: TObject);
    procedure cboxReportsCloseUp(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridSelectEditor(Sender: TObject; aCol, aRow: integer;
      var Editor: TWinControl);
    procedure GridShowHint(Sender: TObject; HintInfo: PHintInfo);
  private
    FDmod: TDirBuilder_dataModule;
    FFldList: TFldDataList;
    FFormCreateFlag: boolean;
    FReportTypes: TStringList;
    FSectionsList: TStringList;
    FIinifile_handler: TIniFileHandler;
    procedure AddDBColumnNames;
    procedure clr_column_values(const pCol: integer);
    function fillColumnFromINI(rpt_hdr: string; const column: integer): boolean;
    function getComboBoxWidth: integer;
    function getDataFromINI_4_report_type(const rprt_type: string): TStrings;
    function Get_report_type(rpt_type: string): string;
    function grid_hasEmptyColumn: integer;
    function isEmptyColumn(const colNum: integer): boolean;
    procedure Refresh_inifile_handler;
    procedure SetReportTypes(AValue: TStringList);
    function rptType_has_column(rpt_type: string): integer;
  public
    //procedure DisplayHint(Sender : TObject; HintInfo : PHintInfo);
    property ReportTypes: TStringList read FReportTypes write SetReportTypes;
  end;

var
  FmMap_to_xml: TFmMap_to_xml;
  Prop_storage_ini: string;

implementation

uses stringGridHelper, IniFiles, frm_select_rprt_type;

{$R *.lfm}

const
  ACTIVE_LISTINGS = 'Active Listings';
  INVENTORY_RPT = 'Inventory';
  OPEN_LISTINGS_LITE = 'Open Listings Lite';
  REPRICING_CENTRAL = 'Repricing Central File';
  REPORT_TYPE_FMT = 'REPORT_TYPE_%s';
  COL_ORIG_WIDTH = 150;

{ TMyStringGrid }

procedure TStringGrid.DrawFocusRect(aCol, aRow: integer; ARect: TRect);
var
  fldName: string;
begin
  inherited DrawFocusRect(aCol, aRow, ARect);
  if aCol = 0 then
  begin
    fldName := FmMap_to_xml.Grid.Cells[0, aRow];
    FmMap_to_xml.StatusBar.SimpleText := Format('Cell[%d, %d]', [aCol, aRow]);
  end;
end;

{ TFmMap_to_xml }

procedure TFmMap_to_xml.FormCreate(Sender: TObject);
begin
  FDmod := DirBuilder_dataModule;
  Refresh_inifile_handler;
  IniPropStorage.IniFileName := Prop_storage_ini;
  FFldList := TFldDataList.Create(True);
  FReportTypes := TStringList.Create;
  FFormCreateFlag := True;
  cboxReports.Items.Add(ACTIVE_LISTINGS);
  cboxReports.Items.Add(INVENTORY_RPT);
  cboxReports.Items.Add(OPEN_LISTINGS_LITE);
  cboxReports.Items.Add(REPRICING_CENTRAL);

  if not Assigned(Report_type_list) then
    Report_type_list := TStringList.Create;
  Report_type_list.AddStrings(cboxReports.Items);
  ActionAddColumnsFromBooks.Execute;
  IniPropStorage.Restore;
  FSectionsList := TStringList.Create;
end;

procedure TFmMap_to_xml.FormShow(Sender: TObject);
begin
  FFormCreateFlag := False;
end;

function TFmMap_to_xml.getComboBoxWidth: integer;
var
  idx, sWidth: integer;
begin
  Result := 0;
  sWidth := 0;
  for idx := 0 to cboxReports.Items.Count - 1 do
  begin

  end;
end;

procedure TFmMap_to_xml.SetReportTypes(AValue: TStringList);
var
  i: integer;
begin
  if FReportTypes.Count > 0 then Exit;
  for i := 0 to cboxReports.Items.Count - 1 do
    FReportTypes.Add(cboxReports.Items[i]);
end;

function TFmMap_to_xml.rptType_has_column(rpt_type: string): integer;
var
  iCol: integer;
  sColVal: string;
begin
  Result := -1;
  iCol := 1;
  rpt_type := LowerCase(rpt_type);
  while iCol < Grid.ColCount do
  begin
    sColVal := Grid.Cells[iCol, 0];
    sColVal := LowerCase(sColVal);
    if sColVal = rpt_type then
    begin
      Result := iCol;
      Exit;
    end;
    Inc(iCol);
  end;
end;

procedure TFmMap_to_xml.GridSelectEditor(Sender: TObject; aCol, aRow: integer;
  var Editor: TWinControl);
var
  cbox: TCustomComboBox;
begin
  if (aCol >= Grid.FixedCols) and (aRow = 0) then
  begin
    cboxReports.BoundsRect := Grid.CellRect(aCol, aRow);
    cboxReports.Text := Grid.Cells[Grid.Col, Grid.Row];
    Editor := cboxReports;
  end;
end;

procedure TFmMap_to_xml.GridShowHint(Sender: TObject; HintInfo: PHintInfo);
var
  pt: TPoint;
  rect: TRect;
  idx: integer;
begin
  if Grid <> TStringGrid(Sender) then
    Exit;
  {  1. which cell are we over.  }
  pt := Mouse.CursorPos;
  pt := TStringGrid(Sender).ScreenToControl(pt);
  for idx := 1 to Grid.RowCount - 1 do
  begin
  end;

  {  2. get the cells[..., ...] text  }
  {  3. use the text found to get the TRecFieldData record from FFldList  }
  {  first, which cell are we over.  }
end;

procedure TFmMap_to_xml.AddDBColumnNames;
var
  rowIdx, colIdx: integer;
  qry: TSQLQuery;
  rec: TRecFieldData;
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
      Grid.RowCount := Grid.RowCount + 1;
      Grid.Cells[0, Grid.RowCount - 1] := qry.FieldByName('field').AsString;

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
    qry.SQL.Clear;
    qry.Close;
    FFormCreateFlag := False;
  end;
end;

procedure {TFmMap_to_xml.}DisplayHint(Sender: TObject; HintInfo: PHintInfo);
var
  pt: TPoint;
begin
  {  1. which cell are we over.  }
  pt := Mouse.CursorPos;
  //ScreenToControl(pt);

  {  2. get the cells[..., ...] text  }
  {  3. use the text found to get the TRecFieldData record from FFldList  }
  {  first, which cell are we over.  }
end;

procedure TFmMap_to_xml.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(FDmod) then
    FDmod.Free;
  if Assigned(FFldList) then
    FFldList.Free;
  if Assigned(FReportTypes) then
    FReportTypes.Free;
  if Assigned(Report_type_list) then
    Report_type_list.Free;
  if Assigned(FSectionsList) then
    FSectionsList.Free;


  FIinifile_handler.RefreshIniFile;
  if Assigned(FIinifile_handler) then
    FIinifile_handler.Free;
  IniPropStorage.Save;
  IniPropStorage.Free;

end;

procedure TFmMap_to_xml.Refresh_inifile_handler;
begin
  if Assigned(FIinifile_handler) then
    FIinifile_handler.Free;
  FIinifile_handler := TIniFileHandler.Create(Prop_storage_ini);
end;

procedure TFmMap_to_xml.ActionAddColumnsFromBooksExecute(Sender: TObject);
begin
  AddDBColumnNames;
end;

procedure TFmMap_to_xml.ActionAddNewColumnExecute(Sender: TObject);
var
  colIdx: integer;
begin
  colIdx := grid_hasEmptyColumn;
  if colIdx < 0 then
  begin
    colIdx := Grid.ColCount;
    Grid.ColCount := Grid.ColCount + 1;
  end;
  Grid.Col := colIdx;      // a return of sorts
  self.clr_column_values(colIdx);
  Grid.ColWidths[colIdx] := COL_ORIG_WIDTH;
end;

procedure TFmMap_to_xml.ActionGetReportTypeExecute(Sender: TObject);
var
  report_type: string;
  iCol: integer;
begin
  report_type := Get_report_type('another report');
  if report_type > EmptyStr then
  begin
    iCol := 1;
    {  does this report have a column for its data?  }
    while iCol < Grid.ColCount do
    begin
      if Grid.Cells[iCol, 0] = report_type then
        Break;    // iCol is the column with data for report_type
      Inc(iCol);
    end;        // if iCol = Grid.ColCount, no Grid column for the data.

    {  2 conditions: there is a column for the data
          or there is no column for the data        }
    if icol < Grid.ColCount then   // a column with data for the report_type
    begin
      {  then, refresh the data from the ini file  }
    end;
    // if so, then exit;
    if iCol = Grid.ColCount then  // ini has data for report_type
      Exit;

    // get a column for the data
    icol := grid_hasEmptyColumn;
    if icol < 0 then;     // this is an NON-empty column


    // otherwise, is there data in the ini file for the report type.
    // if so,
    //    add a column for it
    //    and ActionReadColumnFromINI.Execute

    // if not, add a column for it
  end;
end;

function TFmMap_to_xml.getDataFromINI_4_report_type(const rprt_type: string): TStrings;
begin

end;

function TFmMap_to_xml.grid_hasEmptyColumn: integer;
var
  iRow, iCol: integer;
begin
  Result := -1;
  iCol := 1;
  while iCol < Grid.ColCount do
  begin
    if isEmptyColumn(iCol) then
      Break;
    Inc(iCol);
  end;
  if iCol < grid.ColCount then
    Result := iCol;
end;

function TFmMap_to_xml.isEmptyColumn(const colNum: integer): boolean;
var
  i: integer;
begin
  Result := True;
  i := 0;
  while i < Grid.RowCount do
  begin
    if Grid.Cells[colNum, i] > EmptyStr then
      Break;
    Inc(i);
  end;
  Result := i = Grid.RowCount;   // if True, a non-empty column
end;

procedure TFmMap_to_xml.ActionReadColumnFromINIExecute(Sender: TObject);
begin
  //fillColumnFromINI;
  if True then;
end;

procedure TFmMap_to_xml.ActionRefreshIniHandlerExecute(Sender: TObject);
begin
  Refresh_inifile_handler;
end;

function TFmMap_to_xml.fillColumnFromINI(rpt_hdr: string;
                                    const column: integer): boolean;
var
  aCol, rowDx, idx, cnt, grid_row_dx: integer;
  value_str, aColName: string;
  rpt_type, block_hdr: string;
  ini: TIniFile;
  col_list, ini_col_list: TStringList;
const
  ErrMsg = 'TFmMap_to_xml.fillColumnFromINI: %s';
begin
  Result := False;
  if (Grid.Col = 0) or (Grid.Cells[Grid.Col, 0] = EmptyStr) then Exit;

  col_list := TStringList(Grid.Cols[0]);

  {  these block_hdr names are massaged before writing  }
  block_hdr := Format(REPORT_TYPE_FMT, [rpt_hdr]);

  StatusBar.SimpleText := Format('Reading data from INI file for %s', [rpt_hdr]);
  StatusBar.Update;

  aCol := Grid.Col;
  ini_col_list := TStringList.Create;
  ini := TIniFile.Create(Prop_storage_ini);
  try
    {  get the column names from the ini file  }
    ini.ReadSection(block_hdr, ini_col_list);
    cnt := ini_col_list.Count;

    {  iterate over those column names  }
    idx := 0;
    while idx < ini_col_list.Count do
    begin
      {  find the row in the grid where that column name resides.  }
      grid_row_dx := Grid.Cols[0].IndexOf(ini_col_list[idx]);
      if grid_row_dx < 0 then
        raise Exception.Create(Format(ErrMsg,
          ['col_name from ini not found in list']));
      {  get the value associated with the column name from the ini file  }
      value_str := ini.ReadString(block_hdr, ini_col_list[idx], 'not found');
      if value_str = 'not found' then
        raise Exception.Create(Format(ErrMsg, ['value not found in ini file']));

      {  place value_str in Grid at the appropriate place.  }
      Grid.Cells[column, grid_row_dx] := value_str;
      Inc(idx);
    end;
  finally
    StatusBar.SimpleText := EmptyStr;
    FreeAndNil(ini);
    ini_col_list.Free;
    ActionResizeGridColumns.Execute;
  end;
end;

function TFmMap_to_xml.Get_report_type(rpt_type: string): string;
var
  dlg: Tfm_select_rprt_type;
  str: string;
  iCol: integer;
begin
  Result := EmptyStr;
  dlg := Tfm_select_rprt_type.Create(self);
  try
    dlg.ReportTypeList := Report_type_list;
    dlg.ShowModal;
    if dlg.ModalResult > 0 then
    begin
      Result := dlg.ChosenReport;
      iCol := rptType_has_column(Result);
      if iCol < 0 then
        iCol := grid_hasEmptyColumn;
      if iCol < 0 then
      begin
        Grid.ColCount := Grid.ColCount + 1;
        iCol := Grid.ColCount - 1;
      end;
      Grid.Cells[iCol, 0] := Result;
      fillColumnFromINI(Result, iCol);
    end;
  finally
    dlg.Free;
  end;

end;


procedure TFmMap_to_xml.clr_column_values(const pCol: integer);
var
  iRow: integer;
begin
  iRow := 1;
  while iRow < Grid.RowCount do
  begin
    Grid.Cells[pcol, iRow] := EmptyStr;
    Inc(iRow);
  end;
end;

procedure TFmMap_to_xml.ActionResizeGridColumnsExecute(Sender: TObject);
var
  grHelper: TGridHelper;
  S: string;
begin
  if not Grid.Visible then
    Exit;
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

procedure TFmMap_to_xml.ActionWriteCol2INIExecute(Sender: TObject);
var
  aCol, rowDx, iCol: integer;
  aStr, aColName: string;
  rpt_type, block_hdr: string;
  ini: TIniFile;
begin
  if (Grid.Col = 0) or (Grid.Cells[Grid.Col, 0] = EmptyStr) then Exit;

  rpt_type := Grid.Cells[Grid.Col, 0];
  block_hdr := Format(REPORT_TYPE_FMT, [rpt_type]);
  StatusBar.SimpleText := Format('Writing data to INI file for %s', [aStr]);
  StatusBar.Update;
  aCol := Grid.Col;
  iCol := grid_hasEmptyColumn;
  if iCol < 0 then
  ;
  ini := TIniFile.Create(Prop_storage_ini);
  try
    for rowDx := 1 to Grid.RowCount - 1 do
    begin
      if Grid.Cells[aCol, rowDx] = EmptyStr then
        Continue;
      aColName := Grid.Cells[0, rowDx];
      aStr := Grid.Cells[aCol, rowDx];
      ini.WriteString(block_hdr, aColName, aStr);
    end;

  finally
    StatusBar.SimpleText := EmptyStr;
    FreeAndNil(ini);
    Refresh_inifile_handler;
  end;
end;


procedure TFmMap_to_xml.cboxReportsCloseUp(Sender: TObject);
var
  cbox: TComboBox;
begin
  Grid.Cells[Grid.Col, Grid.Row];
  cbox := TComboBox(Sender);
  try
    Grid.Cells[Grid.Col, Grid.Row] := cbox.SelText;
    Grid.Row := 1;
    Grid.SetFocus;
  finally
    ActionResizeGridColumns.Execute;
    if Grid.ColWidths[Grid.col] < COL_ORIG_WIDTH then
      Grid.ColWidths[Grid.Col] := COL_ORIG_WIDTH;
    cbox.Width := Grid.ColWidths[Grid.Col];
  end;
end;

var
  ext, path, fname: string;

initialization
  ext := ExtractFileExt(argv[0]);
  path := ExtractFileDir(argv[0]);
  fname := ExtractFileName(argv[0]);
  fname := Copy(fname, 1, Length(fname) - Length(ext));

  //path := Copy(path, 1, Pos(ext, path) -1);
  Prop_storage_ini := ConcatPaths([path, Concat(fname, '.ini')]);

  //+'.ini';

end.
