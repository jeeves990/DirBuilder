(*
    TODO's:
    DirBuilder project.
    1. read CSV files including files from Amazon and from InventoryLab.
    2. use those files to create an inventory database for tracking items
       that, in particular, are listed on more than one internet site.
    WARNING: Amazon reports are tab delimited so change the delimiter on
      the "CSV parser props" tab to TAB and then click the "Read CSV file"
      button, again.
    TODO: write the database from the stringgrid.
*)

unit frmDirFromCSV;

{$mode objfpc}{$H+}

interface

uses
  ActnList, Classes, ComCtrls, DBCtrls, DBGrids, ExtCtrls,
  Menus, StdCtrls, SysUtils, Forms, Controls, Graphics, Dialogs, Clipbrd,
  Grids, IniPropStorage, Buttons, DirBuilder_dmod, stringGridHelper,
  frmDisplayCSVFile, StringGridUtil, CSVParser_setup, dmodCSVParser,
  frmChangeCSVProperties, frmNewBooksDb, unitLoad_grid_from_csv, DB, Types;

type
  EMyDBNotOpenException = class(Exception);

  { TfrmFayesDirBuilder }

  TfrmFayesDirBuilder = class(TForm)
				ActionClose_CSV_dataset : TAction;
    ActionGetFilename_2_clipboard: TAction;
    ActionRead_withDataset: TAction;
    ActionChgRowDelimiter: TAction;
    ActionChgColDelimiter: TAction;
    ActionWriteGridToBooksDb: TAction;
    ActionRemoveCurrentFilename_fromList: TAction;
    ActionBooksDb: TAction;
    ActionReadRawFile: TAction;
    ActionFindOutputDir: TAction;
    ActionMkDirs: TAction;
    ActionFindCSV: TAction;
    ActionOpen: TAction;
    ActionResizeColumns: TAction;
    ActionClose: TAction;
    ActionList: TActionList;
    btn_mk_dirs: TBitBtn;
    btn_close: TBitBtn;
    btn_read_CSV_wDataset: TBitBtn;
    btn_read_raw_file: TBitBtn;
    btn_find_output_directory: TBitBtn;
    btn_read_CSV_wParser: TBitBtn;
    btn_find_csv_file: TBitBtn;
    btn_resize_table_columns: TBitBtn;
    cb_OutDir: TComboBox;
    cb_delimiter: TComboBox;
    cb_lineending: TComboBox;
    cb_1stRowIsTitles: TCheckBox;
    cb_CSVFile: TComboBox;
    DBG: TDBGrid;
    DirBuilderPropIni: TIniPropStorage;
    edCellContent: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lblCellContents: TLabel;
    lblOutDir: TLabel;
    lblCSVFile: TLabel;
    mainMnu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem17: TMenuItem;
		MenuItem18 : TMenuItem;
		mnuItm_close_dataset : TMenuItem;
    N11: TMenuItem;
    N10: TMenuItem;
    mnuItm_Open_w_parser: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    mnuWriteGridToBooksDb: TMenuItem;
    N8: TMenuItem;
    mnuItmDltCurCsvFile: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    mnuOpsAdmin: TMenuItem;
    N7: TMenuItem;
    N6: TMenuItem;
    N5: TMenuItem;
    N4: TMenuItem;
    N3: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    N2: TMenuItem;
    Operations: TMenuItem;
    N1: TMenuItem;
    MenuItemFile: TMenuItem;
    pnlCellContent: TPanel;
    pgCtrl: TPageControl;
    pnlTop: TPanel;
    popupMnu: TPopupMenu;
    cb_Popup: TPopupMenu;
    sg_parser_config_popup: TPopupMenu;
    StatusBar: TStatusBar;
    statBar_first_line: TStatusBar;
		CSV_col_grid : TStringGrid;
		db_col_grid : TStringGrid;
		tb_columns_test : TTabSheet;
    tab_CSVFile: TTabSheet;
    ActionRead_withParser: TAction;
    File_grid: TStringGrid;
    pnlBottom: TPanel;
    ActionChangeCSVDelimiter: TAction;
    gridPopup: TPopupMenu;
    ActionAddToDB: TAction;
    MenuItem14: TMenuItem;
    tab_csv_dataset: TTabSheet;
    procedure ActionBooksDbExecute(Sender: TObject);
    procedure ActionCloseExecute(Sender: TObject);
		procedure ActionClose_CSV_datasetExecute(Sender : TObject);
    procedure ActionGetFilename_2_clipboardExecute(Sender: TObject);
    procedure ActionRemoveCurrentFilename_fromListExecute(Sender: TObject);
    procedure ActionFindCSVExecute(Sender: TObject);
    procedure ActionFindOutputDirExecute(Sender: TObject);
    procedure ActionRead_withDatasetExecute(Sender: TObject);
    procedure ActionMkDirsExecute(Sender: TObject);
    procedure ActionRead_withParserExecute(Sender: TObject);
    procedure ActionReadRawFileExecute(Sender: TObject);
    procedure ActionResizeColumnsExecute(Sender: TObject);
    procedure ActionWriteGridToBooksDbExecute(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure cb_1stRowIsTitlesClick(Sender: TObject);
    procedure cb_CSVFileCloseUp(Sender: TObject);
    procedure cb_CSVFileEnter(Sender: TObject);
    procedure cb_delimiterCloseUp(Sender: TObject);
    procedure dbgridCSVCellClick(Column: TColumn);
    procedure dbgridCSVTitleClick(Column: TColumn);
    procedure DirBuilderPropIniRestoringProperties(Sender: TObject);
    procedure DirBuilderPropIniSaveProperties(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure File_gridHeaderClick(Sender: TObject; IsColumn: boolean; Index: integer);
    procedure File_gridDblClick(Sender: TObject);
    procedure File_gridClick(Sender: TObject);
    procedure ActionChangeCSVDelimiterExecute(Sender: TObject);
    procedure File_gridSelection(Sender: TObject; aCol, aRow: integer);
  private
    FDirListColumn: integer;
    FCSVDelimiter: string;
    Fdmod: TDirBuilder_dataModule;
    FBooksDbDlg: TfmNewBooksDb;

    FColDelimiter: ansistring;
    FRowDelimiter: ansistring;

    FCurReportType: TCaption;
    FChgParserProp: integer;

    FFromCheckBox : Boolean;

    function countSubDirs(path: string): integer;
    procedure GetCSVParserProps;
    function isGridPopulated: boolean;
    procedure move_grid_first_line;
		procedure Pop_CSV_columns(lst : TColumnList);
		procedure Pop_DB_columns(lst : TColumnList);
		procedure Pop_test_sgrid(lst : TColumnList; sg : TStringGrid);
		procedure reset_dbGrid;
		procedure resize_dbGrid_columns;
		procedure resize_file_grid_columns;

    procedure Setup_cb_delimiter;
    procedure Setup_cb_lineending;

    function GetColDelimiter: ansistring;
    function GetRowDelimiter: ansistring;

    procedure Test_file_exists;

  public
    property CSVDelimiter: string read FCSVDelimiter write FCSVDelimiter;
    property dmod: TDirBuilder_dataModule read Fdmod write Fdmod;

    property ColDelimiter: ansistring read GetColDelimiter;
    property RowDelimiter: ansistring read GetRowDelimiter;

  end;

var
  frmFayesDirBuilder: TfrmFayesDirBuilder;

implementation

uses csvdataset;

{$R *.lfm}

const
  SG_ROW_4_COL_DELIMITER = 0;
  SG_ROW_4_LINE_DELIMITER = 1;
  SG_COL_4_VALUES = 1;

  SG_ROW_DELIMITER = 'Row delimiter';
  SG_COL_DELIMITER = 'Column delimiter';

{ TfrmFayesDirBuilder }

procedure TfrmFayesDirBuilder.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmFayesDirBuilder.cb_1stRowIsTitlesClick(Sender: TObject);
begin
  FFromCheckBox := True;
  move_grid_first_line;
  FFromCheckBox := False;
end;

procedure TfrmFayesDirBuilder.cb_CSVFileCloseUp(Sender: TObject);
var
  idx: integer;
  txt: string;
  boo: boolean;
begin
  Test_file_exists;
  txt := cb_CSVFile.Text;
  for idx := 0 to cb_CSVFile.Items.Count - 1 do
    if txt = cb_CSVFile.Items[idx] then
      Exit;
  cb_CSVFile.Items.Add(txt);
  if FCurReportType <> cb_CSVFile.Text then
  begin
    //ActionIgnoreFirstLine.Checked := False;
    cb_1stRowIsTitles.Checked := False;
    cb_CSVFile.Invalidate;
  end;
end;

procedure TfrmFayesDirBuilder.cb_CSVFileEnter(Sender: TObject);
begin
  FCurReportType := cb_CSVFile.Text;
end;

procedure TfrmFayesDirBuilder.FormCreate(Sender: TObject);
begin
  if DirectoryExists(cb_OutDir.Text) then
    cb_OutDir.Items.Add(cb_OutDir.Text);
  Fdmod := TDirBuilder_dataModule.Create(self);
  File_grid.Clear;
  GetCSVParserProps;
  pgCtrl.ActivePage := tab_CSVFile;

  //g_path := ExtractFilePath(Application.ExeName);
  //ShowMessage(g_path);
  DirBuilderPropIni.Restore;
  FBooksDbDlg := nil;
  FChgParserProp := ChoseComma;
  Setup_cb_delimiter;
  Setup_cb_lineending;
  Test_file_exists;
end;

procedure TfrmFayesDirBuilder.Setup_cb_delimiter;
var
  idx: integer;
begin
  cb_delimiter.Items.Clear;

  idx := cb_delimiter.Items.Add(COMMA_STRING);
  cb_delimiter.Items.Objects[idx] := TObject(COMMA);

  idx := cb_delimiter.Items.Add(TAB_STRING);
  cb_delimiter.Items.Objects[idx] := TObject(TAB);

  idx := cb_delimiter.Items.Add(SEMICOLON_STRING);
  cb_delimiter.Items.Objects[idx] := TObject(SEMICOLON);
end;

procedure TfrmFayesDirBuilder.Setup_cb_lineending;
var
  idx: integer;
begin
  cb_lineending.Items.Clear;

  idx := cb_lineending.Items.Add(CRLF_STR);
  {  TAKE NOTE: THIS WILL TAKE SOME parsing VV  }
  cb_lineending.Items.Objects[idx] := TObject(CR);

  idx := cb_lineending.Items.Add(LF_STR);
  cb_lineending.Items.Objects[idx] := TObject(LF);
end;

procedure TfrmFayesDirBuilder.GetCSVParserProps;
begin
  //sg_parser_configs.Cells[0, 0] := 'Delimiter';
  //cb := TComboBox.Create(sg_parser_configs);
  //sg_parser_configs.Objects[0, 0] := cb;
  //cb.AddItem('Comma', nil);
  //cb.AddItem('Semicolon', nil);
  //cb.AddItem('Tab', nil);

  //sg_parser_configs.Cells[0, 1] := 'LineEnding';
  //cb := TComboBox.Create(sg_parser_configs);
  //sg_parser_configs.Objects[0, 1] := cb;
  //cb.AddItem('CRLF', nil);
  //cb.AddItem('LF', nil);

  //sg_parser_configs.ColWidths[0] := 100;
  //sg_parser_configs.ColWidths[1] := 100;
  //Parser_setup := TfmCSVParser_setup.Create(self);
  //try
  //  sg_parser_configs.Cells[1, 0] := Parser_setup.Delimiter;
  //  sg_parser_configs.Cells[1, 1] := Parser_setup.Lineending;
  //finally
  //  Parser_setup.Free;
  //end;
end;

procedure TfrmFayesDirBuilder.Test_file_exists;
var
  b: boolean;
begin
  b := FileExists(cb_CSVFile.Text);
  ActionRead_withParser.Enabled := b;
  ActionRead_withDataset.Enabled := b;
end;

procedure TfrmFayesDirBuilder.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  DirBuilderPropIni.Save;
end;

procedure TfrmFayesDirBuilder.dbgridCSVCellClick(Column: TColumn);
begin
  edCellContent.Text := Column.FieldName;
end;

procedure TfrmFayesDirBuilder.dbgridCSVTitleClick(Column: TColumn);
var
  msg, colTitle: string;
const
  MSG_TEXT = 'You are staged to write directories named like the "%s" column.';
begin
  FDirListColumn := Column.Index;
  File_grid.Columns[FDirListColumn].Width := 50;
  colTitle := File_grid.Columns[FDirListColumn].Title.Caption;
  ActionMkDirs.Enabled := True;
  msg := Format(MSG_TEXT, [colTitle]);
  StatusBar.Panels[1].Text := msg;
  ShowMessage(msg);
end;

procedure TfrmFayesDirBuilder.DirBuilderPropIniRestoringProperties(Sender: TObject);
var
  props: TIniPropStorage;
  sg: TStringGrid;
  sValue: string;
begin
  props := DirBuilderPropIni;
  props.IniSection := 'DirBuilderMain';

  //sg := sg_parser_configs;
  //sValue := props.ReadString(SG_ROW_DELIMITER, '');
  //sg.Cells[SG_COL_4_VALUES, SG_ROW_4_LINE_DELIMITER] := sValue;

  //sValue := props.ReadString(SG_COL_DELIMITER, '');
  //sg.Cells[SG_COL_4_VALUES, SG_ROW_4_COL_DELIMITER] := sValue;
end;

procedure TfrmFayesDirBuilder.DirBuilderPropIniSaveProperties(Sender: TObject);
var
  props: TIniPropStorage;
  sg: TStringGrid;
begin
  props := DirBuilderPropIni;
  props.IniSection := 'DirBuilderMain';

  //sg := sg_parser_configs;
  //props.WriteString(SG_ROW_DELIMITER,
  //sg.Cells[SG_COL_4_VALUES, SG_ROW_4_LINE_DELIMITER]);
  //props.WriteString(SG_COL_DELIMITER,
  //sg.Cells[SG_COL_4_VALUES, SG_ROW_4_COL_DELIMITER]);
end;

procedure TfrmFayesDirBuilder.resize_dbGrid_columns;
var
  row, col,
    wd : Integer;  { wd is work variable for current column width }
  ds : TCSVDataset;
  colWidthAra : Array of Integer;
const
  max_col_width = 177;
begin
  ds := dmod.CSVDataset;
  DBG.BeginUpdate;
  SetLength(colWidthAra, ds.Fields.Count);
  {  initialize array  }
  for col := 0 to Length(colWidthAra) -1 do
    colWidthAra[col] := 0;

  {  iterate over rows and columns to find max width of columns contents.  }
  try
    ds.First;
    while not ds.EOF do
    begin
      for col := 0 to ds.Fields.Count -1 do
      begin
        wd := DBG.Canvas.TextExtent(ds.Fields[col].DisplayText).cx;
        if wd > colWidthAra[col] then
          colWidthAra[col] := wd;
			end;
			ds.Next;
		end;
    for col := 0 to ds.Fields.Count -1 do
    begin
      if colWidthAra[col] > max_col_width then
        colWidthAra[col] := max_col_width;

      DBG.Columns.Items[col].Width := colWidthAra[col] +DEF_CELL_BORDER;
    end;
	finally
    DBG.EndUpdate(True);
    SetLength(colWidthAra, 0);
	end;
end;

procedure TfrmFayesDirBuilder.resize_file_grid_columns;
var
  grHelper: TGridHelper;
begin
  if not File_grid.Visible then
    Exit;
  File_grid.Update;
  grHelper := TGridHelper.Create;
  try
    grHelper.maxSize := 250;
    grHelper.SGrid := File_grid;
    grHelper.SetStringGridColumnWidths;
  finally
    grHelper.Free;
    btn_resize_table_columns.Enabled := False;
  end;
end;

procedure TfrmFayesDirBuilder.ActionResizeColumnsExecute(Sender: TObject);
(*
 *    TfrmFayesDirBuilder.ActionResizeColumnsExecute
 *    Looks like this will only be used for gridPopup.
 *)
var
  pt : TPoint;
begin
  pt := File_grid.ScreenToClient(Mouse.CursorPos);
  if (pgCtrl.ActivePage = tab_CSVFile) and (PtInRect(File_grid.BoundsRect, pt)) then
  begin
    resize_file_grid_columns;
    Exit;
	end;

  pt := DBG.ScreenToClient(Mouse.CursorPos);
  if (pgCtrl.ActivePage = tab_csv_dataset) and (PtInRect(DBG.BoundsRect, pt)) then
    resize_dbGrid_columns;
end;

procedure TfrmFayesDirBuilder.ActionWriteGridToBooksDbExecute(Sender: TObject);
var
  outCnt: integer;
  util: TStringGridUtil;
begin
  util := TStringGridUtil.Create;
  util.DbColsCallback := TColumnListCallbackMethod(@Pop_DB_columns);
  util.CsvColsCallback := TColumnListCallbackMethod(@Pop_CSV_columns);

  outCnt := BAD_CHOICE;
  try
    util.WriteGridToBooksDb(dmod, File_grid);
  finally
    util.Free;
  end;
end;

procedure TfrmFayesDirBuilder.ActionCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmFayesDirBuilder.ActionClose_CSV_datasetExecute(Sender : TObject);
begin
  if dmod.CSVDataset.Active then
    dmod.CSVDataset.Close;
end;

procedure TfrmFayesDirBuilder.ActionGetFilename_2_clipboardExecute(Sender: TObject);
begin
  Clipboard.AsText := cb_CSVFile.Text;
end;

procedure TfrmFayesDirBuilder.ActionRemoveCurrentFilename_fromListExecute(
  Sender: TObject);
var
  curDx, len: integer;
begin
  curDx := cb_CSVFile.ItemIndex;
  len := cb_CSVFile.Items.Count;
  cb_CSVFile.Items.Delete(curDx);
  if curDx > 0 then
    cb_CSVFile.ItemIndex := curDx - 1
  else
  begin
    len := cb_CSVFile.Items.Count;
    if len = 0 then
      cb_CSVFile.Text := 'Choose a CSV file';
  end;
end;

procedure TfrmFayesDirBuilder.ActionBooksDbExecute(Sender: TObject);
begin
  if FBooksDbDlg = nil then
    FBooksDbDlg := TfmNewBooksDb.Create(self);
  FBooksDbDlg.Show;
end;


procedure TfrmFayesDirBuilder.File_gridHeaderClick(Sender: TObject;
  IsColumn: boolean; Index: integer);
var
  msg, colTitle: ansistring;
begin
  if not IsColumn then
    Exit;
  colTitle := '';
  FDirListColumn := Index;
  colTitle := File_grid.Cells[Index, 0];
  ActionMkDirs.Enabled := True;
  FmtStr(msg, 'You are staged to write directories named like the "%s" column.',
    [colTitle]);
  StatusBar.Panels[1].Text := msg;
  ShowMessage(msg);
end;


procedure TfrmFayesDirBuilder.File_gridDblClick(Sender: TObject);
begin
  Clipboard.AsText := File_grid.Cells[File_grid.Col, File_grid.Row];
end;

procedure TfrmFayesDirBuilder.File_gridClick(Sender: TObject);
const
  lblS = 'Cell content (%d)';
var
  Str: string;
begin
  str := File_grid.Cells[File_grid.Col, File_grid.Row];
  edCellContent.Text := str;
  lblCellContents.Caption := Format(lblS, [Length(str)]);
end;

function TfrmFayesDirBuilder.GetColDelimiter: ansistring;
var
  ndx: integer;
begin
  ndx := cb_delimiter.ItemIndex;
  FColDelimiter := char(PtrInt(cb_delimiter.Items.Objects[ndx]));
  Result := FColDelimiter;
end;

function TfrmFayesDirBuilder.GetRowDelimiter: ansistring;
var
  ndx: integer;
begin
  ndx := cb_lineending.ItemIndex;
  FRowDelimiter := char(PtrInt(cb_lineending.Items.Objects[ndx]));
  Result := FRowDelimiter;
end;

procedure TfrmFayesDirBuilder.ActionChangeCSVDelimiterExecute(Sender: TObject);
(*
 *  procedure TfrmFayesDirBuilder.ActionChangeCSVDelimiterExecute(Sender: TObject);
 *  this may not be used.
 *)
//var
//ndx : Integer;
//dlg: TfmChangeCSVProperties;
//curDelimiter: char;
begin
  //ndx := cb_delimiter.ItemIndex;
  FColDelimiter := GetColDelimiter;

  //ndx := cb_lineending.ItemIndex;
  FRowDelimiter := GetRowDelimiter;
  Exit;
end;

procedure TfrmFayesDirBuilder.File_gridSelection(Sender: TObject; aCol, aRow: integer);
begin
  Statusbar.Panels[0].Text := Format('Position (%d, %d)', [aRow, aCol]);
end;

procedure TfrmFayesDirBuilder.cb_delimiterCloseUp(Sender: TObject);
var
  ndx: integer;
begin
  ndx := cb_delimiter.ItemIndex;
  FColDelimiter := Chr(PtrInt(cb_delimiter.Items.Objects[ndx]));
end;

procedure TfrmFayesDirBuilder.ActionFindOutputDirExecute(Sender: TObject);
var
  dirDlg: TSelectDirectoryDialog;
  outDir: string;
  idx: integer;
begin
  dirDlg := TSelectDirectoryDialog.Create(nil);
  try
    outDir := cb_OutDir.Text;
    if DirectoryExists(outDir) then
      dirDlg.InitialDir := outDir
    else
      dirDlg.InitialDir := 'c:\';
    if not dirDlg.Execute then
      Exit;
    idx := cb_OutDir.Items.Add(dirDlg.FileName);
    cb_OutDir.ItemIndex := idx;
  finally
    dirDlg.Free;
  end;
end;

procedure TfrmFayesDirBuilder.ActionRead_withDatasetExecute(Sender: TObject);
begin
  dmod.CSVDataset.Close;
  DBG.Columns.Clear;
  DBG.BeginUpdate;
  try
    dmod.CSVDataset.FileName := cb_CSVFile.Text;
    with dmod.CSVDataset.CSVOptions do
    begin
      LineEnding := RowDelimiter;
      Delimiter := ColDelimiter[1];
      FirstLineAsFieldNames := cb_1stRowIsTitles.Checked;
    end;
    dmod.CSVDataset.Open;
    pgCtrl.ActivePage := tab_csv_dataset;
    resize_dbGrid_columns;
    dmod.CSVDataset.First;
	finally
    DBG.EndUpdate(True);
	end;
end;

function TfrmFayesDirBuilder.isGridPopulated: boolean;
begin
  Result := File_grid.ColCount > 3;
end;

procedure TfrmFayesDirBuilder.reset_dbGrid;
begin
  dmod.CSVDataset.Close;
  ActionRead_withDataset.Execute;
end;

procedure TfrmFayesDirBuilder.move_grid_first_line;

  function Is_row_empty(row: TStrings): boolean;
  var
    i: integer;
  begin
    Result := True;
    for i := 0 to row.Count - 1 do
      if row[i] <> '' then
      begin
        Result := False;
        Exit;
      end;
  end;

  function is_stringGrid_empty(sg: TStringGrid): boolean;
    var
      row, col: integer;
      str : String;
    begin
      try
        Result := False;
        if not Assigned(sg.Rows[0]) then
          Exit;
        if sg.rows[0] <> nil then
          Result := True;


        //str := sg.Rows[0][0];
        row := sg.RowCount;
        for row := 0 to sg.RowCount - 1 do
          for col := 0 to sg.ColCount - 1 do
            if sg.Cells[col, row] > '' then
            begin
              Result := True;
              Exit;
            end;
      except
        on Ex: EGridException do
        begin
          ShowMessage(Ex.Message);
          Result := True;
          Exit;
        end;
        on Ex: Exception do
          ShowMessage(Ex.Message);
      end;

    end;

var
  i, ndx: integer;
  aRow: TStrings;
  pt : TPoint;
begin

  pt := DBG.ScreenToClient(Mouse.CursorPos);
  if (pgCtrl.ActivePage = tab_csv_dataset)
    and ((PtInRect(DBG.BoundsRect, pt) or FFromCheckBox)) then
    reset_dbGrid;

  if is_stringGrid_empty(File_grid) then
    Exit;

  pt := File_grid.ScreenToClient(Mouse.CursorPos);
  if (pgCtrl.ActivePage = tab_CSVFile)
      and ((PtInRect(File_grid.BoundsRect, pt) or FFromCheckBox)) then
  begin
    if cb_1stRowIsTitles.Checked then
    begin
      aRow := File_grid.Rows[1];
      File_grid.Rows[0] := aRow;

      ndx := 2;
      while ndx < File_grid.RowCount do
      begin
        File_grid.Rows[ndx - 1] := File_grid.Rows[ndx];
        Inc(ndx);
      end;
      File_grid.DeleteRow(ndx - 1);
      Exit;
    end;
    {  otherwise.  }
    aRow := File_grid.Rows[File_grid.RowCount - 1];

    {  if bottom row is not empty, we need a blank row added below  }
    if not Is_row_empty(aRow) then
      File_grid.RowCount := File_grid.RowCount + 1;

    ndx := File_grid.RowCount - 1;  // the empty row
    while ndx > 0 do
    begin
      File_grid.Rows[ndx] := File_grid.Rows[ndx - 1];
      Dec(ndx);
    end;
    file_grid.Rows[0].Clear;
  end;
end;

procedure TfrmFayesDirBuilder.ActionMkDirsExecute(Sender: TObject);
var
  s, outPath, outDir: string;
  bookMark: integer;
  beginCnt, endCnt: integer;
  rowIdx: integer;
begin
  outPath := cb_OutDir.Text;
  if not DirectoryExists(outPath) then
  begin
    ShowMessage(outpath + ' does not exist as a directory.');
    Exit;
  end;
  beginCnt := countSubDirs(outPath);
  outPath := IncludeTrailingPathDelimiter(outPath);
  bookMark := File_grid.Row;
  File_grid.BeginUpdate;
  try

    rowIdx := 1;
    while rowIdx < File_grid.RowCount do
    begin
      s := File_grid.Cells[FDirListColumn, rowIdx];  // the directory name
      s := StringReplace(s, '/', '-', [rfReplaceAll]);
      s := StringReplace(s, '\\', '-', [rfReplaceAll]);
      s := StringReplace(s, ':', ';', [rfReplaceAll]);

      outDir := ConcatPaths([outPath, s]);
      if not DirectoryExists(outDir) then
        CreateDir(outDir);
      Inc(rowIdx);
    end;
  finally
    File_grid.EndUpdate();
    File_grid.Row := bookMark;
  end;

  pgCtrl.ActivePage := tab_CSVFile;
  endCnt := countSubDirs(outPath);
  if endCnt = beginCnt then
    StatusBar.Panels[1].Text := Format('No subdirectories were added to %s', [outPath])
  else
    StatusBar.Panels[1].Text :=
      Format('There were %d subdirectories added to %s',
      [endCnt - beginCnt, outPath]);
end;

function TfrmFayesDirBuilder.countSubDirs(path: string): integer;
var
  subDirsSrchRec: TSearchRec;
  cnt: longint;
  curDir: string;
  dirName: ansistring;
begin
  cnt := 0;
  curDir := GetCurrentDir;
  try
    begin
      SetCurrentDir(path);

      if FindFirst('*', faDirectory, subDirsSrchRec) = 0 then
      begin
        repeat

          if (subDirsSrchRec.Attr and faDirectory) = faDirectory then
          begin
            dirName := subDirsSrchRec.Name;
            try
              if (dirName = ansistring('.')) or (dirName = ansistring('..')) then
                Continue;

            except
              on e: Exception do
                ShowMessage(e.Message);
            end;
            Inc(cnt);
          end;

        until
          FindNext(subDirsSrchRec) <> 0;
        FindClose(subDirsSrchRec);
      end;

      Result := cnt;
    end;
  finally
    SetCurrentDir(curDir);
  end;
end;

procedure TfrmFayesDirBuilder.ActionFindCSVExecute(Sender: TObject);
var
  dlg: TOpenDialog;
  dir: string;
  i : Integer;
begin
  ActionRead_withParser.Enabled := False;
  ActionRead_withDataset.Enabled := False;
  dlg := TOpenDialog.Create(nil);
  try
    dlg.Options := [ofReadOnly];
    dir := ExtractFilePath(cb_CSVFile.Text);
    if DirectoryExists(dir) then
      dlg.InitialDir := dir    // if the whole of the cbox.text, use it
    else
      dlg.InitialDir := ExtractFileDrive(dir);

    if not dlg.Execute then
      Exit;

    cb_CSVFile.Text := dlg.FileName;
    for i := 0 to cb_CSVFile.Items.Count -1 do
      if Lowercase(cb_CSVFile.Items[i]) = LowerCase(dlg.FileName) then
        Exit;
    {  the file name is fresh.  }
    i := cb_CSVFile.Items.Add(dlg.FileName);
    cb_CSVFile.ItemIndex := i;
    cb_CSVFileCloseUp(self);

  finally
    dlg.Free;
  end;
end;

procedure TfrmFayesDirBuilder.ActionRead_withParserExecute(Sender: TObject);
var
  fileName, first_line: string;
  //delimiter: char;
  parmRec: TParmRec;
begin
  fileName := cb_CSVFile.Text;
  if not FileExists(fileName) then
  begin
    ShowMessage(fileName + ' does not exist. Try again');
    Exit;
  end;
  File_grid.Clear;
  Application.ProcessMessages;

  try
    try
      begin
        File_grid.BeginUpdate;

        parmRec.colDelimiter := ColDelimiter[1];
        parmRec.rowDelimiter := RowDelimiter[1];
        parmRec.ignoreFirstLine := cb_1stRowIsTitles.Checked;
        parmRec.addRows := True;

        LoadGridFromCSVFile(File_grid, fileName, parmRec, first_line);
        if first_line > EmptyStr then
        begin
          cb_1stRowIsTitles.Checked := True;
          statBar_first_line.Visible := True;
          statBar_first_line.SimpleText :=
            Format('Ignored first line: %s', [first_line]);
        end

        else

        begin
          cb_1stRowIsTitles.Checked := False;
          statBar_first_line.Visible := False;
        end;

        if File_grid.RowCount = File_grid.FixedRows then
        begin
          ShowMessage('TfrmFayesDirBuilder.ActionReadCSVExecute: ' +
            sLineBreak + 'StringGrid is not populated?');
          Exit;
        end
        else
          resize_file_grid_columns;
      end;
      pgCtrl.ActivePage := tab_CSVFile;
    finally
      File_grid.EndUpdate();
    end;
  except
    //on e : Exception do
    //  ShowMessage(Format('CSV did not open. %s', [e.Message]));
    //on e : EMyDBNotOpenException do
    //  ShowMessage(Format('CSV did not open. %s', [e.Message]));
  end;
end;

procedure TfrmFayesDirBuilder.ActionReadRawFileExecute(Sender: TObject);
var
  frm: TfmDisplayCSVFile;
begin
  frm := TfmDisplayCSVFile.Create(self, cb_CSVFile.Text);
  frm._col_delimiter := ColDelimiter;
  frm._row_delimiter := RowDelimiter;
  frm.HasHeaders := cb_1stRowIsTitles.Checked;
  frm.DisplayFile;
  frm.Show;
end;

procedure TfrmFayesDirBuilder.Pop_test_sgrid(lst : TColumnList; sg : TStringGrid);
var
  i, sg_row : Integer;
  rec : TColumnRec;
begin
  sg_row := 1;
  for i := 0 to lst.Count -1 do
  begin
    rec := TColumnRec(lst[i]);
    sg_row := sg.RowCount;
    sg.RowCount := sg.RowCount +1;
    sg.Cells[0, sg_row] := rec.colName;
    sg.Cells[1, sg_row] := rec.colName4cmp;
    sg.Cells[2, sg_row] := IntToStr(rec.relativePos);
	end;
end ;

procedure TfrmFayesDirBuilder.Pop_CSV_columns(lst : TColumnList);
begin
  Pop_test_sgrid(lst, CSV_col_grid);
end;

procedure TfrmFayesDirBuilder.Pop_DB_columns(lst : TColumnList);
begin
  Pop_test_sgrid(lst, db_col_grid);
end;

end.
