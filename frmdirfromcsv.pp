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
  frmChangeCSVProperties, frmNewBooksDb, unitLoad_grid_from_csv, Types;

type
  EMyDBNotOpenException = class(Exception);
  //TChgParserProps = (ColDelimiter = 0, RowDelimiter);

  { TfrmFayesDirBuilder }

  TfrmFayesDirBuilder = class(TForm)
		ActionChgRowDelimiter : TAction;
		ActionChgColDelimiter : TAction;
    ActionWriteGridToBooksDb: TAction;
    ActionDeleteCurrentCSVFileFromCheckBox: TAction;
    ActionBooksDb: TAction;
    ActionReadRawFile: TAction;
    ActionSetTitles: TAction;
    ActionFindOutputDir: TAction;
    ActionMkDirs: TAction;
    ActionFindCSV: TAction;
    ActionOpen: TAction;
    ActionResizeColumns: TAction;
    ActionClose: TAction;
    ActionList: TActionList;
    btn_mk_dirs: TBitBtn;
    btn_close: TBitBtn;
    btn_read_raw_file: TBitBtn;
    btn_find_output_directory: TBitBtn;
    btn_read_CSV_file: TBitBtn;
    btn_find_csv_file: TBitBtn;
		btn_resize_table_columns : TBitBtn;
    cboxOutDir: TComboBox;
		cb_delimiter : TComboBox;
		cb_lineending : TComboBox;
    ckbox1stRowIsTitles: TCheckBox;
    cboxCSVFile: TComboBox;
    DirBuilderPropIni: TIniPropStorage;
    edCellContent: TEdit;
		Label1 : TLabel;
		Label2 : TLabel;
    lblCellContents: TLabel;
    lblOutDir: TLabel;
    lblCSVFile: TLabel;
    mainMnu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
		MenuItem13 : TMenuItem;
    mnuWriteGridToBooksDb: TMenuItem;
    N8: TMenuItem;
    mnuItmDltCurCsvFile: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    mnuIgnoreFirstLine: TMenuItem;
    mnuChangeDelimiter: TMenuItem;
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
    cBoxPopup: TPopupMenu;
		sg_parser_configs : TStringGrid;
		sg_parser_config_popup : TPopupMenu;
    StatusBar: TStatusBar;
    statBar_first_line: TStatusBar;
    tabshCSVFile: TTabSheet;
    tabshCSVParserProps: TTabSheet;
    ActionReadCSV: TAction;
    File_grid: TStringGrid;
    pnlBottom: TPanel;
    gpBoxCSVParserProperties: TGroupBox;
    ActionChangeCSVDelimiter: TAction;
    sGridPopup: TPopupMenu;
    N9: TMenuItem;
    mnuChangeCSVDelimiter: TMenuItem;
    ActionAddToDB: TAction;
    MenuItem14: TMenuItem;
    procedure ActionBooksDbExecute(Sender: TObject);
    procedure ActionCloseExecute(Sender: TObject);
    procedure ActionDeleteCurrentCSVFileFromCheckBoxExecute(Sender: TObject);
    procedure ActionFindCSVExecute(Sender: TObject);
    procedure ActionFindOutputDirExecute(Sender: TObject);
    procedure ActionMkDirsExecute(Sender: TObject);
    procedure ActionReadCSVExecute(Sender: TObject);
    procedure ActionReadRawFileExecute(Sender: TObject);
    procedure ActionResizeColumnsExecute(Sender: TObject);
    procedure ActionSetTitlesExecute(Sender: TObject);
    procedure ActionWriteGridToBooksDbExecute(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure cboxCSVFileCloseUp(Sender: TObject);
    procedure cboxCSVFileEnter(Sender: TObject);
    procedure cbox_EditingDone(Sender: TObject);
		procedure cb_delimiterCloseUp(Sender : TObject);
    procedure ckboxShowLineNumbersChange(Sender: TObject);
    procedure dbgridCSVCellClick(Column: TColumn);
    procedure dbgridCSVTitleClick(Column: TColumn);
		procedure DirBuilderPropIniRestoringProperties(Sender : TObject);
		procedure DirBuilderPropIniSaveProperties(Sender : TObject);
    procedure edCSVFileChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure cboxCSVFileChange(Sender: TObject);
    procedure File_gridHeaderClick(Sender: TObject; IsColumn: boolean; Index: integer);
    procedure File_gridDblClick(Sender: TObject);
    procedure File_gridClick(Sender: TObject);
    procedure ActionChangeCSVDelimiterExecute(Sender: TObject);
    procedure File_gridSelection(Sender: TObject; aCol, aRow: integer);
    procedure sg_parser_configsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure sg_parser_configsSelectEditor(Sender: TObject;
      aCol, aRow: integer; var Editor: TWinControl);
  private
    //checkFlag: boolean;
    FDirListColumn: integer;
    FCSVDelimiter: string;
		//FDlgOp : ShortInt;
    Fdmod: TDirBuilder_dataModule;
    FBooksDbDlg: TfmNewBooksDb;

    FColDelimiter: AnsiString;
    FRowDelimiter: AnsiString;

    FCurReportType: TCaption;
    FChgParserProp : Integer;

    function countSubDirs(path: string): integer;
    procedure GetCSVParserProps;
    function isGridPopulated: boolean;
    procedure move_grid_first_line;

    procedure Setup_cb_delimiter;
		procedure Setup_cb_lineending;

    function GetColDelimiter : AnsiString;
    function GetRowDelimiter : AnsiString;

  public
    property CSVDelimiter: string read FCSVDelimiter write FCSVDelimiter;
    property dmod: TDirBuilder_dataModule read Fdmod write Fdmod;

    property ColDelimiter : AnsiString read GetColDelimiter;
    property RowDelimiter : AnsiString read GetRowDelimiter;

  end;

var
  frmFayesDirBuilder: TfrmFayesDirBuilder;

implementation

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

procedure TfrmFayesDirBuilder.cboxCSVFileCloseUp(Sender: TObject);
var
  idx: integer;
  txt: string;
begin
  txt := cboxCSVFile.Text;
  for idx := 0 to cboxCSVFile.Items.Count - 1 do
    if txt = cboxCSVFile.Items[idx] then
      Exit;
  cboxCSVFile.Items.Add(txt);
  if FCurReportType <> cboxCSVFile.Text then
  begin
    //ActionIgnoreFirstLine.Checked := False;
    ckbox1stRowIsTitles.Checked := False;
    cboxCSVFile.Invalidate;
  end;
end;

procedure TfrmFayesDirBuilder.cboxCSVFileEnter(Sender: TObject);
begin
  FCurReportType := cboxCSVFile.Text;
end;

procedure TfrmFayesDirBuilder.ckboxShowLineNumbersChange(Sender: TObject);
begin
  //if cboxShowLineNumbers.Checked then
  //   sgridCSV.FixedCols := 1
  //else
  //   sgridCSV.FixedCols := 0 ;
end;

procedure TfrmFayesDirBuilder.FormCreate(Sender: TObject);
begin
  ckboxShowLineNumbersChange(self);
  if DirectoryExists(cboxOutDir.Text) then
    cboxOutDir.Items.Add(cboxOutDir.Text);
  Fdmod := TDirBuilder_dataModule.Create(self);
  File_grid.Clear;
  GetCSVParserProps;
  pgCtrl.ActivePage := tabshCSVFile;

  //g_path := ExtractFilePath(Application.ExeName);
  //ShowMessage(g_path);
  DirBuilderPropIni.Restore;
  FBooksDbDlg := nil;
  FChgParserProp := ChoseComma;
  Setup_cb_delimiter;
  Setup_cb_lineending;
end;

procedure TfrmFayesDirBuilder.Setup_cb_delimiter;
var
  idx : Integer;
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
  idx : Integer;
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

procedure TfrmFayesDirBuilder.edCSVFileChange(Sender: TObject);
begin
  ActionReadCSV.Enabled := FileExists(cboxCSVFile.Text);
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

procedure TfrmFayesDirBuilder.DirBuilderPropIniRestoringProperties(
			Sender : TObject);
var
  props : TIniPropStorage;
  sg : TStringGrid;
  sValue : String;
begin
  props := DirBuilderPropIni;
  props.IniSection := 'DirBuilderMain';

  sg := sg_parser_configs;
  sValue := props.ReadString(SG_ROW_DELIMITER, '');
  sg.Cells[SG_COL_4_VALUES, SG_ROW_4_LINE_DELIMITER] := sValue;

  sValue := props.ReadString(SG_COL_DELIMITER, '');
  sg.Cells[SG_COL_4_VALUES, SG_ROW_4_COL_DELIMITER] := sValue;
end;

procedure TfrmFayesDirBuilder.DirBuilderPropIniSaveProperties(Sender : TObject);
var
  props : TIniPropStorage;
  sg : TStringGrid;
begin
  props := DirBuilderPropIni;
  props.IniSection := 'DirBuilderMain';

  sg := sg_parser_configs;
  props.WriteString(SG_ROW_DELIMITER,
        sg.Cells[SG_COL_4_VALUES, SG_ROW_4_LINE_DELIMITER]);
  props.WriteString(SG_COL_DELIMITER,
        sg.Cells[SG_COL_4_VALUES, SG_ROW_4_COL_DELIMITER]);
end;

procedure TfrmFayesDirBuilder.ActionResizeColumnsExecute(Sender: TObject);
var
  grHelper: TGridHelper;
  //S: string;
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

procedure TfrmFayesDirBuilder.ActionSetTitlesExecute(Sender: TObject);
begin
  //if ckbox1stRowIsTitles.Checked then
  //begin
  //  Fdmod.setFieldNames;
  //  dbgridCSV.Options := dbgridCSV.Options + [dgTitles];
  //  //dbgridCSV.DataSource.DataSet.Close;
  //  //dbgridCSV.DataSource.DataSet.Open;
  //end
  //else
  //   dbgridCSV.Options := dbgridCSV.Options - [dgTitles]
end;

procedure TfrmFayesDirBuilder.ActionWriteGridToBooksDbExecute(Sender: TObject);
var
  outCnt: integer;
  util: TStringGridUtil;
begin
  util := TStringGridUtil.Create;
  outCnt := BAD_CHOICE;
  try
    util.WriteGridToBooksDb(dmod, File_grid, outCnt);
  finally
    util.Free;
  end;
end;

procedure TfrmFayesDirBuilder.ActionCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmFayesDirBuilder.ActionDeleteCurrentCSVFileFromCheckBoxExecute(
  Sender: TObject);
var
  curDx, len: integer;
begin
  curDx := cboxCSVFile.ItemIndex;
  len := cboxCSVFile.Items.Count;
  cboxCSVFile.Items.Delete(curDx);
  if curDx > 0 then
    cboxCSVFile.ItemIndex := curDx - 1
  else
  begin
    len := cboxCSVFile.Items.Count;
    if len = 0 then
      cboxCSVFile.Text := 'Choose a CSV file';
  end;
end;

procedure TfrmFayesDirBuilder.ActionBooksDbExecute(Sender: TObject);
begin
  if FBooksDbDlg = nil then
    FBooksDbDlg := TfmNewBooksDb.Create(self);
  FBooksDbDlg.Show;
end;

procedure TfrmFayesDirBuilder.cboxCSVFileChange(Sender: TObject);
begin
  //if Fdmod.CSVDataset.Active then
  //  ckbox1stRowIsTitles.Checked:=False;
end;

procedure TfrmFayesDirBuilder.File_gridHeaderClick(Sender: TObject;
  IsColumn: boolean; Index: integer);
var
  msg, colTitle: AnsiString;
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

function TfrmFayesDirBuilder.GetColDelimiter : AnsiString;
var
  ndx : Integer;
begin
  ndx := cb_delimiter.ItemIndex;
  FColDelimiter := Char(PtrInt(cb_delimiter.Items.Objects[ndx]));
  Result := FColDelimiter;
end;

function TfrmFayesDirBuilder.GetRowDelimiter : AnsiString;
var
  ndx : Integer;
begin
  ndx := cb_lineending.ItemIndex;
  FRowDelimiter := Char(PtrInt(cb_lineending.Items.Objects[ndx]));
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

procedure TfrmFayesDirBuilder.sg_parser_configsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  sg: TStringGrid;
  aCol, aRow: integer;
  ed: TWinControl;
const
  msg = 'TfrmFayesDirBuilder.sg_parser_configsMouseDown mouse out of bounds';
begin
  sg := Sender as TStringGrid;
  sg.MouseToCell(X, Y, aCol, aRow);
  if aCol <> SG_COL_4_VALUES then
    Exit;

  if Button = mbRight then
  begin

		case aRow of
      SG_ROW_4_LINE_DELIMITER:
        begin
          ed := cb_lineending;
          FChgParserProp := ChgParser_RowDelim;
				end;
			SG_ROW_4_COL_DELIMITER:
        begin
          ed := cb_delimiter;
          FChgParserProp := ChgParser_ColDelim;
				end;
			else
        begin
          FChgParserProp := -1 ;
    			raise Exception.Create(msg);
				end;
    end;
    Exit;
  end;

  if Button <> mbLeft then
    sg_parser_configsSelectEditor(Sender, aCol, aRow, ed);
end;


procedure TfrmFayesDirBuilder.sg_parser_configsSelectEditor(Sender: TObject;
  aCol, aRow: integer; var Editor: TWinControl);
var
  cb: TComboBox;
  sg: TStringGrid;
begin
  if aCol <> SG_COL_4_VALUES then
    Exit;
  sg := sg_parser_configs;
  if aRow = SG_ROW_4_COL_DELIMITER then
  begin
    cb := cb_delimiter;
    cb.BoundsRect := sg.CellRect(aCol, aRow);
    cb.Text := sg.Cells[sg.Col, sg.Row];
  end
  else if aRow = SG_ROW_4_LINE_DELIMITER then
  begin
    cb := cb_lineending;
    cb.BoundsRect := sg.CellRect(aCol, aRow);
    cb.Text := sg.Cells[sg.Col, sg.Row];
  end;
  Editor := cb;
end;

procedure TfrmFayesDirBuilder.cbox_EditingDone(Sender: TObject);
var
  sg: TStringGrid;
begin
  if not (Sender is TComboBox) then
    Exit;
  sg := sg_parser_configs;
  if TComboBox(Sender) = cb_lineending then
    sg.Cells[SG_COL_4_VALUES, SG_ROW_4_LINE_DELIMITER] := cb_lineending.Text

  else if TComboBox(Sender) = cb_delimiter then
    sg.Cells[SG_COL_4_VALUES, SG_ROW_4_COL_DELIMITER] := cb_delimiter.Text;
end;

procedure TfrmFayesDirBuilder.cb_delimiterCloseUp(Sender : TObject);
var
  ndx : Integer;
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
    outDir := cboxOutDir.Text;
    if DirectoryExists(outDir) then
      dirDlg.InitialDir := outDir
    else
      dirDlg.InitialDir := 'c:\';
    if not dirDlg.Execute then
      Exit;
    idx := cboxOutDir.Items.Add(dirDlg.FileName);
    cboxOutDir.ItemIndex := idx;
  finally
    dirDlg.Free;
  end;
end;

function TfrmFayesDirBuilder.isGridPopulated: boolean;
begin
  Result := File_grid.ColCount > 3;
end;

procedure TfrmFayesDirBuilder.move_grid_first_line;
var
  iCol, iRow: integer;
  first_line: string;
begin
  first_line := EmptyStr;
  for iCol := 0 to File_grid.ColCount - 1 do
    first_line := Concat(first_line, '[', File_grid.Cells[iCol, 0], ']  ');
  statBar_first_line.SimpleText := 'Ignored line: ' + first_line;
  for iRow := 1 to File_grid.RowCount - 1 do
    for iCol := 0 to File_grid.ColCount - 1 do
      File_grid.Cells[iCol, iRow - 1] := File_grid.Cells[iCol, iRow];
  File_grid.RowCount := File_grid.RowCount - 1;
end;


procedure TfrmFayesDirBuilder.ActionMkDirsExecute(Sender: TObject);
var
  s, outPath, outDir: string;
  bookMark: integer;
  beginCnt, endCnt: integer;
  rowIdx: integer;
begin
  outPath := cboxOutDir.Text;
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

  pgCtrl.ActivePage := tabshCSVFile;
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
  dir : string;
begin
  ActionReadCSV.Enabled := False;
  dlg := TOpenDialog.Create(nil);
  try
    dlg.Options := [ofReadOnly];
    dir := cboxCSVFile.Text;
    if DirectoryExists(dir) then
      dlg.InitialDir := dir    // if the whole of the cbox.text, use it
    else
    begin
      dir := ExtractFilePath(dir);
      if DirectoryExists(dir) then
        dlg.InitialDir := dir
      else
        dlg.InitialDir := ExtractFileDrive(dir);
    end;
    if not dlg.Execute then
      Exit;
    cboxCSVFile.Text := dlg.FileName;
    cboxCSVFileCloseUp(self);
    ActionReadCSV.Enabled := True;
    ActionReadRawFile.Enabled := True;
    //btnReadCSV.Enabled := True;
    //btnReadCSV.Enabled;
    //btnReadRawFile.Enabled := True;
  finally
    dlg.Free;
  end;
end;

procedure TfrmFayesDirBuilder.ActionReadCSVExecute(Sender: TObject);
var
  fileName, first_line: string;
  //delimiter: char;
  parmRec: TParmRec;
begin
  fileName := cboxCSVFile.Text;
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
        //get the delimiter from cb_delimiter.objects
        //case FColDelimiter of
        //  SEMICOLON: delimiter := SEMICOLON;
        //  COMMA: delimiter := COMMA;
        //  TAB: delimiter := TAB;
        //  else
        //    delimiter := COMMA;
        //end;

        parmRec.colDelimiter := ColDelimiter[1];
        parmRec.rowDelimiter := RowDelimiter[1];
        parmRec.ignoreFirstLine := ckbox1stRowIsTitles.Checked;
        parmRec.addRows := True;

        LoadGridFromCSVFile(File_grid, fileName, parmRec, first_line);
        if first_line > EmptyStr then
        begin
          ckbox1stRowIsTitles.Checked := True;
          statBar_first_line.Visible := True;
          statBar_first_line.SimpleText :=
            Format('Ignored first line: %s', [first_line]);
        end
        else
        begin
          ckbox1stRowIsTitles.Checked := False;
          statBar_first_line.Visible := False;
        end;

        if File_grid.RowCount = File_grid.FixedRows then
        begin
          ShowMessage('TfrmFayesDirBuilder.ActionReadCSVExecute: ' +
            sLineBreak + 'StringGrid is not populated?');
          Exit;
        end
        else
          ActionResizeColumns.Execute;
      end;
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
  frm := TfmDisplayCSVFile.Create(self, cboxCSVFile.Text);
  frm._col_delimiter := ColDelimiter;
  frm._row_delimiter := RowDelimiter;
  frm.HasHeaders := ckbox1stRowIsTitles.Checked;
  frm.DisplayFile;
  frm.Show;
end;

end.

procedure SetColDelimiter(aValue : String);
procedure SetRowDelimiter(aValue : String);


procedure TfrmFayesDirBuilder.SetColDelimiter(aValue : String);
begin
  sg_parser_configs.Cells[SG_COL_4_VALUES, SG_ROW_4_COL_DELIMITER] := aValue;
end;

procedure TfrmFayesDirBuilder.SetRowDelimiter(aValue : String);
begin
  sg_parser_configs.Cells[SG_COL_4_VALUES, SG_ROW_4_LINE_DELIMITER] := aValue;
end;

function TfrmFayesDirBuilder.GetColDelimiter : String;
begin
  Result := sg_parser_configs.Cells[SG_COL_4_VALUES, SG_ROW_4_COL_DELIMITER];
end;

function TfrmFayesDirBuilder.GetRowDelimiter : Integer;
var
  test_str : String;
begin
  test_str := sg_parser_configs.Cells[SG_COL_4_VALUES, SG_ROW_4_LINE_DELIMITER];
  if test_str = '' then
    Result := ChoseNoChoice
  else
  if test_str = '' then ;
end;

curDelimiter := FColDelim_value;
curLineending := FRowDelim_value;

dlg := TfmChangeCSVProperties.Create(self);
try

	if FChgParserProp = ChgParser_RowDelim then
  begin
    dlg.ParmRec.form_op := ChgParser_RowDelim;
    //dlg.ParmRec.grp_value := GetRowDelimiter;
	end
	else if FChgParserProp = ChgParser_ColDelim  then
  begin
    dlg.ParmRec.form_op := ChgParser_ColDelim;
    dlg.ParmRec.grp_value := GetColDelimiter;
	end;

  dlg.ShowModal;
  case dlg.ModalResult of
    ChoseNoChoice: Exit;
    ChoseComma:
    begin
      str := COMMA_STRING;
      FColDelim_value := COMMA;
    end;
    ChoseTab:
    begin
      str := TAB_STRING;
      FColDelim_value := TAB;
    end;
    ChoseSemicolon:
    begin
      str := SEMICOLON_STRING;
      FColDelim_value := SEMICOLON;
    end;
  end;
  sg_parser_configs.Cells[SG_COL_4_VALUES, SG_ROW_4_COL_DELIMITER] := str;

  case dlg.ModalResult of
    ChoseNoChoice: Exit;
    ChoseCRLF:
    begin
      FRowDelim_value := CRLF;
      str := CRLF_STR;
    end;
    ChoseLF:
    begin
      FRowDelim_value := LF;
      str := LF_STR;
    end;
  end;

  sg_parser_configs.Cells[SG_COL_4_VALUES, SG_ROW_4_LINE_DELIMITER] := str;
  StatusBar.Panels[1].Text :=
        Format('Current column delimiter: %s', [FColDelim_value]);
  if curDelimiter <> FColDelim_value then
    ActionReadCSV.Execute;
finally
  dlg.Free;
end;

procedure TfrmFayesDirBuilder.ActionIgnoreFirstLineExecute(Sender: TObject);
const
  Move_up = True;
  Move_down = False;
begin
  ActionIgnoreFirstLine.Checked := not ActionIgnoreFirstLine.Checked;
  if ActionIgnoreFirstLine.Checked then
  begin
    statBar_first_line.Visible := True;
    move_grid_first_line;
  end
  else
  begin
    statBar_first_line.Visible := False;
    if isGridPopulated then
      ActionReadCSV.Execute;
  end;
end;


