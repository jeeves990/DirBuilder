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
  frmChangeCSVProperties, frmNewBooksDb, unitLoad_grid_from_csv;

type
  EMyDBNotOpenException = class(Exception);

  { TfrmFayesDirBuilder }

  TfrmFayesDirBuilder = class(TForm)
    ActionWriteGridToBooksDb: TAction;
    ActionDeleteCurrentCSVFileFromCheckBox: TAction;
    ActionIgnoreFirstLine: TAction;
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
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    btn_resize_table_columns: TBitBtn;
    btn_read_raw_file: TBitBtn;
    btn_find_output_directory: TBitBtn;
    btn_read_CSV_file: TBitBtn;
    btn_find_csv_file: TBitBtn;
    cboxOutDir: TComboBox;
    ckbox1stRowIsTitles: TCheckBox;
    cboxCSVFile: TComboBox;
    DirBuilderPropIni: TIniPropStorage;
    edCellContent: TEdit;
    lblCellContents: TLabel;
    lblOutDir: TLabel;
    lblCSVFile: TLabel;
    mainMnu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
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
    StatusBar: TStatusBar;
    statBar_first_line: TStatusBar;
    tabshCSVFile: TTabSheet;
    tabshCSVParserProps: TTabSheet;
    ActionReadCSV: TAction;
    Grid: TStringGrid;
    pnlBottom: TPanel;
    gpBoxCSVParserProperties: TGroupBox;
    SGridParserProps: TStringGrid;
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
    procedure ActionIgnoreFirstLineExecute(Sender: TObject);
    procedure ActionMkDirsExecute(Sender: TObject);
    procedure ActionReadCSVExecute(Sender: TObject);
    procedure ActionReadRawFileExecute(Sender: TObject);
    procedure ActionResizeColumnsExecute(Sender: TObject);
    procedure ActionSetTitlesExecute(Sender: TObject);
    procedure ActionWriteGridToBooksDbExecute(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure cboxCSVFileCloseUp(Sender: TObject);
    procedure cboxCSVFileEnter(Sender: TObject);
    procedure ckboxShowLineNumbersChange(Sender: TObject);
    procedure dbgridCSVCellClick(Column: TColumn);
    procedure dbgridCSVTitleClick(Column: TColumn);
    procedure edCSVFileChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure cboxCSVFileChange(Sender: TObject);
    procedure GridHeaderClick(Sender: TObject; IsColumn: boolean; Index: integer);
    procedure GridDblClick(Sender: TObject);
    procedure GridClick(Sender: TObject);
    procedure ActionChangeCSVDelimiterExecute(Sender: TObject);
    procedure GridSelection(Sender: TObject; aCol, aRow: integer);
  private
    checkFlag: boolean;
    FDirListColumn: integer;
    FCSVDelimiter: string;
    Fdmod: TDirBuilder_dataModule;
    FBooksDbDlg: TfmNewBooksDb;
    FDelimiter: char;
    FCurReportType: TCaption;

    function countSubDirs(path: string): integer;
    procedure GetCSVParserProps;
    function isGridPopulated: boolean;
    procedure move_grid_first_line;
  public
    property CSVDelimiter: string read FCSVDelimiter write FCSVDelimiter;
    property dmod: TDirBuilder_dataModule read Fdmod write Fdmod;
  end;

var
  frmFayesDirBuilder: TfrmFayesDirBuilder;

implementation

{$R *.lfm}

{ TfrmFayesDirBuilder }

procedure TfrmFayesDirBuilder.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmFayesDirBuilder.cboxCSVFileCloseUp(Sender: TObject);
var
  idx: integer;
  txt: string;
  pt: TPoint;
begin
  pt := Mouse.CursorPos;
  txt := cboxCSVFile.Text;
  for idx := 0 to cboxCSVFile.Items.Count - 1 do
    if txt = cboxCSVFile.Items[idx] then
      Exit;
  cboxCSVFile.Items.Add(txt);
  if FCurReportType <> cboxCSVFile.Text then
  begin
    ActionIgnoreFirstLine.Checked := False;
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
var
  g_path: TFileName;
begin
  ckboxShowLineNumbersChange(self);
  if DirectoryExists(cboxOutDir.Text) then
    cboxOutDir.Items.Add(cboxOutDir.Text);
  Fdmod := TDirBuilder_dataModule.Create(self);
  Grid.Clear;
  GetCSVParserProps;
  pgCtrl.ActivePage := tabshCSVFile;

  //g_path := ExtractFilePath(Application.ExeName);
  //ShowMessage(g_path);
  DirBuilderPropIni.Restore;
  FBooksDbDlg := nil;
end;

procedure TfrmFayesDirBuilder.GetCSVParserProps;
var
  Parser_setup: TfmCSVParser_setup;
begin
  SGridParserProps.Cells[0, 0] := 'Delimiter';
  SGridParserProps.Cells[0, 1] := 'LineEnding';
  SGridParserProps.ColWidths[0] := 100;
  SGridParserProps.ColWidths[1] := 190;
  Parser_setup := TfmCSVParser_setup.Create(self);
  try
    SGridParserProps.Cells[1, 0] := Parser_setup.Delimiter;
    SGridParserProps.Cells[1, 1] := Parser_setup.Lineending;
  finally
    Parser_setup.Free;
  end;
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
  Grid.Columns[FDirListColumn].Width := 50;
  colTitle := Grid.Columns[FDirListColumn].Title.Caption;
  ActionMkDirs.Enabled := True;
  msg := Format(MSG_TEXT, [colTitle]);
  StatusBar.Panels[1].Text := msg;
  ShowMessage(msg);
end;

procedure TfrmFayesDirBuilder.ActionResizeColumnsExecute(Sender: TObject);
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
  try
    util.WriteGridToBooksDb(dmod, Grid, outCnt, FDelimiter);
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

procedure TfrmFayesDirBuilder.GridHeaderClick(Sender: TObject;
  IsColumn: boolean; Index: integer);
var
  msg, colTitle: string;
begin
  if not IsColumn then
    Exit;

  FDirListColumn := Index;
  colTitle := Grid.Cells[Index, 0];
  ActionMkDirs.Enabled := True;
  FmtStr(msg, 'You are staged to write directories named like the "%s" column.',
    [colTitle]);
  StatusBar.Panels[1].Text := msg;
  ShowMessage(msg);
end;


procedure TfrmFayesDirBuilder.GridDblClick(Sender: TObject);
begin
  Clipboard.AsText := Grid.Cells[Grid.Col, Grid.Row];
end;

procedure TfrmFayesDirBuilder.GridClick(Sender: TObject);
const
  lblS = 'Cell content (%d)';
var
  Str: string;
begin
  str := Grid.Cells[Grid.Col, Grid.Row];
  edCellContent.Text := str;
  lblCellContents.Caption := Format(lblS, [Length(str)]);
end;


procedure TfrmFayesDirBuilder.ActionChangeCSVDelimiterExecute(Sender: TObject);
(*
    procedure TfrmFayesDirBuilder.ActionChangeCSVDelimiterExecute(Sender: TObject);
    this may not be used.
*)
var
  dlg: TfmChangeCSVProperties;
  curDelimiter: char;
begin
  curDelimiter := FDelimiter;
  dlg := TfmChangeCSVProperties.Create(self);
  try
    dlg.CurrentChoice := FDelimiter;
    dlg.ShowModal;
    case dlg.DelimiterChoice of
      ChoseNoChoice: Exit;
      ChoseComma:
      begin
        SGridParserProps.Cells[1, 0] := COMMA_STRING;
        FDelimiter := COMMA;
      end;
      ChoseTab:
      begin
        SGridParserProps.Cells[1, 0] := TAB_STRING;
        FDelimiter := TAB;
      end;
      ChoseSemicolon:
      begin
        SGridParserProps.Cells[1, 0] := SEMICOLON_STRING;
        FDelimiter := SEMICOLON;
      end;
    end;
    StatusBar.Panels[1].Text := Format('Current delimiter: %s', [FDelimiter]);
    if curDelimiter <> FDelimiter then
      ActionReadCSV.Execute;
  finally
    dlg.Free;
  end;
end;

procedure TfrmFayesDirBuilder.GridSelection(Sender: TObject; aCol, aRow: integer);
begin
  Statusbar.Panels[0].Text := Format('Position (%d, %d)', [aRow, aCol]);
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
var
  iCol, iRow: integer;
begin
  Result := Grid.ColCount > 3;
end;

procedure TfrmFayesDirBuilder.move_grid_first_line;
var
  iCol, iRow: integer;
  first_line: string;
begin
  first_line := EmptyStr;
  for iCol := 0 to Grid.ColCount - 1 do
    first_line := Concat(first_line, '[', Grid.Cells[iCol, 0], ']  ');
  statBar_first_line.SimpleText := 'Ignored line: ' + first_line;
  for iRow := 1 to Grid.RowCount - 1 do
    for iCol := 0 to Grid.ColCount - 1 do
      Grid.Cells[iCol, iRow - 1] := Grid.Cells[iCol, iRow];
  Grid.RowCount := Grid.RowCount - 1;
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

procedure TfrmFayesDirBuilder.ActionMkDirsExecute(Sender: TObject);
var
  s, outPath, outDir: string;
  bookMark: integer;
  beginCnt, endCnt: integer;
  rowIdx, colIdx: integer;
begin
  outPath := cboxOutDir.Text;
  if not DirectoryExists(outPath) then
  begin
    ShowMessage(outpath + ' does not exist as a directory.');
    Exit;
  end;
  beginCnt := countSubDirs(outPath);
  outPath := IncludeTrailingPathDelimiter(outPath);
  bookMark := Grid.Row;
  Grid.BeginUpdate;
  try

    rowIdx := 1;
    while rowIdx < Grid.RowCount do
    begin
      s := Grid.Cells[FDirListColumn, rowIdx];  // the directory name
      s := StringReplace(s, '/', '-', [rfReplaceAll]);
      s := StringReplace(s, '\\', '-', [rfReplaceAll]);
      s := StringReplace(s, ':', ';', [rfReplaceAll]);

      outDir := ConcatPaths([outPath, s]);
      if not DirectoryExists(outDir) then
        CreateDir(outDir);
      Inc(rowIdx);
    end;
  finally
    Grid.EndUpdate();
    Grid.Row := bookMark;
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
  fileName, curDir: string;
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

          fileName := subDirsSrchRec.Name;
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
  dir, fname: string;
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
  delimiter: char;
  parmRec: TParmRec;
begin
  fileName := cboxCSVFile.Text;
  if not FileExists(fileName) then
  begin
    ShowMessage(fileName + ' does not exist. Try again');
    Exit;
  end;

  try
    try
      begin
        Grid.BeginUpdate;
        case FDelimiter of
          SEMICOLON: delimiter := SEMICOLON;
          COMMA: delimiter := COMMA;
          TAB: delimiter := TAB;
          else
            delimiter := COMMA;
        end;

        parmRec.delimiter := delimiter;
        parmRec.ignoreFirstLine := ActionIgnoreFirstLine.Checked;
        parmRec.addRows := True;
        parmRec.withHeader := True;

        LoadGridFromCSVFile(Grid, fileName, parmRec, first_line);
        if first_line > EmptyStr then
        begin
          ActionIgnoreFirstLine.Checked := True;
          statBar_first_line.Visible := True;
          statBar_first_line.SimpleText :=
            Format('Ignored first line: %s', [first_line]);
        end
        else
        begin
          ActionIgnoreFirstLine.Checked := False;
          statBar_first_line.Visible := False;
        end;

        if Grid.RowCount = Grid.FixedRows then
        begin
          ShowMessage('TfrmFayesDirBuilder.ActionReadCSVExecute: ' +
            sLineBreak + 'StringGrid is not populated?');
          Exit;
        end
        else
          ActionResizeColumns.Execute;
      end;
    finally
      Grid.EndUpdate();
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
  txt: TFileName;
begin
  txt := cboxCSVFile.Text;
  frm := TfmDisplayCSVFile.Create(self, cboxCSVFile.Text);
  frm.DisplayFile;
  frm.Show;
end;

end.
