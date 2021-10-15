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
  ActnList, Classes, ComCtrls, csvdataset, DB, DBCtrls, DBGrids, ExtCtrls,
  Menus, StdCtrls, SysUtils, Forms, Controls, Graphics, Dialogs, Clipbrd,
  ComboEx, Grids, IniPropStorage, DirBuilder_dmod, stringGridHelper,
  frmDisplayCSVFile, StringGridUtil, CSVParser_setup, dmodCSVParser,
  frmChangeCSVProperties, frmBooksDb, RTTICtrls;

type
  EMyDBNotOpenException = class(Exception);

  { TfrmFayesDirBuilder }

  TfrmFayesDirBuilder = class(TForm)
    ActionBooksDb: TAction;
    ActionReadRawFile: TAction;
    ActionSetTitles: TAction;
    ActionFindOutputDir : TAction;
    ActionMkDirs : TAction;
    ActionFindCSV : TAction;
    ActionOpen : TAction;
    ActionResizeColumns : TAction;
    ActionClose : TAction;
    ActionList : TActionList;
    btnOutDir : TButton;
    btnFindCSV : TButton;
    btnResizeTableColumns : TButton;
    btnReadRawFile: TButton;
    cboxOutDir : TComboBox;
    ckbox1stRowIsTitles: TCheckBox;
    cboxCSVFile: TComboBox;
    DirBuilderPropIni: TIniPropStorage;
    lblCellContents : TLabel;
    lblOutDir : TLabel;
    lblCSVFile : TLabel;
    MainMenu1 : TMainMenu;
    MenuItem1 : TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem15: TMenuItem;
    N7: TMenuItem;
    N6: TMenuItem;
    N5: TMenuItem;
    N4: TMenuItem;
    N3: TMenuItem;
    MenuItem2 : TMenuItem;
    MenuItem3 : TMenuItem;
    MenuItem4 : TMenuItem;
    MenuItem5 : TMenuItem;
    MenuItem6 : TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    N2 : TMenuItem;
    Operations : TMenuItem;
    N1 : TMenuItem;
    MenuItemFile : TMenuItem;
    pgCtrl : TPageControl;
    pnlTop : TPanel;
    popupMnu: TPopupMenu;
    StatusBar: TStatusBar;
    tabshCSVFile : TTabSheet;
    tabshCSVParserProps : TTabSheet;
    ActionReadCSV: TAction;
    btnReadCSV: TButton;
    sGridMain: TStringGrid;
    pnlBottom: TPanel;
    btnMkDirs: TButton;
    btnClose: TButton;
    edCellContent: TEdit;
    gpBoxCSVParserProperties: TGroupBox;
    SGridParserProps: TStringGrid;
    ActionChangeCSVDelimiter: TAction;
    popupMnu1: TPopupMenu;
    N9: TMenuItem;
    MenuItem13: TMenuItem;
    ActionAddToDB: TAction;
    MenuItem14: TMenuItem;
    procedure ActionBooksDbExecute(Sender: TObject);
    procedure ActionCloseExecute(Sender : TObject);
    procedure ActionFindCSVExecute(Sender : TObject);
    procedure ActionFindOutputDirExecute(Sender : TObject);
    procedure ActionMkDirsExecute(Sender : TObject);
    procedure ActionReadCSVExecute(Sender : TObject);
    procedure ActionReadRawFileExecute(Sender: TObject);
    procedure ActionResizeColumnsExecute(Sender : TObject);
    procedure ActionSetTitlesExecute(Sender: TObject);
    procedure btnCloseClick(Sender : TObject);
    procedure cboxCSVFileCloseUp(Sender: TObject);
    procedure ckboxShowLineNumbersChange(Sender : TObject);
    procedure dbgridCSVCellClick(Column : TColumn);
    procedure dbgridCSVTitleClick(Column: TColumn);
    procedure edCSVFileChange(Sender : TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender : TObject);
    procedure MenuItem2Click(Sender : TObject);
    procedure cboxCSVFileChange(Sender: TObject);
    procedure sGridMainHeaderClick(Sender: TObject; IsColumn: Boolean;
    			Index: Integer);
    procedure sGridMainDblClick(Sender: TObject);
    procedure sGridMainClick(Sender: TObject);
    procedure ActionChangeCSVDelimiterExecute(Sender: TObject);
  private
    checkFlag : Boolean;
    FDirListColumn : Integer;
    FCSVDelimiter : String;
    //dmod : TDirBuilder_dataModule;
    FBooksDbDlg : TfmBooksDb;
    function countSubDirs(path: String): Integer;
		procedure GetCSVParserProps;
  public
    property CSVDelimiter : String read FCSVDelimiter write FCSVDelimiter;
  end;

var
  frmFayesDirBuilder : TfrmFayesDirBuilder;

implementation

{$R *.lfm}

{ TfrmFayesDirBuilder }

procedure TfrmFayesDirBuilder.btnCloseClick(Sender : TObject);
begin
  Close;
end;

procedure TfrmFayesDirBuilder.cboxCSVFileCloseUp(Sender: TObject);
var
  idx : Integer;
  txt : String;
begin
  txt := cboxCSVFile.Text;
  for idx := 0 to cboxCSVFile.Items.Count -1 do
    if txt = cboxCSVFile.Items[idx] then
      Exit;
  cboxCSVFile.Items.Add(txt);
end;


procedure TfrmFayesDirBuilder.ckboxShowLineNumbersChange(Sender : TObject);
begin
     //if cboxShowLineNumbers.Checked then
     //   sgridCSV.FixedCols := 1
     //else
     //   sgridCSV.FixedCols := 0 ;
end;

procedure TfrmFayesDirBuilder.FormCreate(Sender : TObject);
var
  g_path : TFileName;
begin
  ckboxShowLineNumbersChange(self);
  if DirectoryExists(cboxOutDir.Text) then
     cboxOutDir.Items.Add(cboxOutDir.Text);
  //dmod := DirBuilder_dataModule;
  sGridMain.Clear;
  GetCSVParserProps;
  pgCtrl.ActivePage := tabshCSVFile;
  //g_path := ExtractFilePath(Application.ExeName);
  //ShowMessage(g_path);
  DirBuilderPropIni.Restore;
  FBooksDbDlg := nil;
end;

procedure TfrmFayesDirBuilder.GetCSVParserProps;
var
  Parser_setup : TfmCSVParser_setup;
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

procedure TfrmFayesDirBuilder.edCSVFileChange(Sender : TObject);
begin
  ActionReadCSV.Enabled := FileExists(cboxCSVFile.Text);
end;

procedure TfrmFayesDirBuilder.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  DirBuilderPropIni.Save;
end;

procedure TfrmFayesDirBuilder.dbgridCSVCellClick(Column : TColumn);
begin
  edCellContent.Text := Column.FieldName;
end;

procedure TfrmFayesDirBuilder.dbgridCSVTitleClick(Column: TColumn);
var
  msg, colTitle : String;
begin
  FDirListColumn := Column.Index;
  sGridMain.Columns[FDirListColumn].Width:=50;
  colTitle := sGridMain.Columns[FDirListColumn].Title.Caption;
  ActionMkDirs.Enabled := True;
  FmtStr (msg,'You are staged to write directories named like the "%s" column.',[colTitle]);
  StatusBar.SimpleText := msg;
  ShowMessage(msg);
end;

procedure TfrmFayesDirBuilder.ActionResizeColumnsExecute(Sender : TObject);
var
  grHelper : TGridHelper;
  S : String;
begin
  if not sGridMain.Visible then
    Exit  ;
  sGridMain.Update;
	grHelper := TGridHelper.Create;
  try
    grHelper.maxSize := 250;
    grHelper.SGrid := sGridMain;
    //S := Format('[row count: %d   column count: %d]', [grHelper.SGrid.RowCount,
    //                                                   grHelper.SGrid.ColCount]);
    //Showmessage(S);
    grHelper.SetStringGridColumnWidths;
    //grHelper.AutoSizeColumns();
    //grHelper.SetDBGridColumnWidths;
  finally
    grHelper.Free;
    btnResizeTableColumns.Enabled := False;
  end;
end;

procedure TfrmFayesDirBuilder.ActionSetTitlesExecute(Sender: TObject);
begin
  //if ckbox1stRowIsTitles.Checked then
  //begin
  //  dmod.setFieldNames;
  //  dbgridCSV.Options := dbgridCSV.Options + [dgTitles];
  //  //dbgridCSV.DataSource.DataSet.Close;
  //  //dbgridCSV.DataSource.DataSet.Open;
  //end
  //else
  //   dbgridCSV.Options := dbgridCSV.Options - [dgTitles]
end;

procedure TfrmFayesDirBuilder.ActionCloseExecute(Sender : TObject);
begin
  Close;
end;

procedure TfrmFayesDirBuilder.ActionBooksDbExecute(Sender: TObject);
begin
  if FBooksDbDlg = nil then
    FBooksDbDlg := TfmBooksDb.Create(self);
  FBooksDbDlg.Show;
end;

procedure TfrmFayesDirBuilder.MenuItem2Click(Sender : TObject);
begin

end;

procedure TfrmFayesDirBuilder.cboxCSVFileChange(Sender: TObject);
begin
  //if dmod.CSVDataset.Active then
  //  ckbox1stRowIsTitles.Checked:=False;
end;

procedure TfrmFayesDirBuilder.sGridMainHeaderClick(Sender: TObject;
			IsColumn: Boolean; Index: Integer);
var
  msg, colTitle : String;
begin
  if not IsColumn then
    Exit;

  FDirListColumn := Index;
  colTitle := sGridMain.Cells[Index, 0];
  ActionMkDirs.Enabled := True;
  FmtStr (msg,'You are staged to write directories named like the "%s" column.',[colTitle]);
  StatusBar.SimpleText := msg;
  ShowMessage(msg);
end;


procedure TfrmFayesDirBuilder.sGridMainDblClick(Sender: TObject);
begin
  Clipboard.AsText := sGridMain.Cells[sGridMain.Col, sGridMain.Row];
end;

procedure TfrmFayesDirBuilder.sGridMainClick(Sender: TObject);
begin
  edCellContent.Text := sGridMain.Cells[sGridMain.Col, sGridMain.Row];
end;


procedure TfrmFayesDirBuilder.ActionChangeCSVDelimiterExecute(Sender: TObject);
var
  dlg : TfmChangeCSVProperties;
begin
  dlg := TfmChangeCSVProperties.Create(self);
  try
    dlg.ShowModal;
    case dlg.DelimiterChoice of
      ChoseNoChoice : Exit;
      ChoseComma :
        begin
          SGridParserProps.Cells[1, 0] := COMMA_STRING;
          FDelimiter := COMMA;
				end;
      ChoseTab :
        begin
          SGridParserProps.Cells[1, 0] := TAB_STRING;
          FDelimiter:= TAB;
				end;
      ChoseSemicolon :
        begin
          SGridParserProps.Cells[1, 0] := SEMICOLON_STRING;
          FDelimiter:= SEMICOLON;
				end;
		end;
	finally
    dlg.Free;
	end;
end;

procedure TfrmFayesDirBuilder.ActionFindOutputDirExecute(Sender : TObject);
var
  dirDlg : TSelectDirectoryDialog;
  outDir : String;
  idx : Integer;
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

procedure TfrmFayesDirBuilder.ActionMkDirsExecute(Sender : TObject);
var
  s, outPath, outDir : String;
  bookMark : Integer;
  beginCnt, endCnt : Integer;
  rowIdx, colIdx : Integer;
begin
  outPath := cboxOutDir.Text;
  if not DirectoryExists(outPath) then
  begin
    ShowMessage(outpath + ' does not exist as a directory.');
    Exit;
  end;
  beginCnt := countSubDirs(outPath);
  outPath := IncludeTrailingPathDelimiter(outPath);
  bookMark := sGridMain.Row;
  sGridMain.BeginUpdate;
  try

    rowIdx := 1;
    while rowIdx < sGridMain.RowCount do
    begin
      s := sGridMain.Cells[FDirListColumn, rowIdx];  // the directory name
      s := StringReplace(s, '/', '-', [rfReplaceAll]);
      s := StringReplace(s, '\\', '-', [rfReplaceAll]);
      s := StringReplace(s, ':', ';', [rfReplaceAll]);

      outDir := ConcatPaths([outPath, s]);
      if not DirectoryExists(outDir) then
         CreateDir(outDir);
      Inc(rowIdx);
    end;
	finally
    sGridMain.EndUpdate();
    sGridMain.Row := bookMark;
	end;

  pgCtrl.ActivePage := tabshCSVFile;
  endCnt := countSubDirs(outPath);
  if endCnt = beginCnt then
     StatusBar.SimpleText := Format('No subdirectories were added to %s', [outPath])
  else
     StatusBar.SimpleText := Format('There were %d subdirectories added to %s',
                                             [endCnt - beginCnt, outPath])
end;

function TfrmFayesDirBuilder.countSubDirs(path : String) : Integer;
var
  subDirsSrchRec : TSearchRec;
  cnt : Longint;
  fileName, curDir : String;
  dirName : AnsiString;
Begin
  cnt := 0;
  curDir := GetCurrentDir;
  try
    begin
      SetCurrentDir(path);

      If FindFirst ('*',faDirectory, subDirsSrchRec)=0 then
      begin
        Repeat

            If (subDirsSrchRec.Attr and faDirectory) = faDirectory then
            begin
              dirName := subDirsSrchRec.Name;
              try
                if (dirName = AnsiString('.')) or (dirName = AnsiString('..')) then
                  Continue;

              except
                on e : Exception do
                  ShowMessage(e.Message);
              end;
              Inc(cnt);
            end;

            fileName := subDirsSrchRec.Name;
        Until
          FindNext(subDirsSrchRec)<>0;
        FindClose(subDirsSrchRec);
       end;

      Result := cnt;
    end;
  finally
    SetCurrentDir(curDir);
  end;
End  ;

procedure TfrmFayesDirBuilder.ActionFindCSVExecute(Sender : TObject);
var
  dlg : TOpenDialog;
  dir, fname : String;
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

procedure TfrmFayesDirBuilder.ActionReadCSVExecute(Sender : TObject);
var
  fileName : String;
  delimiter :Char;
begin
  fileName := cboxCSVFile.Text;
  if not FileExists(fileName) then
  begin
    ShowMessage(fileName + ' does not exist. Try again');
    Exit;
  end;
  try
  begin
    case FDelimiter of
      SEMICOLON : delimiter := SEMICOLON;
      COMMA : delimiter := COMMA;
      TAB: delimiter := TAB;
      ELSE
        delimiter := COMMA;
		end;
    LoadGridFromCSVFile(sGridMain, fileName, delimiter);
    if sGridMain.RowCount = sGridMain.FixedRows then
    begin
      ShowMessage('TfrmFayesDirBuilder.ActionReadCSVExecute: '
              +sLineBreak +'StringGrid is not populated?');
      Exit;
		end
    else
      //ShowMessage('string grid line count is ' +IntToStr(sGridMain.RowCount));
		//dmod.FileName := fileName;
    //dmod.open_DataSet(fileName, ckbox1stRowIsTitles.Checked);
    //if not dmod.CSVDataset.Active then
    //  raise EMyDBNotOpenException.Create('ActionReadCSVExecute(0): Unknown reason.');
    //btnResizeTableColumns.Enabled := True;
    //if not dmod.CSVDataset.Active then
    //  raise EMyDBNotOpenException.Create('ActionReadCSVExecute(1): Unknown reason.');
    //if not dbgridCSV.DataSource.DataSet.Active then
    //  raise EMyDBNotOpenException.Create('ActionReadCSVExecute(2): Unknown reason.');
    ActionResizeColumns.Execute;
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
  frm : TfmDisplayCSVFile;
  txt : TFileName;
begin
  txt := cboxCSVFile.Text;
  frm := TfmDisplayCSVFile.Create(self, cboxCSVFile.Text);
  frm.DisplayFile;
  frm.Show;
end;

end.

