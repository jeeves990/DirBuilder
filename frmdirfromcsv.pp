unit frmDirFromCSV;

{$mode objfpc}{$H+}

interface

uses
  ActnList, Classes, ComCtrls, csvdataset, DB, DBCtrls, DBGrids, ExtCtrls,
	Menus, StdCtrls, SysUtils, Forms, Controls, Graphics, Dialogs, Clipbrd,
	ComboEx, DirBuilder_dmod, dbgridhelper, frmDisplayCSVFile, RTTICtrls;

type

  { TfrmFayesDirBuilder }

  TfrmFayesDirBuilder = class(TForm)
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
    btnMkDirs : TButton;
    btnClose : TButton;
    btnFindCSV : TButton;
    btnResizeTableColumns : TButton;
    btnReadRawFile: TButton;
    cboxOutDir : TComboBox;
    ckbox1stRowIsTitles: TCheckBox;
    cboxCSVFile: TComboBox;
    dbedCellContent : TDBEdit;
    dbgridCSV: TDBGrid;
    lblCellContents : TLabel;
    lblOutDir : TLabel;
    lblCSVFile : TLabel;
    MainMenu1 : TMainMenu;
    MenuItem1 : TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
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
    TabSheet2 : TTabSheet;
		ActionReadCSV: TAction;
		btnReadCSV: TButton;
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
    procedure dbedCellContentMouseDown(Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer);
    procedure dbgridCSVCellClick(Column : TColumn);
    procedure dbgridCSVTitleClick(Column: TColumn);
    procedure edCSVFileChange(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure MenuItem2Click(Sender : TObject);
  private
    checkFlag : Boolean;
    dirListColumn : Integer;
    dmod : TDirBuilder_dataModule;
    function countSubDirs(path: String): Integer;
  public

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
begin
  ckboxShowLineNumbersChange(self);
  if DirectoryExists(cboxOutDir.Text) then
     cboxOutDir.Items.Add(cboxOutDir.Text);
  dmod := DirBuilder_dataModule;

end;

procedure TfrmFayesDirBuilder.edCSVFileChange(Sender : TObject);
begin
  ActionReadCSV.Enabled := FileExists(cboxCSVFile.Text);
end;

procedure TfrmFayesDirBuilder.dbgridCSVCellClick(Column : TColumn);
begin
  dbedCellContent.DataField := Column.FieldName;
end;

procedure TfrmFayesDirBuilder.dbgridCSVTitleClick(Column: TColumn);
var
  msg, colTitle : String;
begin
  dirListColumn := Column.Index;
  dbgridCSV.Columns[dirListColumn].Width:=50;
  colTitle := dbgridCSV.Columns[dirListColumn].Title.Caption;
  ActionMkDirs.Enabled := True;
  FmtStr (msg,'You are staged to write directories named like the "%s" column.',[colTitle]);
  StatusBar.SimpleText := msg;
  ShowMessage(msg);
end;

procedure TfrmFayesDirBuilder.dbedCellContentMouseDown(Sender : TObject;
  Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
  if Button = mbRight then
  begin
    Clipboard.AsText := dbedCellContent.Text;
  end;
end;

procedure TfrmFayesDirBuilder.ActionResizeColumnsExecute(Sender : TObject);
var
  grHelper : TDbGridHelper;
begin
  grHelper := TDbGridHelper.Create;
  try
    grHelper.maxSize := 250;
    grHelper.dbGrid := dbgridCSV;
    //grHelper.AutoSizeColumns();
    grHelper.SetGridColumnWidths;
  finally
    grHelper.Free;
    btnResizeTableColumns.Enabled := False;
  end;
end;

procedure TfrmFayesDirBuilder.ActionSetTitlesExecute(Sender: TObject);
begin
  if ckbox1stRowIsTitles.Checked then
  begin
    dmod.setFieldNames;
    dbgridCSV.Options := dbgridCSV.Options + [dgTitles];
    //dbgridCSV.DataSource.DataSet.Close;
    //dbgridCSV.DataSource.DataSet.Open;
  end
  else
     dbgridCSV.Options := dbgridCSV.Options - [dgTitles]
end;

procedure TfrmFayesDirBuilder.ActionCloseExecute(Sender : TObject);
begin
  Close;
end;

procedure TfrmFayesDirBuilder.MenuItem2Click(Sender : TObject);
begin

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
  dset : TDataSet;
  s, outPath, outDir : String;
  bookMark : TBookMark;
  beganCnt, endCnt : Integer;
begin
  dset := dbgridCSV.DataSource.DataSet;
  outPath := cboxOutDir.Text;
  if not DirectoryExists(outPath) then
  begin
    ShowMessage(outpath + ' does not exist as a directory.');
    Exit;
  end;
  beganCnt := countSubDirs(outPath);
  outPath := IncludeTrailingPathDelimiter(outPath);
  dset.DisableControls;
  try
    bookMark := dset.Bookmark;
    dset.First;
    while not dset.EOF do
    begin

      s := dset.Fields[dirListColumn].Text;  // the directory name
      s := StringReplace(s, '/', '-', [rfReplaceAll]);
      s := StringReplace(s, '\\', '-', [rfReplaceAll]);
      s := StringReplace(s, ':', ';', [rfReplaceAll]);
      outDir := ConcatPaths([outPath, s]);
      if not DirectoryExists(outDir) then
         CreateDir(outDir);
      dset.Next;
    end;
    dset.GotoBookmark(bookMark);
    pgCtrl.ActivePage := tabshCSVFile;
    endCnt := countSubDirs(outPath);
    if endCnt = beganCnt then
       StatusBar.SimpleText := Format('No subdirectories were added to %s', [outPath])
    else
       StatusBar.SimpleText := Format('There were %d subdirectories added to %s',
                                             [endCnt - beganCnt, outPath])
  finally
    dset.EnableControls;
  end;
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
begin
  fileName := cboxCSVFile.Text;
  if not FileExists(fileName) then
  begin
    ShowMessage(fileName + ' does not exist. Try again');
    Exit;
  end;
  try
  begin
    dmod.FileName := fileName;
    dmod.open_DataSet(fileName);
    btnResizeTableColumns.Enabled:=True;
    ActionResizeColumns.Execute;
  end;
  except on e : Exception do
    ShowMessage(Format('CSV did not open. %s', [e.Message]));
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

