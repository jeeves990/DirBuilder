unit frmDirFromCSV;

{$mode objfpc}{$H+}

interface

uses
  ActnList, Classes, ComCtrls, csvdataset, DB, DBCtrls, DBGrids, ExtCtrls,
  Menus, StdCtrls, SysUtils, Forms, Controls, Graphics, Dialogs, Clipbrd,
  DirBuilder_dmod,
  RTTICtrls;

type

  { TfrmFayesDirBuilder }

  TfrmFayesDirBuilder = class(TForm)
    ActionSetTitles: TAction;
    ActionFindOutputDir : TAction;
    ActionMkDirs : TAction;
    ActionFindCSV : TAction;
    ActionReadCSV : TAction;
    ActionOpen : TAction;
    ActionResizeColumns : TAction;
    ActionClose : TAction;
    ActionList : TActionList;
    btnOutDir : TButton;
    btnReadCSV : TButton;
    btnMkDirs : TButton;
    btnClose : TButton;
    btnFindCSV : TButton;
    Button1 : TButton;
    cboxOutDir : TComboBox;
    cboxShowLineNumbers : TCheckBox;
    ckboxColResize: TCheckBox;
    ckbox1stRowIsTitles: TCheckBox;
    dbedCellContent : TDBEdit;
    dbgridCSV : TDBGrid;
    edOutDir : TEdit;
    edCSVFile : TEdit;
    lblCellContents : TLabel;
    lblOutDir : TLabel;
    lblCSVFile : TLabel;
    MainMenu1 : TMainMenu;
    MenuItem1 : TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
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
    procedure ActionCloseExecute(Sender : TObject);
    procedure ActionFindCSVExecute(Sender : TObject);
    procedure ActionFindOutputDirExecute(Sender : TObject);
    procedure ActionMkDirsExecute(Sender : TObject);
    procedure ActionReadCSVExecute(Sender : TObject);
    procedure ActionResizeColumnsExecute(Sender : TObject);
    procedure ActionSetTitlesExecute(Sender: TObject);
    procedure btnCloseClick(Sender : TObject);
    procedure ckboxColResizeMouseEnter(Sender: TObject);
    procedure ckboxColResizeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ckboxShowLineNumbersChange(Sender : TObject);
    procedure dbedCellContentMouseDown(Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer);
    procedure dbgridCSVCellClick(Column : TColumn);
    procedure dbgridCSVTitleClick(Column: TColumn);
    procedure edCSVFileChange(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure AutoResizeDbGridColumns;
    procedure MenuItem2Click(Sender : TObject);
  private
    checkFlag : Boolean;
    dirListColumn : Integer;
    dmod : TDirBuilder_dataModule;
    function countSubDirs(path: String): Integer;
    procedure FixDBGridColumnsWidth(const DBGrid: TDBGrid);
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

procedure TfrmFayesDirBuilder.ckboxColResizeMouseEnter(Sender: TObject);
begin
  checkFlag := ckboxColResize.Checked;
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
  dmod := DirBuilder_dataModule.Create(self);

end;

procedure TfrmFayesDirBuilder.edCSVFileChange(Sender : TObject);
begin
  ActionReadCSV.Enabled := FileExists(edCSVFile.Text);
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

procedure TfrmFayesDirBuilder.AutoResizeDbGridColumns;
//var
  //grHelper : TDbGridHelper;
begin
  FixDBGridColumnsWidth(dbgridCSV);
  //grHelper := TDbGridHelper.Create;
  //try
  //  grHelper.dbGrid := dbgridCSV;
  //  //grHelper.AutoSizeColumns();
  //  grHelper.SetGridColumnWidths;
  //finally
  //  grHelper.Free;
  //end;
end;

procedure TfrmFayesDirBuilder.ActionResizeColumnsExecute(Sender : TObject);
begin
  AutoResizeDbGridColumns;
end;

procedure TfrmFayesDirBuilder.ActionSetTitlesExecute(Sender: TObject);
begin
  if ckbox1stRowIsTitles.Checked then
     dbgridCSV.Options := dbgridCSV.Options + [dgTitles]
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

procedure TfrmFayesDirBuilder.ckboxColResizeMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if checkFlag then
      dbgridCSV.Options := dbgridCSV.Options - [dgColumnResize]
    else
      dbgridCSV.Options := dbgridCSV.Options + [dgColumnResize];
    ckboxColResize.Checked := not checkFlag;
end;

procedure TfrmFayesDirBuilder.ActionFindCSVExecute(Sender : TObject);
var
  dlg : TOpenDialog;
  dir : String;
begin
  ActionReadCSV.Enabled := False;
  dlg := TOpenDialog.Create(nil);
  try
     dlg.Options := [ofReadOnly];
     dir := edCSVFile.Text;
     if DirectoryExists(dir) then
        dlg.InitialDir := dir
     else
        dlg.InitialDir := 'c:\';
     if not dlg.Execute then
        Exit;
     edCSVFile.Text := dlg.FileName;
  finally
    dlg.Free;
  end;
end;

procedure TfrmFayesDirBuilder.ActionReadCSVExecute(Sender : TObject);
var
  fileName : String;
begin
  fileName := edCSVFile.Text;
  if not FileExists(fileName) then
  begin
    ShowMessage(fileName + ' does not exist. Try again');
    Exit;
  end;
  try
  begin
    dmod.FileName := fileName;
    dmod.open_dSet(fileName);
  end;
  except on e : Exception do
         ShowMessage(Format('CSV did not open. %s', [e.Message]));
  end;
end;

procedure TfrmFayesDirBuilder.FixDBGridColumnsWidth(const DBGrid: TDBGrid);
const
  IndicatorWidth = 10;
var
  i, TotWidth, VarWidth : integer;
  ResizableColumnCount : integer;
  AColumn : TColumn;
begin//total width of all columns before resize
  TotWidth := 0;
  //how to divide any extra space in the grid
  VarWidth := 0;
  //how many columns need to be auto-resized
  ResizableColumnCount := 0;
  for i := 0 to -1 + DBGrid.Columns.Count do
  begin
     TotWidth := TotWidth + DBGrid.Columns[i].Width;
     if DBGrid.Columns[i].Field.Tag = 0 then
        Inc(ResizableColumnCount);
  end;

  //add 1px for the column separator lineif dgColLines in DBGrid.Options then
  TotWidth := TotWidth + DBGrid.Columns.Count;

  //add indicator column widthif dgIndicator in DBGrid.Options then
  TotWidth := TotWidth + IndicatorWidth;

  //width vale "left"
  VarWidth := DBGrid.ClientWidth - TotWidth;

  //Equally distribute VarWidth
  //to all auto-resizable columnsif ResizableColumnCount > 0 then
  VarWidth := varWidth div ResizableColumnCount;
  for i := 0 to -1 + DBGrid.Columns.Count do
  begin
    AColumn := DBGrid.Columns[i];
    if AColumn.Field.Tag = 0 then
    begin
       AColumn.Width := AColumn.Width + VarWidth;
       if AColumn.Width <= AColumn.Field.Tag then
          AColumn.Width := AColumn.Field.Tag;
    end;
  end;
end; (*FixDBGridColumnsWidth*)

end.

