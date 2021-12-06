unit frmDisplayCSVFile;

{TODO's
  1. make edFileName a combobox and save the files read.
  2. size the columns after reading.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ActnList,
  Menus, Grids, ComCtrls, ExtCtrls,
  XMLPropStorage, Buttons,
  frmInputNewValue,
  AddQuotes2Files_unit,
  DirBuilder_dmod, frmShowText, ATSynEdit, ATStrings, ATSynEdit_Carets,
  ATStrings_Undo, ATStringProc, ATSynEdit_LineParts;

type

  { TfmDisplayCSVFile }

  TfmDisplayCSVFile = class(TForm)
    ActionClose: TAction;
    ActList4DspCSV: TActionList;
    ActionReadNewFile: TAction;
    ActionAddQuotes: TAction;
    ActionShowLine4Examination: TAction;
    btn_read_raw_file: TBitBtn;
    ed: TATSynEdit;
    CSVDisplayPopup: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    N1: TMenuItem;
    MenuItem3: TMenuItem;
    ActionChangeMaxLines2Read: TAction;
    N2: TMenuItem;
    MenuItem4: TMenuItem;
    ActionReloadFile: TAction;
    MenuItem5: TMenuItem;
    pgCtrl: TPageControl;
    StatusBar1: TStatusBar;
    tabshRawFile: TTabSheet;
    tbCSV_data: TTabSheet;
    sGrid: TStringGrid;
    Button1: TButton;
    pnlTop: TPanel;
    Label1: TLabel;
    edFileName: TEdit;
    ActionReadCSVData: TAction;
    N3: TMenuItem;
    MenuItem6: TMenuItem;
    CSVPropStorage: TXMLPropStorage;
    ToolBar: TToolBar;
    tb_reload_file: TToolButton;
    tb_read_new_file: TToolButton;
    tb_add_double_quotes: TToolButton;
    tb_examine_line: TToolButton;
    tb_read_as_csv: TToolButton;
    tb_close: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure ActionCloseExecute(Sender: TObject);
    procedure ActionReadNewFileExecute(Sender: TObject);
    procedure ActionAddQuotesExecute(Sender: TObject);
    procedure ActionChangeMaxLines2ReadExecute(Sender: TObject);
    procedure ActionReloadFileExecute(Sender: TObject);
    procedure ActionReadCSVDataExecute(Sender: TObject);
    procedure ActionShowLine4ExaminationExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure edKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure edMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure edMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  private
    FFilename: TFileName;
    FMaxLines2Read: integer;
    FRowDelimiter: ansistring;
    atRowDelimiter: TATLineEnds;
    FShowDlg: TfmShowText;
    FParent: TComponent;
    FHasHeaders: boolean;
    FColDelimiter: ansistring;
    FAutoLineNumber : Integer;
	  FHighLightColor : TColor;

    function FixRow(str: string): string;
		function GetALine : TATStringItem;
    function GetCaretPos: TPoint;
    procedure SetFileName(AValue: TFileName);
		procedure SetLines(AValue : TStringList);
    procedure SetRowDelimiter(AValue: ansistring);
    procedure HighLightLine(Sender: TObject; var AParts: TATLineParts;
        ALineIndex, ACharIndex, ALineLen: integer;
        var AColorAfterEol: TColor);
  public
    property filename: TFileName read FFileName write SetFileName;
    constructor Create(aOwner: TComponent; theFileName: TFileName = ''); reintroduce;
    destructor Destroy; override;
    procedure DisplayFile;
    property MaxLines2Read: integer read FMaxLines2Read write FMaxLines2Read;
    property _col_delimiter: ansistring read FColDelimiter write FColDelimiter;
    property _row_delimiter: ansistring read FRowDelimiter write SetRowDelimiter;
    property HasHeaders: boolean read FHasHeaders write FHasHeaders;
    property LineList : TStringList write SetLines;
    property Get_A_Line : TATStringItem read GetALine;
  var
    GetTextFeedBack : TStringCallbackMethod;
    AmIAlive : TBooleanCallbackMethod;
  end;

var
  fmDisplayCSVFile: TfmDisplayCSVFile;

implementation

{$R *.lfm}

uses unitLoad_grid_from_csv;

{ TfmDisplayCSVFile }

procedure TfmDisplayCSVFile.ActionReadNewFileExecute(Sender: TObject);
var
  dlg: TOpenDialog;
begin
  dlg := TOpenDialog.Create(self);
  try
    dlg.InitialDir := ExtractFilePath(FFileName);
    if not dlg.Execute then
      Exit;
    FFilename := dlg.FileName;
    DisplayFile;
    self.Caption := FFilename;
  finally
    dlg.Free;
  end;
end;

procedure TfmDisplayCSVFile.ActionCloseExecute(Sender: TObject);
begin
  self.Visible := False;
end;

procedure TfmDisplayCSVFile.ActionAddQuotesExecute(Sender: TObject);
var
  quot: TAddQuotesToFile;
begin
  quot := TAddQuotesToFile.Create(FFilename);
  try
    DisplayFile;
  finally
    quot.Free;
  end;
end;

procedure TfmDisplayCSVFile.ActionChangeMaxLines2ReadExecute(Sender: TObject);
var
  dlg: TfmGetNewValue;
  ivalue, er: integer;
begin
  dlg := TfmGetNewValue.Create(Self, FMaxLines2Read,
    'Maximum number of lines to read from file', _int_);
  try
    dlg.ShowModal;
    val(dlg.NewValue, ivalue, er);
    if er <> 0 then
      Exit;
    FMaxLines2Read := ivalue;
  finally
    dlg.Free;
  end;
end;

procedure TfmDisplayCSVFile.ActionReloadFileExecute(Sender: TObject);
begin
  DisplayFile;
end;

procedure TfmDisplayCSVFile.ActionReadCSVDataExecute(Sender: TObject);
var
  parmRec: TParmRec;
  first_line: string;
begin
  parmRec.colDelimiter := FColDelimiter;
  parmRec.rowDelimiter := FRowDelimiter;
  parmRec.ignoreFirstLine := FHasHeaders;
  parmRec.addRows := True;

  LoadGridFromCSVFile(sGrid, FFilename, parmRec, first_line);
  if first_line > EmptyStr then;

end;

procedure TfmDisplayCSVFile.ActionShowLine4ExaminationExecute(Sender: TObject);
var
  str: atString;
const msg_fmt = 'From file %s%sRow: %d';
begin
  GetCaretPos;
  FShowDlg.Msg := Format(msg_fmt, [FFilename, LineEnding, ed.Carets[0].PosY]);
  FShowDlg.ed.Text := ed.Strings.Lines[ed.Carets[0].PosY];
  FShowDlg.Show;
end;

procedure TfmDisplayCSVFile.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if FAutoLineNumber >= ed.Strings.Count -1 then
    CloseAction := caFree;
  CSVPropStorage.Save;
end;

procedure TfmDisplayCSVFile.FormCreate(Sender: TObject);
begin
  CSVPropStorage.Restore;
end;

const
  crposMsg = '(Col: %d, Row: %d)';

function TfmDisplayCSVFile.GetCaretPos: TPoint;
var
  Caret: TATCaretItem;
  i: integer;
begin
  Caret := ed.Carets[0];
  Result.X := Caret.PosX;
  Result.Y := Caret.PosY;
  StatusBar1.Panels[1].Text := Format(crposMsg, [Result.X, Result.Y]);
end;

procedure TfmDisplayCSVFile.edKeyUp(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  GetCaretPos;
end;

procedure TfmDisplayCSVFile.edMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var
  pt: TPoint;
begin
  pt := ed.ScreenToClient(Mouse.CursorPos);
  StatusBar1.Panels[3].Text := Format(crposMsg, [pt.Y, pt.X]);
end;

procedure TfmDisplayCSVFile.edMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  GetCaretPos;
end;

procedure TfmDisplayCSVFile.SetFileName(AValue: TFileName);
begin
  if FFileName = AValue then Exit;
  FFileName := AValue;
  self.Caption := FFilename;
  edFileName.Text := FFilename;
end;

procedure TfmDisplayCSVFile.SetLines(AValue : TStringList);
  function MakeMethod( Data, Code: Pointer ): TMethod;
  begin
    Result.Data := Data;
    Result.Code := Code;
  end;
begin
  FAutoLineNumber := 0;
  AmIAlive(True);
  ed.Strings.LineBlockInsert(0, AValue);
  ToolBar.Visible := False;
  pgCtrl.ShowTabs := False;
  self.BorderIcons := [];
  btn_read_raw_file.Visible := False;

  //ed.TATSynEditCalcHiliteEvent(MakeMethod(nil, @HighLightLine))
end;

procedure TfmDisplayCSVFile.SetRowDelimiter(AValue: ansistring);
begin
  if FRowDelimiter = AValue then Exit;
  FRowDelimiter := AValue;

  if FRowDelimiter = CRLF then
    atRowDelimiter := cEndWin
  else if FRowDelimiter = LF then;
  atRowDelimiter := cEndUnix;
end;

constructor TfmDisplayCSVFile.Create(aOwner: TComponent; theFileName: TFileName);
begin
  inherited Create(aOwner);
  FParent := aOwner;
  if theFileName <> '' then
    FFileName := theFileName;
  if not FileExists(FFilename) then
    Exit;
  self.Caption := FFileName;
  FMaxLines2Read := 5;

  pgCtrl.ActivePage := tabshRawFile;
  FShowDlg := TfmShowText.Create(self);
end;

destructor TfmDisplayCSVFile.Destroy;
begin
  inherited Destroy;
end;

function TfmDisplayCSVFile.FixRow(str: string): string;
var
  i: integer;
  c: string;
begin
  Result := '';
  for i := 1 to Length(str) do
  begin
    c := str[i];
    if c = TAB then
      c := '[TAB]';
    Result := Result + c;
  end;
end;

function TfmDisplayCSVFile.GetALine : TATStringItem;
  var
    ptr : PATStringItem;
    parts : TATLineParts;
    LineNdx, CharNdx, LineLen : integer;
  begin
    if FAutoLineNumber  = ed.Strings.Count -1 then
      begin
        AmIAlive(False);
        Close;
  		end;
    ptr := ed.Strings.GetItemPtr(FAutoLineNumber);
    Result := ptr^;
    FHighLightColor := clMoneyGreen;
    //HighLightLine(self, parts, LineNdx, CharNdx, LineLen, FHighLightColor);
    edFileName.Text := Result.Line;
    edFileName.Invalidate;
    Inc(FAutoLineNumber);
end;

procedure TfmDisplayCSVFile.HighLightLine(Sender : TObject;
			var AParts : TATLineParts; ALineIndex, ACharIndex, ALineLen : integer;
			var AColorAfterEol : TColor);
begin

end;

procedure TfmDisplayCSVFile.DisplayFile;
begin
  ed.LoadFromFile(FFilename);
end;

end.
