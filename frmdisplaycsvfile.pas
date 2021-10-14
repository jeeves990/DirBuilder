unit frmDisplayCSVFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ActnList,
	Menus, Grids, ComCtrls, ExtCtrls, Character, frmInputNewValue, stringgridutil,
	unitAddQuotesToFiles, dmodCSVParser;

type

  { TfmDisplayCSVFile }

  TfmDisplayCSVFile = class(TForm)
		ActList4DspCSV: TActionList;
		ActionReadNewFile: TAction;
		ActionAddQuotes: TAction;
		ActionShowLine4Examination: TAction;
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
		tabshRawFile: TTabSheet;
		mmo: TMemo;
		TabSheet2: TTabSheet;
		sGrid: TStringGrid;
		Button1: TButton;
		pnlTop: TPanel;
		Label1: TLabel;
		edFileName: TEdit;
		ActionReadCSVData: TAction;
		N3: TMenuItem;
		MenuItem6: TMenuItem;
		Procedure ActionReadNewFileExecute(Sender: TObject);
		Procedure ActionAddQuotesExecute(Sender: TObject);
		procedure ActionChangeMaxLines2ReadExecute(Sender: TObject);
		procedure ActionReloadFileExecute(Sender: TObject);
		procedure ActionReadCSVDataExecute(Sender: TObject);
  private
    FFilename : TFileName;
    FMaxLines2Read : Integer;
    FSlst : TStringList;
    FParent : TComponent;
    procedure SetFileName(AValue: TFileName);
  public
    property filename : TFileName read FFileName write SetFileName;
    constructor Create(aOwner : TComponent; theFileName : TFileName = ''); reintroduce;
    destructor Destroy; override;
    procedure DisplayFile;
    property MaxLines2Read : Integer read FMaxLines2Read write FMaxLines2Read;
  end;

var
  fmDisplayCSVFile: TfmDisplayCSVFile;

implementation

{$R *.lfm}

{ TfmDisplayCSVFile }

procedure TfmDisplayCSVFile.ActionReadNewFileExecute(Sender: TObject);
var
  dlg : TOpenDialog;
Begin
  dlg := TOpenDialog.Create(self);
  try
    dlg.InitialDir := ExtractFilePath(FFileName);
    if not dlg.Execute then
      Exit;
    FFilename := dlg.FileName;
    DisplayFile;
    self.Caption := FFilename;
	Finally
    dlg.Free;
	End;
end;

procedure TfmDisplayCSVFile.ActionAddQuotesExecute(Sender: TObject);
var
  i : Integer;
  quot : TAddQuotesToFile;
Begin
  quot := TAddQuotesToFile.Create(FFilename);
  try
    DisplayFile;
	finally
    quot.Free;
	end;
end;

procedure TfmDisplayCSVFile.ActionChangeMaxLines2ReadExecute(Sender: TObject);
var
  dlg : TfmGetNewValue;
  ivalue, er : Integer;
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
begin
  LoadGridFromCSVFile(sGrid,FFilename);
end;


procedure TfmDisplayCSVFile.SetFileName(AValue: TFileName);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
  self.Caption:=FFilename;
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
  FSlst := TStringList.Create;
end;

destructor TfmDisplayCSVFile.Destroy;
begin
  FSlst.Free;
  inherited Destroy;
end;

procedure TfmDisplayCSVFile.DisplayFile;
var
  fl: TextFile;
  s: string;
  i : Integer;
begin
  if FSlst.Count = 0 then ;
    FSlst.LoadFromFile(FFilename);
	mmo.Clear;
	for i := 0 to MaxLines2Read do
  	mmo.Lines.Add(FSlst[i]);
end;

end.

