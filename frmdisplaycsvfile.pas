unit frmDisplayCSVFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ActnList,
	Menus, Character, frmInputNewValue;

type

  { TfmDisplayCSVFile }

  TfmDisplayCSVFile = class(TForm)
    mmo: TMemo;
		ActList4DspCSV: TActionList;
		ActionReadNewFile: TAction;
		ActionAddQuotes: TAction;
		ActionShowLine4Examination: TAction;
		Button1: TButton;
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
		Procedure ActionReadNewFileExecute(Sender: TObject);
		Procedure ActionAddQuotesExecute(Sender: TObject);
		procedure ActionChangeMaxLines2ReadExecute(Sender: TObject);
		procedure ActionReloadFileExecute(Sender: TObject);
  private
    FFilename : TFileName;
    FMaxLines2Read : Integer;
    procedure SetFileName(AValue: TFileName);
		Procedure addQuotes;
		function addQuotes2Line(s: String) : String;
  public
    property filename : TFileName read FFileName write SetFileName;
    constructor Create(aOwner : TComponent; theFileName : TFileName = ''); reintroduce;
    procedure DisplayFile;
    property MaxLines2Read : Integer read FMaxLines2Read write FMaxLines2Read;
  end;

var
  fmDisplayCSVFile: TfmDisplayCSVFile;

implementation

{$R *.lfm}

{ TfmDisplayCSVFile }

Procedure TfmDisplayCSVFile.ActionReadNewFileExecute(Sender: TObject);
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
  fl: TextFile;
  s: string;
  i : Integer;
Begin
  addQuotes;
  Exit;
  AssignFile(fl, FFilename);

  try
    reset(fl);

    for i := 0 to MaxLines2Read do
    begin
      readln(fl, s);
      mmo.Lines.Add(s);
    end;

  finally
    CloseFile(fl)
  end
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


procedure TfmDisplayCSVFile.SetFileName(AValue: TFileName);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
  self.Caption:=FFilename;
end;

constructor TfmDisplayCSVFile.Create(aOwner: TComponent; theFileName: TFileName);
begin
  inherited Create(aOwner);
  if theFileName <> '' then
    FFileName := theFileName;
  if not FileExists(FFilename) then
    Exit;
  self.Caption := FFileName;
  FMaxLines2Read := 5;
end;

procedure TfmDisplayCSVFile.DisplayFile;
var
  fl: TextFile;
  s: string;
  i : Integer;
begin
  AssignFile(fl, FFilename);

  try
    reset(fl);
    mmo.Clear;
    for i := 0 to MaxLines2Read do
    begin
      readln(fl, s);
      mmo.Lines.Add(s);
    end;

  finally
    CloseFile(fl)
  end
end;

const
  DOUBLEQUOTE = '"';
  COMMA = ',';
  SPACE = #32;

function TfmDisplayCSVFile.addQuotes2Line(s : String) : String;
  function addQuotes2Item(itm : String) : String;
  { precondition: itm is a string with a positive length }
  var
    n : Double;
    ilen : Integer;
  begin
    try
		  Result := itm;

		  if itm[1] <> DOUBLEQUOTE then
		    itm := DOUBLEQUOTE + itm;
		  ilen := Length(itm);
		  if itm[ilen] <> DOUBLEQUOTE then
		    itm := itm + DOUBLEQUOTE;
		  Result := itm;
		except on e : Exception do
		  begin
				ShowMessage(Format('addQuotes2Item procedure: %s', [e.Message]));
        Result := itm;
      end;
		end;
	end;

var
  sl : TStringList;
  ara : array of String;
  idx, er : Integer;
  n : Double;
  rslt : String;
begin
  sl := TStringList.Create;
  try
    ara := s.Split([',']);
    for idx := 0 to Length(ara) -1 do
    begin
      if Length(ara[idx]) > 0 then
      begin
   		  val(ara[idx], n, er);
  		  if er = 0 then  // this is a number
          continue;
        if Pos(SPACE, ara[idx]) = 0 then
          continue;
        ara[idx] := addQuotes2Item(ara[idx]);
			end;
		end;
    { TODO: convert the ara back to the comma separated string }
    rslt := '';
    idx := 0;
    while idx < Length(ara) -1 do
      begin
        rslt += ara[idx] + COMMA;
        inc(idx);
			end;
		rslt += ara[idx];
    Result := rslt;
	Finally
    sl.Free;
	End;
end;

procedure TfmDisplayCSVFile.addQuotes;
var
  slst: TStringList;
  idx : Integer;
begin
  slst := TStringList.Create;
  try
    // Read the data into memory
    slst.LoadFromFile(FFilename);

    for idx := 0 to slst.Count -1 do
      slst[idx] := addQuotes2Line(slst[idx]);

    slst.SaveToFile(FFilename);
    DisplayFile;

  except
    on E:Exception do
      writeln('File ', FFilename, ' could not be read or written because: ', E.Message);
  end;

  // Clean up
  slst.Free;

end;

end.

