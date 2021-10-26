unit IniFileHandler;

interface

uses
  contnrs, Classes, inifiles, Dialogs, Forms,
  SysUtils;

type

	{ TIniFileHandler }

  TIniFileHandler = class(TMemIniFile)
    { First element of FIniFileRecsList is a TStringList with records of inifile.
      Second element if TStringList with sections from the inifile. }

  private
    FIniFileRecsList: TStringList;
    FSectionsList: TStringList;
    FFileName, errMsg: string;
    function LoadIniFileList: boolean;
    procedure ParseSections;
  public
    property IniFileRecsList: TStringList read FIniFileRecsList write FIniFileRecsList;
    constructor Create(FName: string); reintroduce;
    destructor Destroy; override;
    property FileName: string read FFileName write FFileName;
    function GetListOfSection(section: string): TStringList;
    property SectionsList: TStringList read FSectionsList;
    procedure RefreshIniFile;
  end;

const
  OpenSquareBracket = #91;
  CloseSquareBracket = #93;
implementation

{ TIniFileHandler }

constructor TIniFileHandler.Create(FName: string);
begin
  inherited Create(FName);
  FFileName := FName;
  FIniFileRecsList := TStringList.Create;
  FSectionsList := TStringList.Create;

  // GetStrings(FIniFileRecsList);

  if LoadIniFileList then
    // FSectionsList := self.SectionsList;
    ParseSections;
end;

destructor TIniFileHandler.Destroy;
begin
  if Assigned(FIniFileRecsList) then
    FIniFileRecsList.Free;
  if Assigned(FSectionsList) then
    FSectionsList.Free;
  inherited;
end;

function TIniFileHandler.GetListOfSection(section: string): TStringList;
{
  TIniFileHandler.GetListOfSection:
  caller must free the TStringList that is returned.
  Objective: get a list of the nodes in the parm section in the memIniFile.
}
  function FindTheSection: integer;
  {
    discover where in the memIniFile the parm section resides
  }
  var
    i: integer;
  begin
    Result := -9999;

    FSectionsList.BeginUpdate;
    try
      i := 0;
      while i < FSectionsList.Count do
      begin
        if FSectionsList[i] = LowerCase(section) then
          Break;
        Inc(i);
      end;
      if i = FSectionsList.Count then
      begin
        errMsg := Concat('TIniFileHandler.GetListOfSection', sLineBreak,
          'IniFile section [', section, '] not found.');
        raise Exception.Create(errMsg);
      end;
      {  if execution is here, the section has been found  }
      {  FSectionsList.Objects[] are integers stored as objects  }
      Result := PtrUInt(FSectionsList.Objects[i]) + 1;
    finally
      FSectionsList.EndUpdate;
    end;
    { Result is now the position in FIniFileRecsList where the section begins }
  end;

var
  idx: integer;
  str: string;

begin
  Result := TStringList.Create;

  idx := FindTheSection;
  FIniFileRecsList.BeginUpdate;
  Result.BeginUpdate;
  try
    while idx < FIniFileRecsList.Count do
    begin
      if FIniFileRecsList[idx].StartsWith(OpenSquareBracket) then
        Break; { this would be the begining of the next section }

      if Trim(FIniFileRecsList[idx]) = EmptyStr then
      begin
        Inc(idx);
        Continue; { we don't need blank lines }
      end;

      Result.Add(FIniFileRecsList[idx]);
      Inc(idx);
    end;
    { if Result.Count > 0, Result is the list of nodes in the parm section }
  finally
    FIniFileRecsList.EndUpdate;
    Result.EndUpdate;
  end;
end;

procedure TIniFileHandler.RefreshIniFile;
begin
  UpdateFile;
end;

function TIniFileHandler.LoadIniFileList: boolean;
begin
  try
    GetStrings(FIniFileRecsList);
    Result := True;
  finally

  end;
end;

procedure TIniFileHandler.ParseSections;
var
  i, iPos: Int64;
  s: string;
begin
  FSectionsList.Clear;

  for i := 0 to FIniFileRecsList.Count - 1 do
    if FIniFileRecsList[i].StartsWith(OpenSquareBracket) then
    begin
      iPos := Pos(CloseSquareBracket, FIniFileRecsList[i]);
      if iPos < 0 then
        raise Exception.Create('TIniFileHandler.ParseSections' +
          sLineBreak + 'Close bracket "]" not found');
      s := FIniFileRecsList[i].Substring(1, iPos - 2);
      FSectionsList.AddObject(LowerCase(s), TObject(i));
    end;
end;

end.
