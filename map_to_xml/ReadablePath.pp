program ReadablePath;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
      cthreads,
       {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  Dialogs { you can add units after this };

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  private
    FPath, FReadablePath: string;
    FPathList: TStringList;
    //FStrHelper : TStringHelper;
    FSplitAra: TStringArray;
    procedure Split_path;
  public

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;
  { TMyApplication }
const
  SEMICOLON = ';';

  procedure TMyApplication.DoRun;
  var
    ErrorMsg: string;
  begin
    // quick check parameters
    ErrorMsg := CheckOptions('h', 'help');
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    { add your program here }

    // stop program loop
    Terminate;
  end;

  procedure TMyApplication.Split_path;
  var
    iPos, i, idx: integer;
    s, s2: string;
  begin
    //FSplitAra  := FPath.Split([';']);
    FPathList.Clear;
    FPathList.Sorted := True;
    i := 0;
    try
      while Length(FPath) > 0 do
      begin
        iPos := Pos(SEMICOLON, FPath);
        if iPos = 1 then    // a semicolon with no path
        begin
          FPath := LowerCase(Copy(FPath, iPos + 1));
          Continue;
        end;
        FPathList.Add(Copy(FPath, 1, iPos - 1));
        idx := FPathList.Count;
        Inc(i);
        FPath := LowerCase(Copy(FPath, iPos + 1));
      end;
    except
      on EStringListError do
        WriteLn(Format('string list error (%d): text: [%s]', [i, FPathList[i - 1]]));
    end;
  end;

  constructor TMyApplication.Create(TheOwner: TComponent);
  var
    i: integer;
  begin
    inherited Create(TheOwner);
    StopOnException := True;
    FPathList := TStringList.Create;
    FPath := SysUtils.GetEnvironmentVariable('PATH');
    WriteLn(FPath);
    ReadLn;
    Split_path;
    for i := 0 to FPathList.Count - 1 do
      WriteLn(FPathList[i]);
    ReadLn;
  end;

  destructor TMyApplication.Destroy;
  begin
    inherited Destroy;
    FPathList.Free;
  end;

  procedure TMyApplication.WriteHelp;
  begin
    writeln('Usage: ', ExeName, ' -h');
  end;

var
  Application: TMyApplication;
begin
  Application := TMyApplication.Create(nil);
  Application.Title := 'My Application';
  Application.Run;
  Application.Free;
end.
