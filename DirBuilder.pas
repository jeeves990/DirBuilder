program DirBuilder;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, frmDirFromCSV, DbGridHelper, runtimetypeinfocontrols, DirBuilder_dmod
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmFayesDirBuilder, frmFayesDirBuilder);
  Application.Run;
end.

