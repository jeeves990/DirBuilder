program DirBuilder;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, frmDirFromCSV, DbGridHelper, runtimetypeinfocontrols, DirBuilder_dmod,
  frmDisplayCSVFile, frmInputNewValue
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TDirBuilder_dataModule, DirBuilder_dataModule);
  Application.CreateForm(TfrmFayesDirBuilder, frmFayesDirBuilder);
  Application.CreateForm(TfmDisplayCSVFile, fmDisplayCSVFile);
			Application.CreateForm(TfmGetNewValue, fmGetNewValue);
  Application.Run;
end.

