program DirBuilder;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, frmDirFromCSV, stringGridHelper, runtimetypeinfocontrols,
	anchordockpkg, DirBuilder_dmod, frmDisplayCSVFile, frmInputNewValue,
	AddQuotes2Files_unit, stringgridutil, CSVParser_setup, frmChangeCSVProperties,
	frmNewBooksDB, frmAddEdit;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmFayesDirBuilder, frmFayesDirBuilder);
  Application.Run;
end.

