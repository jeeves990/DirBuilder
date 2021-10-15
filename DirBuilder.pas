program DirBuilder;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, frmDirFromCSV, stringGridHelper, runtimetypeinfocontrols,
	DirBuilder_dmod, frmDisplayCSVFile, frmInputNewValue, unitAddQuotesToFiles,
	stringgridutil, CSVParser_setup, frmChangeCSVProperties, dmodCSVParser,
frmBooksDb, frmNewBooksDB
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  //Application.CreateForm(TDirBuilder_dataModule, DirBuilder_dataModule);
  Application.CreateForm(TfrmFayesDirBuilder, frmFayesDirBuilder);
  //Application.CreateForm(TfmDisplayCSVFile, fmDisplayCSVFile);
  //Application.CreateForm(TfmGetNewValue, fmGetNewValue);
  //Application.CreateForm(TfmCSVParser_setup, fmCSVParser_setup);
  //Application.CreateForm(TfmChangeCSVProperties, fmChangeCSVProperties);
  //Application.CreateForm(TfmNewBooksDb, fmNewBooksDb);
  //Application.CreateForm(TfmBooksDb, fmBooksDb);
  Application.Run;
end.

