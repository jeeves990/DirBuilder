program DirBuilder;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
   {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  frmDirFromCSV, upd_inv_lab_data_with_filenames;

{$R *.res}

begin
  RequireDerivedFormResource := True;
			Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmFayesDirBuilder, frmFayesDirBuilder);
  //Application.CreateForm(TfmShowText, fmShowText);
  Application.Run;
end.
