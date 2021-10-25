program Map_to_ini_proj;

{$mode objfpc}{$H+}

uses
      {$IFDEF UNIX}
      cthreads,
      {$ENDIF}
      {$IFDEF HASAMIGA}
      athreads,
      {$ENDIF}
      Interfaces, // this includes the LCL widgetset
      Forms, frmmap_to_ini, DirBuilder_dmod, stringGridHelper,
			frmDisplayCSVFile, frmSelReportType;

{$R *.res}

begin
      RequireDerivedFormResource := True;
      Application.Scaled := True;
      Application.Initialize;
			Application.CreateForm(TDirBuilder_dataModule, DirBuilder_dataModule);
			Application.CreateForm(TFmMap_to_INI, FmMap_to_INI);
      Application.Run;
end.

