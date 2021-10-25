program Map_to_xml_proj;

{$mode objfpc}{$H+}

uses
      {$IFDEF UNIX}
      cthreads,
      {$ENDIF}
      {$IFDEF HASAMIGA}
      athreads,
      {$ENDIF}
      Interfaces, // this includes the LCL widgetset
      Forms, frmMap_to_xml, DirBuilder_dmod, stringGridHelper,
			frmDisplayCSVFile, frmSelReportType;

{$R *.res}

begin
      RequireDerivedFormResource := True;
      Application.Scaled := True;
      Application.Initialize;
			Application.CreateForm(TFmMap_to_xml, FmMap_to_xml);
      Application.Run;
end.

