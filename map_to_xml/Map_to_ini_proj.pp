program Map_to_ini_proj;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
  cthreads,
    {$ENDIF} {$IFDEF HASAMIGA}
    {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  DirBuilder_dmod,
  frmMap_to_XML,
  IniFileHandler,
  frm_select_rprt_type;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TDirBuilder_dataModule, DirBuilder_dataModule);
  Application.CreateForm(TFmMap_to_XML, FmMap_to_XML);
  //Application.CreateForm(Tfm_select_rprt_type, fm_select_rprt_type);
  Application.Run;
end.
