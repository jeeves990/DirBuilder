unit frm_select_rprt_type;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  ActnList, StdActns, StdCtrls, Buttons, DirBuilder_dmod;

type

  { Tfm_select_rprt_type }

  Tfm_select_rprt_type = class(TForm)
    Action_close_Dialog: TAction;
    Action_Cancel_dialog: TAction;
    Actions: TActionList;
		btnClose : TBitBtn;
		btn_cancel_dialog : TBitBtn;
    FileOpen1: TFileOpen;
    FileSaveAs: TFileSaveAs;
    pnlButtons: TPanel;
    rGrpReportTypes: TRadioGroup;
    ToolBar: TToolBar;
    tbOpen: TToolButton;
    ToolButton1: TToolButton;
		ToolButton2 : TToolButton;
    ToolButton3: TToolButton;
    tbClose: TToolButton;
    procedure Action_close_DialogExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FReportTypeList: TStringList;
    FChosenReport: string;
    procedure SetChosenReport(AValue: string);
    procedure SetReportList(AValue: TStringList);
  public
    property ReportTypeList: TStringList read FReportTypeList write SetReportList;
    property ChosenReport: string read FChosenReport write SetChosenReport;

  end;

var
  fm_select_rprt_type: Tfm_select_rprt_type;

implementation

{$R *.lfm}

{ Tfm_select_rprt_type }

procedure Tfm_select_rprt_type.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  ModalResult := -1;
  if rGrpReportTypes.ItemIndex >= 0 then
  begin
    FChosenReport := rGrpReportTypes.Items[rGrpReportTypes.ItemIndex];
    ModalResult := 1;
  end;
end;

procedure Tfm_select_rprt_type.Action_close_DialogExecute(Sender: TObject);
begin
  Close;
end;

procedure Tfm_select_rprt_type.FormCreate(Sender: TObject);
begin
  SetReportList(nil);
end;

procedure Tfm_select_rprt_type.SetReportList(AValue: TStringList);
var
  i: integer;
begin
  if FReportTypeList = AValue then Exit;
  FReportTypeList := AValue;
  rGrpReportTypes.Items.Clear;
  i := 0;
  while i < AValue.Count do
  begin
    rGrpReportTypes.Items.Add(AValue[i]);
    Inc(i);
  end;

end;

procedure Tfm_select_rprt_type.SetChosenReport(AValue: string);
begin
  if FChosenReport = AValue then Exit;
  FChosenReport := AValue;
end;


end.
