unit frmSelReportType;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  DirBuilder_dmod,
  Menus, ActnList, StdActns;

type

  { TfmSelectReportType }

  TfmSelectReportType = class(TForm)
    ActionOpen: TAction;
    Actions: TActionList;
    FileExit1: TFileExit;
    rGrpReportTypes: TRadioGroup;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
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
  fmSelectReportType: TfmSelectReportType;

implementation

{$R *.lfm}

{ TfmSelectReportType }

procedure TfmSelectReportType.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ModalResult := -1;
  if rGrpReportTypes.ItemIndex >= 0 then
  begin
    FChosenReport := rGrpReportTypes.Items[rGrpReportTypes.ItemIndex];
    ModalResult := 1;
  end;
  //if Assigned(FReportTypeList) then
  //  FreeAndNil(FReportTypeList.Free);
end;

procedure TfmSelectReportType.FormCreate(Sender: TObject);
begin
  SetReportList(nil);
end;

procedure TfmSelectReportType.SetReportList(AValue: TStringList);
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

procedure TfmSelectReportType.SetChosenReport(AValue: string);
begin
  if FChosenReport = AValue then Exit;
  FChosenReport := AValue;
end;

end.
