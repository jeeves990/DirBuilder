unit frmSelReportType;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  DirBuilder_dmod,
	Menus, ActnList;

type

			{ TfmSelectReportType }

      TfmSelectReportType = class(TForm)
				Actions : TActionList;
				rGrpReportTypes : TRadioGroup;
				ToolBar1 : TToolBar;
				ToolButton1 : TToolButton;
				ToolButton2 : TToolButton;
				ToolButton3 : TToolButton;
				ToolButton4 : TToolButton;
				procedure FormClose(Sender : TObject; var CloseAction : TCloseAction
									);
				procedure FormCreate(Sender : TObject);
      private
        FReportList : TStringList;
				procedure SetReportList(AValue : TStringList);
      public
        property ReportList : TStringList read FReportList write SetReportList;
      end;

var
  fmSelectReportType : TfmSelectReportType;

implementation

{$R *.lfm}

{ TfmSelectReportType }

procedure TfmSelectReportType.FormClose(Sender : TObject;
			var CloseAction : TCloseAction);
begin
  if Assigned(FReportList) then
    FReportList.Free;
end;

procedure TfmSelectReportType.FormCreate(Sender : TObject);
begin
  SetReportList(nil);
end;

procedure TfmSelectReportType.SetReportList(AValue : TStringList);
begin
	if Assigned(FReportList) then Exit;
	FReportList := TStringList.Create;
end;

end.

