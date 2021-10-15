unit frmChangeCSVProperties;

{$mode objfpc}{$H+}

interface

uses
  Win32Proc, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

	{ TfmChangeCSVProperties }

  TfmChangeCSVProperties = class(TForm)
    rgrpCSVDelimiter: TRadioGroup;
    rbtnComma: TRadioButton;
    rbtnTab: TRadioButton;
    rbtnSemicolon: TRadioButton;
    btnAccept: TButton;
    btnCancel: TButton;
    procedure btnCancelClick(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
  private
    FDelimiterChoice : Integer;
  public
    property DelimiterChoice : Integer read FDelimiterChoice write FDelimiterChoice ;
  end;

const
  ChoseTab = 1;
  ChoseSemicolon = 2;
  ChoseComma = 0;
  ChoseNoChoice = -999;

var
  fmChangeCSVProperties: TfmChangeCSVProperties;

implementation

{$R *.lfm}

{ TfmChangeCSVProperties }

procedure TfmChangeCSVProperties.btnCancelClick(Sender: TObject);
begin
  FDelimiterChoice := ChoseNoChoice;
  Close;
end;

procedure TfmChangeCSVProperties.btnAcceptClick(Sender: TObject);
var
  idx : Integer;
begin
  idx := rgrpCSVDelimiter.ItemIndex;
  if rbtnComma.Checked then
    FDelimiterChoice := ChoseComma
  else if rbtnTab.Checked then
    FDelimiterChoice := ChoseTab
  else if rbtnSemicolon.Checked then
    FDelimiterChoice := ChoseSemicolon;

 // case rgrpCSVDelimiter.ItemIndex of
 //   0 :
 //   1 : FDelimiterChoice := ChoseTab;
 //   2 : FDelimiterChoice := ChoseSemicolon;
	//end;
  Close;
end;

end.

