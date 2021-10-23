unit frmChangeCSVProperties;

{$mode objfpc}{$H+}

interface

uses
  Win32Proc, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
	StdCtrls, XMLPropStorage;

type

	{ TfmChangeCSVProperties }

  TfmChangeCSVProperties = class(TForm)
    rgrpCSVDelimiter: TRadioGroup;
    rbtnComma: TRadioButton;
    rbtnTab: TRadioButton;
    rbtnSemicolon: TRadioButton;
    btnAccept: TButton;
    btnCancel: TButton;
		CSVPropStorage : TXMLPropStorage;
    procedure btnCancelClick(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
  private
    FDelimiterChoice : Integer;
    FCurrentChoice : Char;
		procedure SetCurrentChoice(AValue : Char);
  public
    property DelimiterChoice : Integer read FDelimiterChoice write FDelimiterChoice ;
    property CurrentChoice : Char read FCurrentChoice write SetCurrentChoice ;
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

uses
  frmNewBooksDB, DirBuilder_dmod;

{ TfmChangeCSVProperties }

procedure TfmChangeCSVProperties.btnCancelClick(Sender: TObject);
begin
  FDelimiterChoice := ChoseNoChoice;
  ModalResult := ChoseNoChoice;
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

  Close;
end;

procedure TfmChangeCSVProperties.SetCurrentChoice(AValue : Char);
begin
  if FCurrentChoice = AValue then Exit;
		FCurrentChoice := AValue;
  case FCurrentChoice of
    COMMA : rbtnComma.Checked := True;
    SEMICOLON : rbtnSemicolon.Checked := True;
    TAB : rbtnTab.Checked := True;
	end;
end;

end.

