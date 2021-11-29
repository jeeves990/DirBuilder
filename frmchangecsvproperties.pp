unit frmChangeCSVProperties;

{$mode objfpc}{$H+}

interface

uses
  Win32Proc, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  DirBuilder_dmod, //LMessages,
  StdCtrls, XMLPropStorage, //Grids,
  ComCtrls, Buttons;

type

  T_ParmRec = class
    form_op: integer;
    grp_value: String;
  end;

  { TfmChangeCSVProperties }

  TfmChangeCSVProperties = class(TForm)
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    pgCtrl: TPageControl;

    pnl_buttons: TPanel;
    CSVPropStorage: TXMLPropStorage;

    rgrpCSVLineending: TRadioGroup;
    rbtnCRLF: TRadioButton;
    rbtnLF: TRadioButton;

    rgrpCSVDelimiter: TRadioGroup;
    rbtnComma: TRadioButton;
    rbtnTab: TRadioButton;
    rbtnSemicolon: TRadioButton;


    tb_col_delimiter: TTabSheet;
    tb_lineEnding: TTabSheet;
    procedure btnCancelClick(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
		procedure FormCreate(Sender : TObject);
		procedure FormDestroy(Sender : TObject);
		procedure FormShow(Sender : TObject);
  private
    FDelimiterChoice: integer;
    FLineendingChoice: integer;
    FCurrentChoice: char;

    FParmRec: T_ParmRec;

    procedure SetCurrentChoice(AValue: char);
  public
    property DelimiterChoice: integer read FDelimiterChoice write FDelimiterChoice;
    property LineendingChoice: integer read FLineendingChoice write FLineendingChoice;
    //property CurrentChoice: char read FCurrentChoice write SetCurrentChoice;

    property ParmRec: T_ParmRec read FParmRec write FParmRec;
  end;

var
  fmChangeCSVProperties: TfmChangeCSVProperties;

implementation

{$R *.lfm}

uses
  frmNewBooksDB;

{ TfmChangeCSVProperties }

procedure TfmChangeCSVProperties.btnCancelClick(Sender: TObject);
begin
  FDelimiterChoice := ChoseNoChoice;
  ModalResult := ChoseNoChoice;
  Close;
end;

procedure TfmChangeCSVProperties.btnAcceptClick(Sender: TObject);
begin
  if pgCtrl.ActivePage = tb_col_delimiter then
  begin
    if rbtnComma.Checked then
      FDelimiterChoice := ChoseComma
    else if rbtnTab.Checked then
      FDelimiterChoice := ChoseTab
    else if rbtnSemicolon.Checked then
      FDelimiterChoice := ChoseSemicolon
    else
      FDelimiterChoice := ChoseNoChoice;
  end

  else

  if pgCtrl.ActivePage = tb_lineEnding then
  begin
    //idx := rgrpCSVLineending.ItemIndex;
    if rbtnCRLF.Checked then
      FDelimiterChoice := ChoseCRLF
    else if rbtnLF.Checked then
      FDelimiterChoice := ChoseLF
    else
      FDelimiterChoice := ChoseNoChoice;
  end;
  ModalResult := FDelimiterChoice;
  Close;
end;

procedure TfmChangeCSVProperties.FormCreate(Sender : TObject);
begin
  FParmRec := T_ParmRec.Create;
end;

procedure TfmChangeCSVProperties.FormDestroy(Sender : TObject);
begin
  FParmRec.Free;
end;

procedure TfmChangeCSVProperties.FormShow(Sender : TObject);
begin
  case FParmRec.form_op of
    ChgParser_RowDelim :
      begin
        pgCtrl.ActivePage := tb_lineEnding;
        rgrpCSVLineending.ItemIndex := 1;
      end;
    ChgParser_ColDelim :
      begin
        pgCtrl.ActivePage := tb_col_delimiter;
        rgrpCSVDelimiter.ItemIndex := 1;
			end;
	end;
end;


procedure TfmChangeCSVProperties.SetCurrentChoice(AValue: char);
begin
  if FCurrentChoice = AValue then Exit;
  FCurrentChoice := AValue;
  case FCurrentChoice of
    COMMA: rbtnComma.Checked := True;
    SEMICOLON: rbtnSemicolon.Checked := True;
    TAB: rbtnTab.Checked := True;
  end;
end;

procedure SetForm_op(AValue: integer);
begin
  //if AValue = ChgParser_RowDelim then
  //begin
  //  pgCtrl.ActivePage := tb_lineEnding;
  //  self.Caption := 'Change line ending property';
  //end;
  //if AValue = Ord(ChgParser_ColDelim) then
  //begin
  //  pgCtrl.ActivePage := tb_col_delimiter;
  //  self.Caption := 'Change column delimiter property';
  //end;
end;

end.
