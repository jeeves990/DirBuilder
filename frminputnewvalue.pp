unit frmInputNewValue;

{$mode objfpc}{$H+}

interface

uses
      Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLType;


const
  _int_ = 0;
  _dbl_ = 1;
  _str_ = 2;

type

      DataTypes = set of Byte; // = (Integer, Double, String);
		  { TfmGetNewValue }

		  TfmGetNewValue = class(TForm)
					  capCurrentValue: TLabel;
					  edCurrentValue: TEdit;
					  capRepresents: TLabel;
					  mmoRepresenting: TMemo;
					  grpboxRepresentingInput: TGroupBox;
					  edNewValue: TEdit;
					  btnAccept: TButton;
					  btnCancel: TButton;
						capRepresents1: TLabel;
						edDataType: TEdit;
					  procedure btnCancelClick(Sender: TObject);
						procedure btnAcceptClick(Sender: TObject);
						procedure edNewValueKeyUp(Sender: TObject; var Key: Word;
									Shift: TShiftState);
		  private
		    FPresentValue : Variant;
			  FNewValue: Variant;
        FDataType : Byte;
        FReturnDataType : DataTypes;
		  public
		    property PresentValue : Variant read FPresentValue write FPresentValue;
		    property NewValue : Variant read FNewValue;
		    constructor Create(aOwner: TComponent;
              const curValue : Variant;
              representing: String;
							dataType: Byte); reintroduce;
        property DataType : Byte read FDataType write FDataType;
end;

var
      fmGetNewValue: TfmGetNewValue;

implementation

{$R *.lfm}

{ TfmGetNewValue }

procedure TfmGetNewValue.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfmGetNewValue.btnAcceptClick(Sender: TObject);
var
  n : Double;
  i : Integer;
  er : Integer;
begin
  case FDataType of
    _int_ :
      begin
        val(edNewValue.Text, i, er);
        if er <> 0 then
        begin
          ShowMessage('Please enter an integer');
          Exit;
				end;
        FNewValue := edNewValue.Text;
			end;
    _dbl_ :
      begin
        val(edNewValue.Text, n, er);
        if er <> 0 then
        begin
          ShowMessage('Please enter an real number');
          Exit;
				end;
        FNewValue := edNewValue.Text;
			end;
    _str_ :
      begin
        FNewValue := edNewValue.Text;
			end;
  end;
  Close;
end;

procedure TfmGetNewValue.edNewValueKeyUp(Sender: TObject; var Key: Word;
			Shift: TShiftState);
begin
  if Key = VK_RETURN then
    btnAccept.Click;
end;

constructor TfmGetNewValue.Create(aOwner: TComponent; const curValue: Variant;
			representing: String; dataType: Byte);
begin
  inherited Create(aOwner);
  edCurrentValue.Text := curValue;
  mmoRepresenting.Text := representing;
  FDataType := dataType;
  //FReturnDataTypes := (_int_, _dbl_, _str_);
  case FDataType of
    _int_ : edDataType.Text := 'Integer value';
    _dbl_ : edDataType.Text := 'Real value';
    _str_ : edDataType.Text := 'String value';
	end;
end;

end.

