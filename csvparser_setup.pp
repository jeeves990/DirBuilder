unit CSVParser_setup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  csvreadwrite, dmodCSVParser;

type

	{ TfmCSVParser_setup }

  TfmCSVParser_setup = class(TForm)
  private
    FParser : TCSVParser;
    FDelimiter, FLineending : String;
  public
    property Parser : TCSVParser read FParser write FParser;
    property Delimiter : String read FDelimiter write FDelimiter;
    property Lineending : String read FLineending write FLineending;
    Constructor Create(aOwner : TComponent); reintroduce;
    Destructor Destroy; reintroduce;
  end;

var
  fmCSVParser_setup: TfmCSVParser_setup;

implementation

{$R *.lfm}

{ TfmCSVParser_setup }

constructor TfmCSVParser_setup.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FParser := TCSVParser.Create;
  FDelimiter := FParser.Delimiter;
  if FDelimiter = ',' then
    FDelimiter := 'Comma';
  FLineending := FParser.LineEnding;
  if FLineending = #13 then
    FLineending := 'CR'
    else if FLineending = #13#10 then
      FLineending := 'CRLF';
end;

destructor TfmCSVParser_setup.Destroy;
begin
  FParser.Free;
end;

end.

