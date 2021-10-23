unit dmodCSVParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniPropStorage;

type

	{ Tdmod_csv_parser }

  Tdmod_csv_parser = class(TDataModule)
  private

  public

  end;

var
  dmod_csv_parser: Tdmod_csv_parser;

const
  SEMICOLON = ';';
  SEMICOLON_STRING = 'Semicolon';
  TAB = #09;
  TAB_STRING = 'Tab';
  COMMA = ',';
  COMMA_STRING = 'Comma';

implementation

{$R *.lfm}

end.

