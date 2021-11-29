unit AddQuotes2Files_unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs;
type

	{ TAddQuotesToFile }

  TAddQuotesToFile = class
    FFileName : String;
    private
  		procedure addQuotes;
  		function addQuotes2Line(s: String) : String;
			function addQuotes2Item(itm: String): String;

    public
      property FileName : String read FFileName write FFileName;
      constructor Create(const _fileName : TFileName); reintroduce;
	end;


implementation

uses
  DirBuilder_dmod;

{ TAddQuotesToFile }

function TAddQuotesToFile.addQuotes2Item(itm : String) : String;
  { precondition: itm is a string with a positive length }
var
  ilen : Integer;
begin
  try
	  Result := itm;

	  if itm[1] <> DOUBLEQUOTE then
	    itm := DOUBLEQUOTE + itm;
	  ilen := Length(itm);
	  if itm[ilen] <> DOUBLEQUOTE then
	    itm := itm + DOUBLEQUOTE;
	  Result := itm;
	except on e : Exception do
	  begin
			ShowMessage(Format('addQuotes2Item procedure: %s', [e.Message]));
      Result := itm;
    end;
	end;
end;

constructor TAddQuotesToFile.Create(const _fileName: TFileName);
begin
  inherited Create;
  FFileName := _fileName;
  addQuotes;
end;

function TAddQuotesToFile.addQuotes2Line(s : String) : String;

var
  sl : TStringList;
  ara : array of String;
  idx, er, _len : Integer;
  n : Double;
  rslt, _S : String;
begin
  sl := TStringList.Create;
  try
    ara := s.Split([',']);
    for idx := 0 to Length(ara) -1 do
    begin
      if Length(ara[idx]) > 0 then
      begin
   		  val(ara[idx], n, er);
  		  if er = 0 then  // this is a number
          continue;
        if Pos(SPACE, ara[idx]) = 0 then
          continue;
        _S := Copy(ara[idx], 1, 6);
        _S := Copy(ara[idx], 1, 6);
        _S := ara[idx];
        _len := Length(_S);
        {
          if there is an embedded COMMA, we split on it, so,
              '"Adlai Stevenson,: Citizen of the world,"'
          becomes
              'Adlai Stevenson' and ': Citizen of the world'
          with the closing quote in a subsequent node of the array.
        }
        try
        if (_S[1] = DOUBLEQUOTE) and (_S[_len] <> DOUBLEQUOTE) then
        begin
          _S += ',' +ara[idx +1];
          _len := Length(_S);

          while _S[_len] <> DOUBLEQUOTE do
          begin

					end;
					//if _S[_len] <> DOUBLEQUOTE then
          //begin
            //S := Format(fmt,[s1, i]);
            //Writeln(fmt:12,'=> ',s);
            //rslt := Format('[line: %s, character: %d]', [S, idx]);
            //raise Exception(rslt);
					//end;
				end;

				except on E : Exception   do
		      begin
		        ShowMessage('addQuotes2Line: file is malformed at ' +sLineBreak
                                                                +E.Message);
		        Exit;
	        end;
				end;
				{ if first char of ara[idx] = DOUBLEQUOTE and the last char is not,
          peek a ara[idx][last char]; if it HAS DOUBLEQUOTE, add ara[idx] to
          ara[idx +1]
          and delete ara[idx +1]   }
        ara[idx] := addQuotes2Item(ara[idx]);
			end;
		end;
    { TODO: convert the ara back to the comma separated string }
    rslt := '';
    idx := 0;
    while idx < Length(ara) -1 do
      begin
        rslt += ara[idx] + COMMA;
        inc(idx);
			end;
		rslt += ara[idx];
    Result := rslt;
	Finally
    sl.Free;
	End;
end;

procedure TAddQuotesToFile.addQuotes;
{
    procedure TAddQuotesToFile.addQuotes;
    There is a problem: TStringList LoadFromFile does not read csv files like
    the TCSVDataset.
}
var
  slst: TStringList;
  idx : Integer;
  begin
  slst := TStringList.Create;
  try
    // Read the data into memory
    slst.LoadFromFile(FFilename);

    for idx := 0 to slst.Count -1 do
      slst[idx] := addQuotes2Line(slst[idx]);

    slst.SaveToFile(FFilename);

  except
    on E:Exception do
      writeln('File ', FFilename, ' could not be read or written because: ', E.Message);
  end;

  // Clean up
  slst.Free;

end;

end.

