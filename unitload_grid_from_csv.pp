unit unitLoad_grid_from_csv;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Grids, DirBuilder_dmod, csvreadwrite, LazFileUtils;

function LoadGridFromCSVFile(Grid : TStringGrid;
                               AFilename : string;
                               parmRec : TParmRec;
                               out the_first_line : String) : String;

implementation


const
  charSize = sizeof(Char);
  LineEndingChars = [CR, LF];

var
  FLineEnding : String;
  FDelimiter : Char;
  EndOfLine, EndOfFile : Boolean;


function GetFirstLine(var fs : TFileStream) : String;
var
  curChar : Char;
  outS : String;
  _pos_, iPos : Integer;
begin

  Result := '';
  outS := '';
  _pos_ := 0;
  while True do
  begin
    curChar := #0;
    iPos := fs.Read(curChar, charSize);
    if iPos < charSize then
      Break;  // is this the end of the file
    if curChar in LineEndingChars then
      Break;
    Inc(_pos_, iPos);
    outS := outS + curChar;
	end;
  while True do
  begin
    fs.read(curChar, charSize);
    if not (curChar in LineEndingChars) then
      Break;
    Inc(_pos_);
	end;
  Dec(_pos_);
  fs.Seek(_pos_, soFromBeginning);
  Result := outS;
end;


function LoadGridFromCSVFile(Grid : TStringGrid; AFilename : string;
			parmRec : TParmRec; out the_first_line : String) : String;
      { Loads (quasi)CSV document in AFilename into Grid, using ADelimiter as
        delimiter. If WithHeader is true, it won't import the first row.
        If AddRows is true, it will add rows if the existing grid is too short.
      }
const
  DefaultRowCount=10; //Number of rows to show by default
var
  fs: TFileStream;
  Parser: TCSVParser;
  RowOffset, fileOffset : integer;
  aStr : String;
begin
  Result := '';
  the_first_line := '';
  Grid.BeginUpdate;
  // Reset the grid:
  Grid.Clear;
  Grid.RowCount:=DefaultRowCount;
  Grid.ColCount:=6; //Vaguely sensible
  if not(FileExistsUTF8(AFileName)) then Exit;

  Parser := TCSVParser.Create;
  FDelimiter := Parser.Delimiter;
  FLineEnding := Parser.LineEnding;

  fs := TFileStream.Create(AFilename, fmOpenRead + fmShareDenyWrite);
  try
    if parmRec.ignoreFirstLine then
     begin
      the_first_line := GetFirstLine(fs);
      fileOffset := fs.Position;
      Result := the_first_line;
		 end;
		Parser.Delimiter := parmRec.delimiter;
    Parser.SetSource(fs);

    // If the grid has fixed rows, those will not receive data, so we need to
    // calculate the offset
    RowOffset := Grid.FixedRows;
    // However, if we have a header row in our CSV data, we need to
    // discount that
    if parmRec.withHeader then
      RowOffset := RowOffset - 1;

    while Parser.ParseNextCell do
    begin
      // Stop if we've filled all existing rows. Todo: check for fixed grids etc,
      //     but not relevant for our case
      if parmRec.addRows = False then
        if Parser.CurrentRow+1 > Grid.RowCount then
          Break; //VisibleRowCount doesn't seem to work.

      // Widen grid if necessary. Slimming the grid will come after import done.
      if Grid.Columns.Enabled then
      begin
        if Grid.Columns.VisibleCount < Parser.CurrentCol+1 then
          Grid.Columns.Add;
      end
      else
      begin
        if Grid.ColCount<Parser.CurrentCol+1 then Grid.ColCount:=Parser.CurrentCol+1;
      end;

      // If header data found, and a fixed row is available, set the caption
      if (parmRec.withHeader) and
        (Parser.CurrentRow = 0) and
        (Parser.CurrentRow < Grid.FixedRows-1) then
      begin
        // Assign header data to the first fixed row in the grid:
        Grid.Columns[Parser.CurrentCol].Title.Caption:=Parser.CurrentCellText;
      end;

      // Actual data import into grid cell, minding fixed rows and header
      if Grid.RowCount<Parser.CurrentRow+1 then
        Grid.RowCount:=Parser.CurrentRow+1;
      Grid.Cells[Parser.CurrentCol,Parser.CurrentRow+RowOffset]:=Parser.CurrentCellText;
      aStr := Grid.Cells[Parser.CurrentCol,Parser.CurrentRow+RowOffset];
    end;

    // Now we know the widest row in the import, we can snip the grid's
    // columns if necessary.
    if Grid.Columns.Enabled then
    begin
      while Grid.Columns.VisibleCount>Parser.MaxColCount do
      begin
        Grid.Columns.Delete(Grid.Columns.Count-1);
      end;
    end
    else
    begin
      if Grid.ColCount>Parser.MaxColCount then
        Grid.ColCount:=Parser.MaxColCount;
    end;

  finally
    Parser.Free;
    fs.Free;
    Grid.EndUpdate;
  end;
end;



end.

