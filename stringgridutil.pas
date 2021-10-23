unit StringGridUtil;
{ Use csvreadwrite to import csv data into stringgrid. Offers more robust
  support for CSV file variations than TStringGrid.LoadFromCSVFile }

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Grids, csvreadwrite, LazFileUtils,
  DirBuilder_dmod;

type
  TParmRec = record
    delimiter : Char;
    withHeader : Boolean;
    addRows : Boolean;
    ignoreFirstLine : Boolean;
  end;

function LoadGridFromCSVFile(Grid : TStringGrid;
                                    AFilename : string;
                                    parmRec : TParmRec) : String;

function WriteGridToBooksDb(dmod : TDirBuilder_dataModule;
                            grid : TStringGrid;
		                        var pRowCnt : Integer;
		                        const delim : Char) : Boolean;

var
  FLineEnding : String;
  FDelimiter : Char;
  EndOfLine, EndOfFile : Boolean;

implementation

uses
  Dialogs, SQLDB, frmDirFromCSV;

const
  charSize = sizeof(Char);
  LineEndingChars = [CR, LF];
  MAX_COLUMNS_LENGTH = 4000;

procedure Write2BooksDbBooks(const ColStr : String;
                             values : TStringList;
                             const PDmod : TDirBuilder_dataModule);   forward;

function WriteGridToBooksDb(dmod : TDirBuilder_dataModule;
                            grid : TStringGrid;
                            var pRowCnt : Integer;
                            const delim : Char) : Boolean;
var
  colCnt, rowCnt : Integer;
  str : String;
  col_list : TStringList;

    function isQuoted(Str : String) : Boolean;
    begin
      Result := False;
      if Length(str) = 0 then
        Exit;
      if str[1] in QUOTES_SET then  // not the first char, so must be False
        Exit;
      // the first char is in QUOTES_SET
      if str[Length(str)] in QUOTES_SET then
        Result := True;
		end;


    function stripDoubleQuotes(str : String) : String;
    {   when a field has embedded DOUBLEQUOTE's,
        strip them out    }
    begin
      Result := EmptyStr;
      str := StringReplace(str, DOUBLEQUOTE, EmptyStr, [rfReplaceAll]);
      //str := Concat(DOUBLEQUOTE, str, DOUBLEQUOTE);
      Result := str;
		end;

    function addUnderscore_to_titles(str : String) : String;
    {   when "unruly" characters are embedded in field names,
        replace them with underscores (_)
    }
    var
      loc : Integer;
    begin
      str := StringReplace(str, SPACE, UNDERSCORE, [rfReplaceAll]);
      str := StringReplace(str, FWDSLASH, UNDERSCORE, [rfReplaceAll]);
      Result := str;
		end;

    function getColumnNames(keys_list : TStringList) : String;
    var
      idx : Integer;
      keys : array of String;
      sCols : String;
    begin
      if not Assigned(keys_list) then
        keys_list := TStringList.Create;
      keys_list.Clear;

		  for idx := 0 to grid.ColCount -1 do
		  begin
        str := stripDoubleQuotes(grid.Cells[idx, 0]);
        str := addUnderscore_to_titles(str);
		    keys_list.Add(str);
			end;
      idx := 0;
      sCols := EmptyStr;
      while idx < Length(keys) do
      begin
        sCols := Concat(sCols, keys[idx], COMMA);
        Inc(idx);
			end;
      // chop that last COMMA
      sCols := Copy(sCols, 1, Length(sCols) -1);
      sCols := Concat(OPENPAREN, sCols, CLOSEPAREN);
      Result := sCols;
		end;

		function replaceCommas_in_values(parmStr : String) : string;
    {
        this is a cheap trick and it may come back to bite:
        Consider: searches on booksdb will most often be done on
        the titles column. If a title has an embedded  COMMA, that
        is a problem.
    }
    begin
      Result := StringReplace(parmStr, COMMA, SEMICOLON, [rfReplaceAll]);
		end;

    function buildValuesStrings(out P_RESULT : Boolean) : TStringList;
    {   parameter P_RESULT allows for output string
        lengths >  MAX_COLUMNS_LENGTH being handled in more than one call  }
      var
        colValues : String; // colValues: work string for gathering field values
		  begin
        P_RESULT := False;
		    col_list.Clear;
		    //SetLength(keys, 0);

		    grid.BeginUpdate;
		    try
		      {
		          then, get the values from the grid; build value strings
		      }
          rowCnt := pRowCnt + 1;
		      while rowCnt < grid.RowCount do
		      begin
            colCnt := 0;;
            colValues := EmptyStr;    // initialize colValues

		        while colCnt < grid.ColCount do
		        begin
              str := stripDoubleQuotes(grid.Cells[colCnt, rowCnt]);
              str := Concat(DOUBLEQUOTE, str, DOUBLEQUOTE);
		          colValues := Concat(colValues, str, COMMA);
		          Inc(colCnt);
					  end;
            {  remove trailing COMMA  }
            colValues := Copy(colValues, 1, Length(colValues) -1);
            colValues := Concat(OPENPAREN, colValues, CLOSEPAREN);
            {   Terminate the columns parms with CRLF   }
            col_list.Add(colValues);

            {  don't let the strings of values get too long  }
						if col_list.Count > MAX_COLUMNS_LENGTH then
            begin
              pRowCnt := rowCnt;
              P_RESULT := True;
              Exit;
		        end;
            Inc(rowCnt);
					end;
				finally
          Result := col_list;
		      grid.EndUpdate;
		 	  end;
      end;

var
  sValues_list, sKeys_list : TStringList;
  sColumns : String;
  fl : TextFile;
  B_RESULT : Boolean;
  i : Integer;
begin
  if not (dmod is TDirBuilder_dataModule) then
    raise Exception.Create('WriteGridToBooksDb: called with wrong datamodule');

  Result := False;
  B_RESULT := False;

  sColumns := getColumnNames(sKeys_list);

  col_list := TStringList.Create;
  try
    buildValuesStrings(B_RESULT);  // builds col_list

    {  B_RESULT signals whether there are more rows of values to build out  }
    while  not B_RESULT do
    begin
      Write2BooksDbBooks(sColumns, col_list, dmod);
      col_list.Clear;
      buildValuesStrings(B_RESULT);
      if col_list.Count = 0 then
        Break;
    end;
    try
      AssignFile(fl, 'myLogFile.txt');
      Rewrite(fl);
      for i := 0 to col_list.Count -1 do
        WriteLn(fl, col_list[i]);
      Close(fl);
    except on E : Exception do
      ShowMessage('WriteGridToBooksDb: ' +E.Message);
    end;

	finally
    col_list.Free;
	end;
end;

procedure forSafeKeeping;
{
    then, get the values from the grid and build the key=value pairs
}
begin
(*
	rowCnt := pRowCnt + 1;
	while rowCnt < grid.RowCount do
	begin
	  colCnt := 0;;
	  colValues := EmptyStr;
	  while colCnt < grid.ColCount do
	  begin
	    str := stripDoubleQuotes(grid.Cells[colCnt, rowCnt]);
	    str := addUnderscore(str);
	    str := AnsiQuotedStr(str, DOUBLEQUOTE);
	    {  commented lines from previous where key=value pairs were built  }
	    //sPair := keys[colCnt] + EQUALS + AnsiQuotedStr(str, DOUBLEQUOTE);
	    //SKey_values := Concat(SKey_values, spair, TAB);
  	    {  the terminator has to be TAB in case of string with COMMA  }
	    colValues := Concat(colValues, str, TAB);
	    Inc(colCnt);
	  end;
	  {  remove trailing COMMA  }
	  colValues := Copy(colValues, 1, Length(colValues) -1);
	  {   Terminate the columns parms with CRLF   }
	  SColumnsValues := Concat(SColumnsValues, colValues, CRLF);
	  if Length(SColumnsValues) > MAX_COLUMNS_LENGTH then
	  begin
	    pRowCnt := rowCnt;
	    P_RESULT := True;
	    Exit;
	  end;
	  Inc(rowCnt);
	end;
	finally
		Result := SColumnsValues;
		SetLength(keys, 0);
		grid.EndUpdate;
	end;
*)
end;


procedure Write2BooksDbBooks(const ColStr : String;
                             values : TStringList;
                             const PDmod : TDirBuilder_dataModule);
var
  cols_list : TStringList;
     procedure setParams;
     var
       idx : Integer;
     begin

		 end;

     procedure buildColList;
     var
       idx, _pos : Integer;
       str : String;
     begin
       // load ColStr into tstringlist
       str := Copy(ColStr, 2, Length(ColStr) -1);  // rid of the parens

       if not Assigned(cols_list) then
         cols_list := TStringList.Create;
       cols_list.Clear;
       while Length(str) > 0 do
       begin
         _pos := Pos(str, COMMA);
         if _pos = 0 then
           Break;
         cols_list.Add(Copy(str, 1, _pos -1));
         str := Copy(str, _pos +1);
			 end;
       if Length(str) > 0 then
         cols_list.Add(str);
		 end;

var
  qry : TSQLQuery;
  tx : TSQLTransaction;
  i : Integer;
  str, sval : String;
begin
  if values.Count = 0 then
    Exit;
  qry := PDmod.qryInsertBooks;
  tx := PDmod.BooksDbTx;
  try
    try
      tx.StartTransaction;
      qry.ParamByName('columns').AsString := ColStr;
      str := qry.ParamByName('columns').AsString;
      for i := 0 to values.Count -1 do
      begin
        qry.ParamByName('values').AsString := values[i];
        sval :=  qry.ParamByName('values').AsString;
        qry.ExecSQL;
			end;
			tx.Commit;
		except on Ex : Exception do
      begin
        ShowMessage(Format('WriteGridToBooksDb' +CRLF +'%s', [Ex.Message]));
        tx.Rollback;
			end;
		end;
	finally
	end;
end;

function GetFirstLine(var fs : TFileStream) : String;
var
  curChar : Char;
  outS : String;
  pos, iPos : Integer;
begin

  Result := '';
  outS := '';
  pos := 0;
  while True do
  begin
    curChar := #0;
    iPos := fs.Read(curChar, charSize);
    if iPos < charSize then
      Break;  // is this the end of the file
    if curChar in LineEndingChars then
      Break;
    Inc(pos, iPos);
    outS := outS + curChar;
	end;
  while True do
  begin
    fs.read(curChar, charSize);
    if not (curChar in LineEndingChars) then
      Break;
    Inc(Pos);
	end;
  Dec(Pos);
  fs.Seek(Pos, soFromBeginning);
  Result := outS;
end;

function LoadGridFromCSVFile(Grid : TStringGrid;
                             AFilename : string;
                             parmRec : TParmRec) : String;
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
  firstLine, aStr : String;
begin
  Result := '';
  firstLine := '';
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
      firstLine := GetFirstLine(fs);
      fileOffset := fs.Position;
      Result := firstLine;
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


//function LoadGridFromCSVFile(Grid : TStringGrid; AFilename: string;
//  ADelimiter : Char = COMMA; WithHeader : Boolean = True;
//  AddRows : Boolean = True; IgnoreFirstLine : Boolean = False) : String;

//function LoadGridFromCSVFile(Grid : TStringGrid; AFilename : string;
//			parmRec : TParmRec) : String;
//begin
//  Result := LoadGridFromCSVFile(Grid,
//                                AFilename,
//                                parmRec.delimiter,
//                                parmRec.withHeader,
//                                parmRec.addRows,
//                                parmRec.ignoreFirstLine);
//end;

(*function LoadGridFromCSVFile(Grid: TStringGrid;
                               AFilename: string;
                               parmRec : TParmRec);
                               //ADelimiter : Char;
                               //WithHeader : Boolean;
                               //AddRows : Boolean;
                               //IgnoreFirstLine : Boolean) : String;
*)

