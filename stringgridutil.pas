unit StringGridUtil;

{ Use csvreadwrite to import csv data into stringgrid. Offers more robust
  support for CSV file variations than TStringGrid.LoadFromCSVFile }

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Grids, // csvreadwrite, LazFileUtils,
  fgl,
  DirBuilder_dmod;

type
  EStringGrid_Improper = TExceptionClass;
  EColNamesNotSame = TExceptionClass;

  { TStringGridUtil }

  TStringGridUtil = class
  private
    procedure Build_values_4_query();
    function cleanUpString(str: string): string;
    procedure cleanUpColumnList(var lst: TColumnList);
    function isQuoted(Str: string): boolean;
    function Make_query_columns: string;
    function stripDoubleQuotes(str: string): string;
    function addUnderscore_to_titles(str: string): string;
    function Build_value_strings(csv_col_list : TColumnList;
      grid: TStringGrid): boolean;
		procedure Write_2_db(value_string : string);
  private
    FQry_columns: string;
    FOut_col_list: TStringList;
    FDb_col_list : TColumnList;
  public
    function Prep_from_grid(parm_dmod: TDirBuilder_dataModule;
      grid: TStringGrid): boolean;
  var
    DbColsCallback: TColumnListCallbackMethod;
    CsvColsCallback: TColumnListCallbackMethod;
  end;

procedure Clean_up_quotes(grid: TStringGrid);


implementation

uses
  Dialogs, SQLDB, frmDirFromCSV, unitLoad_grid_from_csv, StrUtils;

const
  MAX_COLUMNS_LENGTH = 4000;  // max parm length for stored procedure

var
  dmod: TDirBuilder_dataModule;

procedure Clean_up_quotes(grid: TStringGrid);
var
  col, row: integer;
  s: string;
  ptr: PChar;
begin
  ptr := ^s;
  row := 0;
  while row < grid.RowCount do
  begin
    col := 0;
    while col < grid.ColCount do
    begin
      s := grid.Cells[col, row];
      if (Length(s) > 0) and (s[1] = DOUBLEQUOTE) and
        (s[Length(s)] = DOUBLEQUOTE) then
        grid.Cells[col, row] := s[2..Length(s) - 1];
      Inc(col);
    end;
    Inc(row);
  end;
end;

(******************************************************************************)

{  TStringGridUtil : class }
//procedure TStringGridUtil.Write_to_books_controller(
//                        cols_list : TStringList;
//                        val_list : TMyListOfStringLists;
//                        const PDmod : TDirBuilder_dataModule);


function TStringGridUtil.addUnderscore_to_titles(str: string): string;
{   when "unruly" characters are embedded in field names,
    replace them with underscores (_)
}
  //var
  //  loc : Integer;
begin
  str := StringReplace(str, SPACE, UNDERSCORE, [rfReplaceAll]);
  str := StringReplace(str, FWDSLASH, UNDERSCORE, [rfReplaceAll]);
  Result := str;
end;


function TStringGridUtil.isQuoted(Str: string): boolean;
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

function TStringGridUtil.stripDoubleQuotes(str: string): string;
{   when a field has embedded DOUBLEQUOTE's,
    strip them out    }
begin
  Result := EmptyStr;
  str := StringReplace(str, DOUBLEQUOTE, EmptyStr, [rfReplaceAll]);
  //str := Concat(DOUBLEQUOTE, str, DOUBLEQUOTE);
  Result := str;
end;

function TStringGridUtil.cleanUpString(str: string): string;
(*
 *    cleanUpString: remove non ALPHA_CHAR and
 *    return LowerCase string.
 *)
var
  i: integer;
begin
  Result := '';
  for i := 1 to Length(str) do
  begin
    if not (str[i] in ALPHA_CHARS) then
      Continue;
    Result := Result + LowerCase(str[i]);
  end;
end;

procedure TStringGridUtil.cleanUpColumnList(var lst: TColumnList);
(*
 * may not be used
 *)
var
  i: integer;
  rec: TColumnRec;
begin
  for i := 0 to lst.Count - 1 do
  begin
    rec := TColumnRec(lst[i]);
    rec.colName4cmp := cleanUpString(rec.colName);
  end;
end;

function TStringGridUtil.Prep_from_grid(parm_dmod: TDirBuilder_dataModule;
  grid: TStringGrid): boolean;
var
  str: string;
  csv_col_list : TColumnList;
const
  fcn_name = 'TStringGridUtil.WriteGridToBooksDb';

  function add_slst_2_col_list(slst: TStringList): TColumnList;
  var
    i: integer;
    rec: TColumnRec;
  begin
    Result := TColumnList.Create;
    for i := 0 to slst.Count - 1 do
    begin
      rec := TColumnRec.Create;
      rec.colName := slst[i];
      rec.colName4cmp := cleanUpString(rec.colName);
      rec.relativePos := BAD_CHOICE;
      Result.Add(rec);
    end;
  end;

  function getColumnNames(var col_names: TColumnList): boolean;
  var
    idx: integer;
    rec: TColumnRec;
  begin
    Result := False;
    if not Assigned(col_names) then
      col_names := TColumnList.Create;
    col_names.Clear;

    for idx := 0 to grid.ColCount - 1 do
    begin
      str := grid.Cells[idx, 0];
      if str > '' then
        Result := True;

      rec := TColumnRec.Create;
      rec.colName := stripDoubleQuotes(grid.Cells[idx, 0]);
      rec.colName4cmp := cleanUpString(rec.colName);
      rec.relativePos := BAD_CHOICE;
      rec.csv_pos := idx;
      col_names.Add(rec);
    end;
  end;


  function cmp_col_names_get_rel_pos(csv_names, db_names: TColumnList): boolean;
    (*
     *  cmp_col_names_get_rel_pos:
     *)
    function get_relative_position(csv_rec: TColumnRec): integer;
    var
      i: integer;
      str: string;
    begin
      Result := BAD_CHOICE;
      i := 0;
      while i < db_names.Count do
      begin
        str := TColumnRec(db_names[i]).colName4cmp;
        if csv_rec.colName4cmp = TColumnRec(db_names[i]).colName4cmp then
        begin
          csv_rec.relativePos := i;
          Result := i;
          Exit;
        end;
        Inc(i);
      end;
    end;


    procedure set_db_rec_relativePos(const csv_ndx: integer; var csv_rec: TColumnRec);
    var
      i: integer;
      db_rec: TColumnRec;
    begin
      i := csv_rec.relativePos;
      db_rec := TColumnRec(FDb_col_list[i]);
      db_rec.relativePos := csv_ndx;
    end;

  var
    i, j, rtrn: integer;
    db_rowCount, csv_rowCount: integer;
    csv_str, db_str: string;
    csv_rec, db_rec: TColumnRec;
  begin

    db_rowCount := db_names.Count;
    csv_rowCount := csv_names.Count;
    {  What if one is longer than the other.  }
    for i := 0 to csv_names.Count - 1 do
    begin
      csv_rec := TColumnRec(csv_names[i]);
      rtrn := get_relative_position(csv_rec);
      if rtrn >= 0 then
      begin
        csv_rec.relativePos := rtrn;
        set_db_rec_relativePos(i, csv_rec);
      end;
    end;

    //if db_rowCount > csv_rowCount then
    //  ShowMessage('Database has more columns than CSV');
  end;

var
  work_list: TStringList;
  rec: TColumnRec;
begin
  FDb_col_list := TColumnList.Create;

  Result := False;
  work_list := nil;
  if not (parm_dmod is TDirBuilder_dataModule) then
    raise Exception.Create('WriteGridToBooksDb: called with wrong datamodule');
  dmod := parm_dmod;
  try
    {  DONE: query the booksdb schema for the columns names and
       compare those to the column names from getColumnNames   }
    csv_col_list := nil;
    if not getColumnNames(csv_col_list) then
      // fill FDb_col_list with column names
      raise EStringGrid_Improper.Create(fcn_name +
        ' First row does not have column names.');
          {  the values in the later lists will be in the same
           logical order as the column name in keys_list    }

    work_list := dmod.Get_col_names(dmod.BooksDbConn.DatabaseName, 'books');
    FDb_col_list := add_slst_2_col_list(work_list);

    if not cmp_col_names_get_rel_pos(csv_col_list, FDb_col_list) then
      raise EColNamesNotSame.Create(fcn_name +
        ' Failed comparison between csv and database column names');
    DbColsCallback(FDb_col_list);
    CsvColsCallback(csv_col_list);
    FQry_columns := Make_query_columns;

    Result := Build_value_strings(csv_col_list, grid);
    Build_values_4_query;

  finally
    FreeAndNil(csv_col_list);
    FreeAndNil(FDb_col_list);
    FreeAndNil(work_list);
  end;
end;  // function WriteGridToBooksDb(parm_dmod : TDirBuilder_dataModule;

function TStringGridUtil.Make_query_columns : string;
var
  i, ndx: integer;
  rec: TColumnRec;
begin
  Result := '';
  for i := 0 to FDb_col_list.Count - 1 do
  begin
    rec := FDb_col_list[i];
    if rec.relativePos >= 0 then
    begin
      ndx := Get_index_by_cmp_value(FDb_col_list, rec.colName4cmp);
    // this needs to come from fdb_col_list
      Result := Result + FDb_col_list[ndx].colName + COMMA;
    end;
  end;
  if Result[Length(Result)] = COMMA then
    Result := OPENPAREN + Result[1..Length(Result) - 1] +CLOSEPAREN;

end;

function TStringGridUtil.Build_value_strings(csv_col_list : TColumnList;
  grid: TStringGrid): boolean;
var
  row, col, col_names: integer;
  value_str, cmp_str: string;
  out_col_str: string;
  replace_str: string;
begin
  row := 1;
  try
    if not Assigned(FOut_col_list) then
      FOut_col_list := TStringList.Create;

    while row < grid.RowCount do
    begin
      col := 0;
      out_col_str := FQry_columns;
      while col < grid.ColCount do
      begin
        cmp_str := Get_cmp_value_by_gridPos(csv_col_list, col);
        replace_str := AnsiQuotedStr(grid.Cells[col, row], '"');
        out_col_str := StringReplace(out_col_str, cmp_str, replace_str, []);

        Inc(col);
      end;
      out_col_str := OPEN_PAREN + out_col_str + CLOSE_PAREN;
      FOut_col_list.Add(out_col_str);
      Inc(row);
    end;
  finally
  end;
end;

procedure TStringGridUtil.Build_values_4_query();
var
  value_str : String;
begin
  value_str := '' ;
  while True do
  begin
    while Length(value_str) < 4000 do
    begin
      value_str := FOut_col_list[0] + COMMA;
      FOut_col_list.Delete(0);
      if FOut_col_list.Count = 0 then
        Break;
		end;
    if Length(value_str) > 0 then
      value_str := value_str[1..Length(value_str) -1];
    Write_2_db(value_str);
  end;
  if FOut_col_list.Count = 0 then
    Exit;;
end;

procedure TStringGridUtil.Write_2_db(value_string : string);
const
  sql = 'INSERT INTO books %s VALUES %s';
var
  qry : TSQLQuery;
begin
  qry := TSQLQuery.Create(nil);
  qry.DataBase := dmod.BooksDbConn;

  try
    qry.SQL.Text := Format(sql, [FQry_columns, value_string]);
    qry.ExecSQL;

	finally
    qry.Free;
	end;
end;

end.
(* what we have:
 * 1. record : TColumnRec
 *        colName: string;
 *        colName4cmp: string;
 *        relativePos: integer;
 *        csv_pos : Integer;
 * 2. two (2) lists of TColumnRec's:
 *    one for the stringgrid containing data from the csv file
 *    one for the columns from BooksDb.Books table
 * 3. function Get_indexOf(lst : TColumnList; value : String) : Integer;
 *    which returns the index in the TColumnList for the colName4cmp field.
 *)
