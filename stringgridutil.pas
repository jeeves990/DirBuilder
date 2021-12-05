unit StringGridUtil;

{ Use csvreadwrite to import csv data into stringgrid. Offers more robust
  support for CSV file variations than TStringGrid.LoadFromCSVFile }

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Grids, // csvreadwrite, LazFileUtils,
  fgl, LazLogger,
  DirBuilder_dmod;

type
  EStringGrid_Improper = TExceptionClass;
  EColNamesNotSame = TExceptionClass;

  { TStringGridUtil }

  TStringGridUtil = class
  private
    function isQuoted(Str: string): boolean;
    function Make_query_columns: string;
    function stripDoubleQuotes(str: string): string;
    function addUnderscore_to_titles(str: string): string;
    procedure Build_query_values_string(col_values_list : TStringList);
    function Build_value_strings(csv_col_list : TColumnList; grid : TStringGrid
					) : TStringList;
		procedure Write_2_db(value_string : string);
  private
    { FQry_columns and FCol_val_cmp are both built from FDb_col_list.
        FQry_columns is a comma separated list of rec.colName's.
        FCol_val_cmp is a comma separated list of rec.colName_cmp.

        FQry_columns is the string to be used in the output query's column list.
        FCol_val_cmp is the string (copies) whose values are replaced by
          values from the string grid to be used as values lists in the
          ouput query's value clause.}
    FQry_columns: string;   // the columns list in the output query
    FCol_val_cmp : String;  //

  public
    FOut_col_list: TStringList; // strings are for the query values clause
    FDb_col_list : TColumnList;

    constructor Create; reintroduce;
    destructor Destroy; reintroduce;
    procedure BeforeDestruction; reintroduce;
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

function TStringGridUtil.Prep_from_grid(parm_dmod: TDirBuilder_dataModule;
  grid: TStringGrid): boolean;
var
  str: string;
  csv_col_list : TColumnList;
const
  fcn_name = 'TStringGridUtil.WriteGridToBooksDb';

  function getColumnNames_from_CSV: boolean;
  var
    idx: integer;
    rec: TColumnRec;
  begin
    Result := False;
    csv_col_list.Clear;

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
      csv_col_list.Add(rec);
    end;
  end;


  function cmp_col_names_get_rel_pos(csv_names, db_names: TColumnList): boolean;
    (*
     *  cmp_col_names_get_rel_pos: not used
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
        str := db_names[i].colName4cmp;
        if csv_rec.colName4cmp = db_names[i].colName4cmp then
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
      db_rec := FDb_col_list[i];
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
      rtrn := db_names.IndexOf_ColName(csv_rec.colName);
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
  work_list: TColumnList;
  rec: TColumnRec;
  out_string_list : TStringList;
begin

  Result := False;
  work_list := nil;
  if not (parm_dmod is TDirBuilder_dataModule) then
    raise Exception.Create('WriteGridToBooksDb: called with wrong datamodule');
  dmod := parm_dmod;
  csv_col_list := TColumnList.Create(True);
  try
    {  query the booksdb schema for the columns names and
       compare those to the column names from getColumnNames_from_CSV   }

    if not getColumnNames_from_CSV then
      raise EStringGrid_Improper.Create(fcn_name +
        ' First row does not have column names.');
          {  the values in the later lists will be in the same
           logical order as the column name in keys_list    }

    dmod.Get_list_from_books_table(dmod.BooksDbConn.DatabaseName,
                       'books', FDb_col_list);

    // we have both lists (FDb_col_list and csv_col_list) now fix relative positions
    FDb_col_list.Fix_Relative_Positions(csv_col_list);

    //if not cmp_col_names_get_rel_pos(csv_col_list, FDb_col_list) then
    //  raise EColNamesNotSame.Create(fcn_name +
    //    ' Failed comparison between csv and database column names');
    DbColsCallback(FDb_col_list);
    CsvColsCallback(csv_col_list);

    FQry_columns := Make_query_columns;

    FOut_col_list := Build_value_strings(csv_col_list, grid);
    Build_query_values_string(FOut_col_list);

  finally
    FreeAndNil(csv_col_list);
    FreeAndNil(FDb_col_list);
    FreeAndNil(work_list);
  end;
end;  // function WriteGridToBooksDb(parm_dmod : TDirBuilder_dataModule;

function TStringGridUtil.Make_query_columns : string;
(*
 *    TStringGridUtil.Make_query_columns
 *    Result is the string to be used in the SQL for the columns list.
 *    The elements will be FDb_col_list.colName's
 *)
var
  i, ndx: integer;
  rec: TColumnRec;
  dta_type : string;
begin
  Result := '';
  FCol_val_cmp := '';

  for i := 0 to FDb_col_list.Count - 1 do
  begin
    rec := FDb_col_list[i];
    if rec.relativePos >= 0 then   // ignore the BAD_CHOICE's
    begin
      ndx := FDb_col_list.IndexOf_compareStr(rec.colName4cmp, dta_type);
      //Get_index_by_cmp_value(FDb_col_list, rec.colName4cmp);
    // this needs to come from fdb_col_list
      Result := Result + FDb_col_list[ndx].colName + COMMA;
      FCol_val_cmp := FCol_val_cmp +FDb_col_list[ndx].colName4cmp + COMMA;
    end;
  end;
  if Result[Length(Result)] = COMMA then
    Result := OPENPAREN + Result[1..Length(Result) - 1] +CLOSEPAREN;
  if FCol_val_cmp[Length(FCol_val_cmp)] = COMMA then
    FCol_val_cmp := FCol_val_cmp[1..Length(FCol_val_cmp) -1];
end;

function TStringGridUtil.Build_value_strings(csv_col_list : TColumnList;
  grid: TStringGrid): TStringList;
    const
      mysql_date_fmt = '%4.4d-%2.2d-%2.2d';

    function   Fix_date(replace_str: String) : String;

      (*
       * this is where replace_str has to be formatted, if necessary.
       * 	'0000-00-00 00:00:00'
       *)
      var
          space_pos : integer;
          work_str, S : string;
          slash_pos, day, month, year : Integer;

      begin
        space_pos := Pos(SPACE, replace_str);
        work_str := Copy(replace_str, 1, space_pos);
        slash_pos := Pos(FWDSLASH, work_str);
        if slash_pos > 0 then
          begin
            s := Copy(work_str, 1, slash_pos -1);
            if Length(s) < 1 then
              Exit;
            day := StrToInt(s);

            work_str := Copy(work_str, slash_pos +1);
            slash_pos := Pos(FWDSLASH, work_str);

            s := Copy(work_str, 1, slash_pos -1);
            month := StrToInt(s);

            work_str := Copy(work_str, slash_pos +1);
            s := Trim(Copy(work_str, 1));
            year := StrToInt(s);
            replace_str := Format(mysql_date_fmt, [year, month, day]);
					end;

			end;

var
  row, col{, col_names}: integer;
  value_str, cmp_str: string;
  dta_type : string;
  out_col_str: string;
  replace_str: string;
begin
  row := 1;
  if not Assigned(FOut_col_list) then
    FOut_col_list := TStringList.Create;

  { we have the CSV column name; therefore, we can get the colName4cmp from
    the csv_col_list param

  }
  the columns are out of order at this point. the second column should be a date
  while row < grid.RowCount do
  begin
    col := 0;
    out_col_str := FCol_val_cmp;
    while col < grid.ColCount do
    begin
      cmp_str := Get_cmp_value_by_gridPos(csv_col_list, col, dta_type);
      replace_str := grid.Cells[col, row];

      if LowerCase(LeftStr(dta_type, 4)) = 'date' then
        replace_str := Fix_date(replace_str);

      replace_str := AnsiQuotedStr(replace_str, '"');

      out_col_str := StringReplace(out_col_str, cmp_str, replace_str, []);

      Inc(col);
    end;
    out_col_str := OPEN_PAREN + out_col_str + CLOSE_PAREN;
    FOut_col_list.Add(out_col_str);
    Inc(row);
  end;
  Result := FOut_col_list;
end;

procedure TStringGridUtil.Build_query_values_string(col_values_list : TStringList);
(*
 *    Build_query_values_string: the objective is to take the strings from then
 *    col_values_list param and place them in a comma separated string for
 *    the insert query to the books table.
 *)
var
  value_str, vals_str : String;
  len_str : Integer;
begin
  value_str := '' ;
  vals_str := '';

  while True do
  begin
    while Length(value_str) < 4000 do
    begin
      value_str := col_values_list[0] + COMMA;
      vals_str := vals_str +value_str;
      len_str := Length(vals_str);

      Break;

      col_values_list.Delete(0);
      if col_values_list.Count = 0 then
        Break;

		end;
    if vals_str[Length(vals_str)] = COMMA then
      vals_str := vals_str[1..Length(vals_str) -1];
    Write_2_db(vals_str);

    if col_values_list.Count = 0 then
      Break;

    vals_str := '';
  end;
  if Length(vals_str) > 0 then
    Write_2_db(vals_str);
  if col_values_list.Count = 0 then
    col_values_list.Free;
end;

procedure TStringGridUtil.Write_2_db(value_string : string);
const
  sql = 'INSERT INTO books %s VALUES %s';
var
  qry : TSQLQuery;
  sql_str : String;
begin
  qry := TSQLQuery.Create(nil);
  qry.DataBase := dmod.BooksDbConn;

  try
    sql_str := Format(sql, [FQry_columns, value_string]);
    qry.SQL.Text := Format(sql, [FQry_columns, value_string]);
    qry.ExecSQL;

	finally
    qry.Free;
	end;
end;

constructor TStringGridUtil.Create;
begin
  inherited Create;
  FOut_col_list := TStringList.Create;
  FDb_col_list := TColumnList.Create(True);
end;

destructor TStringGridUtil.Destroy;
begin
  inherited Destroy;
end;

procedure TStringGridUtil.BeforeDestruction;
var
  iCnt : Integer;
begin
  FreeAndNil(FOut_col_list);
  FreeAndNil(FDb_col_list);
  inherited BeforeDestruction;
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
