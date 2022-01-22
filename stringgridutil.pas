unit StringGridUtil;

{ Use csvreadwrite to import csv data into stringgrid. Offers more robust
  support for CSV file variations than TStringGrid.LoadFromCSVFile }

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Grids, // csvreadwrite, LazFileUtils,
  fgl, LazLogger,
  DirBuilder_dmod, SQLDB, DB;

type
  EStringGrid_Improper = TExceptionClass;
  EColNamesNotSame = TExceptionClass;

  { TStringGridUtil }

  TStringGridUtil = class
  private
    function addUnderscore_to_titles(str: string): string;
		function Get_DB_dta_type(dtype : string) : TFieldType;
		function Get_InvLab_insert_sql : String;
		procedure pop_parms(var qry : TSQLQuery; vals_list, cmp_list : TStrings);
    function String_together_values_clauses(dummy : Integer) : TStringList;
		function String_together_values_into_list : TColumnValuesList;

  private
    FGrid : TStringGrid;
    FDmod : TDirBuilder_dataModule;
    FOut_values_clauses_list: TStringList; // strings are for the query values clause
    FDb_col_list: TColumnList;
    FCSV_col_list: TColumnList;
    FCol_values_list : TColumnValuesList;

    { FQry_cols_string and FCol_val_cmp_string are both built from FDb_col_list.
        FQry_cols_string is a comma separated list of rec.colName's.
        FCol_val_cmp_string is a comma separated list of rec.colName_cmp.

        FQry_cols_string is the string to be used in the output query's column list.
        FCol_val_cmp_string is the string (copies) whose values are replaced by
          values from the string FGrid to be used as values lists in the
          ouput query's value clause.}
    FQry_cols_string: string;   // the columns list in the output query
    FCol_val_cmp_string: string;
    FCol_val_cmp_list: TStringList;
    FBooksInputQry: TSQLQuery;

    function Prep_from_grid: boolean;
    function Make_query_columns: string;
    procedure Write_2_db;
    procedure Write_2_db(qry: TSQLQuery; value_string: string); overload;

  public

    constructor Create; reintroduce;
    destructor Destroy; reintroduce;
    procedure BeforeDestruction; reintroduce;
    procedure Db_insert_controller(dmod : TDirBuilder_dataModule;
                      grid : TStringGrid; file_name : TFileName);
  var
    DbColsCallback: TColumnListCallbackMethod;
    CsvColsCallback: TColumnListCallbackMethod;
  end;

procedure Clean_up_quotes(grid: TStringGrid);
procedure String_grid_clean_data(grid : TStringGrid);

implementation

uses
  Dialogs, frmDirFromCSV, unitLoad_grid_from_csv, StrUtils;

const
  MAX_COLUMNS_LENGTH = 4000;  // max parm length for stored procedure
  //VAL_CLAUSE_DELIMITER = COMMA;
  VAL_CLAUSE_DELIMITER = '&&&';
  KEY_WORD = 'asin';

var
  dmod: TDirBuilder_dataModule;

procedure Clean_up_quotes(grid : TStringGrid);
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

procedure String_grid_clean_data(grid : TStringGrid);
begin
  grid.Clean(0, 1, grid.ColCount - 1, grid.RowCount - 1, []);
end;

(******************************************************************************)

{  TStringGridUtil : class }

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


function stripQuotes(str : String; quote : char) : String;
var
  i : Integer;
begin
  Result := Trim(str);
  if Result[1] = quote then
    Result := Result[2..Length(Result)];
  if str[Length(Result)] = quote then
    Result := Result[1..Length(Result) -1];
end;

function TStringGridUtil.Prep_from_grid: boolean;
var
  str: string;
const
  fcn_name = 'TStringGridUtil.WriteGridToBooksDb';

  function getColumnNames_from_CSV: boolean;
  var
    idx: integer;
    rec: TColumnRec;
  begin
    Result := False;
    FCSV_col_list.Clear;

    for idx := 0 to FGrid.ColCount - 1 do
    begin
      str := FGrid.Cells[idx, 0];
      if str > '' then
        Result := True;

      rec := TColumnRec.Create;
      rec.colName := stripQuotes(FGrid.Cells[idx, 0], DOUBLEQUOTE);
      rec.colName4cmp := cleanUpString(rec.colName);
      rec.relativePos := BAD_CHOICE;
      rec.csv_pos := idx;
      FCSV_col_list.Add(rec);
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
  rec: TColumnRec;
  out_string_list: TStringList;
begin

  Result := False;

  try
    {  query the booksdb schema for the columns names and
       compare those to the column names from getColumnNames_from_CSV   }

    if not getColumnNames_from_CSV then
      raise EStringGrid_Improper.Create(fcn_name +
        ' First row does not have column names.');
          {  the values in the later lists will be in the same
           logical order as the column name in keys_list    }

    // we have both lists (FDb_col_list and FCSV_col_list) now fix relative positions
    FDb_col_list.Fix_Relative_Positions(FCSV_col_list);

    //if not cmp_col_names_get_rel_pos(FCSV_col_list, FDb_col_list) then
    //  raise EColNamesNotSame.Create(fcn_name +
    //    ' Failed comparison between csv and database column names');
    DbColsCallback(FDb_col_list);
    CsvColsCallback(FCSV_col_list);

    //WRITE A STORED PROCEDURE TO TAKE THE COLUMN AND VALUE STRINGS
    //TO INSERT THEM ONE AT THE TIME SO THE ASIN CAN BE CHECKED ON EACH
    //ITERATION        2021-12-06
    // gave up on that.   2021-12-08

  finally
  end;
end;  // function WriteGridToBooksDb(parm_dmod : TDirBuilder_dataModule;

procedure TStringGridUtil.Db_insert_controller(dmod : TDirBuilder_dataModule;
			grid : TStringGrid; file_name : TFileName);
var
  cmp_list : TStringList;
  i, parm_count : Integer;
  sParm, sql : String;
  parm : TParam;
begin
  FGrid := grid;
  FDmod := dmod;
  FCSV_col_list := TColumnList.Create(True);
  FOut_values_clauses_list := TStringList.Create;
  FDb_col_list := TColumnList.Create(True);
  FCSV_col_list := TColumnList.Create(True);
  FBooksInputQry:= TSQLQuery.Create(nil);
  FBooksInputQry.DataBase := FDmod.BooksDbConn;
  FCol_values_list := TColumnValuesList.Create(True);
  cmp_list := TStringList.Create;

  try
    Fdmod.Get_column_list_from_books_db(dmod.BooksDbConn.DatabaseName,
        'inventory_lab_data', FDb_col_list);    // FDb_col_list is initiated
    Prep_from_grid;
    FQry_cols_string := Make_query_columns;
    { FCSV_col_list is initiated
      and relative positions are set in both it and FDb_col_list }
    FCol_values_list := String_together_values_into_list;

    sql := Get_InvLab_insert_sql ;
    //exit;
    FDmod.qryInsertBooks.SQL.BeginUpdate;
    FDmod.qryInsertBooks.SQL.Text := sql;
    FDmod.qryInsertBooks.SQL.EndUpdate;
    FDmod.qryInsertBooks.Transaction := FDmod.BooksDbTx;
    FDmod.qryInsertBooks.DataBase := FDmod.BooksDbConn;
    FDmod.qryInsertBooks.Prepare;

    Write_2_db;

    FDmod.BooksDbTx.Commit;
    //{  the output values clauses are constructed in FOut_values_clauses_list  }
    //Write_2_db(FOut_values_clauses_list);
  finally
    FCSV_col_list.Free;
    FOut_values_clauses_list.Free;
    FDb_col_list.Free;
    FCol_values_list.Free;
    FBooksInputQry.Free;
    FDmod.qryInsertBooks.UnPrepare;
    cmp_list.Free;
	end;
end;

function TStringGridUtil.Make_query_columns: string;
(*
 *    TStringGridUtil.Make_query_columns
 *    Result is the string to be used in the SQL for the columns list.
 *    The elements will be FDb_col_list.colName's
 *)
var
  i, ndx: integer;
  rec: TColumnRec;
  dta_type: string;
  col4cmp, colName : string;
const
  cmp_str_fmt = '%s=%s';
begin
  Result := '';
  FCol_val_cmp_string := '';
  FCol_values_list.Clear;

  for i := 0 to FDb_col_list.Count - 1 do
  begin
    rec := FDb_col_list[i];
    if rec.relativePos >= 0 then   // ignore the BAD_CHOICE's
    begin
      ndx := FDb_col_list.IndexOf_compareStr(rec.colName4cmp, dta_type);
      //Get_index_by_cmp_value(FDb_col_list, rec.colName4cmp);
      // this needs to come from fdb_col_list
      Result := Result + FDb_col_list[ndx].colName + COMMA;

      //FCol_val_cmp_list.Append(FDb_col_list[ndx].colName);
      { FCol_val_cmp_string := FCol_val_cmp_string
                            + FDb_col_list[ndx].colName + COMMA; }

      col4cmp := FDb_col_list[ndx].colName4cmp;
      colName := FDb_col_list[ndx].colName;
      FCol_val_cmp_string := FCol_val_cmp_string
          + Format(cmp_str_fmt, [col4cmp, colName]) + COMMA;
         {  this is for the long version???   }
    end;
  end;
  if Result[Length(Result)] = COMMA then
    Result := OPENPAREN + Result[1..Length(Result) - 1] + CLOSEPAREN;
  if FCol_val_cmp_string[Length(FCol_val_cmp_string)] = COMMA then
    FCol_val_cmp_string := FCol_val_cmp_string[1..Length(FCol_val_cmp_string) - 1];
end;

function format_date(replace_str: string): string;
    (*
     * this is where replace_str has to be formatted, if necessary.
     *   '0000-00-00 00:00:00'
     *)
const
  mysql_date_fmt = '%4.4d-%2.2d-%2.2d';
var
  space_pos: integer;
  work_str, S: string;
  slash_pos, day, month, year: integer;

begin
  space_pos := Pos(SPACE, replace_str);
  work_str := Copy(replace_str, 1, space_pos);
  slash_pos := Pos(FWDSLASH, work_str);
  if slash_pos > 0 then
  begin
    s := Copy(work_str, 1, slash_pos - 1);
    if Length(s) < 1 then
      Exit;
    month := StrToInt(s);

    work_str := Copy(work_str, slash_pos + 1);
    slash_pos := Pos(FWDSLASH, work_str);

    s := Copy(work_str, 1, slash_pos - 1);
    day := StrToInt(s);

    work_str := Copy(work_str, slash_pos + 1);
    s := Trim(Copy(work_str, 1));
    year := StrToInt(s);
    Result := Format(mysql_date_fmt, [year, month, day]);
  end
  else
    Result := DEFAULT_DATE;
end;

function fix_decimal(replace_str: string): string;
var
  i: integer;
begin
  Result := replace_str;
  if replace_str = '' then
    Result := '0.00';
end;


function replace_CR_and_commas(replace_str: string): string;
begin
  Result := replace_str;
  Result := ReplaceStr(Result, CR, ' ');
  Result := ReplaceStr(Result, COMMA, ' ');
end;

function TStringGridUtil.String_together_values_into_list : TColumnValuesList;
var
  row, col, i: integer;
  row_count, col_count : integer;
  value_str, cmp_str: string;
  dta_type: string;
  out_col_values_str: string;
  out_col_slst : TStringList;
  replace_str: string;
begin
  row := 1;
  FCol_values_list.Clear;

  row_count := FGrid.RowCount;
  col_count := FGrid.ColCount;
  try
    { we have the CSV column name; therefore, we can get the colName4cmp from
      the FCSV_col_list param
        WE DON'T NEED IT. WE HAVE IT IN THE FCol_val_cmp_string string.
    }

    while row < FGrid.RowCount do
    begin
      col := 0;
      if FGrid.Cells[0, row] = '' then
      begin
        Inc(row);
        Continue;
      end;

      out_col_slst := TStringList.Create;

      out_col_values_str := '';

      while col < FGrid.ColCount do
      begin
        cmp_str := Get_cmp_value_by_gridPos(FCSV_col_list, col, dta_type);

        replace_str := FGrid.Cells[col, row];

        if replace_str = '' then
          replace_str := 'null'
        else if LowerCase(RightStr(dta_type, 4)) = 'char' then
          replace_str := replace_CR_and_commas(replace_str)
        else if LowerCase(LeftStr(dta_type, 4)) = 'date' then
          replace_str := format_date(replace_str)
        else if LowerCase(dta_type) = 'decimal' then
          replace_str := fix_decimal(replace_str);


        replace_str := Concat(cmp_str, '=', replace_str);  //AnsiQuotedStr(replace_str, '"'));
        out_col_slst.Add(replace_str);

        //out_col_values_str := StringReplace(out_col_values_str, cmp_str, replace_str, []);

        Inc(col);
      end;

      // add the column values list  to the list of stringlists
      FCol_values_list.Add(out_col_slst);
      Inc(row);
    end;
    Result := FCol_values_list;

  finally
  end;
end;

function TStringGridUtil.String_together_values_clauses(dummy : Integer): TStringList;

  procedure out_array_stringReplace(var ara: TStringArray;
    cmp_ara: TStringArray; cmp_str, replace_str: string);
  var
    i: integer;
  begin
    for i := 0 to Length(cmp_ara) - 1 do
      if cmp_ara[i] = cmp_str then
      begin
        ara[i] := replace_str;
        Exit;
      end;
  end;

var
  row, col, i: integer;
  value_str, cmp_str: string;
  dta_type: string;
  out_col_values_str: string;
  out_col_array, cmp_ara: TStringArray;
  replace_str: string;
begin
  row := 1;
  SetLength(out_col_array, 0);
  SetLength(cmp_ara, 0);
  try
    { we have the CSV column name; therefore, we can get the colName4cmp from
      the FCSV_col_list param
        WE DON'T NEED IT. WE HAVE IT IN THE FCol_val_cmp_string string.
    }

    while row < FGrid.RowCount do
    begin
      col := 0;
      if FGrid.Cells[0, row] = '' then
      begin
        Inc(row);
        Continue;
      end;

      out_col_values_str := '';
      out_col_array := SplitString(FCol_val_cmp_string, COMMA);
      cmp_ara := SplitString(FCol_val_cmp_string, COMMA);
      while col < FGrid.ColCount do
      begin
        cmp_str := Get_cmp_value_by_gridPos(FCSV_col_list, col, dta_type);
        replace_str := replace_CR_and_commas(FGrid.Cells[col, row]);

        //if LowerCase(RightStr(dta_type, 4)) = 'char' then
        if LowerCase(LeftStr(dta_type, 4)) = 'date' then
          replace_str := format_date(replace_str)
        else
        if LowerCase(dta_type) = 'decimal' then
          replace_str := fix_decimal(replace_str);

        replace_str := AnsiQuotedStr(replace_str, '"');

        out_array_stringReplace(out_col_array, cmp_ara, cmp_str, replace_str);
        //out_col_values_str := StringReplace(out_col_values_str, cmp_str, replace_str, []);

        Inc(col);
      end;
      out_col_values_str := '';
      // gather column values into values clause
      for i := 0 to Length(out_col_array) - 1 do
        out_col_values_str := out_col_values_str + out_col_array[i] + COMMA;

      // strip off the trailing COMMA
      if out_col_values_str[Length(out_col_values_str)] = COMMA then
        out_col_values_str := out_col_values_str[1..Length(out_col_values_str) - 1];

      // enclose the values string/list in parens
      out_col_values_str := OPEN_PAREN + out_col_values_str + CLOSE_PAREN;

      // add that values clause to the list
      FOut_values_clauses_list.Add(out_col_values_str);
      Inc(row);
    end;
    Result := FOut_values_clauses_list;

  finally
    SetLength(out_col_array, 0);
    SetLength(cmp_ara, 0);
  end;
end;

function TStringGridUtil.Get_DB_dta_type(dtype: string): TFieldType;
begin
  if LowerCase(RightStr(dtype, 4)) = 'char' then
    Result := ftString
  else
  if LowerCase(LeftStr(dtype, 4)) = 'date' then
    Result := ftDate
  else
  if LowerCase(dtype) = 'decimal' then
    Result := ftCurrency
  else
  if LowerCase(dtype) = 'integer' then
    Result := ftInteger
  else
    Result := ftUnknown;

end;


procedure TStringGridUtil.pop_parms(var qry : TSQLQuery; vals_list, cmp_list : TStrings);
var
  i, iPrm : Integer;
  idx : Integer;
  parmName, s_cmp, cols_str : String;
  compare_value, value : String;
  rec : TColumnRec;
begin
  // cols_str = '"msku=""1027363840""","title=""The Predators""",...
  cols_str := vals_list.CommaText;

  cols_str := vals_list.CommaText;
  for i := 0 to cmp_list.Count -1 do
    begin
      compare_value := TStringList(cmp_list).Names[i];
      parmName := TStringList(cmp_list).Values[compare_value];
      //idx := FDb_col_list.IndexOf_ColName();

      value := vals_list.Values[compare_value];

      if value = 'null' then
      begin
        // get the datatype
        idx := FDb_col_list.IndexOf_ColName(parmName);
        rec := FDb_col_list[idx];
        value := '';
        if RightStr(rec.data_type, 4) = 'date' then
          value := DEFAULT_DATE
        else if rec.data_type = 'decimal' then
          value := '0.0';;
			end;

			qry.ParamByName(parmName).AsString := value;
		end;
end;

function TStringGridUtil.Get_InvLab_insert_sql : String;
const
  INSRT_STR = 'INSERT INTO inventory_lab_data (%s) VALUES (%s)';
var
  iPos, i : Integer;
  work_s, s, col_str, parm_str : String;
  slst : TStringList ;
begin
  Result := '';
  col_str := '';
  parm_str := '';
  slst := TStringList.Create;

  try
  {   FCol_val_cmp_string is comma separated string with the
      column names of both the string grid
      and the inventory_lab_data table  }
    begin
      slst.CommaText := FCol_val_cmp_string;

      for i := 0 to slst.Count -1 do
        begin
          iPos := Pos('=', slst[i]);
          s := Copy(slst[i], iPos +1);
          col_str := col_str + s + COMMA;
          parm_str := parm_str + COLON + s + COMMA;
				end;
		end;

    if col_str[Length(col_str)] = COMMA then
      col_str := col_str[1..Length(col_str) -1];

    if parm_str[Length(parm_str)] = COMMA then
      parm_str := parm_str[1..Length(parm_str) -1];

    Result := Format(INSRT_STR, [col_str, parm_str]);

	finally
    slst.Free;
	end;

end;

procedure TStringGridUtil.Write_2_db;
(*
 *  TStringGridUtil.Write_2_db:
 *  Precondition: FDmod.qryInsertBooks is prepared with insert sql
 *)
var
  qry : TSQLQuery;
  row, col, i : Integer;
  vals_lst : TStrings;
  s : String;
  cmp_list : TStringList;
  values_list : TStrings;
begin
  qry := FDmod.qryInsertBooks;

  cmp_list := TStringList.Create;

  try
    cmp_list.CommaText := FCol_val_cmp_string;
    //values_list := cmp_list.Values;
   // for i := 0 to cmp_list.Count do
   //   begin
   //
			//end;
    {   FCol_values_list is list of TStringList's which are key=value pairs   }
    for row := 0 to FCol_values_list.Count -1 do
    begin
      vals_lst := FCol_values_list[row];
      for col := 0 to vals_lst.Count do
        pop_parms(qry, vals_lst, cmp_list);
      qry.ExecSQL;
		end;

	finally
    cmp_list.Free;
	end;

end;


procedure TStringGridUtil.Write_2_db(qry: TSQLQuery; value_string: string);
  // VAL_CLAUSE_DELIMITER
  // FQry_cols_string
var
  str: string;

begin

  qry := TSQLQuery.Create(nil);
  qry.DataBase := dmod.BooksDbConn;
  try
    //qry.SQL.Text := Format(sql_str, [FQry_cols_string, value_string]);
    dmod.BooksDbTx.StartTransaction;

    try
      qry.ExecSQL;
      dmod.BooksDbTx.Commit;
    except
      on Ex: Exception do
      begin
        ShowMessage('TStringGridUtil.Write_2_db exception: ' + Ex.Message);
        dmod.BooksDbTx.Rollback;
        raise;
      end;
    end;
  finally
    qry.Free;
  end;
end;

constructor TStringGridUtil.Create;
begin
  inherited Create;
  end;

destructor TStringGridUtil.Destroy;
begin
  inherited Destroy;
end;

procedure TStringGridUtil.BeforeDestruction;
begin
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
