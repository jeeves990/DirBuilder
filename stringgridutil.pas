unit StringGridUtil;

{ Use csvreadwrite to import csv data into stringgrid. Offers more robust
  support for CSV file variations than TStringGrid.LoadFromCSVFile }

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Grids, // csvreadwrite, LazFileUtils,
  fgl, Contnrs,
  DirBuilder_dmod;

type
  EStringGrid_Improper = TExceptionClass;
  EColNamesNotSame = TExceptionClass;

  { TStringGridUtil }

  TStringGridUtil = class
  private
    function cleanUpString(str: string): string;
    procedure cleanUpColumnList(var lst: TColumnList);
    function isQuoted(Str: string): boolean;
    function stripDoubleQuotes(str: string): string;
    function addUnderscore_to_titles(str: string): string;
  public
    function WriteGridToBooksDb(parm_dmod: TDirBuilder_dataModule;
              grid: TStringGrid)                                            : boolean;
  var
    DbColsCallback : TColumnListCallbackMethod;
    CsvColsCallback : TColumnListCallbackMethod;
  end;



implementation

uses
  Dialogs, SQLDB, frmDirFromCSV, unitLoad_grid_from_csv, StrUtils;

const
  MAX_COLUMNS_LENGTH = 4000;  // max parm length for stored procedure

var
  dmod: TDirBuilder_dataModule;

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

function TStringGridUtil.WriteGridToBooksDb(parm_dmod: TDirBuilder_dataModule;
  grid: TStringGrid): boolean;
var
  str: string;
  csv_col_names_list, db_col_names_list: TColumnList;
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
      col_names.Add(rec);
    end;
  end;


  function cmp_col_names_get_rel_pos(csv_names, db_names: TColumnList): boolean;
    (*
     *  cmp_col_names_get_rel_pos:
     *)
    function get_relative_position(csv_rec : TColumnRec) : Integer;
    var
      i : Integer;
      str : String;
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


    procedure set_db_rec_relativePos(const csv_ndx : Integer; var csv_rec : TColumnRec);
    var
      i : Integer;
      db_rec : TColumnRec;
    begin
      i := csv_rec.relativePos;
      db_rec := TColumnRec(db_col_names_list[i]);
      db_rec.relativePos := csv_ndx;
		end;


  var
    i, j, rtrn: integer;
    db_rowCount, csv_rowCount : Integer;
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

    if db_rowCount > csv_rowCount then
      ShowMessage('Database has more columns than CSV');
  end;

var
  work_list: TStringList;
  rec : TColumnRec;
begin

  (******** TODO: do we need to set pRowCnt *********)

  Result := False;
  work_list := nil;
  if not (parm_dmod is TDirBuilder_dataModule) then
    raise Exception.Create('WriteGridToBooksDb: called with wrong datamodule');
  dmod := parm_dmod;
  try
    {  DONE: query the booksdb schema for the columns names and
       compare those to the column names from getColumnNames   }
    csv_col_names_list := nil;
    if not getColumnNames(csv_col_names_list) then
      // fill db_col_names_list with column names
      raise EStringGrid_Improper.Create(fcn_name +
        ' First row does not have column names.');
          {  the values in the later lists will be in the same
           logical order as the column name in keys_list    }

    work_list := dmod.Get_col_names(dmod.BooksDbConn.DatabaseName, 'books');
    db_col_names_list := add_slst_2_col_list(work_list);

    if not cmp_col_names_get_rel_pos(csv_col_names_list, db_col_names_list) then
      raise EColNamesNotSame.Create(fcn_name +
        ' Failed comparison between csv and database column names');
    DbColsCallback(db_col_names_list);
    CsvColsCallback(csv_col_names_list);
    Result := True;
  finally
    FreeAndNil(csv_col_names_list);
    FreeAndNil(db_col_names_list);
    FreeAndNil(work_list);
  end;
end;  // function WriteGridToBooksDb(parm_dmod : TDirBuilder_dataModule;

end.


