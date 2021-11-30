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

  TMyListOfStringLists = specialize TFPGObjectList<TStringList>;
  TStringGridUtil = class
  private
    //procedure Write_to_books_controller(cols_list : TStringList;
    //                             val_list : TMyListOfStringLists;
    //                             const PDmod : TDirBuilder_dataModule);
    //procedure buildValuesStrings(valueList : TMyListOfStringLists);
    function isQuoted(Str : String) : Boolean;
    function stripDoubleQuotes(str : String) : String;
    function addUnderscore_to_titles(str : String) : String;
  public
    function WriteGridToBooksDb(parm_dmod : TDirBuilder_dataModule;
					grid : TStringGrid; var pRowCnt : Integer) : Boolean;
	end;



implementation

uses
  Dialogs, SQLDB, frmDirFromCSV, unitLoad_grid_from_csv, StrUtils;

const
  MAX_COLUMNS_LENGTH = 4000;  // max parm length for stored procedure

var
  dmod : TDirBuilder_dataModule;

(******************************************************************************)

{  TStringGridUtil : class }
//procedure TStringGridUtil.Write_to_books_controller(
//                        cols_list : TStringList;
//                        val_list : TMyListOfStringLists;
//                        const PDmod : TDirBuilder_dataModule);


function TStringGridUtil.addUnderscore_to_titles(str : String) : String;
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


function TStringGridUtil.isQuoted(Str : String) : Boolean;
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

function TStringGridUtil.stripDoubleQuotes(str : String) : String;
{   when a field has embedded DOUBLEQUOTE's,
    strip them out    }
begin
  Result := EmptyStr;
  str := StringReplace(str, DOUBLEQUOTE, EmptyStr, [rfReplaceAll]);
  //str := Concat(DOUBLEQUOTE, str, DOUBLEQUOTE);
  Result := str;
end;

function TStringGridUtil.WriteGridToBooksDb(parm_dmod : TDirBuilder_dataModule;
                            grid : TStringGrid;
                            var pRowCnt : Integer) : Boolean;
var
  str : String;
  val_list_o_lists : TMyListOfStringLists;
const
  fcn_name = 'TStringGridUtil.WriteGridToBooksDb';


    function getColumnNames(var col_names : TStringList): Boolean;
    var
      idx : Integer;
    begin
      Result := False;
      if not Assigned(col_names) then
        col_names := TStringList.Create;
      col_names.Clear;

		  for idx := 0 to grid.ColCount -1 do
		  begin
        str := grid.Cells[idx, 0];
        if str > '' then
          Result := True;
        str := stripDoubleQuotes(grid.Cells[idx, 0]);
        str := addUnderscore_to_titles(str);
		    col_names.Add(str);
			end;
		end;


    function set_lower_case(var sLst : TStringList) : Integer;
    // returns list length
    var
      i : Integer;
      sorted : Boolean;
    begin
      sorted := sLst.Sorted;
      sLst.Sorted := False;
      Result := sLst.Count;
      for i := 0 to sLst.Count -1 do
        sLst[i] := LowerCase(sLst[i]);
      slst.Sorted := sorted;
		end;


    function compare_csv_2_db_col_names(csv_names, db_names : TStringlist) : Boolean;
    var
      i, j : Integer;
      csv_str, db_str : String;


      function strings_like(str1, str2 : string) : Boolean;
      begin
        str1 := addUnderscore_to_titles(str1);
        str2 := addUnderscore_to_titles(str1);
			end;


    begin
      for i := 0 to csv_names.Count -1 do
      begin
        csv_str := LowerCase(csv_names[i]);
        db_str := LowerCase(db_names[i]);
        csv_str := soundex(csv_str);
        db_str := soundex(db_str);

        if csv_names[i] = db_names[i] then
          Result := False;
			end;
		end;


var
  csv_col_names_list,
  db_col_names_list : TStringList;
begin

  (******** TODO: do we need to set pRowCnt *********)


  Result := False;
  if not (parm_dmod is TDirBuilder_dataModule) then
    raise Exception.Create('WriteGridToBooksDb: called with wrong datamodule');
  dmod := parm_dmod;

  {  TODO: query the booksdb schema for the columns names and
     compare those to the column names from getColumnNames   }
  csv_col_names_list := nil;
  if not getColumnNames(csv_col_names_list) then   // fill db_col_names_list with column names
    raise EStringGrid_Improper.Create(fcn_name
        + ' First row does not have column names.');
        {  the values in the later lists will be in the same
         logical order as the column name in keys_list    }

  db_col_names_list := dmod.Get_col_names(dmod.BooksDbConn.DatabaseName, 'books');

  set_lower_case(csv_col_names_list);
  csv_col_names_list.Sort;
  set_lower_case(db_col_names_list);
  db_col_names_list.Sort;

  if not compare_csv_2_db_col_names(csv_col_names_list, db_col_names_list) then
    raise EColNamesNotSame.Create(fcn_name
      + ' Failed comparison between csv and database column names');

  {  get the contents of the string grid  }
  val_list_o_lists := TMyListOfStringLists.Create;
  try
    {   builds a TStringList of column values for each row.
        -and- adds the StringList's built to col_list      }
    //buildValuesStrings(val_list_o_lists);

    {   val_list_o_lists now has a string list of
        column values for every row in the grid. }
    //try
    //  AssignFile(fl, 'myLogFile.txt');
    //  Rewrite(fl);
    //  for i := 0 to val_list_o_lists.Count -1 do
    //    WriteLn(fl, val_list_o_lists[i]);
    //  Close(fl);
    //except on E : Exception do
    //  ShowMessage('WriteGridToBooksDb: ' +E.Message);
    //end;
    Result := True;
	finally
    val_list_o_lists.Free;
    if Assigned(db_col_names_list) then
      db_col_names_list.Free;;
	end;
end;  // function WriteGridToBooksDb(parm_dmod : TDirBuilder_dataModule;

end.


