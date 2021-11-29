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
  Dialogs, SQLDB, frmDirFromCSV, unitLoad_grid_from_csv;

const
  MAX_COLUMNS_LENGTH = 4000;  // max parm length for stored procedure

var
  dmod_unit : TDirBuilder_dataModule;

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
  //colCnt, rowCnt : Integer;
  str : String;
  val_list_o_lists : TMyListOfStringLists;



    procedure getColumnNames(keys_list : TStringList);
    var
      idx : Integer;
      //keys : array of String;
      //sCols : String;
    begin
      if not Assigned(keys_list) then
        keys_list := TStringList.Create;

      idx := keys_list.Count;
      if keys_list.Count > 0 then
        keys_list.Clear;

		  for idx := 0 to grid.ColCount -1 do
		  begin
        str := stripDoubleQuotes(grid.Cells[idx, 0]);
        str := addUnderscore_to_titles(str);
		    keys_list.Add(str);
			end;

		end;


var
  //sValues_list,
  sKeys_list : TStringList;
  //fl : TextFile;
  //B_RESULT : Boolean;
  //i : Integer;
begin

  (******** TODO: do we need to set pRowCnt *********)


  Result := False;
  if not (parm_dmod is TDirBuilder_dataModule) then
    raise Exception.Create('WriteGridToBooksDb: called with wrong datamodule');
  dmod_unit := parm_dmod;

  sKeys_list := TStringList.Create;
  getColumnNames(sKeys_list);   // fill sKeys_list with column names
        {  the values in the later lists will be in the same
         logical order as the column name in keys_list    }


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
    sKeys_list.Free;;
	end;
end;  // function WriteGridToBooksDb(parm_dmod : TDirBuilder_dataModule;

end.


