unit DirBuilder_dmod;

{$mode objfpc}{$H+}
{MODESWITCH typehelpers}
{MODESWITCH advancedrecords}
//{$TYPEINFO ON}
{M+}

interface

uses
  Classes, SysUtils, csvdataset, DB, mysql57conn, SQLDB, Dialogs,
  Controls, LMessages, Grids, fgl,
  IniPropStorage;

const
  {  the following are for frmDirFromCSV and its call to frmChangeCSVProperties  }
  ChoseTab = LM_USER + 9;
  ChoseSemicolon = LM_USER + 59;
  ChoseComma = LM_USER + 44;
  ChoseNoChoice = LM_USER;
  ChoseLF = LM_USER + 10;
  ChoseCRLF = LM_USER + 13;

  ChgParser_RowDelim = LM_USER + 201;
  ChgParser_ColDelim = LM_USER + 202;

const
  SQL4BOOKS = 'SELECT * FROM BOOKS';
  HYPHEN = #45;
  SEMICOLON = #59;
  COLON = #58;
  UNDERSCORE = #95;
  EQUALS = #61;
  COMMA = #44;
  TAB = #09;
  CR = #13;
  LF = #10;
  LF_STR = 'LF';
  CRLF = CR + LF;
  CRLF_STR = 'CRLF';
  OPEN_PAREN = '(';
  CLOSE_PAREN = ')';

  BACKQUOTE = #96;
  SINGLEQUOTE = #39;
  DOUBLEQUOTE = #34;
  FWDSLASH = #47;
  BACKSLASH = #92;
  OPENPAREN = #40;
  CLOSEPAREN = #41;
  QUOTES_SET = [BACKQUOTE, SINGLEQUOTE, DOUBLEQUOTE];

  SPACE = #32;
  PROPSTORAGE_FILENAME_INI = 'DirBuilder.ini';
  BAD_CHOICE = -(MaxInt - 1);

  DEF_CELL_BORDER = 10;

  LOWERCASE_ALPHA = ['a'..'z'];
  UPPERCASE_ALPHA = ['A'..'Z'];
  ALPHA_CHARS = LOWERCASE_ALPHA + UPPERCASE_ALPHA;

type
  { TColumnRec }

  TColumnRec = class
    colName: string;       // name from list of CSV or DB column names
    colName4cmp: string;   // cleanUpString for comparison
    relativePos: integer;  // position of colName4cmp in 'other' list
    data_type: string;
    csv_pos: integer;     // column position in csv grid
    function Copy_rec(aRec: TColumnRec): TColumnRec;
    procedure Swap_rec(fromRec, toRec: TColumnRec);
  end;

  (*  there will be two (2) lists: on for column names from the CSV; the other
     *  for column names from the database 'books' table. The two lists will be
     *  compare on colName4cmp strings and relativePos will be the position of
     *  one list in the other list.
     *)

  TColumnList = specialize TFPGObjectList<TColumnRec>;
  TColumnValuesList = specialize TFPGObjectList<TStringList>;

  { TColumnListHelper }

  TColumnListHelper = class helper for TColumnList
  private
    procedure Swap_list(fromLst, toLst: TColumnList);
  public
    //procedure Clear;
    function IndexOf_compareStr(const compareStr : string; out data_type : String
					) : integer;
    function IndexOf_ColName(const colName: string): integer;
    function IndexOf_RelativePos(const thePos: integer): integer;

    function SortOn_ColName: TColumnList;
    function SortOn_ColCmp : TColumnList;
    function SortOn_DataType: TStringList;
    procedure SortOn_RelPos;

    procedure Fix_Relative_Positions(var csv_list : TColumnList);
  end;

type
  TParmRec = record
    colDelimiter: ansistring;
    rowDelimiter: ansistring;
    addRows: boolean;
    ignoreFirstLine: boolean;
  end;

type  { TDirBuilder_dataModule }

  TDirBuilder_dataModule = class(TDataModule)
    CSVDataset: TCSVDataset;
    CSVDsDtaSrc: TDataSource;
    BooksDbConn: TMySQL57Connection;
    BooksDbTx: TSQLTransaction;
    imgList: TImageList;
    IniPropStorage: TIniPropStorage;
    qryInsertBooks: TSQLQuery;
    qryWork: TSQLQuery;
    procedure DataModuleCreate(Sender: TObject);
  private
    FFileName: TFileName;
    //FColName_list: TColumnList;
    procedure Close_books_db;
    function Open_books_db: boolean;
  public
    function open_CSV_dataset(const fileName: TFileName;
      ColNames1stLine: boolean = False): boolean;
    property FileName: TFileName read FFileName write FFileName;
    procedure setFieldNames;
    procedure Get_list_from_books_table(const db_name, table_name : string;
					var db_col_list : TColumnList);
  end;

type
  TStringCallbackMethod = procedure(str: ansistring) of object;
  TColumnListCallbackMethod = procedure(lst: TColumnList) of object;
  TBooleanCallbackMethod = procedure(b : Boolean) of object;

var
  DirBuilder_dataModule: TDirBuilder_dataModule;
  Prop_storage_ini: string;
  Report_type_list: TStringList;


procedure Write_SQL_Qry_to_CSV(qry: TSQLQuery; const fileName: TFileName);
function Get_cmp_value_by_gridPos(lst: TColumnList; Value: integer;
      out dta_type : string): string;
function cleanUpString(str: string): string;

type
  TShellSortItem = integer;

procedure ShellSort(var a: array of TShellSortItem);
//function is_stringGrid_empty(sg : TStringGrid) : Boolean;

implementation

uses StringGridUtil;

{$R *.lfm}

function cleanUpString(str: string): string;
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

procedure Write_SQL_Qry_to_CSV(qry: TSQLQuery; const fileName: TFileName);
{   Precondition: qry parm is open   }
var
  fs: TFileStream;
  fldIter: integer;
  fldVal, outS: string;
begin
  fs := TFileStream.Create(fileName, fmOpenWrite);
  try
    qry.First;
    while not qry.EOF do
    begin
      outS := '';
      for fldIter := 0 to qry.Fields.Count - 1 do
      begin
        fldVal := AnsiQuotedStr(qry.Fields[fldIter].AsString, '"');
        outS := outS + fldVal + COMMA;
      end;
      outS := Copy(outS, 1, Length(outS) - 1);
      fs.Write(outS, Length(outS));
      qry.Next;
    end;
  finally
    fs.Free;
  end;
end;

function Get_index_by_cmp_value(lst: TColumnList; Value: string): integer;
var
  i: integer;
  rec: TColumnRec;
begin
  Value := LowerCase(Value);
  for i := 0 to lst.Count - 1 do
  begin
    if Lowercase(lst[i].colName4cmp) = Value then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function Get_cmp_value_by_gridPos(lst: TColumnList; Value: integer;
      out dta_type : string): string;
var
  i: integer;
  rec: TColumnRec;
begin
  for i := 0 to lst.Count - 1 do
  begin
    if lst[i].csv_pos = Value then
    begin
      rec := lst[i];
      Result := rec.colName4cmp;
      dta_type :=  rec.data_type;
      Break;
    end;
  end;
end;

procedure ShellSort(var a: array of TShellSortItem);
var
  i, j, h, n, v: integer;
begin
  n := length(a);
  h := 1;
  repeat
    h := 3 * h + 1
  until h > n;
  repeat
    h := h div 3;
    for i := h + 1 to n do
    begin
      v := a[i];
      j := i;
      while (j > h) and (a[j - h] > v) do
      begin
        a[j] := a[j - h];
        j := j - h;
      end;
      a[j] := v;
    end
  until h = 1;
end;

{ TColumnRec }

function TColumnRec.Copy_rec(aRec: TColumnRec): TColumnRec;
var
  bRec: TColumnRec;
begin
  Result := nil;
  bRec := TColumnRec.Create;
  bRec.colName := aRec.colName;
  bRec.colName4cmp := aRec.colName4cmp;
  bRec.relativePos := aRec.relativePos;
  bRec.data_type := aRec.data_type;
  Result := bRec;
end;

procedure TColumnRec.Swap_rec(fromRec, toRec: TColumnRec);
var
  swp_rec: TColumnRec;
begin
  try
    swp_rec := Copy_rec(fromRec);  // copy of fromRec
    // copy toRec --> fromRec
    fromRec.colName := toRec.colName;
    fromRec.colName4cmp := toRec.colName4cmp;
    fromRec.relativePos := toRec.relativePos;
    fromRec.data_type := toRec.data_type;

    // copy swp_rec<fromRec>  --> toRec
    toRec.colName := swp_rec.colName;
    toRec.colName4cmp := swp_rec.colName4cmp;
    toRec.relativePos := swp_rec.relativePos;
    toRec.data_type := swp_rec.data_type;

  finally
    swp_rec.Free;
  end;
end;

{ TColumnListHelper }

procedure TColumnListHelper.Swap_list(fromLst, toLst: TColumnList);
(*
 *    TColumnListHelper.Swap_list presumes fromLst and toLst are same length
 *)
var
  i: integer;
begin
  for i := 0 to fromLst.Count - 1 do
    self[i].Swap_rec(fromLst[i], toLst[i]);
end;

//procedure TColumnListHelper.Clear;
//var
//  i : Integer;
//begin
//  inherited Clear;
//  if (self = nil) or (self.Count = 0) then
//    Exit;
//
//  i := 0;
//  try
//
//    for i := self.Count -1 downto 0 do
//    begin
//      self[i].Free;
//      self.Delete(i);
//		end;
//
//  except on Ex : Exception do
//	   ShowMessage('TColumnListHelper.Clear: ' +Ex.Message);
//	end;
//end;

function TColumnListHelper.SortOn_ColName: TColumnList;
var
  i, dx: integer;
  slst: TStringList;
  new_lst: TColumnList;
begin
  if self.Count = 0 then
    Exit;
  Result := TColumnList.Create(True);
  slst := TStringList.Create;
  try
    for i := 0 to self.Count - 1 do ;
    begin
      dx := slst.Add(self[i].colName);
      slst.Objects[dx] := TObject(PtrUInt(i));
    end;
    slst.Sort;

    new_lst := TColumnList.Create(True);
    for i := 1 to slst.Count - 1 do
    begin
      dx := self.IndexOf_RelativePos(PtrInt(slst.Objects[i]));
      // add to new_lst a copy of TColumnRec from
      // self in relativePos order
      new_lst.Add(self[dx].Copy_rec(self[dx]));
    end;
    // swap the TColumnRec's in new_lst back to self
    for i := 0 to new_lst.Count - 1 do
      self[i].Swap_rec(new_lst[i], self[i]);
  finally
    new_lst.Free;
    slst.Free;
  end;
end;

function TColumnListHelper.SortOn_ColCmp: TColumnList;
var
  i, dx: integer;
  slst: TStringList;
  new_lst: TColumnList;
begin
  if self.Count = 0 then
    Exit;
  Result := TColumnList.Create(True);
  slst := TStringList.Create;
  try
    for i := 0 to self.Count - 1 do ;
    begin
      dx := slst.Add(self[i].colName4cmp);
      slst.Objects[dx] := TObject(PtrUInt(i));
    end;
    slst.Sort;

    new_lst := TColumnList.Create(True);
    for i := 1 to slst.Count - 1 do
    begin
      dx := self.IndexOf_RelativePos(PtrInt(slst.Objects[i]));
      // add to new_lst a copy of TColumnRec from
      // self in relativePos order
      new_lst.Add(self[dx].Copy_rec(self[dx]));
    end;
    // swap the TColumnRec's in new_lst back to self
    for i := 0 to new_lst.Count - 1 do
      self[i].Swap_rec(new_lst[i], self[i]);
  finally
    new_lst.Free;
    slst.Free;
  end;
end;

function TColumnListHelper.SortOn_DataType: TStringList;
begin

end;

procedure TColumnListHelper.SortOn_RelPos;
var
  srt_array: array of TShellSortItem;
  i, dx: integer;
  new_list: TColumnList;
begin
  if self.Count = 0 then
    Exit;
  SetLength(srt_array, self.Count);
  for i := 0 to self.Count - 1 do
    srt_array[i + 1] := self[i].relativePos;
  ShellSort(srt_array);  // srt_array is sorted self[i].relativePos's

  new_list := TColumnList.Create(True);
  try
    for i := 1 to Length(srt_array) do
    begin
      dx := self.IndexOf_RelativePos(srt_array[i]);
      // add to new_list a copy of TColumnRec from
      // self in relativePos order
      new_list.Add(self[dx].Copy_rec(self[dx]));
    end;
    // swap the TColumnRec's in new_list back to self
    for i := 0 to new_list.Count - 1 do
      self[i].Swap_rec(new_list[i], self[i]);
  finally
    new_list.Free;
  end;
end;

procedure TColumnListHelper.Fix_Relative_Positions(var csv_list : TColumnList);
var
  i, rtrn : Integer;
  csv_count, my_count : Integer;
  inRec, outRec : TColumnRec;
  dta_type : String;
begin
  csv_count := csv_list.Count;
  my_count := self.Count;
  for i := 0 to csv_count -1 do
  begin
    inRec := csv_list[i];
    rtrn   := self.IndexOf_compareStr(inRec.colName4cmp, dta_type);
    if rtrn >= 0 then
    begin
      inRec.relativePos := rtrn;
      inRec.data_type := dta_type;
		end
		else
      inRec.relativePos := BAD_CHOICE;
	end;
  // self is FDb_col_list
  for i := 0 to self.Count -1 do
  begin
    outRec := self[i];
    rtrn := csv_list.IndexOf_compareStr(outRec.colName4cmp, dta_type);
    if rtrn >= 0 then
    begin
      outRec.relativePos := rtrn;
      outRec.data_type := dta_type;
		end
		else
      outRec.relativePos := BAD_CHOICE;
	end;
end;

function TColumnListHelper.IndexOf_ColName(const colName: string): integer;
  // TODO
var
  dx: integer;
  rec : TColumnRec;
begin
  Result := -1;
  if (self.List = nil) or (self.Count <= 0) then   // no elements have been added??
    Exit;

  for dx := 0 to self.Count - 1 do
    begin
      rec := self[dx];
      if LowerCase(colName) = LowerCase(rec.colName) then
      begin
        Result := dx;
        Exit;
			end;
		end;

end;

function TColumnListHelper.IndexOf_RelativePos(const thePos: integer): integer;
  // DONE
var
  dx: integer;
begin
  Result := -1;
  if self.Count = 0 then
    Exit;
  for dx := 0 to self.Count - 1 do
    if thePos = self[dx].relativePos then
    begin
      Result := dx;
      Exit;
    end;
end;

function TColumnListHelper.IndexOf_compareStr(const compareStr: string;
                        out data_type : String): integer;
  // DONE
var
  dx: integer;
  rec: TColumnRec;
begin
  Result := -1;
  for dx := 0 to self.Count - 1 do
  begin
    rec := self[dx];
    if LowerCase(compareStr) = LowerCase(rec.colName4cmp) then
    begin
      Result := dx;
      data_type := rec.data_type;
      Exit;
    end;
  end;
end;

{ TDirBuilder_dataModule }
procedure TDirBuilder_dataModule.Get_list_from_books_table(const db_name,
			table_name : string; var db_col_list : TColumnList);
(*   TDirBuilder_dataModule.Get_list_from_books_table
 *    caller is responsible for freeing return'd list.
 *)
const
  SQL_4_COL_NAMES = 'SELECT COLUMN_NAME, DATA_TYPE '
        +'FROM INFORMATION_SCHEMA.COLUMNS '
        +'WHERE TABLE_SCHEMA = "%s" AND TABLE_NAME = "%s" '
        +'ORDER BY column_name';

var
  sql: string;
  qry: TSQLQuery;
  rec : TColumnRec;
  iCheck : Integer;
begin
  sql := Format(SQL_4_COL_NAMES, [db_name, table_name]);

  db_col_list.Clear;
  if not Open_books_db then
    raise Exception.Create(
      'TDirBuilder_dataModule.get_col_names: failed opening connection');
  qry := TSQLQuery.Create(self);
  qry.SQL.Text := sql;
  try
    qry.DataBase := BooksDbConn;
    qry.Open;
    while not qry.EOF do
    begin
      rec := TColumnRec.Create;
      rec.colName := LowerCase(qry.Fields[0].AsString);

      // ensure a unique list
       if db_col_list.IndexOf_ColName(rec.colName) < 0 then
      begin
  			rec.colname4cmp := cleanUpString(rec.colName);
        rec.data_type := LowerCase(qry.Fields[1].AsString);
        db_col_list.Add(rec);
			end;

      qry.Next;
    end;
  finally
    qry.Free;
    Close_books_db;
  end;
end;

function TDirBuilder_dataModule.Open_books_db: boolean;
begin
  Result := False;
  if not BooksDbConn.Connected then
    BooksDbConn.Open;
  Result := BooksDbConn.Connected;
end;

procedure TDirBuilder_dataModule.Close_books_db;
begin
  BooksDbConn.Close(True);  // force close
end;

procedure TDirBuilder_dataModule.DataModuleCreate(Sender: TObject);
begin
  CSVDataset.Close;
  BooksDbConn.Close;
  IniPropStorage.IniFileName := PROPSTORAGE_FILENAME_INI;
  //FColName_list := TColumnList.Create(True);
end;

function TDirBuilder_dataModule.open_CSV_dataset(const fileName: TFileName;
  ColNames1stLine: boolean = False): boolean;
var
  s, fldDef: string;
  i: integer;
begin
  s := 'TDirBuilder_dataModule.open_CSV_dataset has failed.' + sLineBreak + '%s';
  try
    //b := CSVDataset.DefaultFields;
    CSVDataset.Close;
    CSVDataset.Clear;
    CSVDataset.FieldDefs.Clear;
    CSVDataset.FileName := fileName;
    CSVDataset.Open;

    if ColNames1stLine then
    begin
      CSVDataset.DisableControls;
      CSVDataset.First;
      try
        for i := 0 to CSVDataset.FieldCount - 1 do
        begin
          fldDef := CSVDataset.FieldDefs[i].DisplayName;
          //fldVal := CSVDataset.Fields[i].Value;
          CSVDataset.FieldDefs[i].Name := fldDef;
          CSVDataset.FieldDefs[i].DisplayName := CSVDataset.Fields[i].Value;
          fldDef := CSVDataset.FieldDefs[i].Name;
          // TODO: this fails when a differently formatted csv file is read
        end;
      finally
        CSVDataset.EnableControls;
      end;
    end;

    if not CSVDataset.Active then
      raise Exception.Create(
        'open_CSV_dataset method: dataset is not open. Cause unknown');
    //__i := CSVDataset.FieldDefs.Count;
    //recCnt := CSVDataset.RecordCount;
  except
    on e: Exception do
      ShowMessage(Format(s, [e.Message]));

  end;
end;

procedure TDirBuilder_dataModule.setFieldNames;
var
  i: integer;
  bkmk: TBookMark;
begin

  CSVDataset.DisableControls;
  bkmk := CSVDataset.Bookmark;
  bkmk := CSVDataset.GetBookmark;
  try
    CSVDataset.First;
    for i := 0 to CSVDataset.FieldCount - 1 do
    begin
      CSVDataset.FieldDefs.Items[i].Name := CSVDataset.Fields[i].Value;
    end;
  finally
    CSVDataset.EnableControls;
    CSVDataset.GotoBookmark(bkmk);
  end;
end;

end.
