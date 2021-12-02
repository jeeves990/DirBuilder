unit DirBuilder_dmod;

{$mode objfpc}{$H+}

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
  ALPHA_CHARS = LOWERCASE_ALPHA +UPPERCASE_ALPHA;

type
  { TColumnRec }

  TColumnRec = class
    colName: string;       // name from list of CSV or DB column names
    colName4cmp: string;   // cleanUpString for comparison
    relativePos: integer;  // position of colName4cmp in 'other' list
    csv_pos : Integer;     // column position in csv grid
  end;

  (*  there will be two (2) lists: on for column names from the CSV; the other
     *  for column names from the database 'books' table. The two lists will be
     *  compare on colName4cmp strings and relativePos will be the position of
     *  one list in the other list.
     *)

  TColumnList = specialize TFPGList<TColumnRec>;


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
    FColName_list : TStringList;
    procedure Close_books_db;
    function Open_books_db: boolean;
  public
    function open_CSV_dataset(const fileName: TFileName;
      ColNames1stLine: boolean = False): boolean;
    property FileName: TFileName read FFileName write FFileName;
    procedure setFieldNames;
    function Get_col_names(const db_name, table_name: string): TStringList;
  end;

type
  TStringCallbackMethod = procedure(str: ansistring) of object;
  TColumnListCallbackMethod = procedure(lst : TColumnList) of object;

var
  DirBuilder_dataModule: TDirBuilder_dataModule;
  Prop_storage_ini: string;
  Report_type_list: TStringList;


procedure Write_SQL_Qry_to_CSV(qry: TSQLQuery; const fileName: TFileName);
function Get_index_by_cmp_value(lst : TColumnList; value : String) : Integer;
function Get_cmp_value_by_gridPos(lst : TColumnList; value : Integer) : String;

//function is_stringGrid_empty(sg : TStringGrid) : Boolean;

implementation

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

function Get_index_by_cmp_value(lst : TColumnList; value : String) : Integer;
var
  i : Integer;
  rec : TColumnRec;
begin
  value := LowerCase(value);
  for i := 0 to lst.Count -1 do
  begin
    if Lowercase(lst[i].colName4cmp) = value then
    begin
      Result := i;
      Break;
		end;
	end;
end;

function Get_cmp_value_by_gridPos(lst : TColumnList; value : Integer) : String;
var
  i : Integer;
  rec : TColumnRec;
begin
  for i := 0 to lst.Count -1 do
  begin
    if lst[i].csv_pos = value then
    begin
      Result := lst[i].colName4cmp;
      Break;
		end;
	end;
end;

{$R *.lfm}

{ TDirBuilder_dataModule }
const
  SQL_4_COL_NAMES = 'SELECT COLUMN_NAME, DATA_TYPE  FROM INFORMATION_SCHEMA.COLUMNS '
    + 'WHERE TABLE_SCHEMA = "%s" AND TABLE_NAME = "%s" ORDER BY column_name';

function TDirBuilder_dataModule.Get_col_names(const db_name, table_name: string)
                                                          : TStringList;
(*   TDirBuilder_dataModule.get_col_names
 *    caller is responsible for freeing return'd list.
 *)
var
  sql, value: string;
  qry: TSQLQuery;
begin
  Result := nil;
  sql := Format(SQL_4_COL_NAMES, [db_name, table_name]);
  if not Assigned(FColName_list) then
    FColName_list := TStringList.Create;
  FColName_list.Clear;
  FColName_list.Sorted := True;
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
      value := LowerCase(qry.Fields[0].AsString);
      if FColName_list.IndexOf(value) < 0 then
          FColName_list.Add(qry.Fields[0].AsString);
      qry.Next;
		end;
    Result := FColName_list;
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
