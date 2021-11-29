unit DirBuilder_dmod;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, csvdataset, DB, mysql57conn, SQLDB, Dialogs,
  Controls, LMessages,
  //Windows,
//, XMLPropStorage;
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

type
  TParmRec = record
    colDelimiter: ansistring;
    rowDelimiter: ansistring;
    //withHeader : Boolean;      // withHeader and ignoreFirstLine are redundant
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
    procedure Close_books_db;
    function Open_books_db: boolean;
  public
    function open_CSV_dataset(const fileName: TFileName;
      ColNames1stLine: boolean = False): boolean;
    property FileName: TFileName read FFileName write FFileName;
    procedure setFieldNames;
  end;

type
  TStringCallbackMethod = procedure(str : AnsiString) of object;

var
  DirBuilder_dataModule: TDirBuilder_dataModule;
  Prop_storage_ini: string;
  Report_type_list: TStringList;


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

procedure Write_SQL_Qry_to_CSV(qry: TSQLQuery; const fileName: TFileName);

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

{$R *.lfm}

{ TDirBuilder_dataModule }

function TDirBuilder_dataModule.Open_books_db: boolean;
begin
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
  i : integer;
begin
  s := 'TDirBuilder_dataModule.open_CSV_dataset has failed.'
    + sLineBreak + '%s';
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
