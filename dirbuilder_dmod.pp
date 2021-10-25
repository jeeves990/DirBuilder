unit DirBuilder_dmod;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, csvdataset, DB, mysql57conn, SQLDB, Dialogs,
  Controls,
  Windows, IniPropStorage;

type

  TParmRec = record
    delimiter : Char;
    withHeader : Boolean;
    addRows : Boolean;
    ignoreFirstLine : Boolean;
  end;

  { TDirBuilder_dataModule }

  TDirBuilder_dataModule = class(TDataModule)
    CSVDataset: TCSVDataset;
    CSVDsDtaSrc: TDataSource;
    BooksDbConn: TMySQL57Connection;
    BooksDbTx: TSQLTransaction;
		imgList : TImageList;
		qryInsertBooks : TSQLQuery;
		qryWork : TSQLQuery;
    procedure DataModuleCreate(Sender: TObject);
  private
    FFileName : TFileName;
  public
    function open_DataSet(const fileName : TFileName; ColNames1stLine : Boolean = False) : Boolean;
    property FileName : TFileName read FFileName write FFileName;
    procedure setFieldNames;
		procedure ReadCSVWithMemDataset(const __fileName: TFileName);
  end;

var
  DirBuilder_dataModule: TDirBuilder_dataModule;

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
  CRLF = CR +LF;
  BACKQUOTE = #96;
  SINGLEQUOTE = #39;
  DOUBLEQUOTE = #34;
  FWDSLASH =  #47;
  BACKSLASH =  #92;
  OPENPAREN = #40;
  CLOSEPAREN = #41;
  QUOTES_SET = [BACKQUOTE, SINGLEQUOTE, DOUBLEQUOTE];

  SPACE = #32;
  PROPSTORAGE_FILENAME = 'DirBuilder.xml';

procedure Write_SQL_Qry_to_CSV(qry : TSQLQuery; const fileName : TFileName);

implementation

procedure Write_SQL_Qry_to_CSV(qry : TSQLQuery; const fileName : TFileName);
{   Precondition: qry parm is open   }
var
  fs : TFileStream;
  fldIter : Integer;
  fldVal, outS : String;
begin
  fs := TFileStream.Create(fileName, fmOpenWrite);
  try
    qry.First;
    while not qry.EOF do
    begin
      outS := '';
      for fldIter := 0 to qry.Fields.Count -1 do
      begin
        fldVal := AnsiQuotedStr(qry.Fields[fldIter].AsString, '"');
        outS := outS + fldVal + COMMA;
			end;
      outS := Copy(outS, 1, Length(outS) -1);
      fs.Write(outS, Length(outS));
			qry.Next;
		end;
	finally
    fs.Free;
	end;
end;

{$R *.lfm}

{ TDirBuilder_dataModule }

procedure TDirBuilder_dataModule.DataModuleCreate(Sender: TObject);
var
  b : Boolean;
begin
   CSVDataset.Close;
end;

procedure TDirBuilder_dataModule.ReadCSVWithMemDataset(
			const __fileName: TFileName);begin

end;

function TDirBuilder_dataModule.open_DataSet(const fileName : TFileName;
                    ColNames1stLine : Boolean = False) : Boolean;
var
  s, fldDef, fldVal : string;
  recCnt, i, __i : Integer;
  b : Boolean;
begin
  s := 'TDirBuilder_dataModule.open_DataSet has failed.'
                       +sLineBreak +'%s';
  try
    b := CSVDataset.DefaultFields;
    CSVDataset.Close;
    CSVDataset.Clear;
    CSVDataset.FieldDefs.Clear;
    __i := CSVDataset.FieldDefs.Count;
    CSVDataset.FileName := fileName;
    CSVDataset.Open;

    if ColNames1stLine then
    begin
      CSVDataset.DisableControls;
      CSVDataset.First;
      try
		    for i := 0 to CSVDataset.FieldCount -1 do
		    begin
          fldDef := CSVDataset.FieldDefs[i].DisplayName;
          fldVal := CSVDataset.Fields[i].Value;
          CSVDataset.FieldDefs[i].Name:=fldDef;
		      CSVDataset.FieldDefs[i].DisplayName := CSVDataset.Fields[i].Value;
          fldDef := CSVDataset.FieldDefs[i].Name;
  		    // TODO: this fails when a differently formatted csv file is read
        end;
		  finally
        CSVDataset.EnableControls;
      end;
		end;

    if not CSVDataset.Active then
      raise Exception.Create('open_DataSet method: dataset is not open. Cause unknown');
    //__i := CSVDataset.FieldDefs.Count;
    //recCnt := CSVDataset.RecordCount;
  except on e : Exception do
    ShowMessage(Format(s, [e.Message]));

  end;
end;

procedure TDirBuilder_dataModule.setFieldNames;
var
  i : Integer;
  bkmk : TBookMark;
begin

  CSVDataset.DisableControls;
  bkmk := CSVDataset.Bookmark;
  bkmk := CSVDataset.GetBookmark;
  try
    CSVDataset.First;
    for i := 0 to CSVDataset.FieldCount -1 do
    begin
      CSVDataset.FieldDefs.Items[i].Name := CSVDataset.Fields[i].Value;
    end;
  finally
    CSVDataset.EnableControls;
    CSVDataset.GotoBookmark(bkmk);
  end;
end;

end.




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
    for i := 0 to CSVDataset.FieldCount -1 do
    begin
      fldDef := CSVDataset.FieldDefs[i].DisplayName;
      fldVal := CSVDataset.Fields[i].Value;
      CSVDataset.FieldDefs[i].Name:=fldDef;
      CSVDataset.FieldDefs[i].DisplayName := CSVDataset.Fields[i].Value;
      fldDef := CSVDataset.FieldDefs[i].Name;
	    // TODO: this fails when a differently formatted csv file is read
    end;
  finally
    CSVDataset.EnableControls;
  end;
end;
recCnt := CSVDataset.RecordCount;
except on e : Exception do
ShowMessage(Format(s, [e.Message]));

end;              CSVDataset.Close;
    CSVDataset.Clear;
    CSVDataset.FieldDefs.Clear;
    CSVDataset.FileName := fileName;
    CSVDataset.Open;

    if ColNames1stLine then
    begin
      CSVDataset.DisableControls;
      CSVDataset.First;
      try
		    for i := 0 to CSVDataset.FieldCount -1 do
		    begin
          fldDef := CSVDataset.FieldDefs[i].DisplayName;
          fldVal := CSVDataset.Fields[i].Value;
          CSVDataset.FieldDefs[i].Name:=fldDef;
		      CSVDataset.FieldDefs[i].DisplayName := CSVDataset.Fields[i].Value;
          fldDef := CSVDataset.FieldDefs[i].Name;
  		    // TODO: this fails when a differently formatted csv file is read
        end;
		  finally
        CSVDataset.EnableControls;
      end;
		end;
    recCnt := CSVDataset.RecordCount;
  except on e : Exception do
    ShowMessage(Format(s, [e.Message]));

  end;
