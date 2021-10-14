unit DirBuilder_dmod;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, csvdataset, DB, mysql57conn, SQLDB, Dialogs, IniPropStorage;

type

 //   TmyCSVDataset = class (TCSVDataset)
 //   TCSVDatasetHelper = class helper for TCSVDataset
 //   property _DefaultFields : Boolean read FDefaultFields write FDefaultFields; override;
	//end;

  { TDirBuilder_dataModule }

  TDirBuilder_dataModule = class(TDataModule)
    CSVDataset: TCSVDataset;
    CSVDsDtaSrc: TDataSource;
    BooksDbConn: TMySQL57Connection;
    BooksDbTx: TSQLTransaction;
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

implementation

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
