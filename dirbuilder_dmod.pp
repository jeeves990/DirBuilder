unit DirBuilder_dmod;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, csvdataset, DB, Dialogs;

type

  { TDirBuilder_dataModule }

  TDirBuilder_dataModule = class(TDataModule)
    CSVDataset: TCSVDataset;
    CSVDsDtaSrc: TDataSource;
    procedure DataModuleCreate(Sender: TObject);
  private
    FFileName : TFileName;

  public
    function open_DataSet(const fileName : TFileName) : Boolean;
    property FileName : TFileName read FFileName write FFileName;
    procedure setFieldNames;
  end;

var
  DirBuilder_dataModule: TDirBuilder_dataModule;

implementation

{$R *.lfm}

{ TDirBuilder_dataModule }

procedure TDirBuilder_dataModule.DataModuleCreate(Sender: TObject);
begin
   CSVDataset.Close;
end;

function TDirBuilder_dataModule.open_DataSet(const fileName : TFileName) : Boolean;
var
  s, fldDef : string;
  recCnt, i : Integer;
begin
  s := 'TDirBuilder_dataModule.open_DataSet has failed.'
                       +sLineBreak +'%s';
  try
    CSVDataset.Close;
    CSVDataset.FileName := fileName;
    CSVDataset.Open;
    for i := 0 to CSVDataset.FieldCount -1 do
    begin
      fldDef := CSVDataset.FieldDefs[i].DisplayName;
      // TODO: this fails when a differently formatted csv file is read
    end;
    recCnt := CSVDataset.RecordCount;
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





