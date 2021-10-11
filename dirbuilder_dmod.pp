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
    procedure SetOpenDset(AValue: Boolean);

  public
    function open_DataSet(const fileName : TFileName) : Boolean;
    property open_dset : Boolean write SetOpenDset;
    property FileName : TFileName read FFileName write FFileName;
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

procedure TDirBuilder_dataModule.SetOpenDset(AValue: Boolean);
begin

end;

function TDirBuilder_dataModule.open_DataSet(const fileName : TFileName) : Boolean;
var
  s : string;
begin
  s := 'TDirBuilder_dataModule.open_DataSet has failed.'
                       +sLineBreak +'%s';
  try
    if CSVDataset.Active then
        CSVDataset.Close;
    CSVDataset.Clear;
    CSVDataset.IndexName := '';
    CSVDataset.IndexDefs.Clear;
    CSVDataset.FieldDefs.Clear;
    CSVDataset.FileName := fileName;
    CSVDataset.Open;
  except on e : Exception do
    ShowMessage(Format(s, [e.Message]));

  end;
end;

end.





