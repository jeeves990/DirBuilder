unit frmAddEdit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, DB, Forms, Controls, Graphics, Dialogs, ComCtrls,
  DirBuilder_dmod,
  ActnList, StdCtrls;

type

  { TfmAddEdit }

  TfmAddEdit = class(TForm)
    ActionAddRow: TAction;
    ActionDeleteRow: TAction;
    ActionEditRow: TAction;
    ActionRefresh: TAction;
    Actions: TActionList;
    btnAdd: TToolButton;
    btnEdit: TToolButton;
    btnRefresh: TToolButton;
    btnRemove: TToolButton;
    dtaSrc: TDataSource;
    Edit1: TEdit;
    frameTblimgList: TImageList;
    qry: TSQLQuery;
    ToolBar1: TToolBar;
    tlbtnSep02: TToolButton;
    tlBtnSep01: TToolButton;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    Fdmod : TDirBuilder_dataModule;
    Fsql: String;
    FRec_id : Integer;
    procedure BuildComponents;
  public
    property dmod : TDirBuilder_dataModule read Fdmod write Fdmod;
    property sql : String read Fsql write Fsql;
    property Rec_id : Integer read FRec_id write FRec_id;
  end;

var
  fmAddEdit: TfmAddEdit;

implementation

{$R *.lfm}

{ TfmAddEdit }

const
  FLD_LBL_LEFT = 32;
  FLD_EDT_LEFT = 240;
  FLDS_TOP = 35;
  FLDS_HEIGHT = 35;
{
    procedure TfmAddEdit.BuildComponents;
    Precondition: sql must have all the fields that can possibly be edited
                  -and- only the first row will be edited.
                  qry is open.
}
procedure TfmAddEdit.BuildComponents;
var
  i, flds_position, idx : Integer;
  lbl : TLabel;
  edt : TEdit;
  cap : String;
begin
  flds_position := FLDS_TOP;
  i := 0;
  while i < qry.FieldDefs.Count do
  begin
    flds_position := FLDS_TOP + (FLDS_HEIGHT * i);
    lbl := TLabel.Create(self);
    cap := qry.FieldDefs[i].DisplayName;
    // if cap contains a hyphen, replace with an underscore
    idx := Pos(cap, HYPHEN);
    if idx > 0 then
      cap[idx] := UNDERSCORE;

    lbl.Left := FLD_LBL_LEFT;
    lbl.Top := flds_position;
    lbl.Name := 'lbl_' + caption;

    edt := TEdit.Create(self);
    edt.Left := FLD_EDT_LEFT;
    edt.Top := flds_position;
    edt.Tag := i;             // to find the edt later
    edt.Name := 'edt_' + caption;

    Inc(i);
  end;
end;

procedure TfmAddEdit.FormCreate(Sender: TObject);
begin
  Fdmod := TDirBuilder_dataModule.Create(self);
  qry.DataBase := Fdmod.BooksDbConn;
  qry.Transaction := Fdmod.BooksDbTx;
  {$DEFINE DEBUG}
  {$IFDEF DEBUG}
  qry.SQL.Add(SQL4BOOKS);
  {$ELSE}
  qry.SQL.Add(Fsql);
  {$ENDIF}
  try
    try
      qry.Open;
      BuildComponents;
      except on e : EDatabaseError do
             begin
               ShowMessage(Format('TfmAddEdit.BuildComponents: database failure: %s',
                                                     [e.Message]));
             end;
             on e : Exception do
             begin
               ShowMessage(Format('TfmAddEdit.BuildComponents: unknown failure: %s',
                                                     [e.Message]));
             end;
    end;
  finally
    qry.Close;
  end;

end;

procedure TfmAddEdit.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Fdmod.Free;
  CloseAction := caFree;
end;

end.

