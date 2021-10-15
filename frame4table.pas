unit frame4table;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, Forms, Controls, ExtCtrls, Grids, ActnList,
  StdCtrls, ComCtrls, Menus, ECGrid;

type

  { TframeTable }

  TframeTable = class(TFrame)
    ActionAddRow: TAction;
    ActionDeleteRow: TAction;
    ActionEditRow: TAction;
    frameTblimgList: TImageList;
    sGrid: TECGrid;
    edTableName: TEdit;
    Label1: TLabel;
    pnlAnnunciator: TPanel;
    pnl: TPanel;
    ToolBar1: TToolBar;
    bntAdd: TToolButton;
    btnEdit: TToolButton;
    toolBtnSeparator: TToolButton;
    btnRemove: TToolButton;
  private
    qry : TSQLQuery;
  public
    procedure ActionAddRowExecute(Sender: TObject);
    procedure ActionDeleteRowExecute(Sender: TObject);
    procedure ActionEditRowExecute(Sender: TObject);
  end;

implementation

{$R *.lfm}

{ TframeTable }

procedure TframeTable.ActionAddRowExecute(Sender: TObject);
begin
//
end;

procedure TframeTable.ActionDeleteRowExecute(Sender: TObject);
begin
//
end;

procedure TframeTable.ActionEditRowExecute(Sender: TObject);
begin
//
end;

end.

