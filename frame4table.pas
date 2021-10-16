unit frame4table;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, Forms, Controls, ExtCtrls, Grids, ActnList,
  StdCtrls, ComCtrls, Menus
  ;

type

  { TframeTable }

  TframeTable = class(TFrame)
    Action1: TAction;
    ActionEditRow: TAction;
    ActionRefresh: TAction;
    ActionDeleteRow: TAction;
    ActionAddRow: TAction;
    Actions: TActionList;
    frameTblimgList: TImageList;
    edTableName: TEdit;
    Label1: TLabel;
    pnlAnnunciator: TPanel;
    pnl: TPanel;
    sGrid: TStringGrid;
    ToolBar1: TToolBar;
    btnAdd: TToolButton;
    btnEdit: TToolButton;
    toolBtnSeparator: TToolButton;
    btnRemove: TToolButton;
    btnRefresh: TToolButton;
    ToolButton1: TToolButton;
    procedure ActionAddRowExecute(Sender: TObject);   virtual;
    procedure ActionDeleteRowExecute(Sender: TObject);    virtual;
    procedure ActionEditRowExecute(Sender: TObject);  virtual;
    procedure ActionRefreshExecute(Sender: TObject);  virtual;
  private
    qry : TSQLQuery;
  public
  end;

implementation

{$R *.lfm}

{ TframeTable }

procedure TframeTable.ActionDeleteRowExecute(Sender: TObject);
begin
  //   virtual
end;

procedure TframeTable.ActionAddRowExecute(Sender: TObject);
begin
//   virtual
end;

procedure TframeTable.ActionEditRowExecute(Sender: TObject);
begin
  //   virtual
end;

procedure TframeTable.ActionRefreshExecute(Sender: TObject);
begin
  //   virtual
end;

end.

