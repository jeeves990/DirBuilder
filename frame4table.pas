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
		ImageList2 : TImageList;
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
		ToolButton2 : TToolButton;
    procedure ActionAddRowExecute(Sender: TObject);
    procedure ActionDeleteRowExecute(Sender: TObject);
    procedure ActionEditRowExecute(Sender: TObject);
    procedure ActionRefreshExecute(Sender: TObject);
  private
    qry : TSQLQuery;
    //function ComponentUnderMouse(_parent : TWinControl = nil): TComponent;
  public
    constructor Create;

  end;

implementation

{$R *.lfm}

uses
  Dialogs, frmNewBooksDB, DirBuilder_dmod, Mouse;

//function TframeTable.ComponentUnderMouse(_parent : TWinControl = nil) : TComponent;
//var
//  ctrl: TComponent;
//  pt: TPoint;
//begin
//  if not Assigned(_parent) then
//    _parent := self.Parent;
//  try
//    pt.X := Mouse.GetMouseX;
//    pt.Y := Mouse.GetMouseY;
//    pt := _parent.ScreenToClient(pt);
//    ctrl := _parent.ControlAtPos(pt, [capfRecursive, capfOnlyWinControls]); //, capfAllowWinControls]);
//    if Assigned(ctrl) then
//      Result := ctrl;
//  except on e: Exception do
//    ShowMessage(e.Message);
//  end;
//end;

{ TframeTable }

procedure TframeTable.ActionDeleteRowExecute(Sender: TObject);
begin
  //   virtual
end;

procedure TframeTable.ActionAddRowExecute(Sender: TObject);
var
  ctrl : TComponent;
  pt : TPoint;
begin
  //ctrl := ComponentUnderMouse(Parent);
  //ShowMessage(ctrl.Name);
  // display a dialog to add the row
  //ActionAddBook.Execute;
end;

procedure TframeTable.ActionEditRowExecute(Sender: TObject);
begin
  //   virtual
end;

procedure TframeTable.ActionRefreshExecute(Sender: TObject);
begin
  //   virtual
end;

constructor TframeTable.Create;
begin
  btnAdd.Hint:='Add a new item to the table';
  btnEdit.Hint:='Edit the current book';
  btnRefresh.Hint:='Refresh the table';
  btnRemove.Hint:='Remove a row from the table';
end;

end.

