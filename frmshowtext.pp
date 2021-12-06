unit frmShowText;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, IniPropStorage, Menus, ComCtrls, ActnList, DirBuilder_dmod,
  ATSynEdit, ATStrings;

type

  { TfmShowText }

  TfmShowText = class(TForm)
    ActionReloadText: TAction;
    ActionShowTabs: TAction;
    ActionGutterVisible: TAction;
    ActionRulerVisible: TAction;
    Actions: TActionList;
    ed: TATSynEdit;
    IniPropStorage: TIniPropStorage;
    mnuItm_ruler_visible: TMenuItem;
    mnuItm_gutter_visible: TMenuItem;
    mnuItm_show_tabs: TMenuItem;
    mmoAnnun: TMemo;
    pnlTop: TPanel;
    toolBar: TToolBar;
    tb_show_tabs: TToolButton;
    tb_reload_text: TToolButton;
    ToolButton2: TToolButton;
    tb_gutter_visible: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    tb_ruler_visible: TToolButton;
    ToolButton6: TToolButton;
    _popupMenu: TPopupMenu;
    procedure ActionGutterVisibleExecute(Sender: TObject);
    procedure ActionReloadTextExecute(Sender: TObject);
    procedure ActionRulerVisibleExecute(Sender: TObject);
    procedure ActionShowTabsExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure mnuItm_gutter_visibleClick(Sender: TObject);
    procedure mnuItm_show_tabsClick(Sender: TObject);
  private
    FMsg: ansistring;
    FLines, keep_text: ansistring;
    FMnu: TPopupMenu;
    FLineNumber : Integer;

    function Get_A_Line : TATStringItem;
		function GetLine : TStringList;
    procedure MarkUpTabs;
    procedure Reload;
    procedure SetLines(AValue: ansistring);
    procedure SetLines(aLst : TStringList);
    procedure SetMsg(AValue: ansistring);
  public
    property Msg: ansistring read FMsg write SetMsg;
    property Lines: ansistring read FLines write SetLines;
    property LineList : TStringList read GetLine write SetLines;
    procedure GetTextFeedback(str : AnsiString);
    property ALine : TATStringItem read Get_A_Line;
  end;
  //FNodeWalker.StatusBarCallback := TStringCallbackMethod(StatusBarFeedback);

var
  fmShowText: TfmShowText;

implementation

{$R *.lfm}

{ TfmShowText }

procedure TfmShowText.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IniPropStorage.Save;
end;

procedure TfmShowText.ActionShowTabsExecute(Sender: TObject);
begin
  ActionShowTabs.Checked := not ActionShowTabs.Checked;
  if ActionShowTabs.Checked then
    MarkUpTabs
  else
    Reload;
end;

procedure TfmShowText.ActionGutterVisibleExecute(Sender: TObject);
begin
  ActionGutterVisible.Checked := not ActionGutterVisible.Checked;
  ed.OptGutterVisible := ActionGutterVisible.Checked;
  ed.Invalidate;
end;

procedure TfmShowText.ActionReloadTextExecute(Sender: TObject);
begin
  Reload;
end;

procedure TfmShowText.ActionRulerVisibleExecute(Sender: TObject);
begin
  ActionRulerVisible.Checked := not ActionRulerVisible.Checked;
  ed.OptRulerVisible := ActionRulerVisible.Checked;
  ed.Invalidate;
end;

procedure TfmShowText.FormCreate(Sender: TObject);
begin
  IniPropStorage.Restore;
  FMnu := ed.PopupText;
  ed.PopupMenu := _popupMenu;
  FLineNumber := 0;
  //_popupMenu.Items.Insert(0, mnuItm_show_tabs);
end;

procedure TfmShowText.mnuItm_gutter_visibleClick(Sender: TObject);
begin
  ed.OptGutterVisible := not mnuItm_gutter_visible.Checked;
end;

procedure TfmShowText.Reload;
begin
  ed.Strings.Clear;
  ed.Strings.LineAdd(keep_text);
end;

const
  tab_s = '[TAB]';

procedure TfmShowText.MarkUpTabs;
var
  i: integer;
  s: string;
begin
  s := '';
  for i := 1 to Length(ed.Text) do
    if ed.Text[i] = TAB then
      s := s + tab_s
    else
      s := s + ed.Text[i];
  ed.Text := s;
end;

function TfmShowText.GetLine : TStringList;
begin

end;

procedure TfmShowText.mnuItm_show_tabsClick(Sender: TObject);
begin
  if mnuItm_show_tabs.Checked then
    Reload
  else
    MarkUpTabs;
end;

function TfmShowText.Get_A_Line : TATStringItem;
var
  ptr : PATStringItem;
begin
  if FLineNumber  = ed.Strings.Count then
    begin
      self.Close;
      //Result := TATStringItem(nil);
      Exit;
		end;
  ptr := ed.Strings.GetItemPtr(FLineNumber);
  Result := ptr^;
  Inc(FLineNumber);

end;

procedure TfmShowText.SetMsg(AValue: ansistring);
begin
  if FMsg = AValue then Exit;
  FMsg := AValue;
  mmoAnnun.Text := FMsg;
end;

procedure TfmShowText.GetTextFeedback(str : AnsiString);
begin
  Lines := str;
end;

procedure TfmShowText.SetLines(AValue: ansistring);
begin
  if FLines = AValue then Exit;
  FLines := AValue;
  ed.Text := FLines;
  keep_text := FLines;
end;

procedure TfmShowText.SetLines(aLst : TStringList);
begin
  ed.Strings.LineBlockInsert(0, aLst);
  FLineNumber := 0;
  toolBar.Visible := False;
end;

end.
