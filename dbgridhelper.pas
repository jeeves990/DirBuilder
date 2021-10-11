unit DbGridHelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, csvdataset, DB, DBCtrls, DBGrids, ExtCtrls, Grids,
  StdCtrls, SysUtils, Forms, Controls, Graphics, Dialogs, Clipbrd, Math;

type

  { TDbGridHelper }

  TDbGridHelper = class
    theGrid: TDbGrid;
  private
  public
    property dbGrid: TDBGrid read theGrid write theGrid;
    function AutoSizeColumns(const MaxRows: integer = 25): integer;
    procedure SetGridColumnWidths;

    constructor Create;
  end;


implementation

constructor TDbGridHelper.Create;
begin
  inherited Create;
end;


procedure TDbGridHelper.SetGridColumnWidths;
const
  DEFBORDER = 10;
var
  temp, n: Integer;
  lmax: array [0..30] of Integer;
begin
  with theGrid do
  begin
    Canvas.Font := Font;
    for n := 0 to Columns.Count - 1 do
      //if columns[n].visible then
      lmax[n] := Canvas.TextWidth(theGrid.DataSource.DataSet.Fields[n].DisplayLabel) + DEFBORDER;
    theGrid.DataSource.DataSet.First;
    while not theGrid.DataSource.DataSet.EOF do
    begin
      for n := 0 to Columns.Count - 1 do
      begin
        //if columns[n].visible then begin
        temp := Canvas.TextWidth(trim(Columns[n].Field.DisplayText)) + DEFBORDER;
        if temp > lmax[n] then lmax[n] := temp;
        //end; { if }
      end; {for}
      theGrid.DataSource.DataSet.Next;
    end; { while }
    theGrid.DataSource.DataSet.First;
    for n := 0 to Columns.Count - 1 do
      if lmax[n] > 0 then
        Columns[n].Width := lmax[n];
  end; { With }
end; {SetGridColumnWidths  }


function TDbGridHelper.AutoSizeColumns(const MaxRows: integer): integer;

var
  DataSet: TDataSet;
  Bookmark: TBookmark;
  Count, i: integer;
  capWidth: integer;
  Caption: string;
  araColWidth: array of integer;
  col: TColumn;
begin
  if theGrid = nil then
  begin
    ShowMessage('TDbGridHelper.AutoSizeColumns: grid must be set.');
    Exit;
  end;
  SetLength(araColWidth, theGrid.Columns.Count);
  for i := 0 to theGrid.Columns.Count - 1 do
  begin
    col := theGrid.Columns[i];
    if col.Visible then
    //if theGrid.Columns[i].Visible then
    begin
      //theGrid.Columns[i];
      Caption := col.Title.Caption;
      Caption := col.Title.Caption + '    ';
      //Caption := theGrid.Columns[i].Title.Caption + '    ';
      araColWidth[i] := theGrid.Canvas.TextWidth(Caption);
    end
    else
      araColWidth[i] := 0;
  end;
exit;


  if theGrid.DataSource <> nil then
    DataSet := theGrid.DataSource.DataSet
  else
    DataSet := nil;
  if (DataSet <> nil) and DataSet.Active then
  begin
    Bookmark := DataSet.GetBookmark;
    DataSet.DisableControls;
    try
      Count := 0;
      DataSet.First;
      while not DataSet.EOF and (Count < MaxRows) do
      begin
        for i := 0 to theGrid.Columns.Count - 1 do
          if theGrid.Columns[i].Visible then
            araColWidth[i] :=
              Max(araColWidth[i], theGrid.Canvas.TextWidth(
              theGrid.Columns[i].Field.Text));
        Inc(Count);
        DataSet.Next;
      end;
    finally
      DataSet.GotoBookmark(Bookmark);
      DataSet.FreeBookmark(Bookmark);
      DataSet.EnableControls;
    end;
  end;
  Count := 0;
  for i := 0 to theGrid.Columns.Count - 1 do
    if theGrid.Columns[i].Visible then
    begin
      theGrid.Columns[i].Width := araColWidth[i];
      Inc(Count, araColWidth[i]);
    end;
  Result := Count - theGrid.ClientWidth;
end;


end.

