unit stringGridHelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls,
  DB, DBCtrls, DBGrids, ExtCtrls, Grids,
  SysUtils, Forms, Controls, Graphics, Dialogs, Clipbrd, Math;

type

  { TGridHelper }

  TGridHelper = class
    FDBGrid: TDbGrid;
    FSGrid : TStringGrid;
    str : String;
  private
    _maxSize : Integer;
		function GetCellWidth(Grid : TStringGrid; const aCol, aRow : Integer;
					defBorder : Integer) : Integer;
  procedure SetFSGrid(AValue: TStringGrid);
  public
    property DbGrid: TDBGrid read FDBGrid write FDBGrid;
    property SGrid: TStringGrid read FSGrid write SetFSGrid;
    function AutoSizeColumns(const MaxRows: integer = 25): integer;
    procedure SetDBGridColumnWidths;
    property maxSize : Integer read _maxSize write _maxSize;
    constructor Create;
		procedure SetStringGridColumnWidths;
  end;


implementation

uses DirBuilder_dmod;

constructor TGridHelper.Create;
begin
  inherited Create;
  _maxSize := 500;
end;


procedure TGridHelper.SetDBGridColumnWidths;
const
  DEFBORDER = 10;
var
  temp, n: Integer;
  lmax: array [0..30] of Integer;
begin
  with FDBGrid do
  begin
    Canvas.Font := Font;
    for n := 0 to Columns.Count - 1 do
      //if columns[n].visible then
      lmax[n] := Canvas.TextWidth(FDBGrid.DataSource.DataSet.Fields[n].DisplayLabel) + DEFBORDER;
    FDBGrid.DataSource.DataSet.First;
    while not FDBGrid.DataSource.DataSet.EOF do
    begin
      for n := 0 to Columns.Count - 1 do
      begin
        temp := Canvas.TextWidth(trim(Columns[n].Field.DisplayText)) + DEFBORDER;
        if temp > lmax[n] then lmax[n] := temp;
      end; { for }
      FDBGrid.DataSource.DataSet.Next;
    end; { while }
    FDBGrid.DataSource.DataSet.First;
    for n := 0 to Columns.Count - 1 do
      if lmax[n] > 0 then
      begin
        if lmax[n] > _maxSize then
            lmax[n] := _maxSize;
        Columns[n].Width := lmax[n];
      end;
  end; { With }
end;

function TGridHelper.GetCellWidth(Grid : TStringGrid;
                            const aCol, aRow : Integer;
                            defBorder : Integer) : Integer;
begin
  Result := Grid.Canvas.TextWidth(Grid.Cells[aCol, aRow]) + defBorder;
end;

procedure TGridHelper.SetStringGridColumnWidths;
var
  temp, n, txtWidth : Integer;
  rowCnt, colCnt : Integer;
  araMax: array of Integer;
  msg : String;
begin
  msg := 'TGridHelper.SetStringGridColumnWidths';
  colCnt := FSGrid.ColCount;
  if colCnt = 0 then
  begin
    ShowMessage('TGridHelper.SetStringGridColumnWidths: string grid is not populated!');
    Exit;
	end;
  SetLength(araMax, FSGrid.ColCount);

  try

    FsGrid.Canvas.Font := FSGrid.Font;
    for n := FSGrid.FixedCols to FsGrid.ColCount - 1 do
    begin
      txtWidth := FsGrid.Canvas.TextWidth(FSGrid.Cells[n, 0]) + DEF_CELL_BORDER;
      //if txtWidth > 200 then
      //  ShowMessage(Format('column: %s has width: %d', [FSGrid.cells[n, 0], txtWidth]));
      araMax[n] := txtWidth;
		end;

    rowCnt := FSGrid.FixedCols +1;
    FSGrid.Rows[rowCnt];
    for rowCnt := FSGrid.FixedRows to FSGrid.RowCount -1 do
        //while rowCnt < FSGrid.RowCount do
    begin
      for colCnt := FSGrid.FixedCols to FSGrid.ColCount - 1 do
      begin
        temp := FsGrid.Canvas.TextWidth(trim(FSGrid.Cells[colCnt, rowCnt])) + DEF_CELL_BORDER;
        if temp > araMax[colCnt] then
          araMax[colCnt] := temp;
      end; {for}
      //Inc(colCnt)
    end;

    {        adjust the column width             }
    colCnt := FSGrid.FixedCols;
		while colCnt < FsGrid.ColCount do
    begin
      if araMax[colCnt] > 0 then
      begin
        if araMax[colCnt] > _maxSize then
            araMax[colCnt] := _maxSize;
        try
          FSGrid.ColWidths[colCnt] := araMax[colCnt];
          //wd := FSGrid.ColCount;
          //colWidth := FSGrid.ColWidths[colCnt];
        except on E : Exception do
          ShowMessage(msg + sLineBreak + E.Message);
				end;
        Inc(colCnt);
			end;

		end;
	finally
    SetLength(araMax, 0);
	end;
end;

procedure TGridHelper.SetFSGrid(AValue: TStringGrid);
begin
  if FSGrid=AValue then Exit;
		FSGrid:=AValue;
end;

function TGridHelper.AutoSizeColumns(const MaxRows: integer): integer;

var
  DataSet: TDataSet;
  Bookmark: TBookmark;
  Count, i: integer;
  Caption: string;
  araColWidth: array of integer;
  col: TColumn;
begin
  if FDBGrid = nil then
  begin
    ShowMessage('TDbGridHelper.AutoSizeColumns: grid must be set.');
    Exit;
  end;
  SetLength(araColWidth, FDBGrid.Columns.Count);
  for i := 0 to FDBGrid.Columns.Count - 1 do
  begin
    col := FDBGrid.Columns[i];
    if col.Visible then
    //if FDBGrid.Columns[i].Visible then
    begin
      //FDBGrid.Columns[i];
      Caption := col.Title.Caption;
      Caption := col.Title.Caption + '    ';
      //Caption := FDBGrid.Columns[i].Title.Caption + '    ';
      araColWidth[i] := FDBGrid.Canvas.TextWidth(Caption);
    end
    else
      araColWidth[i] := 0;
  end;
exit;


  if FDBGrid.DataSource <> nil then
    DataSet := FDBGrid.DataSource.DataSet
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
        for i := 0 to FDBGrid.Columns.Count - 1 do
          if FDBGrid.Columns[i].Visible then
            araColWidth[i] :=
              Max(araColWidth[i], FDBGrid.Canvas.TextWidth(
              FDBGrid.Columns[i].Field.Text));
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
  for i := 0 to FDBGrid.Columns.Count - 1 do
    if FDBGrid.Columns[i].Visible then
    begin
      FDBGrid.Columns[i].Width := araColWidth[i];
      Inc(Count, araColWidth[i]);
    end;
  Result := Count - FDBGrid.ClientWidth;
end;


end.

