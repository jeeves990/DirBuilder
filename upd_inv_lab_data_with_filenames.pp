unit upd_inv_lab_data_with_filenames;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Grids;

type

TRec_msku_title = record
  msku : String;
  title : String;
end;

  { TUpdate_inv_lab_data }

TUpdate_inv_lab_data = class
  Fsql : TStrings;
  Fgrid : TStringGrid;

  public
    property SQL : TStrings read Fsql write Fsql;

  end;

implementation

end.
