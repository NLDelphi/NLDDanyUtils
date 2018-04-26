unit NLDRcs_Resize;

{-$DEFINE FONTRESIZE}
{-$DEFINE STRINGGRIDRESIZE}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Forms, Dialogs, Controls,
  StdCtrls, Buttons, ExtCtrls, ComCtrls, QComCtrls, Mask, Grids;

type
  TDimension = record
    Name: string;
{$IFDEF FONTRESIZE}
    S: Integer;
{$ENDIF}
    W, H, T, L: integer; // S = font.size
  end;

  TDimensions = array of TDimension;

  TResizeCallbackFunction = function(Control: TControl; VFactor, HFactor: real; L, W, H, T
{$IFDEF FONTRESIZE}
    , S
{$ENDIF}
    : integer): boolean;

type TMyResize = class(TObject)
  private
    MyDims: TDimensions;
    DimIndex: integer;
    HFactor: real;
    VFactor: real;
    CallbackFunction: TResizeCallbackFunction;
    procedure MyGetDims(Control: TControl; TopLevel: boolean);
    procedure MyChangeDims(Control: TControl; TopLevel: boolean);
  public
    constructor Create(F: TForm; Fnct: TResizeCallbackFunction = nil);
    destructor Destroy; override;
    procedure Resize(F: TForm);
  end;

implementation

var Created: boolean = false;

procedure TMyResize.MyGetDims(Control: TControl; TopLevel: boolean);
var
  I: integer;
begin
  if not Created then exit;

  if TopLevel then DimIndex := -1;

  inc(DimIndex);

  SetLength(MyDims, DimIndex + 1);

  // get the dimensions/position of the object

  if (Control is TLabel) then TLabel(Control).AutoSize := true;
  if (Control is TEdit) then TEdit(Control).AutoSize := true;
  if (Control is TLabeledEdit) then TLabeledEdit(Control).AutoSize := true;
  //if (Control is TSpinEdit) then TSpinEdit(Control).AutoSize := true;
  if (Control is TMaskEdit) then TMaskEdit(Control).AutoSize := true;

  MyDims[DimIndex].Name := Control.Name;
{$IFDEF FONTRESIZE}
  MyDims[DimIndex].S := TEdit(Control).Font.Size;
{$ENDIF}
  MyDims[DimIndex].W := Control.Width;
  MyDims[DimIndex].H := Control.Height;
  MyDims[DimIndex].L := Control.Left;
  MyDims[DimIndex].T := Control.Top;

  if Control is TWinControl then
  begin
    for I := 0 to (TWinControl(Control).ControlCount - 1) do
      MyGetDims(TWinControl(Control).Controls[I], false);
  end;
end;

procedure TMyResize.MyChangeDims(Control: TControl; TopLevel: boolean);
var
  I, Index: integer;
{$IFDEF FONTRESIZE}
  S: Integer;
{$ENDIF}
  L, T, W, H: integer;
  NewWidth: DWord;
  NewHeight: DWord;
{$IFDEF STRINGGRIDRESIZE}
  Tsg: TStringGrid;
{$ENDIF}
begin
  if not Created then exit;

  if TopLevel then // toplevel, get the new dimensions
  begin
    NewWidth := Control.Width;
    NewHeight := Control.Height;

    HFactor := NewWidth / MyDims[0].W;
    VFactor := NewHeight / MyDims[0].H;

    if HFactor < 1 then
      HFactor := 1.001;
    if VFactor < 1 then
      VFactor := 1.001;
  end
  else
  begin
    // find the index of the control to resize

    Index := 0;
    while Index <= High(MyDims) do
    begin
      if Control.Name = MyDims[Index].Name then
      begin
        break;
      end;
      inc(Index); // next in the list
    end;

    // Index is always >= 0 here

    if (not assigned(CallbackFunction)) or
      (CallBackFunction(Control, VFactor, HFactor,
      MyDims[Index].L, MyDims[Index].W, MyDims[Index].H, MyDims[Index].T
{$IFDEF FONTRESIZE}
      , MyDims[Index].S
{$ENDIF}
      ) = false)
      then
    begin // do the resizing here, else do the resizing in the Callback function

    // resize the control

      W := Trunc(HFactor * MyDims[Index].W);
      H := Trunc(VFactor * MyDims[Index].H);
      L := Trunc(HFactor * MyDims[Index].L);
      T := Trunc(VFactor * MyDims[Index].T);

{$IFDEF FONTRESIZE}
      if VFactor < HFactor
        then S := Trunc(VFactor * MyDims[Index].S)
      else S := Trunc(HFactor * MyDims[Index].S);
{$ENDIF}

{$IFDEF FONTRESIZE}
      TEdit(Control).Font.Size := S;
{$ENDIF}

      if (Control is TComboBox) then // special case
      begin
        Control.Width := W;
        Control.Left := L;
        Control.Top := T;
        TComboBox(Control).ItemHeight := H - 6;
      end
      else
        if ((Control is TLabel) or
        (Control is TEdit) or
        (Control is TLabeledEdit) or
      //(Control is TSpinEdit) or
        (Control is TMaskEdit)
        ) then
      begin
        // do not set the sizes here, set 'autosize' to true in the object inspector
        Control.Left := L;
        Control.Top := T;
        if not (Control is TLabel) then Control.Width := W;
      end
      else
        if Control is TUpDown then
      begin
        if TUpDown(Control).Associate = nil
          then Control.SetBounds(L, T, W, H)
        else
        begin
          Control.Left :=
            TUpDown(Control).Associate.Left + TUpDown(Control).Associate.Width;
          Control.Top := TUpDown(Control).Associate.Top;
          Control.Height := H;
          Control.Width := W;
        end;
      end
      else

{$IFDEF STRINGGRIDRESIZE}
        if Control is TStringGrid then // only for uniforme column widths and row heights
      begin
        Tsg := Control as TStringGrid;

        Tsg.Left := L;
        Tsg.Top := T;
        Tsg.DefaultColWidth := (W div Tsg.ColCount) - 2;
        Tsg.DefaultRowHeight := (H div Tsg.RowCount) - 2;
        Tsg.Width := W;
        Tsg.Height := H;
      end
      else
{$ENDIF}

      begin
        Control.SetBounds(L, T, W, H); // default
      end;
    end;

  end;

  if Control is TWinControl then // resize its descendants
  begin
    for I := 0 to (TWinControl(Control).ControlCount - 1) do
    // process all descendants
    begin
      MyChangeDims(TWinControl(Control).Controls[I], false);
    end;
  end;
end;

constructor TMyResize.Create(F: TForm; Fnct: TResizeCallbackFunction = nil);
begin
  inherited Create;
  CallbackFunction := Fnct;
  Created := true;
  MyGetDims(F, true); // get the original dimensions
end;

procedure TMyResize.Resize(F: TForm);
begin
  MyChangeDims(F, true); // get the original dimensions
end;

destructor TMyResize.Destroy;
begin
  Created := false;
  SetLength(MyDims, 0);
  inherited Destroy;
end;

end.
