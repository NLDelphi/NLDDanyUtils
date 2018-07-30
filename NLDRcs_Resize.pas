unit NLDRcs_Resize;

// Kameleon

{-$DEFINE NoDebug}// Disable debug possibilities and range checking (= faster)
// {.$Define NoDebug}: During debugging
// {$Define NoDebug} : During "normal" use

{ History of this unit:
  11-10-2017: * Initial version
  14-10-2017: * Corrected an error for associated TUpDown components
  30-03-2018: * allow also resize < factor 1
  09-06-2018: * Replaced the conditional compilations by parameters in the create function
  15-06-2018: * Resizing is more linear now (Client sizes used)
  23-06-2018: * "Create" and "Resize" can now have any TControl type to start from
}

{$P+} // Open Strings ON
{$H+} // Long Strings ON

{$IFDEF NoDebug}

{$O+} // Optimisation ON
{$D-} // Debug information OFF
{$I-} // I/O checking OFF
{$L-} // Local Symbols OFF
{$Q-} // Overflow Checking OFF
{$R-} // Range Checking OFF

{$ELSE}
{$O-} // Optimisation OFF
{$D+} // Debug information ON
{$I+} // I/O checking ON
{$L+} // Local Symbols ON
{$Q+} // Overflow Checking ON
{$R+} // Range Checking ON

{$ENDIF}

{$W-} // Stack Frames OFF
{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Forms, Dialogs, Controls,
  StdCtrls, Buttons, ExtCtrls, ComCtrls, Mask, Grids, spin;

type
  TDimension = record
    Name: string;
    S, W, H, T, L, D1, D2: integer; // S = font.size
  end;

  TDimensions = array of TDimension;

  TResizeCallbackFunction = function(Control: TControl; VFactor, HFactor: real; L, W, H, T, S, D1, D2: integer): boolean;

type
  TMyResize = class(TObject)
  private
    MyDims: TDimensions;
    DimIndex: integer;
    HFactor: real;
    VFactor: real;
    FResizeFont: boolean;
    CallbackFunction: TResizeCallbackFunction;
    procedure MyGetDims(Control: TControl; TopLevel: boolean);
    procedure MyChangeDims(Control: TControl; TopLevel: boolean);
  public
    constructor Create(F: TControl; Fnct: TResizeCallbackFunction = nil; ResizeFont: boolean = false);
    destructor Destroy; override;
    procedure Resize(F: TControl);
  end;

implementation

procedure TMyResize.MyGetDims(Control: TControl; TopLevel: boolean);
var
  I: integer;
begin

  if TopLevel then DimIndex := -1;

  inc(DimIndex);

  SetLength(MyDims, DimIndex + 1);

  // get the dimensions/position of the object

  if (Control is TLabel) then TLabel(Control).AutoSize := true;
  if (Control is TEdit) then TEdit(Control).AutoSize := true;
  if (Control is TLabeledEdit) then TLabeledEdit(Control).AutoSize := true;
  if (Control is TSpinEdit) then TSpinEdit(Control).AutoSize := true;
  if (Control is TMaskEdit) then TMaskEdit(Control).AutoSize := true;

  MyDims[DimIndex].Name := Control.Name;
  MyDims[DimIndex].S := TEdit(Control).Font.Size;
  MyDims[DimIndex].W := Control.ClientWidth;
  MyDims[DimIndex].H := Control.ClientHeight;
  MyDims[DimIndex].L := Control.Left;
  MyDims[DimIndex].T := Control.Top;

  if Control is TStringGrid then
  begin
    MyDims[DimIndex].D1 := TStringGrid(Control).DefaultColWidth; // extra dimensions
    MyDims[DimIndex].D2 := TStringGrid(Control).DefaultRowHeight;
  end;

  if Control is TDrawGrid then
  begin
    MyDims[DimIndex].D1 := TDrawGrid(Control).DefaultColWidth; // extra dimensions
    MyDims[DimIndex].D2 := TDrawGrid(Control).DefaultRowHeight;
  end;

  if Control is TWinControl then
  begin
    for I := 0 to (TWinControl(Control).ControlCount - 1) do
      MyGetDims(TWinControl(Control).Controls[I], false);
  end;
end;

procedure TMyResize.MyChangeDims(Control: TControl; TopLevel: boolean);
var
  I, Index: integer;
  S, L, T, W, H: integer;
  NewWidth: DWord;
  NewHeight: DWord;
  Tsg: TStringGrid;
  Tdg: TDrawGrid;
begin

  if TopLevel then // toplevel, get the new dimensions
  begin
    NewWidth := Control.ClientWidth;
    NewHeight := Control.ClientHeight;

    HFactor := NewWidth / MyDims[0].W;
    VFactor := NewHeight / MyDims[0].H;
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
      , MyDims[Index].S, MyDims[Index].D1, MyDims[Index].D2) = false) then
    begin // do the resizing here, else do the resizing in the Callback function

      // resize the control

      W := Trunc(HFactor * MyDims[Index].W);
      H := Trunc(VFactor * MyDims[Index].H);
      L := Trunc(HFactor * MyDims[Index].L);
      T := Trunc(VFactor * MyDims[Index].T);

      if FResizeFont then
      begin
        if VFactor < HFactor then S := Trunc(VFactor * MyDims[Index].S)
        else S := Trunc(HFactor * MyDims[Index].S);
        TEdit(Control).Font.Size := S;
      end;

      if (Control is TComboBox) then // special case
      begin
        Control.ClientWidth := W;
        Control.Left := L;
        Control.Top := T;
        TComboBox(Control).ItemHeight := H - 6;
      end
      else
        if ((Control is TLabel) or
          (Control is TEdit) or
          (Control is TLabeledEdit) or
          (Control is TSpinEdit) or
          (Control is TMaskEdit)
          ) then
        begin
          // do not set the sizes here, set 'autosize' to true in the object inspector
          Control.Left := L;
          Control.Top := T;
          if not (Control is TLabel) then Control.ClientWidth := W;
        end
        else
          if Control is TUpDown then
          begin
            if TUpDown(Control).Associate = nil then
            begin
              Control.Left := L;
              Control.Top := T;
              Control.Width := W;
              Control.Height := H;
            end
            else
            begin
              Control.Left :=
                TUpDown(Control).Associate.Left + TUpDown(Control).Associate.Width;
              Control.Top := TUpDown(Control).Associate.Top;
              Control.Height := TUpDown(Control).Associate.Height;
              Control.Width := W;
            end;
          end
          else
            if Control is TStringGrid then
            begin
              Tsg := Control as TStringGrid;

              Tsg.Left := L;
              Tsg.Top := T;
              Tsg.DefaultColWidth := Trunc(HFactor * MyDims[Index].D1);
              Tsg.DefaultRowHeight := Trunc(VFactor * MyDims[Index].D2);
              Tsg.ClientWidth := W;
              Tsg.ClientHeight := H;
            end
            else
              if Control is TDrawGrid then
              begin
                Tdg := Control as TDrawGrid;

                Tdg.Left := L;
                Tdg.Top := T;
                Tdg.DefaultColWidth := Trunc(HFactor * MyDims[Index].D1);
                Tdg.DefaultRowHeight := Trunc(VFactor * MyDims[Index].D2);
                Tdg.ClientWidth := W;
                Tdg.ClientHeight := H;
              end
              else
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

constructor TMyResize.Create(F: TControl; Fnct: TResizeCallbackFunction = nil;
                             ResizeFont: boolean = false);
begin
  inherited Create;
  CallbackFunction := Fnct;
  FResizeFont := ResizeFont;
  MyGetDims(F, true); // get the original dimensions
end;

procedure TMyResize.Resize(F: TControl);
begin
  MyChangeDims(F, true); // do resize of all components
end;

destructor TMyResize.Destroy;
begin
  SetLength(MyDims, 0);
  inherited Destroy;
end;

end.
