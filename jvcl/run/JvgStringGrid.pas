{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgStringGrid.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvgStringGrid;

interface

uses
  Windows, Messages, Classes, Controls, Graphics, ExtCtrls,
  Grids, StdCtrls, Forms,
  JvgTypes, JvgCommClasses, JvgUtils, JVCLVer;

type
  TglStringGridExtOptions_ = (fsgVertCaptions, fsgHottrack, fsgMemoEditor,
    fsgWordWrap, fsgCellHeightAutoSize, fsgTabThroughCells);
  TglStringGridExtOptions = set of TglStringGridExtOptions_;

  TglGridCellStyle = record
    Hottracking: boolean;
    GradientFilling, Default_Drawing: boolean;
    R: TRect;
    CellBorders: TglSides;
    BevelInner,
      BevelOuter: TPanelBevel;
    BevelBold: boolean;
    FontStyle: TFontStyles;
    FontColor,
      BackgrColor: TColor;
    Interspace: integer
  end;

  TglOnGetCellStyleEvent = procedure(Sender: TObject; ACol, ARow: longint; var
    Style: TglGridCellStyle) of object;
  TglOnGetCellGradientParamsEvent = procedure(Sender: TObject; ACol, ARow:
    longint;
    var CellRect: TRect;
    var Gradient: TJvgGradient) of object;

  TJvgStringGrid = class(TStringGrid)
  private
    FCaptionTextAlignment: TAlignment;
    FCaptFont: TFont;
    FBitmap, bmp: TBitmap;
    FImage: TImage;
    FCaptions: TStringList;
    FHottrackThrought: boolean;
    AHottrackCol, AHottrackRow: longint;
    Memo: TMemo;
    MemoCell: TGridCoord;
    Gradient: TJvgGradient;
    FOnGetCellStyle: TglOnGetCellStyleEvent;
    FOnGetCellGradientParams: TglOnGetCellGradientParamsEvent;
    FExtOptions: TglStringGridExtOptions;
    FTextAlignment: TAlignment;
    FEditorColor: TColor;
    FEditorFont: TFont;
    MemoUpdateTimer: TTimer;
    FAboutJVCL: TJVCLAboutInfo;
    procedure SetCaptionTextAlignment(Value: TAlignment);
    procedure SetCaptFont(Value: TFont);
    function GetBitmap: TBitmap;
    procedure SetBitmap(Value: TBitmap);
    procedure SetImage(Value: TImage);
    procedure SetCaptions(Value: TStringList);
    procedure SetVertCaptions(Value: boolean);
    procedure OnMemoChange(Sender: TObject);
    procedure OnMemoExit(Sender: TObject);
    procedure ShowEditorAtCell(X, Y: Longint);
    procedure UpdateCaptions(Sender: TObject);
    procedure SetHottrack(const Value: boolean);
    procedure SetMemoEditor(const Value: boolean);
    procedure SetWordWrap(const Value: boolean);
    procedure SetExtOptions(const Value: TglStringGridExtOptions);
    procedure SetTextAlignment(const Value: TAlignment);
  protected
    //    function CreateEditor: TInplaceEdit; override;
    function CanEditShow: Boolean; override;

    procedure Loaded; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState:
      TGridDrawState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMChildKey(var Message: TMessage); message CM_CHILDKEY;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      Integer); override;
    //    procedure DblClick; dynamic;

    procedure GetCellStyle(Sender: TObject; var ACol, ARow: Integer; var
      Style: TglGridCellStyle); virtual;
    procedure GetCellGradientParams(Sender: TObject; ACol, ARow: longint; var
      CellRect: TRect; var Gradient: TJvgGradient); virtual;
  public
    //    property InplaceEditor;
    AlignAll: boolean;
    ColsBold: integer;
    RowsBold: integer;
    HottrackColor: TColor;

    property VertCaptions: boolean write SetVertCaptions default false;
    property Hottrack: boolean write SetHottrack default false;
    property MemoEditor: boolean write SetMemoEditor default false;
    property WordWrap: boolean write SetWordWrap default false;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure GetPriorCell(var X, Y: Longint);
    procedure GetNextCell(var X, Y: Longint);
    procedure ClearSelection;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored
      False;
    property CaptionTextAlignment: TAlignment read FCaptionTextAlignment write
      SetCaptionTextAlignment default taCenter;
    property TextAlignment: TAlignment read FTextAlignment write
      SetTextAlignment default taLeftJustify;
    property CaptionFont: TFont read FCaptFont write SetCaptFont;
    property Captions: TStringList read FCaptions write SetCaptions;
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property Image: TImage read FImage write SetImage;
    property ExtOptions: TglStringGridExtOptions read FExtOptions write
      SetExtOptions;
    property EditorColor: TColor read FEditorColor write FEditorColor default
      $00FEE392;
    property EditorFont: TFont read FEditorFont write FEditorFont;
    property OnGetCellStyle: TglOnGetCellStyleEvent read FOnGetCellStyle write
      FOnGetCellStyle;
    property OnGetCellGradientParams: TglOnGetCellGradientParamsEvent read
      FOnGetCellGradientParams write FOnGetCellGradientParams;
  end;

implementation

uses
  Math;

constructor TJvgStringGrid.Create(AOwner: TComponent);
begin
  inherited; //FHottrackThrought := true;

  FCaptFont := TFont.Create;
  FEditorFont := TFont.Create;
  MemoUpdateTimer := TTimer.Create(nil);
  MemoUpdateTimer.Enabled := false;
  MemoUpdateTimer.Interval := 200;
  MemoUpdateTimer.OnTimer := OnMemoChange;
  FCaptionTextAlignment := taCenter;
  FCaptions := TStringList.Create;
  FCaptions.OnChange := UpdateCaptions;
  AHottrackCol := -1;
  HottrackColor := clHighlight;
  FEditorColor := $00FEE392;
  FExtOptions := [fsgHottrack, fsgMemoEditor, fsgWordWrap,
    fsgCellHeightAutoSize, fsgTabThroughCells];
  Options := Options + [goEditing];

  if csDesigning in ComponentState then
    exit;
  Memo := TMemo.Create(Self);
  Memo.Visible := false;
  Memo.BorderStyle := bsNone;
  Memo.Parent := Self;
  Memo.OnChange := OnMemoChange;
  Memo.OnExit := OnMemoExit;
  //  Memo.OnKeyDown := OnMemoKeyDown;
  DefaultDrawing := false;
end;

destructor TJvgStringGrid.Destroy;
begin
  FCaptFont.Free;
  FCaptions.Free;
  FEditorFont.Free;
  MemoUpdateTimer.Free;
  if Assigned(Memo) then
    Memo.Free;
  if Assigned(FBitmap) then
    FBitmap.Free;
  if Assigned(Gradient) then
    Gradient.Free;
  inherited;
end;

procedure TJvgStringGrid.Loaded;
begin
  inherited;
  if Assigned(FBitmap) and (not FBitmap.Empty) then
    Bmp := FBitmap;
  VertCaptions := fsgVertCaptions in FExtOptions;
end;

procedure TJvgStringGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState:
  TGridDrawState);
const
  aAlignments: array[TAlignment] of Longint = (DT_LEFT,
    DT_RIGHT, DT_CENTER);
  aWordWrap: array[boolean] of longint = (0, DT_WORDBREAK);
var
  R: TRect;
  doHottracking, isFixedCell: boolean;
  x, x_, y, y_, IHeight, IWidth, l, t, i, Interspace: integer;
  Style: TglGridCellStyle;
  CellTextAlignment: TAlignment;
begin
  ///  dec(ARect.Bottom);

  isFixedCell := (ACol < FixedCols) or (ARow < FixedRows);

  Interspace := 2;
  if Assigned(OnDrawCell) then
    OnDrawCell(Self, ACol, ARow, ARect, AState);
  if ACol < ColsBold then
    Canvas.Font.Style := Canvas.Font.Style + [fsBold];
  if ARow < RowsBold then
    Canvas.Font.Style := Canvas.Font.Style + [fsBold];
  R := ARect;

  if isFixedCell then
    if IsItAFilledBitmap(bmp) then
    begin
      x := r.Left;
      y := r.top;
      IHeight := r.bottom - r.top;
      IWidth := r.Right - r.Left - 4;
      x_ := x;
      y_ := y;
      l := 0;
      t := DefaultRowHeight * ARow mod bmp.Height;
      while x_ < r.right do
      begin
        if x_ + IWidth > r.right then
          IWidth := r.right - x_;
        while y_ < r.bottom do
        begin
          if y_ + IHeight > r.bottom then
            IHeight := r.bottom - y_;
          BitBlt(Canvas.Handle, x_, y_, min(IWidth, bmp.Width - l),
            min(IHeight, bmp.Height - t), bmp.Canvas.Handle, l, t, SRCCOPY);
          Inc(y_, min(IHeight, bmp.Height));
        end;
        Inc(x_, min(IWidth, bmp.Width));
        y_ := y;
      end;
    end;

  if true then
  begin
    if FHottrackThrought then
      doHottracking := (AHottrackCol = ACol) or (AHottrackRow = ARow)
    else if fsgHottrack in FExtOptions then
      doHottracking := (AHottrackCol = ACol) and (AHottrackRow = ARow) and
        ((ACol < FixedCols) or (ARow < FixedRows))
    else
      doHottracking := false;

    if isFixedCell then
    begin
      Canvas.Font.Assign(FCaptFont);
      CellTextAlignment := CaptionTextAlignment;
      Style.BevelOuter := bvRaised;
      Style.BackgrColor := clBtnFace;
    end
    else
    begin
      Canvas.Font.Assign(Font);
      CellTextAlignment := TextAlignment;
      Style.BevelOuter := bvNone;
      if gdSelected in AState then
        Style.BackgrColor := clHighlight
      else
        Style.BackgrColor := Color;
    end;

    if doHottracking then
      Style.FontColor := HottrackColor
    else
      Style.FontColor := Canvas.Font.Color;

    if gdSelected in AState then
      Style.FontColor := clHighlightText;

    Style.Hottracking := doHottracking;
    Style.GradientFilling := false;
    Style.Default_Drawing := true;
    Style.R := R;
    Style.CellBorders := ALLGLSIDES;
    Style.BevelInner := bvNone;

    Style.BevelBold := false;
    Style.FontStyle := Canvas.Font.Style;
    Style.Interspace := Interspace;
    GetCellStyle(Self, ACol, ARow, Style);
    Canvas.Font.Style := Style.FontStyle;
    R := Style.R;
    if not Style.Default_Drawing then
      exit;

    if Style.GradientFilling then
    begin
      if Gradient = nil then
        Gradient := TJvgGradient.Create;
      Gradient.Active := true;
      ARect := R;
      GetCellGradientParams(Self, ACol, ARow, ARect, Gradient);
      GradientBox(Canvas.Handle, ARect, Gradient, integer(psSolid), 1);
    end;

    DrawBoxEx(Canvas.Handle, R, Style.CellBorders, Style.BevelInner,
      Style.BevelOuter, Style.BevelBold,
      Style.BackgrColor, {(BackgrColor = clBtnFace)or}
      Style.GradientFilling);
    ARect := R;
    inc(ARect.Left, Style.Interspace);
    SetTextColor(Canvas.Handle, ColorToRGB(Style.FontColor));
    SetBkMode(Canvas.Handle, TRANSPARENT);
    DrawText(Canvas.Handle, PChar(Cells[ACol, ARow]), length(Cells[ACol,
      ARow]), R, aAlignments[CellTextAlignment] or DT_WORDBREAK or
        DT_CALCRECT);

    if (fsgCellHeightAutoSize in ExtOptions) and not isFixedCell then
    begin
      i := R.Bottom - R.Top;
      if (i > DefaultRowHeight) and (RowHeights[ARow] < i) then
      begin
        ///RowHeights[ARow] := i;
        exit;
      end;
    end;

    ARect.Top := ARect.Top + max(0, (ARect.Bottom - R.Bottom) div 2);
    DrawText(Canvas.Handle, PChar(Cells[ACol, ARow]), length(Cells[ACol,
      ARow]), ARect, aAlignments[CellTextAlignment] or aWordWrap[fsgWordWrap in
        FExtOptions]);
    SetTextColor(Canvas.Handle, ColorToRGB(Font.Color));
  end
  else // if AlignAll then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ARect);
    inc(ARect.Left, 2);
    R := ARect;
    DrawText(Canvas.Handle, PChar(Cells[ACol, ARow]), length(Cells[ACol,
      ARow]), R, DT_LEFT or DT_WORDBREAK or DT_CALCRECT);
    ARect.Top := ARect.Top + max(0, (ARect.Bottom - R.Bottom) div 2);
    DrawText(Canvas.Handle, PChar(Cells[ACol, ARow]), length(Cells[ACol,
      ARow]), ARect, DT_LEFT or DT_WORDBREAK)
  end; //  else inherited;
  // DefaultDrawing := true;
end;

procedure TJvgStringGrid.SetCaptionTextAlignment(Value: TAlignment);
begin
  FCaptionTextAlignment := Value;
  Repaint;
end;

procedure TJvgStringGrid.SetCaptFont(Value: TFont);
begin
  FCaptFont.Assign(Value);
  Repaint;
end;

function TJvgStringGrid.GetBitmap: TBitmap;
begin
  if not Assigned(FBitmap) then
    FBitmap := TBitmap.Create;
  Result := FBitmap;
end;

procedure TJvgStringGrid.SetBitmap(Value: TBitmap);
begin
  if Assigned(FBitmap) then
    FBitmap.Free;
  FBitmap := TBitmap.Create;
  FBitmap.Assign(Value);
  if Assigned(Value) then
    Bmp := FBitmap
  else if Assigned(FImage) and Assigned(FImage.Picture) and
    Assigned(FImage.Picture.Bitmap) then
    Bmp := FImage.Picture.Bitmap
  else
    Bmp := nil;
  Invalidate;
end;

procedure TJvgStringGrid.SetImage(Value: TImage);
begin
  FImage := Value;
  if Assigned(FImage) and Assigned(FImage.Picture) and
    Assigned(FImage.Picture.Bitmap) then
    Bmp := FImage.Picture.Bitmap
  else if Assigned(FBitmap) then
    Bmp := FBitmap
  else
    Bmp := nil;
  Invalidate;
end;

procedure TJvgStringGrid.SetCaptions(Value: TStringList);
begin
  FCaptions.Assign(Value);
  VertCaptions := fsgVertCaptions in FExtOptions;
end;

//-------------------------------------------------------------------------------

procedure TJvgStringGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
  ACol, ARow: longint;
begin
  inherited;
  if not (fsgHottrack in FExtOptions) then
    exit;
  MouseToCell(x, y, ACol, ARow);
  if (ACol >= FixedCols) and (ARow >= FixedRows) then
  begin
    SetCursor(Screen.Cursors[crCross]);
    exit;
  end;
  SetCursor(Screen.Cursors[crDefault]);

  if ((ACol <> AHottrackCol) or (ARow <> AHottrackRow)) then
  begin
    if (AHottrackCol < FixedCols) or (AHottrackRow < FixedRows) then
    begin
      R := CellRect(AHottrackCol, AHottrackRow);
      InvalidateRect(Handle, @R, false); //DefaultDrawing := false;
    end
    else if FHottrackThrought then
    begin
      if (ACol <> AHottrackCol) and (FixedCols > 0) then
      begin
        R := CellRect(AHottrackCol, 0); //DefaultDrawing := false;
        InvalidateRect(Handle, @R, false);
      end;
      if (ARow <> AHottrackRow) and (FixedRows > 0) then
      begin
        R := CellRect(0, AHottrackRow); //DefaultDrawing := false;
        InvalidateRect(Handle, @R, false);
      end;
    end;

    if (ACol < FixedCols) or (ARow < FixedCols) then
    begin
      R := CellRect(ACol, ARow); //DefaultDrawing := false;
      InvalidateRect(Handle, @R, false);
    end
    else if FHottrackThrought then
    begin
      if (ACol <> AHottrackCol) and (FixedCols > 0) then
      begin
        R := CellRect(ACol, 0); //DefaultDrawing := false;
        InvalidateRect(Handle, @R, false);
      end;
      if (ARow <> AHottrackRow) and (FixedRows > 0) then
      begin
        R := CellRect(0, ARow); //DefaultDrawing := false;
        InvalidateRect(Handle, @R, false);
      end;
    end;
    AHottrackCol := ACol;
    AHottrackRow := ARow;
  end;
end;

procedure TJvgStringGrid.CMMouseLeave(var Message: TMessage);
var
  R: TRect;
begin
  inherited;
  if not (fsgHottrack in FExtOptions) then
    exit;
  R := CellRect(AHottrackCol, AHottrackRow);
  AHottrackCol := -1;
  //DefaultDrawing := false;
  InvalidateRect(Handle, @R, false);
end;

procedure TJvgStringGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  GridCoord: TGridCoord;
begin
  inherited;
  if (not (goEditing in Options)) or (csDesigning in ComponentState) then
    exit;

  if Assigned(OnMouseDown) then
    OnMouseDown(Self, Button, Shift, X, Y);
  if Memo.Focused then
    Cells[MemoCell.X, MemoCell.Y] := Memo.Text;
  GridCoord := MouseCoord(X, Y);
  if (GridCoord.X = 1) and Assigned(OnDrawCell) then
    OnDrawCell(Self, GridCoord.X, GridCoord.Y, CellRect(GridCoord.X,
      GridCoord.Y), [gdFocused]);
  //  ClearSelection;
  ShowEditorAtCell(GridCoord.X, GridCoord.Y);
end;

procedure TJvgStringGrid.ShowEditorAtCell(X, Y: Longint);
var
  R: TRect;
  GridRect: TGridRect;
begin
  if not (fsgMemoEditor in FExtOptions) then
    exit;
  if (X >= FixedCols) and (Y >= FixedRows) then
  begin

    if (GridRect.Left <> X) or (GridRect.Top <> Y) then
    begin
      GridRect.Left := X;
      GridRect.Top := Y;
      GridRect.Right := X;
      GridRect.Bottom := Y;
      Selection := GridRect;
    end;
    Application.ProcessMessages;
    R := CellRect(X, Y);
    Memo.SetBounds(R.left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
    MemoCell.X := X;
    MemoCell.Y := Y;
    Memo.Text := Cells[MemoCell.X, MemoCell.Y];
    Memo.Visible := true;
    Memo.SetFocus;
    Memo.OnChange := OnMemoChange;
    Memo.Color := EditorColor;
    Memo.Font.Assign(EditorFont);
  end;
end;

procedure TJvgStringGrid.OnMemoChange(Sender: TObject);
var
  R: TRect;
  maxHeight, h, i: integer;
begin
  /// need handle it -> if Assigned(OnSetEditText) then ...
  if (MemoCell.X < FixedCols) or (MemoCell.Y < FixedRows) then
    exit;

  MemoUpdateTimer.Enabled := false;
  Cells[MemoCell.X, MemoCell.Y] := Memo.Text;

  if (fsgCellHeightAutoSize in ExtOptions) and (MemoCell.Y <> 0) then
  begin
    Canvas.Font.Assign(Font);
    maxHeight := DefaultRowHeight;
    for i := FixedCols to ColCount - 1 do
    begin
      R := CellRect(i, MemoCell.Y);
      if length(Cells[i, MemoCell.Y]) = 0 then
        continue;

      DrawText(Canvas.Handle, PChar(Cells[i, MemoCell.Y]), length(Cells[i,
        MemoCell.Y]), R, DT_WORDBREAK or DT_CALCRECT);
      h := R.Bottom - R.Top;
      if h > maxHeight then
        maxHeight {RowHeights[MemoCell.Y]} := h;
    end;
    RowHeights[MemoCell.Y] := maxHeight;
    Memo.Height := maxHeight;
  end;
end;

procedure TJvgStringGrid.OnMemoExit(Sender: TObject);
begin
  //  Cells[MemoCell.X, MemoCell.Y] := Memo.Text;
  Memo.Visible := false;
end;

procedure TJvgStringGrid.ClearSelection;
var
  GR: TGridRect;
begin
  GR.Left := 0;
  GR.Top := 0;
  GR.Right := 1;
  GR.Bottom := 1;
  Selection := GR;
end;

procedure TJvgStringGrid.CMChildKey(var Message: TMessage);
begin
  if Message.WParam = VK_TAB then
  begin
    if fsgTabThroughCells in ExtOptions then
    begin
      if Memo.Focused then
        Cells[MemoCell.X, MemoCell.Y] := Memo.Text;
      GetNextCell(MemoCell.X, MemoCell.Y);
      ShowEditorAtCell(MemoCell.X, MemoCell.Y);
      Message.Result := 1;
    end
    else
      inherited;
  end
  else
  begin
    inherited;
    Application.ProcessMessages;
    if Message.WParam > VK_ESCAPE then
    begin
      if not (goEditing in Options) then
        exit;

      if (MemoCell.X < FixedCols) or (MemoCell.Y < FixedRows) then // show editor first time
      begin

        ShowEditorAtCell(Selection.Left, Selection.Top);
      end;

      MemoUpdateTimer.Enabled := true;
    end;
  end;
end;

procedure TJvgStringGrid.GetNextCell(var X, Y: Longint);
begin
  if X < ColCount - 1 then
    inc(X)
  else
  begin
    X := FixedCols;
    if Y < RowCount - 1 then
      inc(Y)
    else
      Y := FixedRows;
  end;
end;

procedure TJvgStringGrid.GetPriorCell(var X, Y: Longint);
begin
  if X > 0 then
    dec(X)
  else
  begin
    X := ColCount - 1;
    if Y > 0 then
      dec(Y)
    else
      Y := RowCount - 1;
  end;
end;

procedure TJvgStringGrid.WMSize(var Message: TWMSize);
var
  i, w: integer;
begin
  inherited;
  w := 0;
  for i := 0 to ColCount - 2 do
    inc(w, ColWidths[i]);
  ColWidths[ColCount - 1] := Width - w;
end;

procedure TJvgStringGrid.GetCellGradientParams(Sender: TObject; ACol,
  ARow: Integer; var CellRect: TRect; var Gradient: TJvgGradient);
begin
  if Assigned(OnGetCellGradientParams) then
    OnGetCellGradientParams(Self, ACol, ARow, CellRect, Gradient);
end;

procedure TJvgStringGrid.GetCellStyle(Sender: TObject; var ACol, ARow: Integer;
  var Style: TglGridCellStyle);
begin
  if Assigned(OnGetCellStyle) then
    OnGetCellStyle(Self, ACol, ARow, Style);
end;

procedure TJvgStringGrid.UpdateCaptions(Sender: TObject);
begin
  SetVertCaptions(fsgVertCaptions in FExtOptions);
end;

procedure TJvgStringGrid.SetHottrack(const Value: boolean);
begin
  if Value then
    Include(FExtOptions, fsgHottrack)
  else
    Exclude(FExtOptions, fsgHottrack);
end;

procedure TJvgStringGrid.SetMemoEditor(const Value: boolean);
begin
  if Value then
    Include(FExtOptions, fsgMemoEditor)
  else
    Exclude(FExtOptions, fsgMemoEditor);
end;

procedure TJvgStringGrid.SetWordWrap(const Value: boolean);
begin
  if Value then
    Include(FExtOptions, fsgWordWrap)
  else
    Exclude(FExtOptions, fsgWordWrap);
end;

procedure TJvgStringGrid.SetVertCaptions(Value: boolean);
var
  i: integer;
begin
  if Value then
    Include(FExtOptions, fsgVertCaptions)
  else
    Exclude(FExtOptions, fsgVertCaptions);
  try
    for i := 0 to FCaptions.Count - 1 do
      if fsgVertCaptions in FExtOptions then
        Cells[0, i] := FCaptions[i]
      else
        Cells[i, 0] := FCaptions[i];
  except
  end;
end;

procedure TJvgStringGrid.SetExtOptions(const Value: TglStringGridExtOptions);
begin
  FExtOptions := Value;
end;

procedure TJvgStringGrid.SetTextAlignment(const Value: TAlignment);
begin
  FTextAlignment := Value;
  Repaint;
end;

{ disallow default editor }

function TJvgStringGrid.CanEditShow: Boolean;
begin
  Result := inherited CanEditShow;
  if fsgMemoEditor in FExtOptions then
    Result := false;
end;

end.

