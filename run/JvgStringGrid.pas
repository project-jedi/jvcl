{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgStringGrid.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgStringGrid;

{$I jvcl.inc}

interface

uses
  Windows, Messages, Classes, Controls, Graphics, ExtCtrls,
  Grids, StdCtrls, Forms,
  JvgTypes, JvgCommClasses, JvgUtils, JVCLVer;

const
  JvDefaultEditorColor = $00FEE392;

type
  TglStringGridExtOption = (fsgVertCaptions, fsgHottrack, fsgMemoEditor,
    fsgWordWrap, fsgCellHeightAutoSize, fsgTabThroughCells);
  TglStringGridExtOptions = set of TglStringGridExtOption;

  TglGridCellStyle = record
    Hottracking: Boolean;
    GradientFilling: Boolean;
    Default_Drawing: Boolean;
    R: TRect;
    CellBorders: TglSides;
    BevelInner: TPanelBevel;
    BevelOuter: TPanelBevel;
    BevelBold: Boolean;
    FontStyle: TFontStyles;
    FontColor: TColor;
    BackgrColor: TColor;
    Interspace: Integer
  end;

  TglOnGetCellStyleEvent = procedure(Sender: TObject; ACol, ARow: Longint;
    var Style: TglGridCellStyle) of object;
  TglOnGetCellGradientParamsEvent = procedure(Sender: TObject;
    ACol, ARow: Longint; var CellRect: TRect; var Gradient: TJvgGradient) of object;

  TJvgStringGrid = class(TStringGrid)
  private
    FCaptionTextAlignment: TAlignment;
    FCaptionFont: TFont;
    FBitmap: TBitmap;
    FBmp: TBitmap;
    FImage: TImage;
    FCaptions: TStringList;
    FHottrackThrought: Boolean;
    AHottrackCol: Longint;
    AHottrackRow: Longint;
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
    procedure SetCaptionFont(Value: TFont);
    function GetBitmap: TBitmap;
    procedure SetBitmap(Value: TBitmap);
    procedure SetImage(Value: TImage);
    function GetCaptions: TStrings;
    procedure SetCaptions(Value: TStrings);
    procedure SetVertCaptions(Value: Boolean);
    procedure OnMemoChange(Sender: TObject);
    procedure OnMemoExit(Sender: TObject);
    procedure ShowEditorAtCell(X, Y: Longint);
    procedure UpdateCaptions(Sender: TObject);
    procedure SetHottrack(const Value: Boolean);
    procedure SetMemoEditor(const Value: Boolean);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetExtOptions(const Value: TglStringGridExtOptions);
    procedure SetTextAlignment(const Value: TAlignment);
  protected
    //    function CreateEditor: TInplaceEdit; override;
    function CanEditShow: Boolean; override;

    procedure Loaded; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState:
      TGridDrawState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMChildKey(var Msg: TMessage); message CM_CHILDKEY;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    //    procedure DblClick; dynamic;

    procedure GetCellStyle(Sender: TObject; var ACol, ARow: Integer; var
      Style: TglGridCellStyle); virtual;
    procedure GetCellGradientParams(Sender: TObject; ACol, ARow: Longint; var
      CellRect: TRect; var Gradient: TJvgGradient); virtual;
  public
    //    property InplaceEditor;
    AlignAll: Boolean;
    ColsBold: Integer;
    RowsBold: Integer;
    HottrackColor: TColor;

    property VertCaptions: Boolean write SetVertCaptions default False;
    property Hottrack: Boolean write SetHottrack default False;
    property MemoEditor: Boolean write SetMemoEditor default False;
    property WordWrap: Boolean write SetWordWrap default False;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure GetPriorCell(var X, Y: Longint);
    procedure GetNextCell(var X, Y: Longint);
    procedure ClearSelection;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property CaptionTextAlignment: TAlignment read FCaptionTextAlignment write
      SetCaptionTextAlignment default taCenter;
    property TextAlignment: TAlignment read FTextAlignment write
      SetTextAlignment default taLeftJustify;
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    property Captions: TStrings read GetCaptions write SetCaptions;
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property Image: TImage read FImage write SetImage;
    property ExtOptions: TglStringGridExtOptions read FExtOptions write SetExtOptions;
    property EditorColor: TColor read FEditorColor write FEditorColor default JvDefaultEditorColor;
    property EditorFont: TFont read FEditorFont write FEditorFont;
    property OnGetCellStyle: TglOnGetCellStyleEvent read FOnGetCellStyle write FOnGetCellStyle;
    property OnGetCellGradientParams: TglOnGetCellGradientParamsEvent read
      FOnGetCellGradientParams write FOnGetCellGradientParams;
  end;

implementation

uses
  Math;

constructor TJvgStringGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner); //FHottrackThrought := True;

  FCaptionFont := TFont.Create;
  FEditorFont := TFont.Create;
  MemoUpdateTimer := TTimer.Create(nil);
  MemoUpdateTimer.Enabled := False;
  MemoUpdateTimer.Interval := 200;
  MemoUpdateTimer.OnTimer := OnMemoChange;
  FCaptionTextAlignment := taCenter;
  FCaptions := TStringList.Create;
  FCaptions.OnChange := UpdateCaptions;
  AHottrackCol := -1;
  HottrackColor := clHighlight;
  FEditorColor := JvDefaultEditorColor;
  FExtOptions := [fsgHottrack, fsgMemoEditor, fsgWordWrap,
    fsgCellHeightAutoSize, fsgTabThroughCells];
  Options := Options + [goEditing];

  if csDesigning in ComponentState then
    Exit;
  Memo := TMemo.Create(Self);
  Memo.Visible := False;
  Memo.BorderStyle := bsNone;
  Memo.Parent := Self;
  Memo.OnChange := OnMemoChange;
  Memo.OnExit := OnMemoExit;
  //  Memo.OnKeyDown := OnMemoKeyDown;
  DefaultDrawing := False;
end;

destructor TJvgStringGrid.Destroy;
begin
  FCaptionFont.Free;
  FCaptions.Free;
  FEditorFont.Free;
  MemoUpdateTimer.Free;
  if Assigned(Memo) then
    Memo.Free;
  if Assigned(FBitmap) then
    FBitmap.Free;
  if Assigned(Gradient) then
    Gradient.Free;
  inherited Destroy;
end;

procedure TJvgStringGrid.Loaded;
begin
  inherited Loaded;
  if Assigned(FBitmap) and (not FBitmap.Empty) then
    FBmp := FBitmap;
  VertCaptions := fsgVertCaptions in FExtOptions;
end;

procedure TJvgStringGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState:
  TGridDrawState);
const
  aAlignments: array[TAlignment] of Longint = (DT_LEFT,
    DT_RIGHT, DT_CENTER);
  aWordWrap: array[Boolean] of Longint = (0, DT_WORDBREAK);
var
  R: TRect;
  doHottracking, isFixedCell: Boolean;
  X, X1, Y, Y1, IHeight, IWidth, l, t, I, Interspace: Integer;
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
    if IsItAFilledBitmap(FBmp) then
    begin
      X := R.Left;
      Y := R.Top;
      IHeight := R.Bottom - R.Top;
      IWidth := R.Right - R.Left - 4;
      X1 := X;
      Y1 := Y;
      l := 0;
      t := DefaultRowHeight * ARow mod FBmp.Height;
      while X1 < R.Right do
      begin
        if X1 + IWidth > R.Right then
          IWidth := R.Right - X1;
        while Y1 < R.Bottom do
        begin
          if Y1 + IHeight > R.Bottom then
            IHeight := R.Bottom - Y1;
          BitBlt(Canvas.Handle, X1, Y1, Min(IWidth, FBmp.Width - l),
            Min(IHeight, FBmp.Height - t), FBmp.Canvas.Handle, l, t, SRCCOPY);
          Inc(Y1, Min(IHeight, FBmp.Height));
        end;
        Inc(X1, Min(IWidth, FBmp.Width));
        Y1 := Y;
      end;
    end;

  if True then
  begin
    if FHottrackThrought then
      doHottracking := (AHottrackCol = ACol) or (AHottrackRow = ARow)
    else
    if fsgHottrack in FExtOptions then
      doHottracking := (AHottrackCol = ACol) and (AHottrackRow = ARow) and
        ((ACol < FixedCols) or (ARow < FixedRows))
    else
      doHottracking := False;

    if isFixedCell then
    begin
      Canvas.Font.Assign(FCaptionFont);
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
    Style.GradientFilling := False;
    Style.Default_Drawing := True;
    Style.R := R;
    Style.CellBorders := ALLGLSIDES;
    Style.BevelInner := bvNone;

    Style.BevelBold := False;
    Style.FontStyle := Canvas.Font.Style;
    Style.Interspace := Interspace;
    GetCellStyle(Self, ACol, ARow, Style);
    Canvas.Font.Style := Style.FontStyle;
    R := Style.R;
    if not Style.Default_Drawing then
      Exit;

    if Style.GradientFilling then
    begin
      if Gradient = nil then
        Gradient := TJvgGradient.Create;
      Gradient.Active := True;
      ARect := R;
      GetCellGradientParams(Self, ACol, ARow, ARect, Gradient);
      GradientBox(Canvas.Handle, ARect, Gradient, Integer(psSolid), 1);
    end;

    DrawBoxEx(Canvas.Handle, R, Style.CellBorders, Style.BevelInner,
      Style.BevelOuter, Style.BevelBold,
      Style.BackgrColor, {(BackgrColor = clBtnFace)or}
      Style.GradientFilling);
    ARect := R;
    Inc(ARect.Left, Style.Interspace);
    SetTextColor(Canvas.Handle, ColorToRGB(Style.FontColor));
    SetBkMode(Canvas.Handle, TRANSPARENT);
    DrawText(Canvas.Handle, PChar(Cells[ACol, ARow]), length(Cells[ACol,
      ARow]), R, aAlignments[CellTextAlignment] or DT_WORDBREAK or
        DT_CALCRECT);

    if (fsgCellHeightAutoSize in ExtOptions) and not isFixedCell then
    begin
      I := R.Bottom - R.Top;
      if (I > DefaultRowHeight) and (RowHeights[ARow] < I) then
      begin
        ///RowHeights[ARow] := I;
        Exit;
      end;
    end;

    ARect.Top := ARect.Top + Max(0, (ARect.Bottom - R.Bottom) div 2);
    DrawText(Canvas.Handle, PChar(Cells[ACol, ARow]), length(Cells[ACol,
      ARow]), ARect, aAlignments[CellTextAlignment] or aWordWrap[fsgWordWrap in
        FExtOptions]);
    SetTextColor(Canvas.Handle, ColorToRGB(Font.Color));
  end
  else // if AlignAll then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ARect);
    Inc(ARect.Left, 2);
    R := ARect;
    DrawText(Canvas.Handle, PChar(Cells[ACol, ARow]), length(Cells[ACol,
      ARow]), R, DT_LEFT or DT_WORDBREAK or DT_CALCRECT);
    ARect.Top := ARect.Top + Max(0, (ARect.Bottom - R.Bottom) div 2);
    DrawText(Canvas.Handle, PChar(Cells[ACol, ARow]), length(Cells[ACol,
      ARow]), ARect, DT_LEFT or DT_WORDBREAK)
  end; //  else inherited;
  // DefaultDrawing := True;
end;

procedure TJvgStringGrid.SetCaptionTextAlignment(Value: TAlignment);
begin
  FCaptionTextAlignment := Value;
  Repaint;
end;

procedure TJvgStringGrid.SetCaptionFont(Value: TFont);
begin
  FCaptionFont.Assign(Value);
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
    FBmp := FBitmap
  else
  if Assigned(FImage) and Assigned(FImage.Picture) and
    Assigned(FImage.Picture.Bitmap) then
    FBmp := FImage.Picture.Bitmap
  else
    FBmp := nil;
  Invalidate;
end;

procedure TJvgStringGrid.SetImage(Value: TImage);
begin
  FImage := Value;
  if Assigned(FImage) and Assigned(FImage.Picture) and
    Assigned(FImage.Picture.Bitmap) then
    FBmp := FImage.Picture.Bitmap
  else
  if Assigned(FBitmap) then
    FBmp := FBitmap
  else
    FBmp := nil;
  Invalidate;
end;

function TJvgStringGrid.GetCaptions: TStrings;
begin
  Result := FCaptions;
end;

procedure TJvgStringGrid.SetCaptions(Value: TStrings);
begin
  FCaptions.Assign(Value);
  VertCaptions := fsgVertCaptions in FExtOptions;
end;

//-------------------------------------------------------------------------------

procedure TJvgStringGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
  ACol, ARow: Longint;
begin
  inherited MouseMove(Shift, X, Y);
  if not (fsgHottrack in FExtOptions) then
    Exit;
  MouseToCell(X, Y, ACol, ARow);
  if (ACol >= FixedCols) and (ARow >= FixedRows) then
  begin
    SetCursor(Screen.Cursors[crCross]);
    Exit;
  end;
  SetCursor(Screen.Cursors[crDefault]);

  if ((ACol <> AHottrackCol) or (ARow <> AHottrackRow)) then
  begin
    if (AHottrackCol < FixedCols) or (AHottrackRow < FixedRows) then
    begin
      R := CellRect(AHottrackCol, AHottrackRow);
      InvalidateRect(Handle, @R, False); //DefaultDrawing := False;
    end
    else
    if FHottrackThrought then
    begin
      if (ACol <> AHottrackCol) and (FixedCols > 0) then
      begin
        R := CellRect(AHottrackCol, 0); //DefaultDrawing := False;
        InvalidateRect(Handle, @R, False);
      end;
      if (ARow <> AHottrackRow) and (FixedRows > 0) then
      begin
        R := CellRect(0, AHottrackRow); //DefaultDrawing := False;
        InvalidateRect(Handle, @R, False);
      end;
    end;

    if (ACol < FixedCols) or (ARow < FixedCols) then
    begin
      R := CellRect(ACol, ARow); //DefaultDrawing := False;
      InvalidateRect(Handle, @R, False);
    end
    else
    if FHottrackThrought then
    begin
      if (ACol <> AHottrackCol) and (FixedCols > 0) then
      begin
        R := CellRect(ACol, 0); //DefaultDrawing := False;
        InvalidateRect(Handle, @R, False);
      end;
      if (ARow <> AHottrackRow) and (FixedRows > 0) then
      begin
        R := CellRect(0, ARow); //DefaultDrawing := False;
        InvalidateRect(Handle, @R, False);
      end;
    end;
    AHottrackCol := ACol;
    AHottrackRow := ARow;
  end;
end;

procedure TJvgStringGrid.CMMouseLeave(var Msg: TMessage);
var
  R: TRect;
begin
  inherited;
  if not (fsgHottrack in FExtOptions) then
    Exit;
  R := CellRect(AHottrackCol, AHottrackRow);
  AHottrackCol := -1;
  //DefaultDrawing := False;
  InvalidateRect(Handle, @R, False);
end;

procedure TJvgStringGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  GridCoord: TGridCoord;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (not (goEditing in Options)) or (csDesigning in ComponentState) then
    Exit;

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
    Exit;
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
    Memo.SetBounds(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
    MemoCell.X := X;
    MemoCell.Y := Y;
    Memo.Text := Cells[MemoCell.X, MemoCell.Y];
    Memo.Visible := True;
    Memo.SetFocus;
    Memo.OnChange := OnMemoChange;
    Memo.Color := EditorColor;
    Memo.Font.Assign(EditorFont);
  end;
end;

procedure TJvgStringGrid.OnMemoChange(Sender: TObject);
var
  R: TRect;
  maxHeight, h, I: Integer;
begin
  /// need handle it -> if Assigned(OnSetEditText) then ...
  if (MemoCell.X < FixedCols) or (MemoCell.Y < FixedRows) then
    Exit;

  MemoUpdateTimer.Enabled := False;
  Cells[MemoCell.X, MemoCell.Y] := Memo.Text;

  if (fsgCellHeightAutoSize in ExtOptions) and (MemoCell.Y <> 0) then
  begin
    Canvas.Font.Assign(Font);
    maxHeight := DefaultRowHeight;
    for I := FixedCols to ColCount - 1 do
    begin
      R := CellRect(I, MemoCell.Y);
      if length(Cells[I, MemoCell.Y]) = 0 then
        continue;

      DrawText(Canvas.Handle, PChar(Cells[I, MemoCell.Y]), length(Cells[I,
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
  Memo.Visible := False;
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

procedure TJvgStringGrid.CMChildKey(var Msg: TMessage);
begin
  if Msg.WParam = VK_TAB then
  begin
    if fsgTabThroughCells in ExtOptions then
    begin
      if Memo.Focused then
        Cells[MemoCell.X, MemoCell.Y] := Memo.Text;
      GetNextCell(MemoCell.X, MemoCell.Y);
      ShowEditorAtCell(MemoCell.X, MemoCell.Y);
      Msg.Result := 1;
    end
    else
      inherited;
  end
  else
  begin
    inherited;
    Application.ProcessMessages;
    if Msg.WParam > VK_ESCAPE then
    begin
      if not (goEditing in Options) then
        Exit;

      if (MemoCell.X < FixedCols) or (MemoCell.Y < FixedRows) then // show editor first time
        ShowEditorAtCell(Selection.Left, Selection.Top);

      MemoUpdateTimer.Enabled := True;
    end;
  end;
end;

procedure TJvgStringGrid.GetNextCell(var X, Y: Longint);
begin
  if X < ColCount - 1 then
    Inc(X)
  else
  begin
    X := FixedCols;
    if Y < RowCount - 1 then
      Inc(Y)
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

procedure TJvgStringGrid.WMSize(var Msg: TWMSize);
var
  I, W: Integer;
begin
  inherited;
  W := 0;
  for I := 0 to ColCount - 2 do
    Inc(W, ColWidths[I]);
  ColWidths[ColCount - 1] := Width - W;
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

procedure TJvgStringGrid.SetHottrack(const Value: Boolean);
begin
  if Value then
    Include(FExtOptions, fsgHottrack)
  else
    Exclude(FExtOptions, fsgHottrack);
end;

procedure TJvgStringGrid.SetMemoEditor(const Value: Boolean);
begin
  if Value then
    Include(FExtOptions, fsgMemoEditor)
  else
    Exclude(FExtOptions, fsgMemoEditor);
end;

procedure TJvgStringGrid.SetWordWrap(const Value: Boolean);
begin
  if Value then
    Include(FExtOptions, fsgWordWrap)
  else
    Exclude(FExtOptions, fsgWordWrap);
end;

procedure TJvgStringGrid.SetVertCaptions(Value: Boolean);
var
  I: Integer;
begin
  if Value then
    Include(FExtOptions, fsgVertCaptions)
  else
    Exclude(FExtOptions, fsgVertCaptions);
  try
    for I := 0 to Captions.Count - 1 do
      if fsgVertCaptions in FExtOptions then
        Cells[0, I] := Captions[I]
      else
        Cells[I, 0] := Captions[I];
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
    Result := False;
end;

end.

