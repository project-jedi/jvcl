{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

{*******************************************************}
{                                                       }
{     Delphi Visual Component Library                   }
{                                                       }
{     Copyright (c) 1996,97 Borland International       }
{     Portions copyright (c) 1997 Master-Bank           }
{                                                       }
{*******************************************************}

unit HexDump;

interface

uses
  SysUtils, {$IFDEF WIN32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Menus;

const
  MAXDIGITS = 16;

{ THexDump }

type
  THexStr = array[0..2] of Char;
  THexStrArray = array[0..MAXDIGITS-1] of THexStr;

  THexDump = class(TCustomControl)
  private
    FActive: Boolean;
    FAddress: Pointer;
    FDataSize: Longint;
    FTopLine: Longint;
    FCurrentLine: Longint;
    FVisibleLines: Integer;
    FLineCount: Longint;
    FBytesPerLine: Integer;
    FItemHeight: Integer;
    FItemWidth: Integer;
    FFileColors: array[0..2] of TColor;
    FShowLineMarker: Boolean;
    FShowCharacters: Boolean;
    FShowAddress: Boolean;
    FRelativeAddress: Boolean;
    FBorder: TBorderStyle;
    FHexData: THexStrArray;
    FLineChars: array[0..MAXDIGITS] of Char;
    FLineAddr: array[0..15] of Char;
    procedure CalcPaintParams;
    procedure SetTopLine(Value: Longint);
    procedure SetCurrentLine(Value: Longint);
    procedure SetFileColor(Index: Integer; Value: TColor);
    function GetFileColor(Index: Integer): TColor;
    procedure SetShowCharacters(Value: Boolean);
    procedure SetShowAddress(Value: Boolean);
    procedure SetShowLineMarker(Value: Boolean);
    procedure SetRelativeAddress(Value: Boolean);
    procedure SetBorder(Value: TBorderStyle);
    procedure SetAddress(Value: Pointer);
    procedure SetDataSize(Value: Longint);
    procedure AdjustScrollBars;
    procedure InvalidateLineMarker;
    procedure SetScroll(Value: Longint);
    function LineAddr(Index: Longint): PChar;
    function LineData(Index: Longint): PChar;
    function LineChars(Index: Longint; MaxLen: Integer): PChar;
    function ScrollIntoView: Boolean;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMLostFocus); message CM_EXIT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property CurrentLine: Longint read FCurrentLine write SetCurrentLine;
    property LineCount: Longint read FLineCount;
    property Address: Pointer read FAddress write SetAddress;
    property DataSize: Longint read FDataSize write SetDataSize;
  published
    property Align;
    property Border: TBorderStyle read FBorder write SetBorder default bsSingle;
    property Color default clWindow;
    property Ctl3D default True;
    property Font;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property ShowAddress: Boolean read FShowAddress write SetShowAddress default True;
    property ShowCharacters: Boolean read FShowCharacters write SetShowCharacters default True;
    property ShowLineMarker: Boolean read FShowLineMarker write SetShowLineMarker default True;
    property RelativeAddress: Boolean read FRelativeAddress write SetRelativeAddress default False;
    property AddressColor: TColor index 0 read GetFileColor write SetFileColor default clWindowText;
    property HexDataColor: TColor index 1 read GetFileColor write SetFileColor default clWindowText;
    property AnsiCharColor: TColor index 2 read GetFileColor write SetFileColor default clHighlight;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

function CreateHexDump(AOwner: TWinControl): THexDump;

implementation

uses JvJCLUtils;

{ Create THexDump control }

function CreateHexDump(AOwner: TWinControl): THexDump;
begin
  Result := THexDump.Create(AOwner);
  with Result do begin
    Parent := AOwner;
    Font.Name := 'Courier';
    Align := alClient;
  end;
end;

{$IFNDEF WIN32}
function LongMulDiv(Mult1, Mult2, Div1: Longint): Longint; assembler;
{ copied from GRIDS.PAS }
type
  Quadword = record
    w0, w1, w2, w3: Word;
  end;
var
  Temp: Quadword;
asm
{ Mul }
        MOV     DX,Mult1.Word[2]
        MOV     AX,Mult1.Word[0]
        MOV     CX,Mult2.Word[2]
        MOV     BX,Mult2.Word[0]
        MOV     DI,DX
        MOV     SI,AX
        MUL     BX
        MOV     Temp.w0,AX
        MOV     Temp.w1,DX
        MOV     AX,DI
        MUL     CX
        MOV     Temp.w2,AX
        MOV     Temp.w3,DX
        MOV     AX,DI
        MUL     BX
        ADD     Temp.w1,AX
        ADC     Temp.w2,DX
        ADC     Temp.w3,0
        MOV     AX,SI
        MUL     CX
        ADD     Temp.w1,AX
        ADC     Temp.w2,DX
        ADC     Temp.w3,0
        MOV     DX,Temp.w3
        MOV     SI,Temp.w2
        MOV     BX,Temp.w1
        MOV     AX,Temp.w0
{ rounding }
        MOV     CX,Div1.Word[2]
        MOV     DI,Div1.Word[0]
        SHR     CX,1
        RCR     DI,1
        ADD     AX,DI
        ADC     BX,CX
        ADC     SI,0
        ADC     DX,0
{ Div }
        MOV     CX,32
        CLC
@1:     RCL     AX,1
        RCL     BX,1
        RCL     SI,1
        RCL     DX,1
        JNC     @3
@2:     SUB     SI,Div1.Word[0]
        SBB     DX,Div1.Word[2]
        STC
        LOOP    @1
        JMP     @5
@3:     CMP     DX,Div1.Word[2]
        JC      @4
        JNE     @2
        CMP     SI,Div1.Word[0]
        JNC     @2
@4:     CLC
        LOOP    @1
@5:     RCL     AX,1
        RCL     BX,1
        MOV     CX,SI
        MOV     DX,BX
end;
{$ENDIF}

{ THexDump }

constructor THexDump.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csFramed, csOpaque, csCaptureMouse, csClickEvents,
    csDoubleClicks];
  Ctl3D := True;
  FBorder := bsSingle;
  FShowLineMarker := True;
  ParentColor := False;
  Color := clWindow;
  FFileColors[0] := clWindowText;
  FFileColors[1] := clWindowText;
  FFileColors[2] := clHighlight;
  FShowAddress := True;
  FShowCharacters := True;
  Width := 300;
  Height := 200;
  FillChar(FHexData, SizeOf(FHexData), #9);
  TabStop := True;
end;

destructor THexDump.Destroy;
begin
  inherited Destroy;
end;

procedure THexDump.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do begin
    if (FBorder = bsSingle) then
{$IFDEF WIN32}
      if NewStyleControls and Ctl3D then
        ExStyle := ExStyle or WS_EX_CLIENTEDGE
      else Style := Style or WS_BORDER;
{$ELSE}
      Style := Style or WS_BORDER;
{$ENDIF}
    Style := Style or WS_VSCROLL;
  end;
end;

{ VCL Command Messages }

procedure THexDump.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Canvas.Font := Self.Font;
  FItemHeight := Canvas.TextHeight('A') + 2;
  FItemWidth := Canvas.TextWidth('D') + 1;
  CalcPaintParams;
  AdjustScrollBars;
end;

procedure THexDump.CMCtl3DChanged(var Message: TMessage);
begin
{$IFDEF WIN32}
  if NewStyleControls and (FBorder = bsSingle) then RecreateWnd;
  inherited;
{$ELSE}
  inherited;
  Invalidate;
{$ENDIF}
end;

procedure THexDump.CMEnter;
begin
  inherited;
  InvalidateLineMarker;
end;

procedure THexDump.CMExit;
begin
  inherited;
  InvalidateLineMarker;
end;

{ Windows Messages }

procedure THexDump.WMSize(var Message: TWMSize);
begin
  inherited;
  CalcPaintParams;
  AdjustScrollBars;
end;

procedure THexDump.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure THexDump.WMVScroll(var Message: TWMVScroll);
var
  NewTopLine: Longint;
  LinesMoved: Longint;
  R: TRect;
begin
  inherited;
  if (DataSize = 0) or (Address = nil) then Exit;
  NewTopLine := FTopLine;
  case Message.ScrollCode of
    SB_LINEDOWN: Inc(NewTopLine);
    SB_LINEUP: Dec(NewTopLine);
    SB_PAGEDOWN: Inc(NewTopLine, FVisibleLines - 1);
    SB_PAGEUP: Dec(NewTopLine, FVisibleLines - 1);
    SB_TOP: NewTopLine := 0;
    SB_BOTTOM: NewTopLine := FLineCount - 1;
    SB_THUMBPOSITION, SB_THUMBTRACK:
{$IFDEF WIN32}
      NewTopLine := Message.Pos;
{$ELSE}
      NewTopLine := LongMulDiv(Message.Pos, FLineCount - 1, MaxInt);
{$ENDIF}
  end;
  if NewTopLine >= FLineCount then NewTopLine := FLineCount - 1;
  if NewTopLine < 0 then NewTopLine := 0;
  if NewTopLine <> FTopLine then begin
    LinesMoved := FTopLine - NewTopLine;
    FTopLine := NewTopLine;
    SetScroll(FTopLine);
    if Abs(LinesMoved) = 1 then begin
      R := Bounds(0, 0, ClientWidth, ClientHeight - FItemHeight);
      if LinesMoved = 1 then OffsetRect(R, 0, FItemHeight);
      ScrollWindow(Handle, 0, FItemHeight * LinesMoved, @R, nil);
      if LinesMoved = -1 then
      begin
        R.Top := ClientHeight - FItemHeight;
        R.Bottom := ClientHeight;
      end
      else
      begin
        R.Top := 0;
        R.Bottom := FItemHeight;
      end;
      InvalidateRect(Handle, @R, False);
    end
    else Invalidate;
  end;
end;

{ Painting Related }

procedure THexDump.CalcPaintParams;
const
  Divisor: array[Boolean] of Integer = (3, 4);
var
  CharsPerLine: Integer;
begin
  if FItemHeight < 1 then Exit;
  FVisibleLines := (ClientHeight div FItemHeight) + 1;
  CharsPerLine := ClientWidth div FItemWidth;
  if FShowAddress then Dec(CharsPerLine, 10);
  FBytesPerLine := CharsPerLine div Divisor[FShowCharacters];
  if FBytesPerLine < 1 then FBytesPerLine := 1
  else if FBytesPerLine > MAXDIGITS then FBytesPerLine := MAXDIGITS;
  FLineCount := (DataSize div FBytesPerLine);
  if Boolean(DataSize mod FBytesPerLine) then Inc(FLineCount);
  if (FLineCount - 1) < FCurrentLine then CurrentLine := FLineCount - 1;
  if (FLineCount - 1) < FTopLine then SetTopLine(FLineCount - 1);
end;

procedure THexDump.SetScroll(Value: Longint);
begin
{$IFDEF WIN32}
  SetScrollPos(Handle, SB_VERT, Value, True);
{$ELSE}
  SetScrollPos(Handle, SB_VERT, LongMulDiv(Value, MaxInt,
    FLineCount - 1), True);
{$ENDIF}
end;

procedure THexDump.AdjustScrollBars;
begin
{$IFDEF WIN32}
  SetScrollRange(Handle, SB_VERT, 0, FLineCount - 1, True);
{$ELSE}
  if FLineCount > 1 then SetScrollRange(Handle, SB_VERT, 0, MaxInt, True)
  else SetScrollRange(Handle, SB_VERT, 0, 0, True);
{$ENDIF}
end;

function THexDump.ScrollIntoView: Boolean;
begin
  Result := False;
  if FCurrentLine < FTopLine then begin
    Result := True;
    SetTopLine(FCurrentLine);
  end
  else if FCurrentLine >= (FTopLine + FVisibleLines) - 1 then begin
    SetTopLine(FCurrentLine - (FVisibleLines - 2));
    Result := True;
  end;
end;

procedure THexDump.SetTopLine(Value: Longint);
var
  LinesMoved: Longint;
  R: TRect;
begin
  if Value >= FLineCount then Value := FLineCount - 1;
  if Value < 0 then Value := 0;
  if Value <> FTopLine then begin
    LinesMoved := FTopLine - Value;
    FTopLine := Value;
    SetScroll(FTopLine);
    if Abs(LinesMoved) = 1 then begin
      R := Bounds(1, 0, ClientWidth, ClientHeight - FItemHeight);
      if LinesMoved = 1 then OffsetRect(R, 0, FItemHeight);
      ScrollWindow(Handle, 0, FItemHeight * LinesMoved, @R, nil);
      if LinesMoved = -1 then begin
        R.Top := ClientHeight - FItemHeight;
        R.Bottom := ClientHeight;
      end
      else begin
        R.Top := 0;
        R.Bottom := FItemHeight;
      end;
      InvalidateRect(Handle, @R, False);
    end
    else Invalidate;
  end;
end;

procedure THexDump.SetCurrentLine(Value: Longint);
var
  R: TRect;
begin
  if Value >= FLineCount then Value := FLineCount - 1;
  if Value < 0 then Value := 0;
  if (Value <> FCurrentLine) then begin
    if (FCurrentLine >= FTopLine) and (FCurrentLine < FTopLine + FVisibleLines{ - 1}) then
    begin
      R := Bounds(0, 0, ClientWidth, FItemHeight);
      OffsetRect(R, 0, (FCurrentLine - FTopLine) * FItemHeight);
      if FShowLineMarker then {!!}
        InvalidateRect(Handle, @R, True);
    end;
    FCurrentLine := Value;
    R := Bounds(0, 0, ClientWidth, FItemHeight);
    OffsetRect(R, 0, (FCurrentLine - FTopLine) * FItemHeight);
    if FShowLineMarker then {!!}
      InvalidateRect(Handle, @R, True);
    ScrollIntoView;
  end;
end;

procedure THexDump.InvalidateLineMarker;
var
  R: TRect;
begin
  if FShowLineMarker then begin
    R := Bounds(0, 0, ClientWidth, FItemHeight);
    OffsetRect(R, 0, (FCurrentLine - FTopLine) * FItemHeight);
    InvalidateRect(Handle, @R, True);
  end;
end;

procedure THexDump.Paint;
var
  R, ItemRect: TRect;
  I: Integer;
  AddressWidth: Integer;
  TabStop: Integer;
  ByteCnt: Integer;
begin
  inherited Paint;
  Canvas.Brush.Color := Self.Color;
  Canvas.FillRect(Rect(0, 0, ClientWidth, ClientHeight));
  if FShowAddress then AddressWidth := FItemWidth * 10
  else AddressWidth := 0;
  R := Bounds(1, 0, ClientWidth, FItemHeight);
  TabStop := FItemWidth * 3;
  Canvas.Font.Color := FFileColors[1];
  ByteCnt := FBytesPerLine;
  for I := 0 to FVisibleLines - 1 do begin
    R.Left := 1;
    if I + FTopLine < FLineCount then begin
      if FShowAddress then begin
        Canvas.Font.Color := FFileColors[0];
        R.Right := R.Left + AddressWidth;
        ExtTextOut(Canvas.Handle, R.Left, R.Top, ETO_OPAQUE or ETO_CLIPPED, @R,
          LineAddr(I + FTopLine), 10, nil);
        R.Left := R.Right;
        R.Right := ClientWidth;
        Canvas.Font.Color := FFileColors[1];
      end;
      if FShowLineMarker and ((I + FTopLine) = FCurrentLine) then begin
        Canvas.Brush.Color := clHighlight;
        Canvas.Font.Color := clHighlightText;
        ItemRect := Bounds(AddressWidth, 0, (FItemWidth * (FBytesPerLine * 3)) -
          FItemWidth + 1, FItemHeight);
        OffsetRect(ItemRect, 0, (FCurrentLine - FTopLine) * FItemHeight);
        Canvas.FillRect(ItemRect);
      end;
      if (I + FTopLine = FLineCount - 1) and ((DataSize mod FBytesPerLine) > 0) then
        ByteCnt := DataSize mod FBytesPerLine;
      TabbedTextOut(Canvas.Handle, R.Left, R.Top, LineData(I + FTopLine),
        (ByteCnt * 3) - 1, 1, TabStop, R.Left);
      if FShowLineMarker and ((I + FTopLine) = FCurrentLine) and Focused then
        Canvas.DrawFocusRect(ItemRect);
      Canvas.Brush.Color := Self.Color;
      Canvas.Font.Color := FFileColors[1];
      if FShowCharacters then begin
        R.Left := AddressWidth + (FItemWidth * (FBytesPerLine * 3));
        R.Right := ClientWidth;
        Canvas.Font.Color := FFileColors[2];
        ExtTextOut(Canvas.Handle, R.Left, R.Top, ETO_OPAQUE or ETO_CLIPPED, @R,
          LineChars(I + FTopLine, ByteCnt), ByteCnt, nil);
        Canvas.Font.Color := FFileColors[1];
      end;
    end
    else ExtTextOut(Canvas.Handle, R.Left, R.Top, ETO_OPAQUE or ETO_CLIPPED,
      @R, nil, 0, nil);
    OffsetRect(R, 0, FItemHeight);
  end;
end;

{ Event Overrides }

procedure THexDump.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if not FActive then Exit;
  case Key of
    VK_DOWN:
      if FShowLineMarker then
        CurrentLine := CurrentLine + 1
      else
        CurrentLine := FTopLine + FVisibleLines - 1;
    VK_UP:
      if FShowLineMarker then
        CurrentLine := CurrentLine - 1
      else
        CurrentLine := FTopLine - 1;
    VK_NEXT: CurrentLine := CurrentLine + FVisibleLines;
    VK_PRIOR: CurrentLine := CurrentLine - FVisibleLines;
    VK_HOME: CurrentLine := 0;
    VK_END: CurrentLine := FLineCount - 1;
  end;
end;

procedure THexDump.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if not Focused and CanFocus then SetFocus;
  if (Button = mbLeft) and FActive then
    CurrentLine := FTopLine + (Y div FItemHeight);
end;

{ Property Set/Get Routines }

procedure THexDump.SetBorder(Value: TBorderStyle);
begin
  if Value <> FBorder then begin
    FBorder := Value;
    RecreateWnd;
  end;
end;

procedure THexDump.SetRelativeAddress(Value: Boolean);
begin
  if FRelativeAddress <> Value then begin
    FRelativeAddress := Value;
    if ShowAddress then Invalidate;
  end;
end;

procedure THexDump.SetShowAddress(Value: Boolean);
begin
  if FShowAddress <> Value then begin
    FShowAddress := Value;
    CalcPaintParams;
    Invalidate;
    AdjustScrollBars;
  end;
end;

procedure THexDump.SetShowCharacters(Value: Boolean);
begin
  if Value <> FShowCharacters then begin
    FShowCharacters := Value;
    CalcPaintParams;
    Invalidate;
    AdjustScrollBars;
  end;
end;

procedure THexDump.SetShowLineMarker(Value: Boolean);
begin
  if Value <> FShowLineMarker then begin
    FShowLineMarker := Value;
    Invalidate;
  end;
end;

procedure THexDump.SetFileColor(Index: Integer; Value: TColor);
begin
  if FFileColors[Index] <> Value then
  begin
    FFileColors[Index] := Value;
    Invalidate;
  end;
end;

function THexDump.GetFileColor(Index: Integer): TColor;
begin
  Result := FFileColors[Index];
end;

procedure THexDump.SetAddress(Value: Pointer);
begin
  FActive := (Value <> nil);
  FAddress := Value;
  if not FActive then SetDataSize(0)
  else Invalidate;
  if FActive then begin
    CurrentLine := 0;
    ScrollIntoView;
  end;
end;

procedure THexDump.SetDataSize(Value: Longint);
begin
  FDataSize := Value;
  CalcPaintParams;
  Invalidate;
  AdjustScrollBars;
end;

function THexDump.LineAddr(Index: Longint): PChar;
begin
  if RelativeAddress then
    Result := StrFmt(FLineAddr, '%p: ', [HugeOffset(Pointer(0),
      Index * FBytesPerLine)])
  else
    Result := StrFmt(FLineAddr, '%p: ', [HugeOffset(FAddress,
      Index * FBytesPerLine)]);
end;

function THexDump.LineData(Index: Longint): PChar;

  procedure SetData(P: PChar);
  const
    HexDigits : array[0..15] of Char = '0123456789ABCDEF';
  var
    I: Integer;
    B: Byte;
  begin
    for I := 0 to FBytesPerLine - 1 do begin
      try
        B := Byte(P[I]);
        FHexData[I][0] := HexDigits[B SHR $04];
        FHexData[I][1] := HexDigits[B AND $0F];
      except
        FHexData[I][0] := '?';
        FHexData[I][1] := '?';
      end;
    end;
  end;

begin
  SetData(PChar(HugeOffset(FAddress, Index * FBytesPerLine)));
  Result := FHexData[0];
end;

function THexDump.LineChars(Index: Longint; MaxLen: Integer): PChar;
var
  I: Integer;
begin
  Move(HugeOffset(FAddress, Index * FBytesPerLine)^, FLineChars, MaxLen);
  Result := FLineChars;
  for I := 0 to MaxLen - 1 do
    if Result[I] < #32 then Result[I] := '.';
end;

end.
