{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvStaticText.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].
Peter Thörnqvist <peter3@peter3.com>

Last Modified: 2000-10-22

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
=============
Changes 2002-10-22:
===================
Totally reimplemented (though a lot of code was taken from original TStaticText) to add new properties:
WordWrap, Layout (vertial Alignment), TextMargins (to offset from the edges)

Also adds virtual DrawItem and AdjustBounds methods to make it easier to derive new
components that handles the drawing differently

-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvStaticText;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls, Forms,
  JVCLVer;

type
  TJvTextMargins = class(TPersistent)
  private
    FY: Word;
    FX: Word;
    FOnChange: TNotifyEvent;
    procedure SetX(const Value: Word);
    procedure SetY(const Value: Word);
    procedure Change;
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property X: Word read FX write SetX;
    property Y: Word read FY write SetY;
  end;

  TJvCustomStaticText = class(TWinControl)
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FHintColor: TColor;
    FSaved: TColor;
    FOver: Boolean;
    FFontSave: TFont;
    FHotTrack: Boolean;
    FHotTrackFont: TFont;
    FAboutJVCL: TJVCLAboutInfo;
    FLayout: TTextLayout;
    FAlignment: TAlignment;
    FAutoSize: Boolean;
    FBorderStyle: TStaticBorderStyle;
    FFocusControl: TWinControl;
    FShowAccelChar: Boolean;
    FTextMargins: TJvTextMargins;
    FWordWrap: Boolean;
    procedure CMDialogChar(var Msg: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure SetAlignment(Value: TAlignment);
    procedure SetBorderStyle(Value: TStaticBorderStyle);
    procedure SetFocusControl(Value: TWinControl);
    procedure SetShowAccelChar(Value: Boolean);
    procedure SetHotTrackFont(const Value: TFont);
    procedure SetLayout(const Value: TTextLayout);
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    procedure SetTextMargins(const Value: TJvTextMargins);
    procedure SetWordWrap(const Value: Boolean);
    procedure DoMarginsChange(Sender: TObject);
  protected
    procedure Loaded; override;
    procedure AdjustBounds; dynamic;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetAutoSize(Value: Boolean);
      {$IFDEF COMPILER6_UP} override; {$ENDIF}
    procedure DrawItem(const DrawItemStruct: TDrawItemStruct); virtual;
    function GetTextDisplayInfo(aDC: HDC; var ARect: TRect): Cardinal;
    procedure CreateParams(var Params: TCreateParams); override;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property BorderStyle: TStaticBorderStyle read FBorderStyle write SetBorderStyle default sbsNone;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property ShowAccelChar: Boolean read FShowAccelChar write SetShowAccelChar default True;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property HotTrackFont: TFont read FHotTrackFont write SetHotTrackFont;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property Layout: TTextLayout read FLayout write SetLayout;
    property WordWrap: Boolean read FWordWrap write SetWordWrap;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property TextMargins: TJvTextMargins read FTextMargins write SetTextMargins;
  end;

  TJvStaticText = class(TJvCustomStaticText)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property HintColor;
    property HotTrack;
    property HotTrackFont;
    property Layout;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnContextPopup;
    property OnCtl3DChanged;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnParentColorChange;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

//=== TJvCustomStaticText ====================================================

constructor TJvCustomStaticText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTextMargins := TJvTextMargins.Create;
  FTextMargins.OnChange := DoMarginsChange;

  FHintColor := clInfoBk;
  FHotTrackFont := TFont.Create;
  FFontSave := TFont.Create;
  FOver := False;
  FLayout := tlTop;
  ControlStyle := [csCaptureMouse, csClickEvents, csSetCaption,
    csOpaque, csReplicatable, csDoubleClicks];

  Width := 65;
  Height := 17;
  FAutoSize := True;
  FShowAccelChar := True;
  //  ControlStyle := ControlStyle + [csAcceptsControls];
end;

destructor TJvCustomStaticText.Destroy;
begin
  FHotTrackFont.Free;
  FFontSave.Free;
  FTextMargins.Free;
  inherited Destroy;
end;

procedure TJvCustomStaticText.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

procedure TJvCustomStaticText.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvCustomStaticText.CMMouseEnter(var Msg: TMessage);
begin
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FHintColor;
    if FHotTrack then
    begin
      FFontSave.Assign(Font);
      Font.Assign(FHotTrackFont);
    end;
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
  inherited;
end;

procedure TJvCustomStaticText.CMMouseLeave(var Msg: TMessage);
begin
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if FOver then
  begin
    Application.HintColor := FSaved;
    if FHotTrack then
      Font.Assign(FFontSave);
    FOver := False;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
  inherited;
end;

procedure TJvCustomStaticText.SetHotTrackFont(const Value: TFont);
begin
  FHotTrackFont.Assign(Value);
end;

procedure TJvCustomStaticText.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, 'STATIC');
  with Params do
    Style := Style or SS_NOTIFY or SS_OWNERDRAW;
end;

procedure TJvCustomStaticText.SetLayout(const Value: TTextLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TJvCustomStaticText.CNDrawItem(var Msg: TWMDrawItem);
begin
  DrawItem(Msg.DrawItemStruct^);
end;

procedure TJvCustomStaticText.DrawItem(const DrawItemStruct: TDrawItemStruct);
const
  cBorders: array [TStaticBorderStyle] of DWORD = (0, BF_MONO, BF_SOFT);
var
  R: TRect;
  DrawStyle: Cardinal;
  B: HBRUSH;
begin
  B := CreateSolidBrush(ColorToRGB(Color));
  try
    with DrawItemStruct do
    begin
      R := rcItem;
      FillRect(hDC, R, B);
      if BorderStyle <> sbsNone then
        DrawEdge(hDC, R, BDR_SUNKENOUTER, BF_ADJUST or BF_RECT or cBorders[BorderStyle]);
      DrawStyle := GetTextDisplayInfo(hDC, R);
      case Layout of
        tlTop:
          OffsetRect(R, 0, FTextMargins.Y);
        tlBottom:
          OffsetRect(R, 0, (ClientHeight - R.Bottom) - FTextMargins.Y);
        tlCenter:
          OffsetRect(R, 0, (ClientHeight - R.Bottom) div 2);
      end;
      case Alignment of
        taLeftJustify:
          OffsetRect(R, FTextMargins.X, 0);
        taRightJustify:
          OffsetRect(R, (Width - R.Right) - FTextMargins.X, 0);
        taCenter:
          OffsetRect(R, (Width - R.Right) div 2, 0);
      end;
      SetBkMode(hDC, Windows.TRANSPARENT);
      DrawText(hDC, PChar(Caption), Length(Caption), R, DrawStyle);
    end;
  finally
    DeleteObject(B);
  end;
end;

procedure TJvCustomStaticText.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    if Value then
      AdjustBounds;
  end;
end;

procedure TJvCustomStaticText.AdjustBounds;
var
  DC: HDC;
  R: TRect;
  SaveFont: HFont;
  TextSize: TSize;
begin
  if not (csReading in ComponentState) and FAutoSize and HandleAllocated then
  begin
    DC := GetDC(0);
    if not WordWrap then
    begin
      SaveFont := SelectObject(DC, Font.Handle);
      GetTextExtentPoint32(DC, PChar(Caption), Length(Caption), TextSize);
      SelectObject(DC, SaveFont);
      SetBounds(Left, Top,
        TextSize.cx + (GetSystemMetrics(SM_CXBORDER) * 4),
        TextSize.cy + (GetSystemMetrics(SM_CYBORDER) * 4));
    end
    else
    begin
      R := ClientRect;
      GetTextDisplayInfo(DC, R);
      SetBounds(Left, Top, R.Right, R.Bottom);
    end;
    ReleaseDC(0, DC);
  end;
end;

procedure TJvCustomStaticText.CMDialogChar(var Msg: TCMDialogChar);
begin
  if (FFocusControl <> nil) and Enabled and ShowAccelChar and
    IsAccel(Msg.CharCode, Caption) then
    with FFocusControl do
      if CanFocus then
      begin
        SetFocus;
        Msg.Result := 1;
      end;
end;

procedure TJvCustomStaticText.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  AdjustBounds;
end;

procedure TJvCustomStaticText.CMTextChanged(var Msg: TMessage);
begin
  inherited;
  AdjustBounds;
end;

procedure TJvCustomStaticText.Loaded;
begin
  inherited Loaded;
  AdjustBounds;
end;

procedure TJvCustomStaticText.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure TJvCustomStaticText.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TJvCustomStaticText.SetBorderStyle(Value: TStaticBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    Invalidate;
  end;
end;

procedure TJvCustomStaticText.SetFocusControl(Value: TWinControl);
begin
  FFocusControl := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TJvCustomStaticText.SetShowAccelChar(Value: Boolean);
begin
  if FShowAccelChar <> Value then
  begin
    FShowAccelChar := Value;
    Invalidate;
  end;
end;

procedure TJvCustomStaticText.SetTextMargins(const Value: TJvTextMargins);
begin
  if Value = nil then
  begin
    FTextMargins.X := 0;
    FTextMargins.Y := 0
  end
  else
  begin
    FTextMargins.X := Value.X;
    FTextMargins.Y := Value.Y;
  end;
end;

procedure TJvCustomStaticText.DoMarginsChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvCustomStaticText.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    AdjustBounds;
    Invalidate;
  end;
end;

function TJvCustomStaticText.GetTextDisplayInfo(aDC: HDC; var ARect: TRect): Cardinal;
const
  cAlignment: array [Boolean, TAlignment] of DWORD =
    ((DT_LEFT, DT_RIGHT, DT_CENTER), (DT_RIGHT, DT_LEFT, DT_CENTER));
  cLayout: array [TTextLayout] of DWORD = (DT_TOP, DT_VCENTER, DT_BOTTOM);
  cDrawAccel: array [Boolean] of DWORD = (DT_NOPREFIX, 0);
  cWordWrap: array [Boolean] of DWORD = (DT_SINGLELINE, DT_WORDBREAK);
begin
  Result := DT_EXPANDTABS or cAlignment[UseRightToLeftAlignment, Alignment] or
    cLayout[Layout] or cDrawAccel[ShowAccelChar] or cWordWrap[WordWrap];

  DrawText(aDC, PChar(Caption), Length(Caption), ARect, Result or DT_CALCRECT);
end;

procedure TJvCustomStaticText.WMSize(var Msg: TWMSize);
begin
  inherited;
  Invalidate;
end;

//=== TJvTextMargins =========================================================

procedure TJvTextMargins.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TJvTextMargins.SetX(const Value: Word);
begin
  if FX <> Value then
  begin
    FX := Value;
    Change;
  end;
end;

procedure TJvTextMargins.SetY(const Value: Word);
begin
  if FY <> Value then
  begin
    FY := Value;
    Change;
  end;
end;

end.

