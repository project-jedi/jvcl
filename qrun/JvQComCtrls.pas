{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvComCtrls.PAS, released Oct 10, 1999.

The Initial Developer of the Original Code is Petr Vones (petr dott v att mujmail dott cz)
Portions created by Petr Vones are Copyright (C) 1999 Petr Vones.
Portions created by Microsoft are Copyright (C) 1998, 1999 Microsoft Corp.
All Rights Reserved.

Contributor(s):
Peter Below [100113 dott 1101 att compuserve dott com] - alternate TJvPageControl.OwnerDraw routine
Peter Thörnqvist [peter3 at sourceforge dot net] added TJvIPAddress.AddressValues and TJvPageControl.ReduceMemoryUse
Alfi [alioscia_alessi att onde dott net] alternate TJvPageControl.OwnerDraw routine
Rudy Velthuis - ShowRange in TJvTrackBar
Andreas Hausladen - TJvIPAddress designtime bug, components changed to JvExVCL
Kai Gossens - TJvIPAddress: changing Color, drawing bug on XP (fat frame on edits removed)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  TJvTreeView:
    When dragging an item and MultiSelect is True droptarget node is not painted
    correctly.
-----------------------------------------------------------------------------}
// $Id$

unit JvQComCtrls;

{$I jvcl.inc}

interface

uses
  QWindows, QMessages, Contnrs, QGraphics, QControls, QForms,
  Classes, // (ahuser) "Classes" after "Forms" (D5 warning)
  QMenus, QComCtrls, QImgList, QButtons,  
  QExtCtrls, 
  JvQJVCLUtils, JvQComponent, JvQExControls, JvQExComCtrls;

const
  JvDefPageControlBorder = 4; 
  JvDefaultInactiveColorFrom = TColor($D7D7D7);
  JvDefaultInactiveColorTo= TColor($ADADAD);

type 

  // TJvHintSource is a hint enumeration type to describe how to display hints for
  // controls that have hint properties both for the main control as well as
  // for it's subitems (like a PageControl)
  // TODO: (p3) this should really be moved to JvTypes or something...
  TJvHintSource =
    (
    hsDefault, // use default hint behaviour (i.e as regular control)
    hsForceMain, // use the main hint even if subitems have hints
    hsForceChildren, // always use subitems hints even if empty
    hsPreferMain, // use main control hint unless empty then use subitems hints
    hsPreferChildren // use subitems hints unless empty then use main control hint
    );

  // painters that can be used to draw the tabs of a TPageControl or TTabControl
  TJvTabControlPainter = class(TJvComponent)
  private
    FClients: TList;
  protected
    // descendants must override and implement this method
    procedure DrawTab(AControl: TCustomTabControl; Canvas: TCanvas;
      Images: TCustomImageList; ImageIndex: Integer; const Caption: string;
      const Rect: TRect; Active, Enabled: Boolean); virtual; abstract;
    procedure Change; virtual;

    procedure RegisterChange(AControl: TCustomTabControl);
    procedure UnRegisterChange(AControl: TCustomTabControl);
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    destructor Destroy; override;
  end;

  TJvTabDefaultPainter = class(TJvTabControlPainter)
  private
    FActiveFont: TFont;
    FDisabledFont: TFont;
    FInactiveFont: TFont;
    FInactiveColorTo: TColor;
    FActiveColorTo: TColor;
    FDisabledColorTo: TColor;
    FInactiveColorFrom: TColor;
    FActiveColorFrom: TColor;
    FDisabledColorFrom: TColor;
    FActiveGradientDirection: TFillDirection;
    FInactiveGradientDirection: TFillDirection;
    FDisabledGradientDirection: TFillDirection;
    FGlyphLayout: TButtonLayout;
    FDivider: Boolean;
    FShowFocus: Boolean;
    procedure SetActiveFont(const Value: TFont);
    procedure SetDisabledFont(const Value: TFont);
    procedure SetInactiveFont(const Value: TFont);
    procedure SetActiveColorFrom(const Value: TColor);
    procedure SetActiveColorTo(const Value: TColor);
    procedure SetActiveGradientDirection(const Value: TFillDirection);
    procedure SetDisabledColorFrom(const Value: TColor);
    procedure SetDisabledColorTo(const Value: TColor);
    procedure SetDisabledGradientDirection(const Value: TFillDirection);
    procedure SetInactiveColorFrom(const Value: TColor);
    procedure SetInactiveColorTo(const Value: TColor);
    procedure SetInactiveGradientDirection(const Value: TFillDirection);
    function IsActiveFontStored: Boolean;
    function IsInactiveFontStored: Boolean;
    function IsDisabledFontStored: Boolean;
    procedure SetGlyphLayout(const Value: TButtonLayout);
    procedure SetDivider(const Value: Boolean);
    procedure SetShowFocus(const Value: Boolean);
  protected
    procedure DrawTab(AControl: TCustomTabControl; Canvas: TCanvas;
      Images: TCustomImageList; ImageIndex: Integer; const Caption: string;
      const Rect: TRect; Active, Enabled: Boolean); override;
    procedure DoFontChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ActiveFont: TFont read FActiveFont write SetActiveFont stored IsActiveFontStored;
    property ActiveColorFrom: TColor read FActiveColorFrom write SetActiveColorFrom default clWhite;
    property ActiveColorTo: TColor read FActiveColorTo write SetActiveColorTo default clBtnFace;
    property ActiveGradientDirection: TFillDirection read FActiveGradientDirection write SetActiveGradientDirection default fdTopToBottom;
    property InactiveFont: TFont read FInactiveFont write SetInactiveFont stored IsInactiveFontStored;
    property InactiveColorFrom: TColor read FInactiveColorFrom write SetInactiveColorFrom default JvDefaultInactiveColorFrom;
    property InactiveColorTo: TColor read FInactiveColorTo write SetInactiveColorTo default JvDefaultInactiveColorTo;
    property InactiveGradientDirection: TFillDirection read FInactiveGradientDirection write SetInactiveGradientDirection default fdTopToBottom;
    property DisabledFont: TFont read FDisabledFont write SetDisabledFont stored IsDisabledFontStored;
    property DisabledColorFrom: TColor read FDisabledColorFrom write SetDisabledColorFrom default clBtnFace;
    property DisabledColorTo: TColor read FDisabledColorTo write SetDisabledColorTo default clBtnFace;
    property DisabledGradientDirection: TFillDirection read FDisabledGradientDirection write SetDisabledGradientDirection default fdTopToBottom;
    property GlyphLayout: TButtonLayout read FGlyphLayout write SetGlyphLayout default blGlyphLeft;
    property Divider: Boolean read FDivider write SetDivider default False;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus default False;
  end;

  TJvTabControl = class(TJvExTabControl)
  private
    FTabPainter: TJvTabControlPainter;
    FRightClickSelect: Boolean; 
    procedure SetTabPainter(const Value: TJvTabControlPainter); // not WantKeys
  protected 
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean): Boolean; override;  
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property RightClickSelect: Boolean read FRightClickSelect write FRightClickSelect default False;
    property TabPainter: TJvTabControlPainter read FTabPainter write SetTabPainter;
    property HintColor;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
    property Color;
  end;

  TJvPageControl = class(TJvExPageControl)
  private
    FClientBorderWidth: TBorderWidth;
    FHideAllTabs: Boolean;
    FHandleGlobalTab: Boolean;
    FHintSource: TJvHintSource;
    FReduceMemoryUse: Boolean;
    FTabPainter: TJvTabControlPainter;
    FRightClickSelect: Boolean;
    procedure SetClientBorderWidth(const Value: TBorderWidth); 
    procedure SetHideAllTabs(const Value: Boolean);
    function FormKeyPreview: Boolean;
    procedure SetReduceMemoryUse(const Value: Boolean);
    procedure SetTabPainter(const Value: TJvTabControlPainter);
  protected
    function HintShow(var HintInfo: THintInfo): Boolean; override;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;

    procedure Loaded; override;
    function CanChange: Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override; 
    function DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean): Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;  
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateTabImages;
  published
    property TabPainter: TJvTabControlPainter read FTabPainter write SetTabPainter;
    property HintSource: TJvHintSource read FHintSource write FHintSource default hsDefault;
    property HandleGlobalTab: Boolean read FHandleGlobalTab write FHandleGlobalTab default False;
    property ClientBorderWidth: TBorderWidth read FClientBorderWidth write SetClientBorderWidth default JvDefPageControlBorder;
    property ParentColor;
    property RightClickSelect: Boolean read FRightClickSelect write FRightClickSelect default False;
    property ReduceMemoryUse: Boolean read FReduceMemoryUse write SetReduceMemoryUse default False;
    property HideAllTabs: Boolean read FHideAllTabs write SetHideAllTabs default False;
    property HintColor;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
    property Color;
  end;

  TJvTrackToolTipSide = (tsLeft, tsTop, tsRight, tsBottom);
  TJvTrackToolTipEvent = procedure(Sender: TObject; var ToolTipText: string) of object;

  TJvTrackBar = class(TJvExTrackBar)
  private
    FOnChanged: TNotifyEvent;
    FShowRange: Boolean; 
    procedure SetShowRange(const Value: Boolean);
  protected
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override; 
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ShowRange: Boolean read FShowRange write SetShowRange default True; 
    property HintColor;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property Color;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp; 
  end;
 

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils,
  JclStrings,
  JvQConsts, JvQJCLUtils;



//=== { TJvTabControlPainter } ===============================================

destructor TJvTabControlPainter.Destroy;
begin
  if FClients <> nil then
    while FClients.Count > 0 do
      UnRegisterChange(TCustomTabControl(FClients.Last));
  FreeAndNil(FClients);
  inherited Destroy;
end;

procedure TJvTabControlPainter.Change;
var
  I: Integer;
begin
  if FClients <> nil then
    for I := 0 to FClients.Count - 1 do
      TCustomTabControl(FClients[I]).Invalidate;
end;

procedure TJvTabControlPainter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent is TCustomTabControl) and (FClients <> nil) then
    FClients.Remove(AComponent);
end;

procedure TJvTabControlPainter.RegisterChange(AControl: TCustomTabControl);
begin
  if FClients = nil then
    FClients := TList.Create;
  if AControl <> nil then
  begin
    FClients.Add(AControl);
    AControl.FreeNotification(Self);
    AControl.Invalidate;
  end;
end;

procedure TJvTabControlPainter.UnRegisterChange(AControl: TCustomTabControl);
begin
  if FClients <> nil then
  begin
    FClients.Remove(AControl);
    if (AControl <> nil) and not (csDestroying in AControl.ComponentState) then
      AControl.Invalidate;
  end;
end;

//=== { TJvTabDefaultPainter } ===============================================

constructor TJvTabDefaultPainter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActiveFont := TFont.Create;
  if Owner is TForm then
    FActiveFont.Assign(TForm(Owner).Font)
  else  
    FActiveFont.Assign(Screen.HintFont); 
  FActiveFont.Color := clHighlight;
  FActiveFont.OnChange := DoFontChange;
  FActiveColorFrom := clWhite;
  FActiveColorTo := clBtnFace;
  FActiveGradientDirection := fdTopToBottom;

  FDisabledFont := TFont.Create;
  if Owner is TForm then
    FDisabledFont.Assign(TForm(Owner).Font)
  else  
    FDisabledFont.Assign(Screen.HintFont); 
  FDisabledFont.Color := clGrayText;
  FDisabledFont.OnChange := DoFontChange;
  FDisabledColorFrom := clBtnFace;
  FDisabledColorTo := clBtnFace;
  FDisabledGradientDirection := fdTopToBottom;

  FInactiveFont := TFont.Create;
  if Owner is TForm then
    FInactiveFont.Assign(TForm(Owner).Font)
  else  
    FInactiveFont.Assign(Screen.HintFont); 
  FInactiveFont.OnChange := DoFontChange;
  FInactiveColorFrom := JvDefaultInactiveColorFrom;
  FInactiveColorTo := JvDefaultInactiveColorTo;
  FInactiveGradientDirection := fdTopToBottom;
  FGlyphLayout := blGlyphLeft;
end;

destructor TJvTabDefaultPainter.Destroy;
begin
  FActiveFont.Free;
  FDisabledFont.Free;
  FInactiveFont.Free;
  inherited Destroy;
end;

procedure TJvTabDefaultPainter.DoFontChange(Sender: TObject);
begin
  Change;
end;

procedure TJvTabDefaultPainter.DrawTab(AControl: TCustomTabControl;
  Canvas: TCanvas; Images: TCustomImageList; ImageIndex: Integer;
  const Caption: string; const Rect: TRect; Active, Enabled: Boolean);
var
  TextRect, ImageRect: TRect;
  SaveState: Integer;
  procedure DrawDivider(X, Y, X1, Y1: Integer);
  begin
    Canvas.Pen.Color := clBtnShadow;
    Canvas.MoveTo(X, Y);
    Canvas.LineTo(X1, Y1);
    Canvas.Pen.Color := clHighlightText;
    Canvas.MoveTo(X + 1, Y + 1);
    Canvas.LineTo(X1 + 1, Y1 + 1);
  end;
begin
  TextRect := Rect;
  ImageRect := Rect;
  if not Enabled then
  begin
    GradientFillRect(Canvas, TextRect, DisabledColorFrom, DisabledColorTo, DisabledGradientDirection, 255);
    Canvas.Font := DisabledFont;
  end
  else
  if Active then
  begin
    GradientFillRect(Canvas, TextRect, ActiveColorFrom, ActiveColorTo, ActiveGradientDirection, 255);
    Canvas.Font := ActiveFont;
  end
  else
  begin
    GradientFillRect(Canvas, TextRect, InactiveColorFrom, InactiveColorTo, InactiveGradientDirection, 255);
    Canvas.Font := InactiveFont;
  end;
  if Assigned(Images) and (ImageIndex >= 0) and (ImageIndex < Images.Count) then
  begin // GlyphLayout is only used if we have images
    case GlyphLayout of
      blGlyphLeft:
        begin
          Inc(ImageRect.Left, 4);
          ImageRect.Right := ImageRect.Left + Images.Width + 4;
          TextRect.Left := ImageRect.Right;
        end;
      blGlyphRight:
        begin
          Dec(ImageRect.Right, 4);
          ImageRect.Left := ImageRect.Right - Images.Width - 4;
          TextRect.Right := ImageRect.Left;
        end;
      blGlyphTop:
        begin
          Dec(ImageRect.Bottom, RectHeight(Rect) div 2);
          TextRect.Top := ImageRect.Bottom;
          if Divider and (Caption <> '') then
            DrawDivider(Rect.Left + 4 + Ord(Active), Rect.Top + RectHeight(Rect) div 2, Rect.Right - 4 - Ord(Active), Rect.Top + RectHeight(Rect) div 2);
        end;
      blGlyphBottom:
        begin
          Inc(ImageRect.Top, RectHeight(Rect) div 2);
          TextRect.Bottom := ImageRect.Top;
          if Divider and (Caption <> '') then
            DrawDivider(Rect.Left + 4 + Ord(Active), Rect.Top + RectHeight(Rect) div 2, Rect.Right - 4 - Ord(Active), Rect.Top + RectHeight(Rect) div 2);
        end;
    end;
    InflateRect(ImageRect, -(RectWidth(ImageRect) - Images.Width) div 2, -(RectHeight(ImageRect) - Images.Height) div 2);
    SaveState := SaveDC(Canvas.Handle);
    try
      Images.Draw(Canvas, ImageRect.Left, ImageRect.Top, ImageIndex, 
      itImage, 
      Enabled);
    finally
      RestoreDC(Canvas.Handle, SaveState);
    end;
  end;
  if Caption <> '' then
  begin
//    InflateRect(TextRect, -2, -2);
    SetBkMode(Canvas.Handle, TRANSPARENT);
    DrawText(Canvas, Caption, Length(Caption), TextRect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
  end;
  if Active and ShowFocus then
  begin
    TextRect := Rect;
    InflateRect(TextRect, -3, -3);
    Canvas.DrawFocusRect(TextRect);
  end;
end;

procedure TJvTabDefaultPainter.SetActiveColorFrom(const Value: TColor);
begin
  if FActiveColorFrom <> Value then
  begin
    FActiveColorFrom := Value;
    Change;
  end;
end;

procedure TJvTabDefaultPainter.SetActiveFont(const Value: TFont);
begin
  FActiveFont.Assign(Value);
end;

procedure TJvTabDefaultPainter.SetActiveColorTo(const Value: TColor);
begin
  if FActiveColorTo <> Value then
  begin
    FActiveColorTo := Value;
    Change;
  end;
end;

procedure TJvTabDefaultPainter.SetActiveGradientDirection(
  const Value: TFillDirection);
begin
  if FActiveGradientDirection <> Value then
  begin
    FActiveGradientDirection := Value;
    Change;
  end;
end;

procedure TJvTabDefaultPainter.SetDisabledColorFrom(const Value: TColor);
begin
  if FDisabledColorFrom <> Value then
  begin
    FDisabledColorFrom := Value;
    Change;
  end;
end;

procedure TJvTabDefaultPainter.SetDisabledColorTo(const Value: TColor);
begin
  if FDisabledColorTo <> Value then
  begin
    FDisabledColorTo := Value;
    Change;
  end;
end;

procedure TJvTabDefaultPainter.SetDisabledFont(const Value: TFont);
begin
  FDisabledFont.Assign(Value);
end;

procedure TJvTabDefaultPainter.SetDisabledGradientDirection(
  const Value: TFillDirection);
begin
  if FDisabledGradientDirection <> Value then
  begin
    FDisabledGradientDirection := Value;
    Change;
  end;
end;

procedure TJvTabDefaultPainter.SetInactiveColorFrom(const Value: TColor);
begin
  if FInactiveColorFrom <> Value then
  begin
    FInactiveColorFrom := Value;
    Change;
  end;
end;

procedure TJvTabDefaultPainter.SetInactiveColorTo(const Value: TColor);
begin
  if FInactiveColorTo <> Value then
  begin
    FInactiveColorTo := Value;
    Change;
  end;
end;

procedure TJvTabDefaultPainter.SetInactiveFont(const Value: TFont);
begin
  FInactiveFont.Assign(Value);
end;

procedure TJvTabDefaultPainter.SetInactiveGradientDirection(const Value: TFillDirection);
begin
  if FInactiveGradientDirection <> Value then
  begin
    FInactiveGradientDirection := Value;
    Change;
  end;
end;

function TJvTabDefaultPainter.IsActiveFontStored: Boolean;
begin
  Result := True;
end;

function TJvTabDefaultPainter.IsDisabledFontStored: Boolean;
begin
  Result := True;
end;

function TJvTabDefaultPainter.IsInactiveFontStored: Boolean;
begin
  Result := True;
end;

//=== { TJvTabControl } ======================================================

constructor TJvTabControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner); 
  InputKeys := [ikTabs]; 
end;





procedure TJvTabControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_TAB) and (Shift * KeyboardShiftStates >= [ssCtrl]) then
  begin
    if (Shift * KeyboardShiftStates >= [ssShift]) then
    begin
      if TabIndex = 0 then
        TabIndex := Tabs.Count - 1
      else
        TabIndex := TabIndex - 1;
    end
    else
      TabIndex := (TabIndex + 1) mod Tabs.Count;
    Key := 0;
  end
  else
    inherited KeyDown(Key, Shift);
end;

procedure TJvTabControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
  R: TRect;
  P: TPoint;
begin
  if RightClickSelect and (Button = mbRight) then
  begin
    P := Point(X,Y);
    for I := 0 to Tabs.Count -1 do
    begin
      R := TabRect(I);
      if PtInRect(R, P) then
      begin
        if (TabIndex <> I) and CanChange then
        begin
          TabIndex := I;
          Change;
        end;
        Break;
      end;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

function TJvTabControl.DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean): Boolean;
begin
  Result := True;
  if Assigned(TabPainter) then
    TabPainter.DrawTab(Self, Canvas, Images, TabIndex, Tabs[TabIndex].Caption, Rect, TabIndex = Self.TabIndex, Enabled)
  else
    Result := inherited DrawTab(TabIndex, Rect, Active);
end;





procedure TJvTabControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = TabPainter) then
    TabPainter := nil;

end;

procedure TJvTabControl.SetTabPainter(const Value: TJvTabControlPainter);
begin
  if FTabPainter <> Value then
  begin
    if FTabPainter <> nil then
      FTabPainter.UnRegisterChange(Self);
    FTabPainter := Value;
    if FTabPainter <> nil then
    begin
      FTabPainter.FreeNotification(Self);
      FTabPainter.RegisterChange(Self);
    end;
    Invalidate;
  end;
end;

//=== { TJvPageControl } =====================================================

constructor TJvPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClientBorderWidth := JvDefPageControlBorder;
  FHintSource := hsDefault;
end;

function TJvPageControl.FormKeyPreview: Boolean;
var
  F: TCustomForm;
begin
  F := GetParentForm(Self);
  if F <> nil then
    Result := F.KeyPreview
  else
    Result := False;
end;

function TJvPageControl.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
var
  ThisTab, Tab: TTabSheet;
  Forwrd: Boolean;
begin
  Result := False;
  if HandleGlobalTab and not FormKeyPreview and
    (Key = VK_TAB) and (Shift * KeyboardShiftStates >= [ssCtrl]) then
  begin
    ThisTab := ActivePage;
    Forwrd := (Shift * KeyboardShiftStates >= [ssShift]);
    Tab := ThisTab;
    repeat
      Tab := FindNextPage(Tab, Forwrd, True);
    until (Tab = nil) or Tab.Enabled or (Tab = ThisTab);
    if Tab <> ThisTab then
    begin
      if CanChange then
      begin
        ActivePage := Tab;
        Result := True;
        Change;
      end;
      Exit;
    end;
  end;
  Result := inherited WantKey(Key, Shift, KeyText);
end;



function TJvPageControl.DrawTab(TabIndex: Integer; const Rect: TRect;
  Active: Boolean): Boolean;

var
  I, RealIndex: Integer;
begin 
  Result := False; 
  if TabPainter <> nil then
  begin
    RealIndex := 0;
    I := 0;
    while I <= TabIndex + RealIndex do
    begin
      if not Pages[I].TabVisible then Inc(RealIndex);
      Inc(I);
    end;
    RealIndex := RealIndex + TabIndex;
    if RealIndex < PageCount then
      TabPainter.DrawTab(Self, Canvas, Images, Pages[RealIndex].ImageIndex, Pages[RealIndex].Caption, Rect, Active, Pages[RealIndex].Enabled);
  end
  else  Result :=  inherited DrawTab(TabIndex, Rect, Active);
end;

procedure TJvPageControl.Loaded;
begin
  inherited Loaded;
  HideAllTabs := FHideAllTabs;
end;

procedure TJvPageControl.SetClientBorderWidth(const Value: TBorderWidth);
begin
  if FClientBorderWidth <> Value then
  begin
    FClientBorderWidth := Value;
    RecreateWnd;
  end;
end;

procedure TJvPageControl.SetHideAllTabs(const Value: Boolean);
var
  I: Integer;
  SaveActivePage: TTabSheet;
begin
  FHideAllTabs := Value;
  if (csDesigning in ComponentState) then
    Exit;
  if HandleAllocated then
  begin
    SaveActivePage := ActivePage;
    for I := 0 to PageCount - 1 do
      Pages[I].TabVisible := Pages[I].TabVisible and not FHideAllTabs;
    ActivePage := SaveActivePage;
    if FHideAllTabs then
      TabStop := False;
  end;
end;



procedure TJvPageControl.UpdateTabImages;
begin
  inherited UpdateTabImages;
end;



function TJvPageControl.HintShow(var HintInfo: THintInfo): Boolean;
var
  TabNo: Integer;
  Tab: TTabSheet;
begin
  Result := inherited HintShow(HintInfo);

  if FHintSource = hsDefault then
    Exit;

  if Result then
    Exit;
  (*
      hsDefault,    // use default hint behaviour (i.e as regular control)
      hsForceMain,  // use the main controls hint even if subitems have hints
      hsForceChildren, // always use subitems hints even if empty and main control has hint
      hsPreferMain, // use main control hint unless empty then use subitems hints
      hsPreferChildren // use subitems hints unless empty then use main control hint
      );
  *)

  if Result or (Self <> HintInfo.HintControl) then
    Exit; // strange, hint requested by other component. Why should we deal with it?
  with HintInfo.CursorPos do
    TabNo := IndexOfTabAt(X, Y); // X&Y are expected in Client coordinates

  if (TabNo >= 0) and (TabNo < PageCount) then
    Tab := Pages[TabNo]
  else
    Tab := nil;
  if (FHintSource = hsForceMain) or ((FHintSource = hsPreferMain) and (GetShortHint(Hint) <> '')) then
    HintInfo.HintStr := GetShortHint(Hint)
  else
  if (Tab <> nil) and ((FHintSource = hsForceChildren) or ((FHintSource = hsPreferChildren) and
    (GetShortHint(Tab.Hint) <> ''))) then
    HintInfo.HintStr := GetShortHint(Tab.Hint)
end;

type
  TTabSheetAccessProtected = class(TTabSheet);

function TJvPageControl.CanChange: Boolean;
begin
  Result := inherited CanChange;
  if Result and (ActivePage <> nil) and ReduceMemoryUse then
    TTabSheetAccessProtected(ActivePage).DestroyHandle;
end;

procedure TJvPageControl.SetReduceMemoryUse(const Value: Boolean);
begin
  FReduceMemoryUse := Value;
end;

procedure TJvPageControl.SetTabPainter(const Value: TJvTabControlPainter);
begin
  if FTabPainter <> Value then
  begin
    if FTabPainter <> nil then
      FTabPainter.UnRegisterChange(Self);
    FTabPainter := Value;
    if FTabPainter <> nil then
    begin
      FTabPainter.FreeNotification(Self);
      FTabPainter.RegisterChange(Self);
    end;
    Invalidate;
  end;
end;

procedure TJvPageControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = TabPainter) then
    TabPainter := nil;
end;


procedure TJvPageControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  R: TRect;
  P: TPoint;
begin
  if RightClickSelect and (Button = mbRight) then
  begin
    P := Point(X,Y);
    for I := 0 to PageCount -1 do
    begin
      R := TabRect(I);
      if PtInRect(R, P) then
      begin
        if (ActivePageIndex <> I) and CanChange then
        begin
          ActivePageIndex := I;
          Change;
        end;
        Break;
      end;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;


//=== { TJvTrackBar } ========================================================

constructor TJvTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // ControlStyle := ControlStyle + [csAcceptsControls]; 
  FShowRange := True;
end;



procedure TJvTrackBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TJvTrackBar.SetShowRange(const Value: Boolean);
begin
  if FShowRange <> Value then
  begin
    FShowRange := Value;
    RecreateWnd;
  end;
end;



//=== { TJvTreeNode } ========================================================




procedure TJvTabDefaultPainter.SetGlyphLayout(const Value: TButtonLayout);
begin
  if FGlyphLayout <> Value then
  begin
    FGlyphLayout := Value;
    Change;
  end;
end;

procedure TJvTabDefaultPainter.SetDivider(const Value: Boolean);
begin
  if FDivider <> Value then
  begin
    FDivider := Value;
    Change;
  end;
end;

procedure TJvTabDefaultPainter.SetShowFocus(const Value: Boolean);
begin
  if FShowFocus <> Value then
  begin
    FShowFocus := Value;
    Change;
  end;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

