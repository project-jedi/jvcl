{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvXPCore.PAS, released on 2004-01-01.

The Initial Developer of the Original Code is Marc Hoffman.
Portions created by Marc Hoffman are Copyright (C) 2002 APRIORI business solutions AG.
Portions created by APRIORI business solutions AG are Copyright (C) 2002 APRIORI business solutions AG
All Rights Reserved.

Contributor(s):

Last Modified: 2004-01-01

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvXPCore;

interface

uses
  Windows, Messages, Classes, Controls, Graphics, Forms;

const
  { color constants.

    these constants are used as default colors for descendant controls
    and may be replaced with other (common) values.

    syntax: JvXPColor_[Control]_[Enabled: Enb, Dis]_[Type]_[Theme: WXP, OXP]     }

  { button colors - WindowsXP }
  dxColor_Btn_Enb_Border_WXP   = $00733800; // border line
  dxColor_Btn_Dis_Border_WXP   = $00BDC7CE; // border line (disabled)
  dxColor_Btn_Enb_Edges_WXP    = $00AD9E7B; // border edges
  dxColor_Btn_Dis_Edges_WXP    = $00BDC7CE; // border edges (disabled)
  dxColor_Btn_Enb_BgFrom_WXP   = $00FFFFFF; // background from
  dxColor_Btn_Enb_BgTo_WXP     = $00E7EBEF; // background to
  dxColor_Btn_Enb_CkFrom_WXP   = $00C6CFD6; // clicked from
  dxColor_Btn_Enb_CkTo_WXP     = $00EBF3F7; // clicked to
  dxColor_Btn_Enb_FcFrom_WXP   = $00FFE7CE; // focused from
  dxColor_Btn_Enb_FcTo_WXP     = $00EF846D; // focused to
  dxColor_Btn_Enb_HlFrom_WXP   = $00CEF3FF; // highlight from
  dxColor_Btn_Enb_HlTo_WXP     = $000096E7; // highlight to

  { checkbox colors - WindowsXP }
  dxColor_Chk_Enb_Border_WXP   = $00845118; // border line
  dxColor_Chk_Enb_NmSymb_WXP   = $0021A621; // symbol normal

  { misc colors - WindowsXP }
  dxColor_Msc_Dis_Caption_WXP  = $0094A6A5; // caption color (disabled)

  dxColor_DotNetFrame          = $00F7FBFF; // $00E7EBEF;
  dxColor_BorderLineOXP        = $00663300;
  dxColor_BgOXP                = $00D6BEB5;
  dxColor_BgCkOXP              = $00CC9999;

type
  TJvXPCustomStyleControl = class;

  TJvXPBoundLines = set of
   (
    blLeft,                             // left line
    blTop,                              // top line
    blRight,                            // right line
    blBottom                            // bottom line
   );

  TJvXPControlStyle = set of
   (
    csRedrawCaptionChanged,             // (default)
    csRedrawBorderChanged,              //
    csRedrawEnabledChanged,             // (default)
    csRedrawFocusedChanged,             // (default)
    csRedrawMouseDown,                  // (default)
    csRedrawMouseEnter,                 // (default)
    csRedrawMouseLeave,                 // (default)
    csRedrawMouseMove,                  //
    csRedrawMouseUp,                    // (default)
    csRedrawParentColorChanged,         // (default)
    csRedrawParentFontChanged,          //
    csRedrawPosChanged,                 //
    csRedrawResized                     //
   );

  TJvXPDrawState = set of
   (
    dsDefault,                          // default
    dsHighlight,                        // highlighted
    dsClicked,                          // clicked
    dsFocused                           // focused
   );

  TJvXPGlyphLayout =
   (
    glBottom,                           // bottom glyph
    glCenter,                           // centered glyph
    glTop                               // top glyph
   );

  TJvXPTheme =
   (
    WindowsXP,                          // WindowsXP theme
    OfficeXP                            // OfficeXP theme
   );

  { baseclass for non-focusable component descendants. }
  {$IFDEF USEJVCL}
  TJvXPCustomComponent = class(TComponent)
  public
    constructor Create(AOwner: TComponent); override;
  end;
  {$ELSE}
  TJvXPCustomComponent = class(TComponent)
  private
    FVersion: string;
    procedure SetVersion(Value: string);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Version: string read FVersion write SetVersion stored False;
  end;
  {$ENDIF USEJVCL}


  TJvXPWinControl = class(TWinControl)
  published
    property Color;
  end;

  { baseclass for focusable control descendants. }

  TJvXPCustomControl = class(TCustomControl)
  private
    FClicking: Boolean;
    FDrawState: TJvXPDrawState;
    FIsLocked: Boolean;
    FIsSibling: Boolean;
    FModalResult: TModalResult;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    {$IFNDEF USEJVCL}
    FVersion: string;
    procedure SetVersion(Value: string);
    {$ENDIF USEJVCL}
    procedure CMDialogChar(var Msg: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMBorderChanged(var Msg: TMessage); message CM_BORDERCHANGED;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFocusChanged(var Msg: TMessage); message CM_FOCUSCHANGED;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CMParentFontChanged(var Msg: TMessage); message CM_PARENTFONTCHANGED;
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    procedure WMMouseMove(var Msg: TWMMouse); message WM_MOUSEMOVE;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMWindowPosChanged(var Msg: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
  protected
    ExControlStyle: TJvXPControlStyle;
    procedure InternalRedraw; dynamic;
    procedure HookBorderChanged; dynamic;
    procedure HookEnabledChanged; dynamic;
    procedure HookFocusedChanged; dynamic;
    procedure HookMouseDown; dynamic;
    procedure HookMouseEnter; dynamic;
    procedure HookMouseLeave; dynamic;
    procedure HookMouseMove(X: Integer = 0; Y: Integer = 0); dynamic;
    procedure HookMouseUp; dynamic;
    procedure HookParentColorChanged; dynamic;
    procedure HookParentFontChanged; dynamic;
    procedure HookPosChanged; dynamic;
    procedure HookResized; dynamic;
    procedure HookTextChanged; dynamic;
    procedure MouseDown(Button:TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button:TMouseButton; Shift:TShiftState; X, Y: Integer); override;
    property ModalResult: TModalResult read FModalResult write FModalResult default 0;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    procedure BeginUpdate; dynamic;
    procedure EndUpdate; dynamic;
    property Canvas;
    property DrawState: TJvXPDrawState read FDrawState write FDrawState;
    property IsLocked: Boolean read FIsLocked write FIsLocked;
    property IsSibling: Boolean read FIsSibling write FIsSibling;
  published
    //property BevelInner;
    //property BevelOuter;
    //property BevelWidth;
    //property BiDiMode;
    //property Ctl3D;
    //property DockSite;
    //property ParentBiDiMode;
    //property ParentCtl3D;
    //property TabOrder;
    //property TabStop;
    //property UseDockManager default True;
    property Align;
    property Anchors;
    //property AutoSize;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    //property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    {$IFNDEF USEJVCL}
    property Version: string read FVersion write SetVersion stored False;
    {$ENDIF USEJVCL}
    property Visible;
    //property OnDockDrop;
    //property OnDockOver;
    //property OnEndDock;
    //property OnGetSiteInfo;
    //property OnStartDock;
    //property OnUnDock;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    {$IFDEF COMPILER6_UP}
    property OnContextPopup;
    {$ENDIF COMPILER6_UP}
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

  TJvXPUnlimitedControl = class(TJvXPCustomControl);

  TJvXPStyle = class(TPersistent)
  private
    FTheme: TJvXPTheme;
    FUseStyleManager: Boolean;
  protected
    Parent: TJvXPCustomStyleControl;
    procedure SetTheme(Value: TJvXPTheme); virtual;
    procedure SetUseStyleManager(Value: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent);
    function GetTheme: TJvXPTheme;
  published
    property Theme: TJvXPTheme read FTheme write SetTheme default WindowsXP;
    property UseStyleManager: Boolean read FUseStyleManager write SetUseStyleManager default True;
  end;

  TJvXPStyleManager = class(TJvXPCustomComponent)
  private
    FControls: TList;
    FTheme: TJvXPTheme;
    FOnThemeChanged: TNotifyEvent;
    procedure InvalidateControls;
  protected
    procedure SetTheme(Value: TJvXPTheme); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RegisterControls(const AControls: array of TJvXPCustomControl);
    procedure UnregisterControls(const AControls: array of TJvXPCustomControl);
  published
    property Theme: TJvXPTheme read FTheme write SetTheme default WindowsXP;
    property OnThemeChanged: TNotifyEvent read FOnThemeChanged write FOnThemeChanged;
  end;

  TJvXPCustomStyleControl = class(TJvXPCustomControl)
  private
    FStyle: TJvXPStyle;
    FStyleManager: TJvXPStyleManager;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure SetStyleManager(Value: TJvXPStyleManager); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property Style: TJvXPStyle read FStyle write FStyle;
    property StyleManager: TJvXPStyleManager read FStyleManager write SetStyleManager;
  end;

  TJvXPGradientColors = 2..255;

  TJvXPGradientStyle = (gsLeft, gsTop, gsRight, gsBottom);

  TJvXPGradient = class(TPersistent)
  private
    FColors: TJvXPGradientColors;
    FDithered: Boolean;
    FEnabled: Boolean;
    FEndColor: TColor;
    FStartColor: TColor;
    FGradientStyle: TJvXPGradientStyle;
  protected
    Parent: TJvXPCustomControl;
    procedure SetDithered(Value: Boolean); virtual;
    procedure SetColors(Value: TJvXPGradientColors); virtual;
    procedure SetEnabled(Value: Boolean); virtual;
    procedure SetEndColor(Value: TColor); virtual;
    procedure SetGradientStyle(Value: TJvXPGradientStyle); virtual;
    procedure SetStartColor(Value: TColor); virtual;
  public
    Bitmap: TBitmap;
    constructor Create(AOwner: TControl);
    destructor Destroy; override;
    procedure RecreateBands; virtual;
  published
    property Dithered: Boolean read FDithered write SetDithered default True;
    property Colors: TJvXPGradientColors read FColors write SetColors default 16;
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property EndColor: TColor read FEndColor write SetEndColor default clSilver;
    property StartColor: TColor read FStartColor write SetStartColor default clGray;
    property Style: TJvXPGradientStyle read FGradientStyle write SetGradientStyle default gsLeft;
  end;

implementation

uses
  JvXPCoreUtils;

{$R ..\Resources\JvXPCore.res}

{$IFNDEF USEJVCL}
resourcestring
  SCopyright = 'Design eXperience. © 2002 M. Hoffmann Version ';
  SCopyright2 = 'Design eXperience II - © 2002 M. Hoffmann Version ';
  SVersion = '2.0.1'; // always increase version number on new releases!
{$ENDIF USEJVCL}

//=== TJvXPCustomComponent ===================================================

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomComponent.Create
  Author:    mh
  Date:      24-Jun-2002
  Arguments: AOwner: TComponent
  Result:    None
-----------------------------------------------------------------------------}

constructor TJvXPCustomComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFNDEF USEJVCL}
  FVersion := SCopyright + SVersion;
  {$ENDIF USEJVCL}
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomComponent.SetVersion
  Author:    mh
  Date:      24-Jun-2002
  Arguments: Value: string
  Result:    None
-----------------------------------------------------------------------------}

{$IFNDEF USEJVCL}
procedure TJvXPCustomComponent.SetVersion(Value: string);
begin
  // do not enable overwriting this constant.
end;
{$ENDIF USEJVCL}

//=== TJvXPCustomControl =====================================================

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.Create
  Author:    mh
  Date:      22-Feb-2002
  Arguments: AOwner: TComponent
  Result:    None
-----------------------------------------------------------------------------}

constructor TJvXPCustomControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csReplicatable];
  DoubleBuffered := True;
  ExControlStyle := [csRedrawEnabledChanged, csRedrawFocusedChanged,
    csRedrawMouseDown, csRedrawMouseEnter, csRedrawMouseLeave, csRedrawMouseUp,
    csRedrawParentColorChanged, csRedrawCaptionChanged];
  FClicking := False;
  FDrawState := [dsDefault];
  FIsLocked := False;
  FIsSibling := False;
  FModalResult := 0;
  {$IFNDEF USEJVCL}
  FVersion := SCopyright2 + SVersion;
  {$ENDIF USEJVCL}
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.SetVersion
  Author:    mh
  Date:      07-Mrz-2002
  Arguments: Value: string
  Result:    None
-----------------------------------------------------------------------------}

{$IFNDEF USEJVCL}
procedure TJvXPCustomControl.SetVersion(Value: string);
begin
  // disallow changing this property.
end;
{$ENDIF USEJVCL}

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.BeginUpdate
  Author:    mh
  Date:      22-Feb-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.BeginUpdate;
begin
  FIsLocked := True;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.EndUpdate
  Author:    mh
  Date:      22-Feb-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.EndUpdate;
begin
  FIsLocked := False;
  InternalRedraw;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.InternalRedraw
  Author:    mh
  Date:      30-Okt-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.InternalRedraw;
begin
  if not FIsLocked then
    Invalidate;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.CMDialogChar
  Author:    mh
  Date:      22-Feb-2002
  Arguments: var Msg: TCMDialogChar
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.CMDialogChar(var Msg: TCMDialogChar);
begin
  with Msg do
  if IsAccel(CharCode, Caption) and CanFocus and
    (Focused or ((GetKeyState(VK_MENU) and $8000) <> 0)) then
  begin
    Click;
    Result := 1;
  end
  else
    inherited;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.CMBorderChanged
  Author:    mh
  Date:      13-Aug-2002
  Arguments: var Msg: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.CMBorderChanged(var Msg: TMessage);
begin
  // delegate message "BorderChanged" to hook.
  inherited;
  HookBorderChanged;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.CMEnabledChanged
  Author:    mh
  Date:      21-Feb-2002
  Arguments: var Msg: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.CMEnabledChanged(var Msg: TMessage);
begin
  // delegate message "EnabledChanged" to hook.
  inherited;
  HookEnabledChanged;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.CMFocusChanged
  Author:    mh
  Date:      22-Feb-2002
  Arguments: var Msg: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.CMFocusChanged(var Msg: TMessage);
begin
  // delegate message "FocusChanged" to hook.
  inherited;
  HookFocusedChanged;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.CMMouseEnter
  Author:    mh
  Date:      21-Feb-2002
  Arguments: var Msg: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.CMMouseEnter(var Msg: TMessage);
begin
  // delegate message "MouseEnter" to hook.
  inherited;
  HookMouseEnter;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.CMMouseLeave
  Author:    mh
  Date:      21-Feb-2002
  Arguments: var Msg: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.CMMouseLeave(var Msg: TMessage);
begin
  // delegate message "MouseLeave" to hook.
  inherited;
  HookMouseLeave;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.CMParentColorChanged
  Author:    mh
  Date:      22-Feb-2002
  Arguments: var Msg: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.CMParentColorChanged(var Msg: TMessage);
begin
  // delegate message "ParentColorChanged" to hook.
  inherited;
  HookParentColorChanged;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.CMParentFontChanged
  Author:    mh
  Date:      30-Okt-2002
  Arguments: var Msg: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.CMParentFontChanged(var Msg: TMessage);
begin
  // delegate message "ParentFontChanged" to hook.
  inherited;
  HookParentFontChanged;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.CMTextChanged
  Author:    mh
  Date:      22-Feb-2002
  Arguments: var Msg: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.CMTextChanged(var Msg: TMessage);
begin
  // delegate message "TextChanged" to hook.
  inherited;
  HookTextChanged;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.WMMouseMove
  Author:    mh
  Date:      10-May-2002
  Arguments: var Msg: TWMMouse
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.WMMouseMove(var Msg: TWMMouse);
begin
  // delegate message "MouseMove" to hook.
  inherited;
  HookMouseMove(Msg.XPos, Msg.YPos);
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.WMSize
  Author:    mh
  Date:      22-Feb-2002
  Arguments: var Msg: TWMSize
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.WMSize(var Msg: TWMSize);
begin
  // delegate message "Size" to hook.
  inherited;
  HookResized;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.WMWindowPosChanged
  Author:    mh
  Date:      16-Aug-2002
  Arguments: var Msg: TWMWindowPosChanged
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.WMWindowPosChanged(var Msg: TWMWindowPosChanged);
begin
  // delegate message "WindowPosChanged" to hook.
  inherited;
  HookPosChanged;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.MouseDown
  Author:    mh
  Date:      10-May-2002
  Arguments: Button: TMouseButton; Shift: TShiftState; X, Y: Integer
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // delegate message "MouseDown" to hook.
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FClicking := True;
    HookMouseDown;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.MouseUp
  Author:    mh
  Date:      10-Mai-2002
  Arguments: Button: TMouseButton; Shift: TShiftState; X, Y: Integer
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // delegate message "MouseUp" to hook.
  inherited  MouseUp(Button, Shift, X, Y);
  if FClicking then
  begin
    FClicking := False;
    HookMouseUp;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.Click
  Author:    mh
  Date:      22-Feb-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.Click;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if Form <> nil then
    Form.ModalResult := ModalResult;
  inherited Click;
end;

//
// hooks are used to interrupt default windows messages in an easier
// way - it's possible to override them in descendant classes.
// Beware of multiple redraw calls - if you know that the calling
// hooks always redraws the component, use the lock i.e. unlock methods.
//

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.HookBorderChanged
  Author:    mh
  Date:      13-Aug-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.HookBorderChanged;
begin
  // this hook is called, if the border property was changed.
  // in that case we normaly have to redraw the control.
  if csRedrawBorderChanged in ExControlStyle then
    InternalRedraw;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.HookEnabledChanged
  Author:    mh
  Date:      22-Feb-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.HookEnabledChanged;
begin
  // this hook is called, if the enabled property was switched.
  // in that case we normaly have to redraw the control.
  if csRedrawEnabledChanged in ExControlStyle then
    InternalRedraw;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.HookFocusedChanged
  Author:    mh
  Date:      22-Feb-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.HookFocusedChanged;
begin
  // this hook is called, if the currently focused control was changed.
  if Focused then
    Include(FDrawState, dsFocused)
  else
  begin
    Exclude(FDrawState, dsFocused);
    Exclude(FDrawState, dsClicked);
  end;
  FIsSibling := GetParentForm(Self).ActiveControl is TJvXPCustomControl;
  if csRedrawFocusedChanged in ExControlStyle then
    InternalRedraw;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.HookMouseEnter
  Author:    mh
  Date:      22-Feb-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.HookMouseEnter;
begin
  // this hook is called, if the user moves (hover) the mouse over the control.
  Include(FDrawState, dsHighlight);
  if csRedrawMouseEnter in ExControlStyle then
    InternalRedraw;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.HookMouseLeave
  Author:    mh
  Date:      22-Feb-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.HookMouseLeave;
begin
  // this hook is called, if the user moves the mouse away (unhover) from
  // the control.
  Exclude(FDrawState, dsHighlight);
  if csRedrawMouseLeave in ExControlStyle then
    InternalRedraw;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.HookMouseMove
  Author:    mh
  Date:      22-Feb-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.HookMouseMove(X: Integer = 0; Y: Integer = 0);
begin
  // this hook is called if the user moves the mouse inside the control.
  if csRedrawMouseMove in ExControlStyle then
    InternalRedraw;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.HookMouseDown
  Author:    mh
  Date:      22-Feb-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.HookMouseDown;
begin
  // this hook is called, if the user presses the left mouse button over the
  // controls.
  if not Focused and CanFocus then
    SetFocus;
  Include(FDrawState, dsClicked);
  if csRedrawMouseDown in ExControlStyle then
    InternalRedraw;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.HookMouseUp
  Author:    mh
  Date:      22-Feb-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.HookMouseUp;
var
  CurrentPos: TPoint;
  NewControl: TWinControl;
begin
  // this hook is called, if the user releases the left mouse button.
  begin
    Exclude(FDrawState, dsClicked);
    if csRedrawMouseUp in ExControlStyle then
      InternalRedraw;

    // does the cursor is over another supported control?
    GetCursorPos(CurrentPos);
    NewControl := FindVCLWindow(CurrentPos);
    if (NewControl <> nil) and (NewControl <> Self) and
      (NewControl.InheritsFrom(TJvXPCustomControl)) then
      TJvXPCustomControl(NewControl).HookMouseEnter;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.HookParentColorChanged
  Author:    mh
  Date:      22-Feb-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.HookParentColorChanged;
begin
  // this hook is called if, the parent color was changed.
  if csRedrawParentColorChanged in ExControlStyle then
    InternalRedraw;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.HookParentFontChanged
  Author:    mh
  Date:      30-Okt-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.HookParentFontChanged;
begin
  // this hook is called if, the parent font was changed.
  if csRedrawParentFontChanged in ExControlStyle then
    InternalRedraw;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.HookPosChanged
  Author:    mh
  Date:      16-Aug-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.HookPosChanged;
begin
  // this hook is called, if the window position was changed.
  if csRedrawPosChanged in ExControlStyle then
    InternalRedraw;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.HookResized
  Author:    mh
  Date:      22-Feb-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.HookResized;
begin
  // this hook is called, if the control was resized.
  if csRedrawResized in ExControlStyle then
    InternalRedraw;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomControl.HookTextChanged
  Author:    mh
  Date:      22-Feb-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomControl.HookTextChanged;
begin
  // this hook is called, if the caption was changed.
  if csRedrawCaptionChanged in ExControlStyle then
    InternalRedraw;
end;

//=== TJvXPStyle =============================================================

{-----------------------------------------------------------------------------
  Procedure: TJvXPStyle.Create
  Author:    mh
  Date:      04-Jul-2002
  Arguments: AOwner: TComponent
  Result:    None
-----------------------------------------------------------------------------}

constructor TJvXPStyle.Create(AOwner: TComponent);
begin
  inherited Create;
  Parent := TJvXPCustomStyleControl(AOwner);
  FTheme := WindowsXP;
  FUseStyleManager := True;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPStyle.SetTheme
  Author:    mh
  Date:      04-Jul-2002
  Arguments: Value: TJvXPTheme
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPStyle.SetTheme(Value: TJvXPTheme);
begin
  if Value <> FTheme then
  begin
    FTheme := Value;
    Parent.InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPStyle.GetTheme
  Author:    mh
  Date:      05-Jul-2002
  Arguments: None
  Result:    TJvXPTheme
-----------------------------------------------------------------------------}

function TJvXPStyle.GetTheme: TJvXPTheme;
begin
  Result := FTheme;
  if FUseStyleManager and Assigned(Parent.StyleManager) then
    Result := Parent.StyleManager.Theme;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPStyle.SetUseStyleManager
  Author:    mh
  Date:      05-Jul-2002
  Arguments: Value: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPStyle.SetUseStyleManager(Value: Boolean);
begin
  if Value <> FUseStyleManager then
  begin
    FUseStyleManager := Value;
    Parent.InternalRedraw;
  end;
end;

//=== TJvXPStyleManager ======================================================

{-----------------------------------------------------------------------------
  Procedure: TJvXPStyleManager.Create
  Author:    mh
  Date:      25-Jun-2002
  Arguments: AOwner: TComponent
  Result:    None
-----------------------------------------------------------------------------}

constructor TJvXPStyleManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FControls := TList.Create;
  FTheme := WindowsXP;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPStyleManager.Destroy
  Author:    mh
  Date:      25-Jun-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

destructor TJvXPStyleManager.Destroy;
begin
  InvalidateControls;
  FControls.Free;
  inherited Destroy;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPStyleManager.InvalidateControls
  Author:    mh
  Date:      25-Jun-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPStyleManager.InvalidateControls;
var
  I: Integer;
begin
  for I := 0 to FControls.Count - 1 do
  with TJvXPCustomControl(FControls[I]) do
    InternalRedraw;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPStyleManager.SetTheme
  Author:    mh
  Date:      25-Jun-2002
  Arguments: Value: TJvXPTheme
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPStyleManager.SetTheme(Value: TJvXPTheme);
begin
  if Value <> FTheme then
  begin
    FTheme := Value;
    if Assigned(FOnThemeChanged) then
      FOnThemeChanged(Self);
    InvalidateControls;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPStyleManager.RegisterControls
  Author:    mh
  Date:      05-Jul-2002
  Arguments: const AControls: array of TJvXPCustomControl
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPStyleManager.RegisterControls(const AControls: array of TJvXPCustomControl);
var
  I: Integer;
begin
  for I := Low(AControls) to High(AControls) do
  if FControls.IndexOf(AControls[I]) = -1 then
    FControls.Add(AControls[I]);
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPStyleManager.UnregisterControls
  Author:    mh
  Date:      05-Jul-2002
  Arguments: const AControls: array of TJvXPCustomControl
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPStyleManager.UnregisterControls(const AControls: array of TJvXPCustomControl);
var
  I: Integer;
begin
  for I := Low(AControls) to High(AControls) do
  if FControls.IndexOf(AControls[I]) <> -1 then
    FControls.Delete(FControls.IndexOf(AControls[I]));
end;

//=== TJvXPCustomStyleControl ================================================

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomStyleControl.Create
  Author:    mh
  Date:      04-Jul-2002
  Arguments: AOwner: TComponent
  Result:    None
-----------------------------------------------------------------------------}

constructor TJvXPCustomStyleControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStyle := TJvXPStyle.Create(Self);
  FStyleManager := nil;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomStyleControl.Destroy
  Author:    mh
  Date:      04-Jul-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

destructor TJvXPCustomStyleControl.Destroy;
begin
  if FStyleManager <> nil then
    FStyleManager.UnregisterControls([Self]);
  FStyle.Free;
  inherited Destroy;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomStyleControl.Notification
  Author:    mh
  Date:      04-Jul-2002
  Arguments: AComponent: TComponent; Operation: TOperation
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomStyleControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent is TJvXPStyleManager) and (Operation = opRemove) then
    FStyleManager := nil;
  inherited Notification(AComponent, Operation);
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomStyleControl.SetStyleManager
  Author:    mh
  Date:      04-Jul-2002
  Arguments: Value: TJvXPStyleManager
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomStyleControl.SetStyleManager(Value: TJvXPStyleManager);
begin
  if Value <> FStyleManager then
  begin
    if Value <> nil then
      Value.RegisterControls([Self])
    else
      FStyleManager.UnregisterControls([Self]);
    FStyleManager := Value;
    InternalRedraw;
  end;
end;

//=== TJvXPGradient ==========================================================

{-----------------------------------------------------------------------------
  Procedure: TJvXPGradient.Create
  Author:    M. Hoffmann
  Date:      07-Nov-2001
  Arguments: AOwner: TControl
  Result:    None
-----------------------------------------------------------------------------}

constructor TJvXPGradient.Create(AOwner: TControl);
begin
  inherited Create;
  Parent := TJvXPCustomControl(AOwner);
  Bitmap := TBitmap.Create;
  FColors := 16;
  FDithered := True;
  FEnabled := False;
  FEndColor := clSilver;
  FGradientStyle := gsLeft;
  FStartColor := clGray;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPGradient.Destroy
  Author:    M. Hoffmann
  Date:      07-Nov-2001
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

destructor TJvXPGradient.Destroy;
begin
  Bitmap.Free;
  inherited Destroy;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPGradient.RecreateBands
  Author:    M. Hoffmann
  Date:      07-Nov-2001
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPGradient.RecreateBands;
begin
  if Assigned(Bitmap) then
    JvXPCreateGradientRect(Parent.Width, Parent.Height, FStartColor, FEndColor,
      FColors, FGradientStyle, FDithered, Bitmap);
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPGradient.SetDithered
  Author:    M. Hoffmann
  Date:      07-Nov-2001
  Arguments: Value: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPGradient.SetDithered(Value: Boolean);
begin
  if FDithered <> Value then
  begin
    FDithered := Value;
    RecreateBands;
    Parent.InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPGradient.SetColors
  Author:    mh
  Date:      05-Jul-2002
  Arguments: Value: TJvXPGradientColors
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPGradient.SetColors(Value: TJvXPGradientColors);
begin
  if FColors <> Value then
  begin
    FColors := Value;
    RecreateBands;
    Parent.InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPGradient.SetEnabled
  Author:    M. Hoffmann
  Date:      07-Nov-2001
  Arguments: Value: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPGradient.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Parent.InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPGradient.SetEndColor
  Author:    M. Hoffmann
  Date:      07-Nov-2001
  Arguments: Value: TColor
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPGradient.SetEndColor(Value: TColor);
begin
  if FEndColor <> Value then
  begin
    FEndColor := Value;
    RecreateBands;
    Parent.InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPGradient.SetGradientStyle
  Author:    M. Hoffmann
  Date:      07-Nov-2001
  Arguments: Value: TJvXPGradientStyle
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPGradient.SetGradientStyle(Value: TJvXPGradientStyle);
begin
  if FGradientStyle <> Value then
  begin
    FGradientStyle := Value;
    RecreateBands;
    Parent.InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPGradient.SetStartColor
  Author:    M. Hoffmann
  Date:      07-Nov-2001
  Arguments: Value: TColor
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPGradient.SetStartColor(Value: TColor);
begin
  if FStartColor <> Value then
  begin
    FStartColor := Value;
    RecreateBands;
    Parent.InternalRedraw;
  end;
end;

end.

