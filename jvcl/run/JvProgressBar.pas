{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvProgressBar.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com]
                Michiel Koot [makoot att gmx dott net] (inverted property)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvProgressBar;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages,
  CommCtrl,
  {$IFDEF RTL210_UP}
  ShlObj,
  {$ENDIF RTL210_UP}
  SysUtils, Classes, Graphics, Controls, Forms, ComCtrls,
  JvExComCtrls;

{$IFNDEF RTL210_UP}
const
  SID_ITaskbarList                            = '{56FDF342-FD6D-11D0-958A-006097C9A090}';
  SID_ITaskbarList2                           = '{602D4995-B13A-429B-A66E-1935E44F4317}';
  SID_ITaskbarList3                           = '{EA1AFB91-9E28-4B86-90E9-9E9F8A5EEFAF}';
  SID_ITaskbarList4                           = '{C43DC798-95D1-4BEA-9030-BB99E2983A1A}';

  CLSID_TaskbarList: TGUID                            = '{56FDF344-FD6D-11d0-958A-006097C9A090}';

type
  {$IFNDEF RTL185_UP}
  {$IFNDEF RTL150_UP}
  UInt64 = Int64; // kludge, use with caution
  {$ENDIF RTL150_UP}
  DWORDLONG = UInt64;
  {$EXTERNALSYM DWORDLONG}
  ULONGLONG = UInt64;
  {$EXTERNALSYM ULONGLONG}
  PULONGLONG = ^UInt64;
  {$EXTERNALSYM PULONGLONG}
  {$ENDIF RTL185_UP}

  ITaskbarList = interface(IUnknown)
    [SID_ITaskbarList]
    function HrInit: HRESULT; stdcall;
    function AddTab(hwnd: HWND): HRESULT; stdcall;
    function DeleteTab(hwnd: HWND): HRESULT; stdcall;
    function ActivateTab(hwnd: HWND): HRESULT; stdcall;
    function SetActiveAlt(hwnd: HWND): HRESULT; stdcall;
  end;
  
  ITaskbarList2 = interface(ITaskbarList)
    [SID_ITaskbarList2]
    function MarkFullscreenWindow(hwnd: HWND; fFullscreen: BOOL): HRESULT; stdcall;
  end;
  
type
  THUMBBUTTON = record 
    dwMask: DWORD;
    iId: UINT;
    iBitmap: UINT;
    hIcon: HICON;
    szTip: packed array[0..259] of WCHAR;
    dwFlags: DWORD;
  end;
  tagTHUMBBUTTON = THUMBBUTTON;
  TThumbButton = THUMBBUTTON;
  PThumbButton = ^TThumbButton;

// THUMBBUTTON flags
const
  THBF_ENABLED        =  $0000;
  THBF_DISABLED       =  $0001;
  THBF_DISMISSONCLICK =  $0002;
  THBF_NOBACKGROUND   =  $0004;
  THBF_HIDDEN         =  $0008;
  THBF_NONINTERACTIVE = $10; 
// THUMBBUTTON mask
  THB_BITMAP          =  $0001;
  THB_ICON            =  $0002;
  THB_TOOLTIP         =  $0004;
  THB_FLAGS           =  $0008;
  THBN_CLICKED        =  $1800;

const
  TBPF_NOPROGRESS    = 0; 
  TBPF_INDETERMINATE = $1; 
  TBPF_NORMAL        = $2; 
  TBPF_ERROR         = $4; 
  TBPF_PAUSED        = $8; 
  
  TBATF_USEMDITHUMBNAIL   = $1; 
  TBATF_USEMDILIVEPREVIEW = $2; 
  
type
  ITaskbarList3 = interface(ITaskbarList2)
    [SID_ITaskbarList3]
    function SetProgressValue(hwnd: HWND; ullCompleted: ULONGLONG; 
      ullTotal: ULONGLONG): HRESULT; stdcall;
    function SetProgressState(hwnd: HWND; tbpFlags: Integer): HRESULT; stdcall;
    function RegisterTab(hwndTab: HWND; hwndMDI: HWND): HRESULT; stdcall;
    function UnregisterTab(hwndTab: HWND): HRESULT; stdcall;
    function SetTabOrder(hwndTab: HWND; hwndInsertBefore: HWND): HRESULT; stdcall;
    function SetTabActive(hwndTab: HWND; hwndMDI: HWND; 
      tbatFlags: Integer): HRESULT; stdcall;
    function ThumbBarAddButtons(hwnd: HWND; cButtons: UINT;
      pButton: PThumbButton): HRESULT; stdcall;
    function ThumbBarUpdateButtons(hwnd: HWND; cButtons: UINT;
      pButton: PThumbButton): HRESULT; stdcall;
    function ThumbBarSetImageList(hwnd: HWND; himl: HIMAGELIST): HRESULT; stdcall;
    function SetOverlayIcon(hwnd: HWND; hIcon: HICON;
      pszDescription: LPCWSTR): HRESULT; stdcall;
    function SetThumbnailTooltip(hwnd: HWND; pszTip: LPCWSTR): HRESULT; stdcall;
    function SetThumbnailClip(hwnd: HWND; var prcClip: TRect): HRESULT; stdcall;
  end;
  {$ENDIF !RTL210_UP}

type
  TJvTaskBarProgressState = (tpsNormal, tpsNoProgress, tpsIndeterminate, tpsError, tpsPaused);

  TJvBaseProgressBar = class(TGraphicControl)
  private
    FBlockSize: Integer;
    FSmooth: Boolean;
    FPosition: Integer;
    FMin: Integer;
    FMax: Integer;
    FOrientation: TProgressBarOrientation;
    FBarColor: TColor;
    FSteps: Integer;
    FOnChange: TNotifyEvent;
    FTaskBar: ITaskBarList3;
    FTaskBarTested: Boolean;
    FDisplayOnTaskbar: Boolean;
    FTaskbarState: TJvTaskBarProgressState;
    FParentForm: TCustomForm;

    procedure SetMax(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetOrientation(Value: TProgressBarOrientation);
    procedure SetPosition(Value: Integer);
    procedure SetSmooth(const Value: Boolean);
    procedure SetBlockSize(const Value: Integer);
    procedure SetBarColor(const Value: TColor);
    procedure SetSteps(const Value: Integer);
    procedure SetDisplayOnTaskbar(const Value: Boolean);
    procedure SetTaskbarState(const Value: TJvTaskBarProgressState);
  protected
    // BarSize is the upper limit of the area covered by the progress bar
    // Derived classes should override this method to provide their own drawing
    // routine. The base class enmulates the look of the standard TProgressBar
    procedure DrawBar(ACanvas: TCanvas; BarSize: Integer); virtual;
    // GetMaxBarSize returns the maximum size of the bar in pixels.
    // For example, if the control has a 2 pixel border, when at Max,
    // GetMaxBarSize should return Self.Width - 4 when horizontal
    // and Self.Height - 4 when vertical. The default implementation returns
    // Self.Width when horizontal and Self.Height when vertical.
    function GetMaxBarSize: Integer; virtual;
    procedure Paint; override;
    procedure Change; virtual;
    procedure WndProc(var Message: TMessage); override;
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure StepIt; virtual;
    procedure StepBy(Delta: Integer); virtual;
  public
    property Steps: Integer read FSteps write SetSteps default 10;
    property BarColor: TColor read FBarColor write SetBarColor default clHighlight;
    property BlockSize: Integer read FBlockSize write SetBlockSize default 10;
    property Max: Integer read FMax write SetMax default 100;
    property Min: Integer read FMin write SetMin default 0;
    property Orientation: TProgressBarOrientation read FOrientation write SetOrientation default pbHorizontal;
    property Position: Integer read FPosition write SetPosition default 0;
    property Smooth: Boolean read FSmooth write SetSmooth default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    { For Windows >= 7 }
    property DisplayOnTaskbar: Boolean read FDisplayOnTaskbar write SetDisplayOnTaskbar default False;
    property TaskbarState: TJvTaskBarProgressState read FTaskbarState write SetTaskbarState default tpsNormal;
  published
    property Width default 150;
    {$IFDEF HAS_PROPERTY_STYLEELEMENTS}
    property StyleElements;
    {$ENDIF HAS_PROPERTY_STYLEELEMENTS}
  end;

  { For Windows >= Vista }
  TJvProgressBarState = (pbsNormal, pbsError, pbsPaused);


  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvProgressBar = class(TJvExProgressBar)
  private
    FFillColor: TColor;
    FMarquee: Boolean;
    FMarqueePaused: Boolean;
    FMarqueeDelay: Integer;
    FSmoothReverse: Boolean;
    FState: TJvProgressBarState;
    FDisplayOnTaskbar: Boolean;
    FTaskBar: ITaskBarList3;
    FTaskbarState: TJvTaskBarProgressState;
    FParentForm: TCustomForm;

    procedure SetFillColor(const Value: TColor);
    procedure SetMarquee(Value: Boolean);
    procedure SetMarqueePaused(Value: Boolean);
    procedure SetMarqueeDelay(Value: Integer);
    procedure SetSmoothReverse(Value: Boolean);
    procedure SetState(Value: TJvProgressBarState);
    procedure SetDisplayOnTaskbar(Value: Boolean);
    procedure SetTaskbarState(const Value: TJvTaskBarProgressState);
    procedure SetTaskbarPosition(Value: Integer);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure PbmSetPos(var Msg: TMessage); message PBM_SETPOS;
    procedure PbmStepIp(var Msg: TMessage); message PBM_STEPIT;
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property FillColor: TColor read FFillColor write SetFillColor default clHighlight;

    { For Windows >= XP }
    property Marquee: Boolean read FMarquee write SetMarquee default False;
    property MarqueePaused: Boolean read FMarqueePaused write SetMarqueePaused default False;
    property MarqueeDelay: Integer read FMarqueeDelay write SetMarqueeDelay default 25;
    { For Windows >= Vista }
    property SmoothReverse: Boolean read FSmoothReverse write SetSmoothReverse default False;
    property State: TJvProgressBarState read FState write SetState default pbsNormal;
    { For Windows >= 7 }
    property DisplayOnTaskbar: Boolean read FDisplayOnTaskbar write SetDisplayOnTaskbar default False;
    property TaskbarState: TJvTaskBarProgressState read FTaskbarState write SetTaskbarState default tpsNormal;

    property HintColor;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
    property Color;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  TJvBaseGradientProgressBar = class(TJvBaseProgressBar)
  private
    FBarColorFrom: TColor;
    FBarColorTo: TColor;
    FInverted: Boolean;
    procedure SetBarColorFrom(Value: TColor);
    procedure SetBarColorTo(const Value: TColor);
    procedure SetInverted(const Value: Boolean);
  public
    property BarColorFrom: TColor read FBarColorFrom write SetBarColorFrom;
    property BarColorTo: TColor read FBarColorTo write SetBarColorTo;
    property Inverted: Boolean read FInverted write SetInverted default False; // Michiel Koot: enabling inverted drawing behaviour.
  end;

  TJvCustomGradientProgressBar = class(TJvBaseGradientProgressBar)
  protected
    procedure DrawBar(ACanvas: TCanvas; BarSize: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvGradientProgressBar = class(TJvCustomGradientProgressBar)
  published
    property BarColorFrom default clWhite;
    property BarColorTo default clBlack;
    property Max;
    property Min;
    property Orientation;
    property Position;
    property Smooth;
    property Inverted;

    property Align;
    property Anchors;
    property Color default clWindow;
    property Constraints;
    property DragKind;
    property DragCursor;
    property OnEndDock;
    property OnStartDock;
    property OnCanResize;
    property DragMode;
    property Hint;
    property ParentColor default False;
    property PopupMenu;
    property ParentShowHint;
    property ShowHint;

    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDrag;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  ComObj,
  JclSysInfo,
  JvJCLUtils, JvJVCLUtils;

const
  { For Windows >= XP }
  {$EXTERNALSYM PBS_MARQUEE}
  PBS_MARQUEE             = $08;
  {$EXTERNALSYM PBM_SETMARQUEE}
  PBM_SETMARQUEE          = WM_USER+10;

  { For Windows >= Vista }
  {$EXTERNALSYM PBS_SMOOTHREVERSE}
  PBS_SMOOTHREVERSE       = $10;

  { For Windows >= Vista }
  {$EXTERNALSYM PBM_GETSTEP}
  PBM_GETSTEP             = WM_USER+13;
  {$EXTERNALSYM PBM_GETBKCOLOR}
  PBM_GETBKCOLOR          = WM_USER+14;
  {$EXTERNALSYM PBM_GETBARCOLOR}
  PBM_GETBARCOLOR         = WM_USER+15;
  {$EXTERNALSYM PBM_SETSTATE}
  PBM_SETSTATE            = WM_USER+16;  { wParam = PBST_[State] (NORMAL, ERROR, PAUSED) }
  {$EXTERNALSYM PBM_GETSTATE}
  PBM_GETSTATE            = WM_USER+17;

  { For Windows >= Vista }
  {$EXTERNALSYM PBST_NORMAL}
  PBST_NORMAL             = $0001;
  {$EXTERNALSYM PBST_ERROR}
  PBST_ERROR              = $0002;
  {$EXTERNALSYM PBST_PAUSED}
  PBST_PAUSED             = $0003;

  cProgressStates: array[TJvProgressBarState] of Cardinal = (PBST_NORMAL, PBST_ERROR, PBST_PAUSED);

  cTBPFFlags: array[TJvTaskBarProgressState] of Integer = (TBPF_NORMAL, TBPF_NOPROGRESS, TBPF_INDETERMINATE, TBPF_ERROR, TBPF_PAUSED);

//=== { TJvBaseProgressBar } =================================================

constructor TJvBaseProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  FMin := 0;
  FMax := 100;
  FOrientation := pbHorizontal;
  FBlockSize := 10;
  FBarColor := clHighlight;
  FSteps := 10;
  Width := 150;
  Height := GetSystemMetrics(SM_CYVSCROLL);
end;

procedure TJvBaseProgressBar.Paint;
var
  ASize, APos: Integer;
begin
  if (Max - Min <= 0) or (Width <= 0) or (Height <= 0) then
    Exit;
  // calculate the size of the bar based on Min, Max, Position and Width or Height
  APos := Position;
  ASize := MulDiv(GetMaxBarSize, (APos - Min), (Max - Min));
  DrawBar(Canvas, ASize);
end;

procedure TJvBaseProgressBar.SetMax(Value: Integer);
begin
  if Value < FMin then
    Value := FMin;
  if FPosition > Value then
    FPosition := Value;
  if FMax <> Value then
  begin
    FMax := Value;
    Change;
    Invalidate;
  end;
end;

procedure TJvBaseProgressBar.SetMin(Value: Integer);
begin
  if Value > FMax then
    Value := FMax;
  if FPosition < FMin then
    FPosition := FMin;
  if FMin <> Value then
  begin
    FMin := Value;
    Change;
    Invalidate;
  end;
end;

procedure TJvBaseProgressBar.SetOrientation(Value: TProgressBarOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    if not (csLoading in ComponentState) then // fixes property load order
      SetBounds(Left, Top, Height, Width);
  end;
end;

procedure TJvBaseProgressBar.SetParent(AParent: TWinControl);
var
  CurParent: TControl;
begin
  inherited SetParent(AParent);

  FParentForm := nil;
  CurParent := AParent;

  while not Assigned(FParentForm) and Assigned(CurParent) do
  begin
    if CurParent is TCustomForm then
      FParentForm := TCustomForm(CurParent);    

    CurParent := CurParent.Parent;
  end; 
end;

procedure TJvBaseProgressBar.SetPosition(Value: Integer);
begin
  if Value > FMax then
    Value := FMax;
  if Value < FMin then
    Value := FMin;
  if FPosition <> Value then
  begin
    FPosition := Value;
    Change;
    Invalidate;

    if not (TaskbarState in [tpsNormal, tpsError, tpsPaused]) then
      TaskbarState := tpsNormal;

    if DisplayOnTaskbar and Assigned(FTaskbar) then
    begin
      {$IFDEF RTL185_UP}
      if Application.MainFormOnTaskbar then
      begin
        if Assigned(FParentForm) then
          FTaskBar.SetProgressValue(FParentForm.Handle, Value, Max);
      end
      else
      {$ENDIF RTL185_UP}
      begin
        FTaskBar.SetProgressValue(Application.Handle, Value, Max);
      end;
    end;
  end;
end;

procedure TJvBaseProgressBar.SetSmooth(const Value: Boolean);
begin
  if FSmooth <> Value then
  begin
    FSmooth := Value;
    Invalidate;
  end;
end;

procedure TJvBaseProgressBar.DrawBar(ACanvas: TCanvas; BarSize: Integer);
var
  R: TRect;
begin
  R := ClientRect;
  ACanvas.Brush.Color := Color;
  ACanvas.FillRect(R);
  DrawEdge(ACanvas.Handle, R, BDR_SUNKENOUTER, BF_ADJUST or BF_RECT);
  if BarSize = 0 then
    Exit;
  ACanvas.Brush.Color := BarColor;
  if Orientation = pbHorizontal then
  begin
    if Smooth then
    begin
      R.Right := R.Left + BarSize;
      InflateRect(R, -1, -1);
      if R.Right > Width - 2 then
        R.Right := Width - 2;
      if R.Right > R.Left then
        ACanvas.FillRect(R);
    end
    else
    begin
      R.Right := R.Left + Steps;
      InflateRect(R, -1, -1);
      while BarSize > 0 do
      begin
        if R.Right > Width - 3 then
          R.Right := Width - 3;
        if R.Left >= R.Right then
          Exit;
        ACanvas.FillRect(R);
        OffsetRect(R, RectWidth(R) + 2, 0);
        Dec(BarSize, RectWidth(R) + 2);
      end;
    end;
  end
  else
  begin
    if Smooth then
    begin
      R.Top := R.Bottom - BarSize;
      if R.Top < 2 then
        R.Top := 2;
      InflateRect(R, -1, -1);
      ACanvas.FillRect(R);
    end
    else
    begin
      OffsetRect(R, 0, Height - Steps - 2);
      R.Bottom := R.Top + Steps;
      InflateRect(R, -1, -1);
      while BarSize > 0 do
      begin
        if R.Top < 3 then
          R.Top := 3;
        ACanvas.FillRect(R);
        OffsetRect(R, 0, -Steps);
        Dec(BarSize, Steps);
      end;
    end;
  end;
end;

function TJvBaseProgressBar.GetMaxBarSize: Integer;
begin
  if Orientation = pbHorizontal then
    Result := Width
  else
    Result := Height;
end;

procedure TJvBaseProgressBar.SetSteps(const Value: Integer);
begin
  if FSteps <> Value then
  begin
    FSteps := Value;
    if FSteps < 1 then
      FSteps := 1;
  end;
end;

procedure TJvBaseProgressBar.SetTaskbarState(
  const Value: TJvTaskBarProgressState);
begin
  FTaskbarState := Value;

  if DisplayOnTaskbar and Assigned(FTaskbar) then
  begin
    {$IFDEF RTL185_UP}
    if Application.MainFormOnTaskbar then
    begin
      if Assigned(FParentForm) then
        FTaskbar.SetProgressState(FParentForm.Handle, cTBPFFlags[TaskbarState]);
    end
    else
    {$ENDIF RTL185_UP}
    begin
      FTaskbar.SetProgressState(Application.Handle, cTBPFFlags[TaskbarState]);
    end;
  end;
end;

procedure TJvBaseProgressBar.StepIt;
begin
  StepBy(Steps);
end;

procedure TJvBaseProgressBar.WndProc(var Message: TMessage);
var
  TaskBarList: IInterface;
begin
  if not (FTaskBarTested) and not Assigned(FTaskBar) then
  begin
    TaskBarList := CreateComObject(CLSID_TaskbarList);
    TaskBarList.QueryInterface(ITaskBarList3, FTaskBar);
    FTaskBarTested := True;
  end;

  inherited WndProc(Message);
end;

procedure TJvBaseProgressBar.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvBaseProgressBar.StepBy(Delta: Integer);
begin
  if Position + Delta > Max then
    Position := Max
  else
  if Position + Delta < Min then
    Position := Min
  else
    Position := Position + Delta;
end;

procedure TJvBaseProgressBar.SetBlockSize(const Value: Integer);
begin
  if FBlockSize <> Value then
  begin
    FBlockSize := Value;
    if FBlockSize <= 0 then
      FBlockSize := 1;
    Invalidate;
  end;
end;

procedure TJvBaseProgressBar.SetDisplayOnTaskbar(const Value: Boolean);
begin
  FDisplayOnTaskbar := Value;
end;

procedure TJvBaseProgressBar.SetBarColor(const Value: TColor);
begin
  if FBarColor <> Value then
  begin
    FBarColor := Value;
    Invalidate;
  end;
end;

//=== { TJvProgressBar } =====================================================

constructor TJvProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FillColor := clHighlight;
  FMarqueeDelay := 25;
end;

procedure TJvProgressBar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if Marquee and not (csDesigning in ComponentState) then
    Params.Style := Params.Style or PBS_MARQUEE;
  if SmoothReverse then
    Params.Style := Params.Style or PBS_SMOOTHREVERSE;
end;

procedure TJvProgressBar.CreateWnd;
var
  TaskBarList: IInterface;
begin
  inherited CreateWnd;
  SendMessage(Handle, PBM_SETBARCOLOR, 0, ColorToRGB(FFillColor));
  if Marquee then
    SendMessage(Handle, PBM_SETMARQUEE, Ord(not MarqueePaused), LPARAM(MarqueeDelay));
  if State <> pbsNormal then
    SendMessage(Handle, PBM_SETSTATE, cProgressStates[State], 0);

  TaskBarList := CreateComObject(CLSID_TaskbarList);
  TaskBarList.QueryInterface(ITaskBarList3, FTaskBar);
end;

procedure TJvProgressBar.PbmSetPos(var Msg: TMessage);
begin
  inherited;

  SetTaskbarPosition(Msg.WParam);
end;

procedure TJvProgressBar.PbmStepIp(var Msg: TMessage);
begin
  inherited;

  SetTaskbarPosition(Position);
end;

procedure TJvProgressBar.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  // Reduce flicker
  DefaultHandler(Message);
end;

procedure TJvProgressBar.SetDisplayOnTaskbar(Value: Boolean);
begin
  FDisplayOnTaskbar := Value;
end;

procedure TJvProgressBar.SetFillColor(const Value: TColor);
begin
  if FFillColor <> Value then
  begin
    FFillColor := Value;
    if HandleAllocated then
    begin
      SendMessage(Handle, PBM_SETBARCOLOR, 0, ColorToRGB(FFillColor));
      // (rom) Invalidate is not good enough
      Repaint;
    end;
  end;
end;

procedure TJvProgressBar.SetMarquee(Value: Boolean);
begin
  if Value <> FMarquee then
  begin
    FMarquee := Value;
    if HandleAllocated and not (csDesigning in ComponentState) then
      RecreateWnd;
  end;
end;

procedure TJvProgressBar.SetMarqueePaused(Value: Boolean);
begin
  if Value <> FMarqueePaused then
  begin
    FMarqueePaused := Value;
    if Marquee and HandleAllocated and not (csDesigning in ComponentState) then
      SendMessage(Handle, PBM_SETMARQUEE, Ord(not MarqueePaused), LPARAM(MarqueeDelay));
  end;
end;

procedure TJvProgressBar.SetParent(AParent: TWinControl);
var
  CurParent: TControl;
begin
  inherited SetParent(AParent);

  FParentForm := nil;
  CurParent := AParent;

  while not Assigned(FParentForm) and Assigned(CurParent) do
  begin
    if CurParent is TCustomForm then
      FParentForm := TCustomForm(CurParent);    

    CurParent := CurParent.Parent;
  end; 
end;

procedure TJvProgressBar.SetMarqueeDelay(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> FMarqueeDelay then
  begin
    FMarqueeDelay := Value;
    if Marquee and HandleAllocated and not (csDesigning in ComponentState) then
      SendMessage(Handle, PBM_SETMARQUEE, Ord(not MarqueePaused), LPARAM(MarqueeDelay));
  end;
end;

procedure TJvProgressBar.SetSmoothReverse(Value: Boolean);
begin
  if Value <> FSmoothReverse then
  begin
    FSmoothReverse := Value;
    if HandleAllocated then
      RecreateWnd;
  end;
end;

procedure TJvProgressBar.SetState(Value: TJvProgressBarState);
begin
  if Value <> FState then
  begin
    FState := Value;
    if HandleAllocated then
      SendMessage(Handle, PBM_SETSTATE, cProgressStates[State], 0);
  end;
end;

procedure TJvProgressBar.SetTaskbarPosition(Value: Integer);
begin
  if not (TaskbarState in [tpsNormal, tpsError, tpsPaused]) then
    TaskbarState := tpsNormal;

  if DisplayOnTaskbar and Assigned(FTaskbar) then
  begin
    {$IFDEF RTL185_UP}
    if Application.MainFormOnTaskbar then
    begin
      if Assigned(FParentForm) then
        FTaskBar.SetProgressValue(FParentForm.Handle, Value, Max);
    end
    else
    {$ENDIF RTL185_UP}
    begin
      FTaskBar.SetProgressValue(Application.Handle, Value, Max);
    end;
  end;
end;

procedure TJvProgressBar.SetTaskbarState(const Value: TJvTaskBarProgressState);
begin
  FTaskbarState := Value;

  if DisplayOnTaskbar and Assigned(FTaskbar) then
  begin
    {$IFDEF RTL185_UP}
    if Application.MainFormOnTaskbar then
    begin
      if Assigned(FParentForm) then
        FTaskbar.SetProgressState(FParentForm.Handle, cTBPFFlags[TaskbarState]);
    end
    else
    {$ENDIF RTL185_UP}
    begin
      FTaskbar.SetProgressState(Application.Handle, cTBPFFlags[TaskbarState]);
    end;
  end;
end;

//=== { TJvBaseGradientProgressBar } =========================================

procedure TJvBaseGradientProgressBar.SetBarColorFrom(Value: TColor);
begin
  if FBarColorFrom <> Value then
  begin
    FBarColorFrom := Value;
    Invalidate;
  end;
end;

procedure TJvBaseGradientProgressBar.SetBarColorTo(const Value: TColor);
begin
  if FBarColorTo <> Value then
  begin
    FBarColorTo := Value;
    Invalidate;
  end;
end;

procedure TJvBaseGradientProgressBar.SetInverted(const Value: Boolean);
begin
  if FInverted <> Value then
  begin
    FInverted := Value;
    Invalidate;
  end;
end;

//=== { TJvGradientProgressBar } =============================================

constructor TJvCustomGradientProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBarColorFrom := clWhite;
  FBarColorTo := clBlack;
  BlockSize := 6;
end;

procedure TJvCustomGradientProgressBar.DrawBar(ACanvas: TCanvas; BarSize: Integer);
var
  R: TRect;
  LBlockSize: Double;
  I: Integer;
begin
  R := ClientRect;
  ACanvas.Brush.Color := Color;
  ACanvas.FillRect(R);
  DrawEdge(ACanvas.Handle, R, BDR_SUNKENOUTER, BF_ADJUST or BF_RECT);
  InflateRect(R, -1, -1);
  if Orientation = pbHorizontal then
  begin
    if not FInverted then
      R.Right := BarSize
    else
      R.Left := R.Right - BarSize;
    if R.Right > ClientWidth - 2 then
      R.Right := ClientWidth - 2;
    GradientFillRect(ACanvas, R, BarColorFrom, BarColorTo, fdLeftToRight, 255);
  end
  else
  begin
    if not FInverted then
      R.Top := R.Bottom - BarSize
    else
      R.Bottom := R.Top + BarSize;
    if R.Top < 2 then
      R.Top := 2;
    GradientFillRect(ACanvas, R, BarColorFrom, BarColorTo, fdBottomToTop, 255);
  end;
  if not Smooth then
  begin
    ACanvas.Pen.Color := Color;
    if Position > 0 then
      LBlockSize := (GetMaxBarSize * BlockSize - 4.0) / 100.0
    else
      LBlockSize := 0;
    I := 0;
    if Orientation = pbHorizontal then
    begin
      R := ClientRect;
      if not FInverted then
      begin
        InflateRect(R, -2, -2);
        R.Right := R.Left + Round(LBlockSize);
        while R.Left <= BarSize do
        begin
          ACanvas.MoveTo(R.Left, R.Top);
          ACanvas.LineTo(R.Left, R.Bottom);
          Inc(I);
          R := ClientRect;
          InflateRect(R, -2, -2);
          R.Right := R.Left + Round(LBlockSize);
          OffsetRect(R, Round(I * LBlockSize), 0);
        end;
      end
      else // Inverted horizontal
      begin
        InflateRect(R, 2, 2);
        R.Left := R.Right - Round(LBlockSize);
        while (BarSize <> 0) and (R.Left >= (GetMaxBarSize - BarSize)) do
        begin
          ACanvas.MoveTo(R.Right, R.Top);
          ACanvas.LineTo(R.Right, R.Bottom);
          Inc(I);
          R := ClientRect;
          InflateRect(R, 2, 2);
          R.Left := R.Right - Round(LBlockSize);
          OffsetRect(R, -Round(I * LBlockSize), 0);
        end;
      end;
    end
    else
    begin
      R := ClientRect;
      if not FInverted then
      begin
        InflateRect(R, -2, -2);
        R.Top := R.Bottom - Round(LBlockSize);
        while R.Bottom >= GetMaxBarSize - BarSize do
        begin
          ACanvas.MoveTo(R.Left, R.Bottom);
          ACanvas.LineTo(R.Right, R.Bottom);
          Inc(I);
          R := ClientRect;
          InflateRect(R, -2, -2);
          R.Top := R.Bottom - Round(LBlockSize);
          OffsetRect(R, 0, -Round(I * LBlockSize));
        end;
      end
      else // Inverted vertical
      begin
        InflateRect(R, 2, 2);
        R.Bottom := R.Top + Round(LBlockSize);
        while (BarSize <> 0) and (R.Top <= BarSize) do
        begin
          ACanvas.MoveTo(R.Left, R.Top);
          ACanvas.LineTo(R.Right, R.Top);
          Inc(I);
          R := ClientRect;
          InflateRect(R, 2, 2);
          R.Bottom := R.Top + Round(LBlockSize);
          OffsetRect(R, 0, Round(I * LBlockSize));
        end;
      end;
    end;
  end;
end;


{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
