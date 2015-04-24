{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPanel.pas, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
pongtawat
Peter Thornqvist [peter3 at sourceforge dot net]
Jens Fudickar [jens dott fudickar att oratool dott de]
dejoy den [dejoy att ynl dott gov dott cn]

Changes:

>> dejoy --2005-04-28
  - Change TJvArrangeSettings to inherited from TJvPersistentProperty.
  - TJvCustomArrangePanel implemented interface of IJvHotTrack.
  - Renamed HotColor property to HotTrackOptions.Color.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvPanel;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages,
  SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  JvTypes, JvExtComponent, JvExControls,
  JvHotTrackPersistent;

type
  TJvPanelResizeParentEvent = procedure(Sender: TObject; nLeft, nTop, nWidth, nHeight: Integer) of object;
  TJvPanelChangedSizeEvent = procedure(Sender: TObject; ChangedSize: Integer) of object;
  TJvAutoSizePanel = (asNone, asWidth, asHeight, asBoth);
  TJvArrangeSettingsVAlignment = (asTop, asVCenter, asBottom);
  TJvArrangeSettingsHAlignment = (asLeft, asCenter, asRight);

  TJvArrangeSettings = class(TJvPersistentProperty)
  private
    FAutoArrange: Boolean;
    FAutoSize: TJvAutoSizePanel;
    FWrapControls: Boolean;
    FBorderLeft: Integer;
    FBorderTop: Integer;
    FDistanceVertical: Integer;
    FDistanceHorizontal: Integer;
    FShowNotVisibleAtDesignTime: Boolean;
    FMaxWidth: Integer;
    FVerticalAlignment: TJvArrangeSettingsVAlignment;
    FHorizontalAlignment: TJvArrangeSettingsHAlignment;
    FMaxControlsPerLine: Integer;
    FHorizontalAlignLines: Boolean;
    procedure SetWrapControls(Value: Boolean);
    procedure SetAutoArrange(Value: Boolean);
    procedure SetAutoSize(Value: TJvAutoSizePanel);
    procedure SetBorderLeft(Value: Integer);
    procedure SetBorderTop(Value: Integer);
    procedure SetDistanceVertical(Value: Integer);
    procedure SetDistanceHorizontal(Value: Integer);
    procedure SetMaxWidth(Value: Integer);
    procedure SetHorizontalAlignment(const Value: TJvArrangeSettingsHAlignment);
    procedure SetVerticalAlignment(const Value: TJvArrangeSettingsVAlignment);
    procedure SetMaxControlsPerLine(const Value: Integer);
    procedure SetHorizontalAlignLines(const Value: Boolean);
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property WrapControls: Boolean read FWrapControls write SetWrapControls default True;
    property BorderLeft: Integer read FBorderLeft write SetBorderLeft default 0;
    property BorderTop: Integer read FBorderTop write SetBorderTop default 0;
    property DistanceVertical: Integer read FDistanceVertical write SetDistanceVertical default 0;
    property DistanceHorizontal: Integer read FDistanceHorizontal write SetDistanceHorizontal default 0;
    property ShowNotVisibleAtDesignTime: Boolean read FShowNotVisibleAtDesignTime write FShowNotVisibleAtDesignTime default True;
    property AutoSize: TJvAutoSizePanel read FAutoSize write SetAutoSize default asNone;
    property AutoArrange: Boolean read FAutoArrange write SetAutoArrange default False;
    property MaxWidth: Integer read FMaxWidth write SetMaxWidth default 0;
    { MaxControlsPerLine specifies the max. number of controls that fit into a line. The following
      controls are moved to the next line. A value of zero means no limit. WrapControls is still
      considered. }
    property MaxControlsPerLine: Integer read FMaxControlsPerLine write SetMaxControlsPerLine default 0;
    { VerticalAlignment aligns the arranged control-block. in the panel unless AutoSize is asBoth or asHeight. }
    property VerticalAlignment: TJvArrangeSettingsVAlignment read FVerticalAlignment write SetVerticalAlignment default asTop;
    { HorizontalAlignment aligns the arranged control-block in the panel unless AutoSize is asBoth or asWidth. }
    property HorizontalAlignment: TJvArrangeSettingsHAlignment read FHorizontalAlignment write SetHorizontalAlignment default asLeft;
    { HorizontalAlignLines aligns the control-lines. This only works if WrapControls or MaxControlsPerLine is enabled }
    property HorizontalAlignLines: Boolean read FHorizontalAlignLines write SetHorizontalAlignLines default False;
  end;

  IJvArrangePanel = interface
  ['{8EE63749-CDDC-4436-9067-4EF0434B43C2}']
    procedure ArrangeControls;
    procedure DisableArrange;
    procedure EnableArrange;
    function GetArrangeSettings: TJvArrangeSettings;
    procedure SetArrangeSettings(const Value: TJvArrangeSettings);
    property ArrangeSettings: TJvArrangeSettings read GetArrangeSettings write SetArrangeSettings;
  end;

  TJvPanelHotTrackOptions = class(TJvHotTrackOptions)
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property Color default clBtnFace;
  end;

  TJvPanelMoveEvent = procedure(Sender: TObject; X, Y: Integer; var Allow: Boolean) of object;

  TJvCustomArrangePanel = class(TJvCustomPanel, IJvDenySubClassing, IJvHotTrack, IJvArrangePanel)
  private
    FTransparent: Boolean;
    FFlatBorder: Boolean;
    FFlatBorderColor: TColor;
    FMultiLine: Boolean;
    FSizeable: Boolean;
    FDragging: Boolean;
    FLastPos: TPoint;
    FEnableArrangeCount: Integer;
    FArrangeControlActive: Boolean;
    FArrangeWidth: Integer;
    FArrangeHeight: Integer;
    FArrangeSettings: TJvArrangeSettings;
    FOnResizeParent: TJvPanelResizeParentEvent;
    FOnChangedWidth: TJvPanelChangedSizeEvent;
    FOnChangedHeight: TJvPanelChangedSizeEvent;
    FOnPaint: TNotifyEvent;
    FMovable: Boolean;
    FWasMoved: Boolean;
    FOnAfterMove: TNotifyEvent;
    FOnBeforeMove: TJvPanelMoveEvent;
    FHotTrack: Boolean;
    FHotTrackFont: TFont;
    FHotTrackFontOptions: TJvTrackFontOptions;
    FHotTrackOptions: TJvHotTrackOptions;
    FLastScreenCursor: TCursor;
    FPainting: Integer;
    function GetArrangeSettings: TJvArrangeSettings;
    function GetHeight: Integer;
    procedure SetHeight(Value: Integer);
    function GetWidth: Integer;
    procedure SetWidth(Value: Integer);
    procedure SetArrangeSettings(const Value: TJvArrangeSettings);
    procedure SetTransparent(const Value: Boolean);
    procedure SetFlatBorder(const Value: Boolean);
    procedure SetFlatBorderColor(const Value: TColor);
    procedure SetMultiLine(const Value: Boolean);
    procedure SetSizeable(const Value: Boolean);

    {IJvHotTrack}   //added by dejoy 2005-04-28
    function GetHotTrack: Boolean;
    function GetHotTrackFont: TFont;
    function GetHotTrackFontOptions: TJvTrackFontOptions;
    function GetHotTrackOptions: TJvHotTrackOptions;
    procedure SetHotTrack(Value: Boolean);
    procedure SetHotTrackFont(Value: TFont);
    procedure SetHotTrackFontOptions(Value: TJvTrackFontOptions);
    procedure SetHotTrackOptions(Value: TJvHotTrackOptions);
    procedure IJvHotTrack_Assign(Source: IJvHotTrack);
    procedure IJvHotTrack.Assign = IJvHotTrack_Assign;
    function IsHotTrackFontStored: Boolean;
  protected
    procedure DrawCaption; dynamic;
    procedure DrawCaptionTo(ACanvas: TCanvas ); dynamic;
    procedure DrawBorders; dynamic;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    procedure ParentColorChanged; override;
    procedure TextChanged; override;
    procedure Paint; override;
    function DoEraseBackground(Canvas: TCanvas; Param: LPARAM): Boolean; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMExitSizeMove(var Msg: TMessage); message WM_EXITSIZEMOVE;
    function DoBeforeMove(X, Y: Integer): Boolean; dynamic;
    procedure DoAfterMove; dynamic;
    procedure Loaded; override;
    procedure Resize; override;
    procedure Rearrange;
    procedure DoArrangeSettingsPropertyChanged(Sender: TObject; const PropName: string); virtual;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    function GetNextControlByTabOrder(ATabOrder: Integer): TWinControl;
    procedure SetSizeableCursor;
    procedure RestoreSizeableCursor;
    function GetControlSize(Control: TControl): TSize;
    procedure SetControlBounds(Control: TControl; const R: TRect);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure ArrangeControls;
    procedure EnableArrange;
    procedure DisableArrange;
    function ArrangeEnabled: Boolean;
    property ArrangeWidth: Integer read FArrangeWidth;
    property ArrangeHeight: Integer read FArrangeHeight;
    property DockManager;
    property Canvas;

    property HotTrack: Boolean read GetHotTrack write SetHotTrack default False;
    property HotTrackFont: TFont read GetHotTrackFont write SetHotTrackFont stored IsHotTrackFontStored;
    property HotTrackFontOptions: TJvTrackFontOptions read GetHotTrackFontOptions write SetHotTrackFontOptions default
      DefaultTrackFontOptions;
    property HotTrackOptions: TJvHotTrackOptions read GetHotTrackOptions write SetHotTrackOptions;

    property Movable: Boolean read FMovable write FMovable default False;
    property Sizeable: Boolean read FSizeable write SetSizeable default False;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property MultiLine: Boolean read FMultiLine write SetMultiLine default False;
    //FlatBorder used the BorderWidth to draw the border
    property FlatBorder: Boolean read FFlatBorder write SetFlatBorder default False;
    property FlatBorderColor: TColor read FFlatBorderColor write SetFlatBorderColor default clBtnShadow;
    property OnBeforeMove: TJvPanelMoveEvent read FOnBeforeMove write FOnBeforeMove;
    property OnAfterMove: TNotifyEvent Read FOnAfterMove write FOnAfterMove;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;

    property ArrangeSettings: TJvArrangeSettings read GetArrangeSettings write SetArrangeSettings;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property OnResizeParent: TJvPanelResizeParentEvent read FOnResizeParent write FOnResizeParent;
    property OnChangedWidth: TJvPanelChangedSizeEvent read FOnChangedWidth write FOnChangedWidth;
    property OnChangedHeight: TJvPanelChangedSizeEvent read FOnChangedHeight write FOnChangedHeight;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvPanel = class(TJvCustomArrangePanel)
  private
    FFilerTag: string;
    procedure ReadData(Reader: TReader);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  published
    property HotTrack;
    property HotTrackFont;
    property HotTrackFontOptions;
    property HotTrackOptions;

    property Movable;
    property Sizeable;
    property HintColor;
    property Transparent;
    property MultiLine;
    property FlatBorder;
    property FlatBorderColor;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnBeforeMove;
    property OnAfterMove;
    property OnParentColorChange;
    property OnPaint;

    property ArrangeSettings;
    property Width;
    property Height;
    property OnResizeParent;
    property OnChangedWidth;
    property OnChangedHeight;

    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property FullRepaint;
    property Locked;
    property ParentBiDiMode;
    property OnCanResize;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnUnDock;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    {$IFDEF DELPHI2009_UP}
    property DoubleBuffered;
    {$ENDIF DELPHI2009_UP}
    property DragMode;
    property Enabled;
    property Font;
    {$IFDEF DELPHI2006_UP}
    property Padding;
    {$ENDIF DELPHI2006_UP}
    {$IFDEF JVCLThemesEnabled}
    property ParentBackground default True;
    {$ENDIF JVCLThemesEnabled}
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    {$IFDEF DELPHI2010_UP}
    property Touch;
    {$ENDIF DELPHI2010_UP}
    property Visible;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
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
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  Types, JvThemes,
  JvJCLUtils, JvJVCLUtils, JvResources;

const
  BkModeTransparent = TRANSPARENT;

//=== { TJvArrangeSettings } =================================================

constructor TJvArrangeSettings.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FMaxWidth := 0;
  FBorderLeft := 0;
  FBorderTop := 0;
  FDistanceVertical := 0;
  FDistanceHorizontal := 0;
  FMaxControlsPerLine := 0;
  FWrapControls := True;
  FShowNotVisibleAtDesignTime := True;
  FAutoSize := asNone;
  AutoArrange := False;
end;

destructor TJvArrangeSettings.Destroy;
begin
  if (Owner is TJvPanel) and not (csDestroying in TJvPanel(Owner).ComponentState) then
  begin
    // User code tried to destroy the TJvPanel.ArrangeSettings
    // objects leaving the panel in a broken state. Please fix your code by adding
    //
    //    if not ((Components[I] is TJvArrangeSettings) or
    //            (Components[I] is TJvPanelHotTrackOptions)) then
    //
    // or by using the Controls[] array property if possible.

    raise EJVCLException.CreateRes(@RsDestroyingArrangeSettingsNotAllowed);
  end;
  inherited Destroy;
end;

procedure TJvArrangeSettings.SetWrapControls(Value: Boolean);
begin
  if Value <> FWrapControls then
  begin
    Changing;
    ChangingProperty('WrapControls');
    FWrapControls := Value;
    ChangedProperty('WrapControls');
    Changed;
  end;
end;

procedure TJvArrangeSettings.SetAutoArrange(Value: Boolean);
begin
  if Value <> FAutoArrange then
  begin
    Changing;
    ChangingProperty('AutoArrange');
    FAutoArrange := Value;
    ChangedProperty('AutoArrange');
    Changed;
  end;
end;

procedure TJvArrangeSettings.SetAutoSize(Value: TJvAutoSizePanel);
begin
  if Value <> FAutoSize then
  begin
    Changing;
    ChangingProperty('AutoSize');
    FAutoSize := Value;
    ChangedProperty('AutoSize');
    Changed;
  end;
end;

procedure TJvArrangeSettings.SetBorderLeft(Value: Integer);
begin
  if Value <> FBorderLeft then
  begin
    Changing;
    ChangingProperty('BorderLeft');
    FBorderLeft := Value;
    ChangedProperty('BorderLeft');
    Changed;
  end;
end;

procedure TJvArrangeSettings.SetBorderTop(Value: Integer);
begin
  if Value <> FBorderTop then
  begin
    Changing;
    ChangingProperty('BorderTop');
    FBorderTop := Value;
    ChangedProperty('BorderTop');
    Changed;
  end;
end;

procedure TJvArrangeSettings.SetMaxControlsPerLine(const Value: Integer);
begin
  if Value <> FMaxControlsPerLine then
  begin
    Changing;
    ChangingProperty('MaxControlsPerLine');
    FMaxControlsPerLine := Value;
    ChangedProperty('MaxControlsPerLine');
    Changed;
  end;
end;

procedure TJvArrangeSettings.SetDistanceVertical(Value: Integer);
begin
  if Value <> FDistanceVertical then
  begin
    Changing;
    ChangingProperty('DistanceVertical');
    FDistanceVertical := Value;
    ChangedProperty('DistanceVertical');
    Changed;
  end;
end;

procedure TJvArrangeSettings.SetHorizontalAlignment(const Value: TJvArrangeSettingsHAlignment);
begin
  if Value <> FHorizontalAlignment then
  begin
    Changing;
    ChangingProperty('HorizontalAlignment');
    FHorizontalAlignment := Value;
    ChangedProperty('HorizontalAlignment');
    Changed;
  end;
end;

procedure TJvArrangeSettings.SetDistanceHorizontal(Value: Integer);
begin
  if Value <> FDistanceHorizontal then
  begin
    Changing;
    ChangingProperty('DistanceHorizontal');
    FDistanceHorizontal := Value;
    ChangedProperty('DistanceHorizontal');
    Changed;
  end;
end;

procedure TJvArrangeSettings.SetMaxWidth(Value: Integer);
begin
  if Value <> FMaxWidth then
  begin
    Changing;
    ChangingProperty('MaxWidth');
    FMaxWidth := Value;
    ChangedProperty('MaxWidth');
    Changed;
  end;
end;

procedure TJvArrangeSettings.SetHorizontalAlignLines(const Value: Boolean);
begin
  if Value <> FHorizontalAlignLines then
  begin
    Changing;
    ChangingProperty('HorizontalAlignLines');
    FHorizontalAlignLines := Value;
    ChangedProperty('HorizontalAlignLines');
    Changed;
  end;
end;

procedure TJvArrangeSettings.SetVerticalAlignment(const Value: TJvArrangeSettingsVAlignment);
begin
  if Value <> FVerticalAlignment then
  begin
    Changing;
    ChangingProperty('VerticalAlignment');
    FVerticalAlignment := Value;
    ChangedProperty('VerticalAlignment');
    Changed;
  end;
end;

procedure TJvArrangeSettings.Assign(Source: TPersistent);
var
  A: TJvArrangeSettings;
begin
  if Source is TJvArrangeSettings then
  begin
    BeginUpdate;
    try
      A := TJvArrangeSettings(Source);
      AutoArrange := A.AutoArrange;
      AutoSize := A.AutoSize;
      WrapControls := A.WrapControls;
      BorderLeft := A.BorderLeft;
      BorderTop := A.BorderTop;
      DistanceVertical := A.DistanceVertical;
      DistanceHorizontal := A.DistanceHorizontal;
      ShowNotVisibleAtDesignTime := A.ShowNotVisibleAtDesignTime;
      MaxWidth := A.MaxWidth;
      MaxControlsPerLine := A.MaxControlsPerLine;
      VerticalAlignment := A.VerticalAlignment;
      HorizontalAlignment := A.HorizontalAlignment;
      HorizontalAlignLines := A.HorizontalAlignLines;
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

//=== { TJvPanelHotTrackOptions } ============================================

constructor TJvPanelHotTrackOptions.Create(AOwner: TPersistent);
begin
  inherited;
  Color := clBtnFace;
end;

//=== { TJvCustomArrangePanel } ==============================================

constructor TJvCustomArrangePanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IncludeThemeStyle(Self, [csNeedsBorderPaint, csParentBackground]);
  ControlStyle := ControlStyle - [csSetCaption];
  FMultiLine := False;
  FTransparent := False;
  FFlatBorder := False;
  FFlatBorderColor := clBtnShadow;
  FHotTrack := False;
  FHotTrackFont := TFont.Create;
  FHotTrackFontOptions := DefaultTrackFontOptions;
  FHotTrackOptions := TJvPanelHotTrackOptions.Create(Self);
  FArrangeSettings := TJvArrangeSettings.Create(Self); // "Self" is a must, otherwise the ObjectInspector has problems
  FArrangeSettings.OnChangedProperty := DoArrangeSettingsPropertyChanged;
end;

destructor TJvCustomArrangePanel.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FHotTrackFont);
end;

procedure TJvCustomArrangePanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if Transparent then
  begin
    Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
    ControlStyle := ControlStyle - [csOpaque];
  end
  else
  begin
    Params.ExStyle := Params.ExStyle and not WS_EX_TRANSPARENT;
    ControlStyle := ControlStyle + [csOpaque];
  end;
end;

procedure TJvCustomArrangePanel.WMNCHitTest(var Msg: TWMNCHitTest);
var
  P: TPoint;
begin
  inherited;
  if not (csDesigning in ComponentState) and Movable then
  begin
    P := ScreenToClient(SmallPointToPoint(Msg.Pos));
    if (P.X > 5) and (P.Y > 5) and (P.X < Width - 5) and (P.Y < Height - 5) and DoBeforeMove(P.X,P.Y) then
    begin
      Msg.Result := HTCAPTION;
      FWasMoved := True;
    end;
  end;
end;

procedure TJvCustomArrangePanel.WMExitSizeMove(var Msg: TMessage);
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    if FWasMoved then
      DoAfterMove;
    FWasMoved := False;
  end;
end;

function TJvCustomArrangePanel.DoBeforeMove(X,Y: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnBeforeMove) then
    FOnBeforeMove(Self, X, Y, Result);
end;

procedure TJvCustomArrangePanel.DoAfterMove;
begin
  if Assigned(FOnAfterMove) then
    FOnAfterMove(Self);
end;

procedure TJvCustomArrangePanel.Paint;
var
  X, Y: Integer;
  R: TRect;
  OldPenColor:TColor;
  OldPenWidth: Integer;
  ControlIndex: Integer;
  CurControl: TControl;
begin
  if Assigned(FOnPaint) then
  begin
    FOnPaint(Self);
    Exit;
  end;

  Inc(FPainting);
  try
    // must force child controls to redraw completely, even their non client areas (Mantis 4406)
    if Transparent and (FPainting = 1) then
    begin
      for ControlIndex := 0 to ControlCount - 1 do
      begin
        CurControl := Controls[ControlIndex];
        if CurControl is TWinControl then
        begin
          CurControl.Invalidate;
          RedrawWindow(TWinControl(CurControl).Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE);
          // Must update here so that the invalidate message is processed immediately
          // If not, there is a very strong risk of creating a refresh loop
          CurControl.Update;
        end;
      end;
    end;

    if MouseOver and HotTrack then
    begin
      Canvas.Font := Self.HotTrackFont;
      if HotTrackOptions.Enabled then
      begin
        Canvas.Brush.Color := HotTrackOptions.Color;
        if HotTrackOptions.FrameVisible then
        begin
          Canvas.Brush.Style := bsSolid;
          OldPenColor := Canvas.Pen.Color;
          Canvas.Pen.Color := HotTrackOptions.FrameColor;
          Canvas.Rectangle(0, 0, Width, Height);
          Canvas.Pen.Color := OldPenColor;
        end
        else
        begin
          R := ClientRect;
          InflateRect(R, -BevelWidth, -BevelWidth);
          Canvas.FillRect(R);
        end;
      end;
    end
    else
    begin
      Canvas.Font := Self.Font;
      Canvas.Brush.Color := Color;
      if not Transparent then
        DrawThemedBackground(Self, Canvas, ClientRect)
      else
        Canvas.Brush.Style := bsClear;
      if FFlatBorder then
      begin
        if BorderWidth > 0 then
        begin
          OldPenWidth:= Canvas.Pen.Width;
          OldPenColor := Canvas.Pen.Color;
          Canvas.Pen.Width := BorderWidth;
          Canvas.Pen.Color := FFlatBorderColor;
          Canvas.Brush.Style := bsClear;

          R := ClientRect;
          X := (BorderWidth div 2);
          if Odd(BorderWidth) then
            Y := X
          else
            Y := X -1;

          Inc(R.Left,X);
          Inc(R.Top,X);
          Dec(R.Bottom,Y);
          Dec(R.Right,Y);

          Canvas.Rectangle(R);

          Canvas.Pen.Width := OldPenWidth;
          Canvas.Pen.Color := OldPenColor;
       end;
      end
      else
        DrawBorders;
    end;

    DrawCaption;
    if Sizeable then
    begin
      {$IFDEF JVCLThemesEnabled}
      if StyleServices.Enabled then
        StyleServices.DrawElement(Canvas.Handle, StyleServices.GetElementDetails(tsGripper),
          Rect(ClientWidth - GetSystemMetrics(SM_CXVSCROLL) - BevelWidth - 2,
            ClientHeight - GetSystemMetrics(SM_CYHSCROLL) - BevelWidth - 2,
            ClientWidth - BevelWidth - 2, ClientHeight - BevelWidth - 2))
      else
      {$ENDIF JVCLThemesEnabled}
      begin
        Canvas.Font.Name := 'Marlett';
        Canvas.Font.Charset := DEFAULT_CHARSET;
        Canvas.Font.Size := 12;
        Canvas.Font.Style := [];
        Canvas.Brush.Style := bsClear;
        X := ClientWidth - GetSystemMetrics(SM_CXVSCROLL) - BevelWidth - 2;
        Y := ClientHeight - GetSystemMetrics(SM_CYHSCROLL) - BevelWidth - 2;
        // (rom) bsClear takes care of that already
        //if Transparent then
        //  SetBkMode(Handle, BkModeTransparent);
        Canvas.Font.Color := clBtnHighlight;
        Canvas.TextOut(X, Y, 'o');
        Canvas.Font.Color := clBtnShadow;
        Canvas.TextOut(X, Y, 'p');
      end;
    end;
  finally
    Dec(FPainting);
  end;
end;

procedure TJvCustomArrangePanel.DrawBorders;
var
  Rect: TRect;
  TopColor, BottomColor: TColor;

  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then
      TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then
      BottomColor := clBtnHighlight;
  end;

begin
  Rect := ClientRect;
  if BevelOuter <> bvNone then
  begin
    AdjustColors(BevelOuter);
    Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;
  Frame3D(Canvas, Rect, Color, Color, BorderWidth);
  if BevelInner <> bvNone then
  begin
    AdjustColors(BevelInner);
    Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;
end;

procedure TJvCustomArrangePanel.DrawCaption;
begin
  DrawCaptionTo(Self.Canvas);
end;

procedure TJvCustomArrangePanel.DrawCaptionTo(ACanvas: TCanvas );
const
  Alignments: array [TAlignment] of Longint = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWrap: array [Boolean] of Longint = (DT_SINGLELINE, DT_WORDBREAK);
var
  ATextRect: TRect;
  BevelSize: Integer;
  Flags: Longint;
begin
  with ACanvas do
  begin
    if Caption <> '' then
    begin
      if (MouseOver or FDragging) and HotTrack then
        ACanvas.Font := Self.HotTrackFont
      else
        ACanvas.Font := Self.Font;

      SetBkMode(Handle, BkModeTransparent);
      Font := Self.Font;
      ATextRect := GetClientRect;
      InflateRect(ATextRect, -BorderWidth, -BorderWidth);
      BevelSize := 0;
      if BevelOuter <> bvNone then
        Inc(BevelSize, BevelWidth);
      if BevelInner <> bvNone then
        Inc(BevelSize, BevelWidth);
      InflateRect(ATextRect, -BevelSize, -BevelSize);
      Flags := DT_EXPANDTABS or WordWrap[MultiLine] or Alignments[Alignment];
      Flags := DrawTextBiDiModeFlags(Flags);
      //calculate required rectangle size
      DrawText(ACanvas.Handle, Caption, -1, ATextRect, Flags or DT_CALCRECT);
      // adjust the rectangle placement
      OffsetRect(ATextRect, 0, -ATextRect.Top + (Height - (ATextRect.Bottom - ATextRect.Top)) div 2);
      case Alignment of
        taRightJustify:
          OffsetRect(ATextRect, -ATextRect.Left + (Width - (ATextRect.Right - ATextRect.Left) - BorderWidth -
            BevelSize), 0);
        taCenter:
          OffsetRect(ATextRect, -ATextRect.Left + (Width - (ATextRect.Right - ATextRect.Left)) div 2, 0);
      end;
      if not Enabled then
        Font.Color := clGrayText;
      //draw text
      if Transparent then
        SetBkMode(ACanvas.Handle, BkModeTransparent);
      DrawText(ACanvas.Handle, Caption, -1, ATextRect, Flags);
    end;
  end;
end;

procedure TJvCustomArrangePanel.ParentColorChanged;
begin
  Invalidate;
  inherited ParentColorChanged;
end;

procedure TJvCustomArrangePanel.MouseEnter(Control: TControl);
var
  NeedRepaint: Boolean;
  OtherDragging:Boolean;
begin
  if csDesigning in ComponentState then
    Exit;

  if not MouseOver and Enabled and (Control = nil) then
  begin
    OtherDragging := Mouse.IsDragging;
    NeedRepaint := not Transparent and
     ((FHotTrack and Enabled and not FDragging and not OtherDragging));
    inherited MouseEnter(Control); // set MouseOver
    if NeedRepaint then
      Repaint;
  end
  else
    inherited MouseEnter(Control);
end;

procedure TJvCustomArrangePanel.MouseLeave(Control: TControl);
var
  NeedRepaint: Boolean;
  OtherDragging:Boolean;
begin
  if csDesigning in ComponentState then
    Exit;
  OtherDragging := Mouse.IsDragging;
  if MouseOver and Enabled and (Control = nil) then
  begin
    NeedRepaint := not Transparent and
     ((FHotTrack and (FDragging or (Enabled and not OtherDragging))));
    inherited MouseLeave(Control); // set MouseOver

    if Sizeable then
      RestoreSizeableCursor;;

    if NeedRepaint then
      Repaint;
  end
  else
    inherited MouseLeave(Control);
end;

procedure TJvCustomArrangePanel.SetSizeableCursor;
begin
  if Screen.Cursor <> crSizeNWSE then
  begin
    FLastScreenCursor := Screen.Cursor;
    Screen.Cursor := crSizeNWSE;
  end;
end;

procedure TJvCustomArrangePanel.RestoreSizeableCursor;
begin
  if Screen.Cursor = crSizeNWSE then
    Screen.Cursor := FLastScreenCursor;
end;

function TJvCustomArrangePanel.GetControlSize(Control: TControl): TSize;
begin
  {$IFDEF COMPILER10_UP} // Delphi 2006+
  Result.cx := Control.Margins.ControlWidth;
  Result.cy := Control.Margins.ControlHeight;
  {$ELSE}
  Result.cx := Control.Width;
  Result.cy := Control.Height;
  {$ENDIF COMPILER10_UP}
end;

procedure TJvCustomArrangePanel.SetControlBounds(Control: TControl; const R: TRect);
begin
  {$IFDEF COMPILER10_UP} // Delphi 2006+
  Control.Margins.SetControlBounds(R);
  {$ELSE}
  Control.BoundsRect := R;
  {$ENDIF COMPILER10_UP}
end;

procedure TJvCustomArrangePanel.SetTransparent(const Value: Boolean);
begin
  if Value <> FTransparent then
  begin
    FTransparent := Value;
    RecreateWnd;
  end;
end;

procedure TJvCustomArrangePanel.SetFlatBorder(const Value: Boolean);
begin
  if Value <> FFlatBorder then
  begin
    FFlatBorder := Value;
    Invalidate;
  end;
end;

procedure TJvCustomArrangePanel.SetFlatBorderColor(const Value: TColor);
begin
  if Value <> FFlatBorderColor then
  begin
    FFlatBorderColor := Value;
    Invalidate;
  end;
end;

function TJvCustomArrangePanel.DoEraseBackground(Canvas: TCanvas; Param: LPARAM): Boolean;
begin
  // Mantis 3624: Draw our parent's image first if we are transparent.
  // This might not seem useful at first as we have removed the csOpaque
  // from our style and the API is doing the drawing just fine. But this
  // is required for other transparent controls placed on us. This way,
  // they call us with their own canvas into which we draw what we are
  // placed on. This way, there is an automatic chain of transparency up
  // to the controls at the bottom that are not transparent.
  if Transparent then
  begin
    CopyParentImage(Self, Canvas);
    Result := True;
  end
  else
    Result := inherited DoEraseBackground(Canvas, Param);
end;

procedure TJvCustomArrangePanel.SetMultiLine(const Value: Boolean);
begin
  if FMultiLine <> Value then
  begin
    FMultiLine := Value;
    Invalidate;
  end;
end;

procedure TJvCustomArrangePanel.TextChanged;
begin
  inherited TextChanged;
  Invalidate;
end;

procedure TJvCustomArrangePanel.SetSizeable(const Value: Boolean);
begin
  if FSizeable <> Value then
  begin
    if FDragging and FSizeable then
      MouseCapture := False;
    FSizeable := Value;
    Invalidate;
  end;
end;

procedure TJvCustomArrangePanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Sizeable and (Button = mbLeft) and ((Width - X) < 12) and ((Height - Y) < 12) then
  begin
    FDragging := True;
    FLastPos := Point(X, Y);
    MouseCapture := True;
    SetSizeableCursor;
  end
  else
    inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvCustomArrangePanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
  X1, Y1: Integer;
  Changed : Boolean;
begin
  if FDragging and Sizeable then
  begin
    R := BoundsRect;
    X1 := R.Right - R.Left + X - FLastPos.X;
    Y1 := R.Bottom - R.Top + Y - FLastPos.Y;
    if (X1 > 1) and (Y1 > 1) then
    begin
      if (Constraints.MinWidth > 0) and (X1 < Constraints.MinWidth) then
        X1 := Constraints.MinWidth;
      if (Constraints.MinHeight > 0) and (Y1 < Constraints.MinHeight) then
        Y1 := Constraints.MinHeight;
      if (Constraints.MaxWidth > 0) and (X1 > Constraints.MaxWidth) then
        X1 := Constraints.MaxWidth;
      if (Constraints.MaxHeight > 0) and (Y1 > Constraints.MaxHeight) then
        Y1 := Constraints.MaxHeight;
      Changed := False;
      if (X1 >= 0) and (X1 <> Width) then
      begin
        FLastPos.X := X;
        Changed:= True;
      end;
      if (Y1 >= 0) and (Y1 <> Height) then
      begin
        FLastPos.Y := Y;
        Changed := True;
      end;
      if Changed then
      begin
        SetBounds(Left, Top, X1, Y1);
        Refresh;
      end;
    end;
  end
  else
    inherited MouseMove(Shift, X, Y);
  if Sizeable then
  begin
    if ((Width - X) < 12) and ((Height - Y) < 12) then
      SetSizeableCursor
    else
      RestoreSizeableCursor;
  end;
end;

procedure TJvCustomArrangePanel.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if FDragging and Sizeable then
  begin
    FDragging := False;
    MouseCapture := False;
    RestoreSizeableCursor;
    Refresh;
  end
  else
    inherited MouseUp(Button, Shift, X, Y);
end;

procedure TJvCustomArrangePanel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if Transparent then
    Invalidate;
end;

procedure TJvCustomArrangePanel.Resize;
begin
  if Assigned(FArrangeSettings) then // (asn)
    if FArrangeSettings.AutoArrange then
      ArrangeControls;
  inherited Resize;
end;

procedure TJvCustomArrangePanel.EnableArrange;
begin
  EnableAlign;
  if FEnableArrangeCount > 0 then
    Dec(FEnableArrangeCount);
end;

procedure TJvCustomArrangePanel.DisableArrange;
begin
  Inc(FEnableArrangeCount);
  DisableAlign;
end;

function TJvCustomArrangePanel.ArrangeEnabled: Boolean;
begin
  Result := FEnableArrangeCount <= 0;
end;

procedure TJvCustomArrangePanel.Loaded;
begin
  inherited Loaded;
  if FArrangeSettings.AutoArrange then
    ArrangeControls;
end;

procedure TJvCustomArrangePanel.AlignControls(AControl: TControl; var Rect: TRect);
begin
  inherited AlignControls(AControl, Rect);
  if FArrangeSettings.AutoArrange then
    ArrangeControls;
end;

function TJvCustomArrangePanel.GetNextControlByTabOrder(ATabOrder: Integer): TWinControl;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to ControlCount - 1 do
    if Controls[I] is TWinControl then
      if TWinControl(Controls[I]).TabOrder = ATabOrder then
      begin
        Result := TWinControl(Controls[I]);
        Break;
      end;
end;

procedure TJvCustomArrangePanel.ArrangeControls;
type
  TControlRect = record
    Control: TControl;
    BoundsRect: TRect;
    LineBreak: Boolean;
  end;
var
  AktX, AktY, NewX, NewY, MaxY, NewMaxX: Integer;
  ControlMaxX, ControlMaxY: Integer;
  TmpWidth, TmpHeight: Integer;
  LastTabOrder: Integer;
  CurrControl: TWinControl;
  I: Integer;
  OldHeight, OldWidth: Integer;
  OffsetX, OffsetY: Integer;
  NumControlsPerLine: Integer;
  ControlRects: array of TControlRect;
  LineOffsets: array of Integer;
  LineCount, Len: Integer;
  ArrS: TJvArrangeSettings;
  ControlSize: TSize;
begin
  if not ArrangeEnabled or FArrangeControlActive or (ControlCount = 0) or
     ([csLoading, csReading] * ComponentState <> []) then
    Exit;
  FArrangeWidth := 0;
  FArrangeHeight := 0;
  FArrangeControlActive := True;
  ArrS := FArrangeSettings;
  try
    OldHeight := Height;
    OldWidth := Width;
    TmpHeight := Height;
    TmpWidth := Width;
    AktY := ArrS.BorderTop;
    AktX := ArrS.BorderLeft;
    LastTabOrder := -1;
    MaxY := -1;
    if (ArrS.AutoSize in [asWidth, asBoth]) then
      ControlMaxX := TmpWidth - 2 * ArrS.BorderLeft
    else
      ControlMaxX := -1;
    if (ArrS.AutoSize in [asHeight, asBoth]) then
      ControlMaxY := TmpHeight - 2 * ArrS.BorderTop
    else
      ControlMaxY := -1;

    SetLength(ControlRects, ControlCount);
    for I := 0 to ControlCount - 1 do
      if Controls[I] is TWinControl then
      begin
        if Controls[I] is TJvCustomArrangePanel then
          TJvCustomArrangePanel(Controls[I]).Rearrange;
        ControlSize := GetControlSize(Controls[I]);
        if (ControlSize.cx + 2 * ArrS.BorderLeft > TmpWidth) then
          TmpWidth := ControlSize.cx + 2 * ArrS.BorderLeft;
      end;

    if (TmpWidth > ArrS.MaxWidth) and (ArrS.MaxWidth > 0) then
      TmpWidth := ArrS.MaxWidth;
    CurrControl := GetNextControlByTabOrder(LastTabOrder + 1);
    I := 0;
    NumControlsPerLine := 0;
    LineCount := 0;
    while Assigned(CurrControl) do
    begin
      LastTabOrder := CurrControl.TabOrder;
      ControlRects[I].Control := nil;
      ControlRects[I].LineBreak := False;
      if CurrControl.Visible or
        ((csDesigning in ComponentState) and ArrS.ShowNotVisibleAtDesignTime) then
      begin
        ControlSize := GetControlSize(CurrControl);
        NewMaxX := AktX + ControlSize.cx + ArrS.DistanceHorizontal + ArrS.BorderLeft;
        if ((ArrS.MaxControlsPerLine > 0) and (NumControlsPerLine >= ArrS.MaxControlsPerLine)) or
           ((((NewMaxX > TmpWidth) and not (ArrS.AutoSize in [asWidth, asBoth])) or
            ((NewMaxX > ArrS.MaxWidth) and (ArrS.MaxWidth > 0))) and
           (AktX > ArrS.BorderLeft) and // Only Valid if there is one control in the current line
           ArrS.WrapControls) then
        begin
          AktX := ArrS.BorderLeft;
          AktY := AktY + MaxY + ArrS.DistanceVertical;
          MaxY := -1;
          NewX := AktX;
          NewY := AktY;
          NumControlsPerLine := 1;
          ControlRects[I].LineBreak := True;
          Inc(LineCount);
        end
        else
        begin
          NewX := AktX;
          NewY := AktY;
          Inc(NumControlsPerLine);
        end;
        AktX := AktX + ControlSize.cx;
        if AktX > ControlMaxX then
          ControlMaxX := AktX;
        AktX := AktX + ArrS.DistanceHorizontal;
        ControlRects[I].Control := CurrControl;
        ControlRects[I].BoundsRect := Rect(NewX, NewY, NewX + ControlSize.cx, NewY + ControlSize.cy);
        if CurrControl.Height > MaxY then
          MaxY := ControlSize.cy;
        ControlMaxY := AktY + MaxY;
      end;
      CurrControl := GetNextControlByTabOrder(LastTabOrder + 1);
      Inc(I);
    end;
    if (Length(ControlRects) > 0) and not ControlRects[High(ControlRects)].LineBreak then
      Inc(LineCount);

    { Vertical/Horizontal alignment }
    OffsetX := 0;
    OffsetY := 0;
    if not (ArrS.AutoSize in [asBoth, asHeight]) then
      case ArrS.VerticalAlignment of
        asVCenter:
          OffsetY := (ClientHeight - ControlMaxY) div 2;
        asBottom:
          OffsetY := ClientHeight - ControlMaxY;
      end;
    if not (ArrS.AutoSize in [asBoth, asWidth]) then
      case ArrS.HorizontalAlignment of
        asCenter:
          OffsetX := (ClientWidth - ControlMaxX) div 2;
        asRight:
          OffsetX := ClientWidth - ControlMaxX;
      end;

    { Calculate the horizontal line alignment }
    if Arrs.HorizontalAlignLines then
    begin
      SetLength(LineOffsets, LineCount);
      Len := Length(ControlRects);
      I := 0;
      LineCount := 0;
      while I < Len do
      begin
        { Skip unused slots }
        while (I < Len) and (ControlRects[I].Control = nil) do
          Inc(I);
        if I < Len then
        begin
          LineOffsets[LineCount] := ControlRects[I].BoundsRect.Left;
          { Find last control in the line }
          while (I + 1 < Len) and not ControlRects[I + 1].LineBreak do
            Inc(I);
          LineOffsets[LineCount] := (ControlMaxX - (ControlRects[I].BoundsRect.Right - LineOffsets[LineCount])) div 2;
          Inc(LineCount);
        end;
        Inc(I);
      end;
    end;

    { Apply the new BoundRects to the controls }
    LineCount := 0;
    for I := 0 to High(ControlRects) do
    begin
      if ControlRects[I].Control <> nil then
      begin
        OffsetRect(ControlRects[I].BoundsRect, OffsetX, OffsetY);
        if ArrS.HorizontalAlignLines then
        begin
          if ControlRects[I].LineBreak then
            Inc(LineCount);
          OffsetRect(ControlRects[I].BoundsRect, LineOffsets[LineCount], 0);
        end;
        SetControlBounds(ControlRects[I].Control, ControlRects[I].BoundsRect);
      end;
    end;

    { Adjust panel bounds }
    if not (csLoading in ComponentState) then
    begin
      if ArrS.AutoSize in [asWidth, asBoth] then
        if ControlMaxX >= 0 then
          if (ArrS.MaxWidth > 0) and (ControlMaxX >= ArrS.MaxWidth) then
            TmpWidth := ArrS.MaxWidth
          else
            TmpWidth := ControlMaxX + ArrS.BorderLeft
        else
          TmpWidth := 0;
      if ArrS.AutoSize in [asHeight, asBoth] then
        if ControlMaxY >= 0 then
          TmpHeight := ControlMaxY + ArrS.BorderTop
        else
          TmpHeight := 0;
      Width := TmpWidth;
      Height := TmpHeight;
    end;
    FArrangeWidth := ControlMaxX + 2 * ArrS.BorderLeft;
    FArrangeHeight := ControlMaxY + 2 * ArrS.BorderTop;
    if (OldWidth <> TmpWidth) or (OldHeight <> Height) then
      UpdateWindow(Handle);
  finally
    FArrangeControlActive := False;
  end;
end;

procedure TJvCustomArrangePanel.SetWidth(Value: Integer);
var
  Changed: Boolean;
begin
  Changed := inherited Width <> Value;
  inherited Width := Value;
  if Changed then
  begin
    if Assigned(FOnChangedWidth) then
      FOnChangedWidth (Self, Value);
    if Assigned(FOnResizeParent) then
      FOnResizeParent(Self, Left, Top, Value, Height)
    else
    if Parent is TJvCustomArrangePanel then
      TJvCustomArrangePanel(Parent).Rearrange;
  end;
end;

function TJvCustomArrangePanel.GetWidth: Integer;
begin
  Result := inherited Width;
end;

procedure TJvCustomArrangePanel.SetHeight(Value: Integer);
var
  Changed: Boolean;
begin
  Changed := inherited Height <> Value;
  inherited Height := Value;
  if Changed then
  begin
    if Assigned(FOnChangedHeight) then
      FOnChangedHeight (Self, Value);
    if Assigned(FOnResizeParent) then
      FOnResizeParent(Self, Left, Top, Width, Value)
    else
    if Parent is TJvCustomArrangePanel then
      TJvCustomArrangePanel(Parent).Rearrange;
  end;
end;

function TJvCustomArrangePanel.GetHeight: Integer;
begin
  Result := inherited Height;
end;

procedure TJvCustomArrangePanel.SetArrangeSettings(const Value:
    TJvArrangeSettings);
begin
  if (Value <> nil) and (Value <> FArrangeSettings) then
    FArrangeSettings.Assign(Value);
end;

function TJvCustomArrangePanel.GetHotTrack: Boolean;
begin
  Result := FHotTrack;
end;

function TJvCustomArrangePanel.GetHotTrackFont: TFont;
begin
  Result := FHotTrackFont;
end;

function TJvCustomArrangePanel.GetHotTrackFontOptions: TJvTrackFontOptions;
begin
  Result := FHotTrackFontOptions;
end;

function TJvCustomArrangePanel.GetHotTrackOptions: TJvHotTrackOptions;
begin
  Result := FHotTrackOptions;
end;

procedure TJvCustomArrangePanel.SetHotTrack(Value: Boolean);
begin
  FHotTrack := Value;
end;

procedure TJvCustomArrangePanel.SetHotTrackFont(Value: TFont);
begin
  if (FHotTrackFont<>Value) and (Value <> nil) then
    FHotTrackFont.Assign(Value);
end;

procedure TJvCustomArrangePanel.SetHotTrackFontOptions(Value: TJvTrackFontOptions);
begin
  if FHotTrackFontOptions <> Value then
  begin
    FHotTrackFontOptions := Value;
    UpdateTrackFont(HotTrackFont, Font, FHotTrackFontOptions);
  end;
end;

procedure TJvCustomArrangePanel.SetHotTrackOptions(Value: TJvHotTrackOptions);
begin
  if (FHotTrackOptions <> Value) and (Value <> nil) then
    FHotTrackOptions.Assign(Value);
end;

procedure TJvCustomArrangePanel.IJvHotTrack_Assign(
  Source: IJvHotTrack);
begin
  if (Source <> nil) and (IJvHotTrack(Self) <> Source) then
  begin
    HotTrack := Source.HotTrack;
    HotTrackFont :=Source.HotTrackFont;
    HotTrackFontOptions := Source.HotTrackFontOptions;
    HotTrackOptions := Source.HotTrackOptions;
  end;
end;

function TJvCustomArrangePanel.IsHotTrackFontStored: Boolean;
begin
  Result := IsHotTrackFontDfmStored(HotTrackFont, Font, HotTrackFontOptions);
end;

procedure TJvCustomArrangePanel.Rearrange;
begin
  if FArrangeSettings.AutoArrange and not (csLoading in ComponentState) then
    ArrangeControls;
end;

procedure TJvCustomArrangePanel.DoArrangeSettingsPropertyChanged(Sender: TObject;
  const PropName: string);
begin
  if SameText(PropName, 'AutoArrange') then
  begin
    if ArrangeSettings.AutoArrange then
      Rearrange;
  end
  else
  if SameText(PropName, 'AutoSize') then
  begin
    if ArrangeSettings.AutoSize <> asNone then
      Rearrange;
  end
  else //otherwise call Rearrange
    Rearrange;
end;

function TJvCustomArrangePanel.GetArrangeSettings: TJvArrangeSettings;
begin
  Result := fArrangeSettings;
end;

{ TJvPanel }

procedure TJvPanel.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  { For backward compatibility }
  FFilerTag := 'HotColor';
  Filer.DefineProperty(FFilerTag, ReadData, nil, False);
end;

procedure TJvPanel.ReadData(Reader: TReader);
var
  C: Integer;
begin
  if SameText(FFilerTag, 'HotColor') then
  begin
    if Reader.NextValue = vaIdent then
    begin
      if IdentToColor(Reader.ReadIdent, C) then
        HotTrackOptions.Color := C;
    end
    else
      HotTrackOptions.Color := Reader.ReadInteger;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

