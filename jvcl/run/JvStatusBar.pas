{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvStatusBar2.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvStatusBar;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages,
  CommCtrl,
  SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, StdActns,
  JVCLVer, JvExComCtrls;

type
  TJvStatusPanel = class(TStatusPanel)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FControl: TControl;
    FMarginLeft: Integer;
    FMarginTop: Integer;
    procedure SetControl(const Value: TControl);
    procedure SetMarginLeft(const Value: Integer);
    procedure SetMarginTop(const Value: Integer);
    procedure Changed(AllItems: Boolean);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Control: TControl read FControl write SetControl;
    property MarginLeft: Integer read FMarginLeft write SetMarginLeft default 3;
    property MarginTop: Integer read FMarginTop write SetMarginTop default 3;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvStatusBar = class(TJvExStatusBar)
  private
    FAutoHintShown: Boolean;
    FHiddenControls: array of TControl;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
  protected
    procedure BoundsChanged; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MovePanelControls;
    function GetPanelClass: TStatusPanelClass;  override;
    procedure SBSetParts(var msg: TMessage); message SB_SETPARTS;
    {$IFDEF COMPILER16}
    procedure WndProc(var Msg: TMessage); override;
    {$ENDIF COMPILER16}
  public
    constructor Create(AOwner: TComponent); override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    property AutoHintShown: Boolean read FAutoHintShown;
  published
    property Color;
    property Font;
    property HintColor;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
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
  JvThemes, JvResources, JvTypes, JvJVCLUtils;

//=== { TJvStatusBar } =======================================================

constructor TJvStatusBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
end;

procedure TJvStatusBar.BoundsChanged;
begin
  inherited BoundsChanged;
  Realign;
  MovePanelControls;
end;



procedure TJvStatusBar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  {$IFDEF JVCLThemesEnabled}
  if not StyleServices.Enabled then
  {$ENDIF JVCLThemesEnabled}
    with Params do
      WindowClass.Style := WindowClass.Style and not CS_HREDRAW;
end;

procedure TJvStatusBar.WMPaint(var Msg: TWMPaint);
begin
  if FAutoHintShown then
    DefaultHandler(Msg)
  else
    inherited;
end;





function TJvStatusBar.ExecuteAction(Action: TBasicAction): Boolean;
var
  HintText: string;
  PanelEdges: Integer;
  Flags: DWORD;

  procedure CancelAutoHintShown;
  var
    I: Integer;
  begin
    if FAutoHintShown then
    begin
      Panels.EndUpdate;
      for I := 0 to Length(FHiddenControls) - 1 do
        FHiddenControls[I].Visible := True;
      FHiddenControls := nil;
      FAutoHintShown := False;
    end;
  end;

  procedure SetAutoHintShown;
  var
    I: Integer;
  begin
    if not FAutoHintShown then
    begin
      Panels.BeginUpdate;
      FHiddenControls := nil;
      for I := 0 to ControlCount - 1 do
        if Controls[I].Visible then
        begin
          SetLength(FHiddenControls, Length(FHiddenControls) + 1);
          FHiddenControls[Length(FHiddenControls) - 1] := Controls[I];
          FHiddenControls[I].Visible := False;
        end;
      FAutoHintShown := True;
    end;
  end;

begin
  if AutoHint and (Action is THintAction) and not DoHint then
  begin
    HintText := Trim(THintAction(Action).Hint);
    if Length(HintText) = 0 then
      CancelAutoHintShown
    else
    begin
      SetAutoHintShown;
      PanelEdges := -1;
      Flags := SBT_NOBORDERS;
      if UseRightToLeftReading then
        Flags := Flags or SBT_RTLREADING;
      SendMessage(Handle, SB_SETPARTS, 1, LPARAM(@PanelEdges));
      SendMessage(Handle, SB_SETTEXT, Flags, LPARAM(PChar(HintText)));
      // (rom) may need VisualCLX part here
    end;
    Result := True;
  end
  else
  begin
    CancelAutoHintShown;
    Result := inherited ExecuteAction(Action);
  end;
end;

procedure TJvStatusBar.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and not (csDestroying in ComponentState) then
    for I := 0 to Panels.Count - 1 do
    begin
      if TJvStatusPanel(Panels[I]).Control = AComponent then
        TJvStatusPanel(Panels[I]).Control := nil;
    end;
end;

procedure TJvStatusBar.MovePanelControls;
var
  I, ALeft: Integer;
  TmpPanel: TJvStatusPanel;
begin
  ALeft := 0;
  for I := 0 to Panels.Count - 1 do
  begin
    TmpPanel := TJvStatusPanel(Panels[I]);
    if TmpPanel.Control <> nil then
      with TmpPanel do
        Control.SetBounds(ALeft + MarginLeft, MarginTop, Control.Width, Control.Height);
    Inc(ALeft, TJvStatusPanel(Panels[I]).Width);
  end;
end;

function TJvStatusBar.GetPanelClass: TStatusPanelClass;
begin
  Result := TJvStatusPanel;
end;

procedure TJvStatusBar.SBSetParts(var msg: TMessage);
begin
  inherited;
  MovePanelControls;
end;

{$IFDEF COMPILER16}
procedure TJvStatusBar.WndProc(var Msg: TMessage);
var
  DC, PaintDC: HDC;
  Buffer: TBitmap;
  PS: TPaintStruct;
begin
  // TStatusBarStyleHook.Paint catches all WM_PAINT but doesn't call Control.PaintControls()
  // what causes TGraphicControls to not be painted. With this code we call the PaintControls
  // function in that case.
  // This bug was fixed with XE3
  if (Msg.Msg = WM_PAINT) and StyleServices.Enabled and not StyleServices.IsSystemStyle then
  begin
    DC := HDC(Msg.WParam);
    if DoubleBuffered and (DC = 0) then
    begin
      PaintDC := BeginPaint(Handle, PS);
      try
        Buffer := TBitmap.Create;
        try
          Buffer.SetSize(Width, Height);
          Msg.WParam := WPARAM(Buffer.Canvas.Handle);
          inherited WndProc(Msg);
          Msg.WParam := WPARAM(DC);
          PaintControls(Buffer.Canvas.Handle, nil);
          BitBlt(PaintDC, 0, 0, Buffer.Width, Buffer.Height, Buffer.Canvas.Handle, 0, 0, SRCCOPY);
        finally
          Buffer.Free;
        end;
      finally
        EndPaint(Handle, PS);
      end;
    end
    else
    begin
      if DC <> 0 then
        PaintDC := DC
      else
        PaintDC := BeginPaint(Handle, PS);
      try
        Msg.WParam := WPARAM(PaintDC);
        inherited WndProc(Msg);
        Msg.WParam := WPARAM(DC);
        PaintControls(PaintDC, nil);
      finally
        if DC = 0 then
          EndPaint(Handle, PS);
      end;
    end;
  end
  else
    inherited WndProc(Msg);
end;
{$ENDIF COMPILER16}

//=== { TJvStatusPanel } =====================================================

type
  TStatusPanelsAccessProtected = class(TStatusPanels);

constructor TJvStatusPanel.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FMarginLeft := 3;
  FMarginTop := 3;
end;

destructor TJvStatusPanel.Destroy;
begin
  Control := nil;
  inherited Destroy;
end;

procedure TJvStatusPanel.Changed(AllItems: Boolean);
begin
  inherited Changed(AllItems);
  (TStatusPanelsAccessProtected(Collection).GetOwner as TJvStatusBar).MovePanelControls;
end;

procedure TJvStatusPanel.SetControl(const Value: TControl);
var
  S: TJvStatusBar;
begin
  S := TStatusPanelsAccessProtected(Collection).Owner as TJvStatusBar;
  ReplaceComponentReference(S, Value, TComponent(FControl));
  if FControl <> nil then
  begin
    if FControl = S then
    begin
      FControl := nil; // discard new control
      raise EJVCLException.CreateRes(@RsEInvalidControlSelection);
    end;
    FControl.Parent := S;
    FControl.Height := S.ClientHeight - 4;
  end;
  Changed(False);
end;

procedure TJvStatusPanel.SetMarginLeft(const Value: Integer);
begin
  if FMarginLeft <> Value then
  begin
    FMarginLeft := Value;
    Changed(False);
  end;
end;

procedure TJvStatusPanel.SetMarginTop(const Value: Integer);
begin
  if FMarginTop <> Value then
  begin
    FMarginTop := Value;
    Changed(False);
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
