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
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvStatusBar;

interface

uses
  SysUtils, Classes, Contnrs,
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms, ComCtrls, CommCtrl, StdActns,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QControls, QForms, QComCtrls, QStdActns,
  {$ENDIF VisualCLX}
  JVCLVer, JvExComCtrls;

type
  {$IFDEF COMPILER6_UP}
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
  {$ELSE}
  TJvStatusPanel = class(TStatusPanel)
  private
    FAboutJVCL: TJVCLAboutInfo;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;
  {$ENDIF COMPILER6_UP}

  TJvStatusBar = class(TJvExStatusBar)
  private
    FAutoHintShown: Boolean;
    FHiddenControls: array of TControl;
    {$IFDEF VCL}
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    {$ENDIF VCL}
  protected
    procedure DoBoundsChanged; override;
    {$IFDEF VisualCLX}
    procedure Paint; override;
    {$ENDIF VisualCLX}
    {$IFDEF VCL}
    procedure CreateParams(var Params: TCreateParams); override;
    {$ENDIF VCL}
    {$IFDEF COMPILER6_UP}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MovePanelControls;
    function GetPanelClass: TStatusPanelClass; override;
    {$ENDIF COMPILER6_UP}
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

implementation

uses
  Math,
  JvResources, JvTypes;

//=== TJvStatusBar ===========================================================

constructor TJvStatusBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
end;

procedure TJvStatusBar.DoBoundsChanged;
begin
  inherited DoBoundsChanged;
  Realign;
  {$IFDEF COMPILER6_UP}
  MovePanelControls;
  {$ENDIF COMPILER6_UP}
  Invalidate; //Force full redraw, cause it's a lot buggy on XP without that!!!
end;

{$IFDEF VCL}

procedure TJvStatusBar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
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

{$ENDIF VCL}

{$IFDEF VisualCLX}

procedure TJvStatusBar.Paint;
begin
  {if FAutoHintShown then
    DefaultHandler(Msg) // VisualCLX has no DefaultHandler
  else}
    inherited Paint;
end;

{$ENDIF VisualCLX}

function TJvStatusBar.ExecuteAction(Action: TBasicAction): Boolean;
var
  HintText: string;
  {$IFDEF VCL}
  PanelEdges: Integer;
  Flags: DWORD;
  {$ENDIF VCL}

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
      {$IFDEF VCL}
      PanelEdges := -1;
      Flags := SBT_NOBORDERS;
      if UseRightToLeftReading then
        Flags := Flags or SBT_RTLREADING;
      SendMessage(Handle, SB_SETPARTS, 1, Integer(@PanelEdges));
      SendMessage(Handle, SB_SETTEXT, Flags, LPARAM(PChar(HintText)));
      {$ENDIF VCL}
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

{$IFDEF COMPILER6_UP}

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

{$ENDIF COMPILER6_UP}

//=== TJvStatusPanel =========================================================

{$IFDEF COMPILER6_UP}

type
  // access protected properties and methods
  THackStatusPanels = class(TStatusPanels);

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
  (THackStatusPanels(Collection).GetOwner as TJvStatusBar).MovePanelControls;
end;

procedure TJvStatusPanel.SetControl(const Value: TControl);
var
  S: TJvStatusBar;
begin
  S := THackStatusPanels(Collection).Owner as TJvStatusBar;
  if FControl <> nil then
    FControl.RemoveFreeNotification(S);
  FControl := Value;
  if FControl <> nil then
  begin
    if FControl = S then
    begin
      FControl := nil; // discard new control
      raise EJVCLException.Create(RsEInvalidControlSelection);
    end;
    FControl.Parent := S;
    FControl.Height := S.ClientHeight - 4;
    FControl.FreeNotification(S);
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

{$ENDIF COMPILER6_UP}

end.

