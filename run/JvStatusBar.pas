{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvStatusBar2.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvStatusBar;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ComCtrls, Forms,
  JvTypes, StdActns, CommCtrl, JVCLVer, Contnrs;

type
  TJvStatusPanel = class(TStatusPanel)
  {$IFDEF COMPILER6_UP}
  private
    FControl: TControl;
    FMarginLeft: integer;
    FMarginTop: integer;
    procedure SetControl(const Value: TControl);
    procedure SetMarginLeft(const Value: integer);
    procedure SetMarginTop(const Value: integer);
    procedure Changed(AllItems:boolean);
  public
    constructor Create(Collection: TCollection); override;
  published
    property Control:TControl read FControl write SetControl;
    property MarginLeft:integer read FMarginLeft write SetMarginLeft default 3;
    property MarginTop:integer read FMarginTop write SetMarginTop default 3;
  end;
  {$ELSE}
  ;
  {$ENDIF}
  // access protected properties and methods
  TJvStatusPanels = class(TStatusPanels);

  TJvStatusBar = class(TStatusBar)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FHintColor: TColor;
    FSaved: TColor;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FAutoHintShown: Boolean;
    FHiddenControls: array of TControl;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure MouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure WMSize(var Msg: TMessage); message WM_SIZE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
   {$IFDEF COMPILER6_UP}
    procedure MovePanelControls;
    function GetPanelClass: TStatusPanelClass; override;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    property AutoHintShown: Boolean read FAutoHintShown;
  published
    property Color;
    property Font;
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  end;

implementation

uses
  Math,
  JvConsts, JvResources;
  
constructor TJvStatusBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHintColor := clInfoBk;
  ControlStyle := ControlStyle + [csAcceptsControls];
end;

procedure TJvStatusBar.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
    WindowClass.Style := WindowClass.Style and not CS_HREDRAW;
end;

procedure TJvStatusBar.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

procedure TJvStatusBar.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvStatusBar.WMSize(var Msg: TMessage);
begin
  inherited;
  Realign;
  {$IFDEF COMPILER6_UP}
  MovePanelControls;
  {$ENDIF}
  Invalidate; //Force full redraw, cause it's a lot buggy on XP without that!!!
end;

procedure TJvStatusBar.MouseEnter(var Msg: TMessage);
begin
  FSaved := Application.HintColor;
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  Application.HintColor := FHintColor;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvStatusBar.MouseLeave(var Msg: TMessage);
begin
  Application.HintColor := FSaved;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
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
      SendMessage(Handle, SB_SETPARTS, 1, Integer(@PanelEdges));
      SendMessage(Handle, SB_SETTEXT, Flags, LPARAM(PChar(HintText)));
    end;
    Result := True;
  end
  else
  begin
    CancelAutoHintShown;
    Result := inherited ExecuteAction(Action);
  end;
end;

procedure TJvStatusBar.WMPaint(var Msg: TWMPaint);
begin
  if FAutoHintShown then
    DefaultHandler(Msg)
  else
    inherited;
end;

procedure TJvStatusBar.Notification(AComponent: TComponent; Operation: TOperation);
{$IFDEF COMPILER6_UP}
var i:integer;
{$ENDIF}
begin
  {$IFDEF COMPILER6_UP}
  inherited;
  if Operation = opRemove then
    for i := 0 to Panels.Count - 1 do
      if TJvStatusPanel(Panels[i]).Control = AComponent then
        TJvStatusPanel(Panels[i]).Control := nil;
  {$ENDIF}
end;

{$IFDEF COMPILER6_UP}
procedure TJvStatusBar.MovePanelControls;
var i, ALeft:integer;
  tmpJvPanel : TJvStatusPanel;
begin
  ALeft := 0;
  for i := 0 to Panels.Count - 1 do
  begin
    tmpJvPanel := TJvStatusPanel(Panels[i]);
    if tmpJvPanel.Control <> nil then
      with tmpJvPanel do
        Control.SetBounds(ALeft + MarginLeft,MarginTop,Control.Width, Control.Height);
    Inc(ALeft,TJvStatusPanel(Panels[i]).Width);
  end;
end;

function TJvStatusBar.GetPanelClass: TStatusPanelClass;
begin
  Result := TJvStatusPanel;
end;
{$ENDIF}

{ TJvStatusPanel }

{$IFDEF COMPILER6_UP}
procedure TJvStatusPanel.Changed(AllItems: boolean);
begin
  inherited Changed(AllItems);
  (TJvStatusPanels(Collection).GetOwner as TJvStatusBar).MovePanelControls;
end;

constructor TJvStatusPanel.Create(Collection: TCollection);
begin
  inherited;
  FMarginLeft := 3;
  FMarginTop := 3;
end;

procedure TJvStatusPanel.SetControl(const Value: TControl);
var S:TJvStatusBar;
begin
    S := TJvStatusPanels(Collection).Owner as TJvStatusBar;
    if FControl <> nil then
      FControl.RemoveFreeNotification(S);
    FControl := Value;
    if FControl <> nil then
    begin
      if FControl = S then
        raise Exception.Create(sInvalidControlSelection);
      FControl.Parent := S;
      FControl.Height := S.ClientHeight - 4;
      FControl.FreeNotification(S);
    end;
    Changed(false);
end;

procedure TJvStatusPanel.SetMarginLeft(const Value: integer);
begin
  if FMarginLeft <> Value then
  begin
    FMarginLeft := Value;
    Changed(false);
  end;
end;

procedure TJvStatusPanel.SetMarginTop(const Value: integer);
begin
  if FMarginTop <> Value then
  begin
    FMarginTop := Value;
    Changed(false);
  end;
end;
{$ENDIF}
end.

