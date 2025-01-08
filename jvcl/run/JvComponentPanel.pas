{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvComponentPanel.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):
  Andreas Hausladen

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

components : TJvComponentPanel
description: Component panel for GUI developers

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvComponentPanel;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages,
  Classes, Controls, Buttons, Forms,
  JvButtons, JvExtComponent, JvExButtons;

type
  TButtonClick = procedure(Sender: TObject; Button: Integer) of object;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvComponentPanel = class(TJvCustomPanel)
  private
    FButtonWidth: Integer;
    FButtonHeight: Integer;
    FButtons: TList;
    FOnClick: TButtonClick;
    FOnDblClick: TButtonClick;
    FButtonPointer: TJvExSpeedButton;
    FButtonLeft: TJvNoFrameButton;
    FButtonRight: TJvNoFrameButton;
    FFirstVisible: Integer;
    FLockUpdate: Integer;
    FSelectButton: TJvExSpeedButton;
    function GetButton(Index: Integer): TJvExSpeedButton;
    function GetButtonCount: Integer;
    procedure SetButtonCount(AButtonCount: Integer);
    procedure SetButtonWidth(AButtonWidth: Integer);
    procedure SetButtonHeight(AButtonHeight: Integer);
    procedure SetFirstVisible(AButton: Integer);
    procedure BtnClick(Sender: TObject);
    procedure BtnDblClick(Sender: TObject);
    procedure MoveClick(Sender: TObject);
    function GetVisibleCount: Integer;
    procedure SetSelectedButton(Value: Integer);
    function GetSelectedButton: Integer;
    procedure WMSetText(var Msg: TWMSetText); message WM_SETTEXT;
  protected
    procedure Resize; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RecreateButtons;
    procedure SetMainButton;
    procedure Invalidate; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    property Buttons[Index: Integer]: TJvExSpeedButton read GetButton; default;
    property FirstVisible: Integer read FFirstVisible write SetFirstVisible;
    property ButtonLeft: TJvNoFrameButton read FButtonLeft;
    property ButtonRight: TJvNoFrameButton read FButtonRight;
    property VisibleCount: Integer read GetVisibleCount;
    property SelectedButton: Integer read GetSelectedButton write SetSelectedButton;
  published
    property Align;
    property OnClick: TButtonClick read FOnClick write FOnClick;
    property OnDblClick: TButtonClick read FOnDblClick write FOnDblClick;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth default 28;
    property ButtonHeight: Integer read FButtonHeight write SetButtonHeight default 28;
    property ButtonCount: Integer read GetButtonCount write SetButtonCount default 0;
    property Anchors;
    property Constraints;
    property AutoSize;
    property BiDiMode;
    property UseDockManager default True;
    property DockSite;
    property ParentBiDiMode;
    property DragKind;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnUnDock;
    property OnCanResize;
    property OnConstrainedResize;
    property OnPaintContent;
    property PopupMenu;
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

{$R JvComponentPanel.res}

constructor TJvComponentPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  BevelOuter := bvNone;
  FButtons := TList.Create;
  FFirstVisible := 0;
  FButtonWidth := 28;
  FButtonHeight := 28;
  FButtonLeft := TJvNoFrameButton.Create(Self);
  FButtonLeft.RepeatedClick := True;
  FButtonRight := TJvNoFrameButton.Create(Self);
  FButtonRight.RepeatedClick := True;
  FButtonPointer := TJvExSpeedButton.Create(Self);
  with FButtonLeft do
  begin
    Parent := Self;
    Tag := 0;
    Width := 12;
    Top := 0;
    Glyph.LoadFromResourceName(HInstance, 'JvComponentPanelLEFT');
    NumGlyphs := 2;
    OnClick := MoveClick;
  end;
  with FButtonRight do
  begin
    Parent := Self;
    Tag := 1;
    Width := 12;
    Top := 0;
    Glyph.LoadFromResourceName(HInstance, 'JvComponentPanelRIGHT');
    NumGlyphs := 2;
    OnClick := MoveClick;
  end;
  with FButtonPointer do
  begin
    Flat := True;
    Parent := Self;
    Top := 0;
    Glyph.LoadFromResourceName(HInstance, 'JvComponentPanelPOINTER');
    GroupIndex := 1;
    OnClick := BtnClick;
  end;
  SetMainButton;
end;

destructor TJvComponentPanel.Destroy;
var
  I: Integer;
begin
  for I := 0 to FButtons.Count - 1 do
    TSpeedButton(FButtons[I]).Free;
  FButtons.Free;
  inherited Destroy;
end;

procedure TJvComponentPanel.Invalidate;
begin
  if FLockUpdate = 0 then
    inherited Invalidate;
end;

procedure TJvComponentPanel.RecreateButtons;
var
  I: Integer;
  TmpNum: Integer;
begin
  TmpNum := FButtons.Count;
  for I := 0 to FButtons.Count - 1 do
    TSpeedButton(FButtons[I]).Free;
  FButtons.Clear;
  FFirstVisible := 0;
  ButtonCount := TmpNum;
end;

procedure TJvComponentPanel.SetMainButton;
begin
  FButtonPointer.Down := True;
  FSelectButton := FButtonPointer;
end;

procedure TJvComponentPanel.SetSelectedButton(Value: Integer);
begin
  if (Value <> GetSelectedButton) and (Value >= -1) and (Value < ButtonCount) then
  begin
    if Value = -1 then
      SetMainButton
    else
    begin
      FSelectButton := Buttons[Value];
      FSelectButton.Down := True;
    end;
  end;
end;

function TJvComponentPanel.GetSelectedButton: Integer;
begin
  if FSelectButton <> nil then
  begin
    for Result := 0 to ButtonCount - 1 do
      if Buttons[Result] = FSelectButton then
        Exit;
  end;
  Result := -1;
end;

function TJvComponentPanel.GetButton(Index: Integer): TJvExSpeedButton;
begin
  if (Index < 0) or (Index > FButtons.Count - 1) then
    Result := nil
  else
    Result := TJvExSpeedButton(FButtons[Index]);
end;

function TJvComponentPanel.GetButtonCount: Integer;
begin
  Result := FButtons.Count;
end;

function TJvComponentPanel.GetVisibleCount: Integer;
begin
  Result := (Width - (12 + 12 + FButtonWidth)) div FButtonWidth;
end;

procedure TJvComponentPanel.SetButtonCount(AButtonCount: Integer);
var
  TmpButton: TJvExSpeedButton;
begin
  if AButtonCount < 0 then
    Exit;
  BeginUpdate;
  try
    SetMainButton;
    while FButtons.Count > AButtonCount do
    begin
      TSpeedButton(FButtons[FButtons.Count - 1]).Free;
      FButtons.Delete(FButtons.Count - 1);
    end;
    while FButtons.Count < AButtonCount do
    begin
      TmpButton := TJvExSpeedButton.Create(Self);
      with TmpButton do
      begin
        Flat := True;
        Top := 0;
        GroupIndex := 1;
        HintWindowClass := Self.HintWindowClass;
        Parent := Self;
        OnClick := BtnClick;
        OnDblClick := BtnDblClick;
      end;
      FButtons.Add(TmpButton);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TJvComponentPanel.SetButtonWidth(AButtonWidth: Integer);
begin
  if FButtonWidth <> AButtonWidth then
  begin
    FButtonWidth := AButtonWidth;
    Resize;
  end;
end;

procedure TJvComponentPanel.SetButtonHeight(AButtonHeight: Integer);
begin
  if FButtonHeight <> AButtonHeight then
  begin
    FButtonHeight := AButtonHeight;
    Resize;
  end;
end;

procedure TJvComponentPanel.MoveClick(Sender: TObject);
begin
  case TSpeedButton(Sender).Tag of
    0:
      if FFirstVisible > 0 then
        Dec(FFirstVisible);
    1:
      if FButtons.Count > FFirstVisible + VisibleCount then
        Inc(FFirstVisible);
  end;
  Resize;
end;

procedure TJvComponentPanel.BtnClick(Sender: TObject);
begin
  if FSelectButton <> Sender then
  begin
    FSelectButton := TJvExSpeedButton(Sender);
    if Assigned(FOnClick) then
      FOnClick(Sender, FButtons.IndexOf(FSelectButton));
  end;
end;

procedure TJvComponentPanel.BtnDblClick(Sender: TObject);
begin
  if Assigned(FOnDblClick) then
    FOnDblClick(Sender, FButtons.IndexOf(Sender));
end;


procedure TJvComponentPanel.WMSetText(var Msg: TWMSetText);
begin
  inherited;
  Caption := '';
end;




procedure TJvComponentPanel.Resize;
var
  I: Integer;
begin
  Height := FButtonHeight;
  if FButtonPointer = nil then
    Exit; // asn: for visualclx
  DisableAlign;
  try
    FButtonPointer.Height := FButtonHeight;
    FButtonPointer.Width := FButtonWidth;
    FButtonLeft.Height := FButtonHeight;
    FButtonRight.Height := FButtonHeight;
    FButtonPointer.Left := 0;
    FButtonLeft.Left := FButtonWidth + 6;
    FButtonRight.Left := (FButtonWidth + 12 + 6) + VisibleCount * FButtonWidth;
    FButtonLeft.Enabled := FFirstVisible > 0;
    FButtonRight.Enabled := FButtons.Count > FFirstVisible + VisibleCount;
    for I := 0 to FButtons.Count - 1 do
    begin
      if (I >= FFirstVisible) and (I < FFirstVisible + VisibleCount) then
        TSpeedButton(FButtons[I]).SetBounds((FButtonWidth + 12 + 6) + (I - FFirstVisible) * FButtonWidth, 0, FButtonWidth, FButtonHeight)
      else
        TSpeedButton(FButtons[I]).SetBounds(-100, 0, FButtonWidth, FButtonHeight);
    end;
  finally
    ControlState := ControlState - [csAlignmentNeeded];
    EnableAlign;
  end;
end;

function TJvComponentPanel.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result then
  begin
    Result := True;

    WheelDelta := WheelDelta div WHEEL_DELTA;
    while WheelDelta <> 0 do
    begin
      if WheelDelta < 0 then
      begin
        if ButtonRight.Enabled then
          ButtonRight.Click
        else
          Break;
      end
      else
      begin
        if ButtonLeft.Enabled then
          ButtonLeft.Click
        else
          Break;
      end;

      if WheelDelta < 0 then
        Inc(WheelDelta)
      else
        Dec(WheelDelta);
    end;
  end;
end;

procedure TJvComponentPanel.SetFirstVisible(AButton: Integer);
begin
  if AButton >= ButtonCount then
    AButton := ButtonCount - 1;
  if AButton < 0 then
    AButton := 0;
  if FFirstVisible <> AButton then
  begin
    FFirstVisible := AButton;
    Resize;
  end;
end;

procedure TJvComponentPanel.BeginUpdate;
begin
  Inc(FLockUpdate);
  DisableAlign;
end;

procedure TJvComponentPanel.EndUpdate;
begin
  Dec(FLockUpdate);
  if FLockUpdate = 0 then
  begin
    Resize;
    ControlState := ControlState - [csAlignmentNeeded];
    EnableAlign;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.