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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

components : TJvComponentPanel
description: Component panel for GUI developers

Known Issues:
  Some russian comments were translated to english; these comments are marked
  with [translated]
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvComponentPanel;

interface

uses
  {$IFDEF VCL}
  Messages,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Types, QTypes,
  {$ENDIF VisualCLX}
  Classes, Buttons,
  JvButtons, JvComponent;

type
  TButtonClick = procedure(Sender: TObject; Button: Integer) of object;

  TJvComponentPanel = class(TJvCustomPanel)
  private
    FButtonWidth: Integer;
    FButtonHeight: Integer;
    FButtons: TList;
    FOnClick: TButtonClick;
    FOnDblClick: TButtonClick;
    FButtonPointer: TSpeedButton;
    FButtonLeft: TJvNoFrameButton;
    FButtonRight: TJvNoFrameButton;
    FFirstVisible: Integer;
    FLockUpdate: Integer;
    FSelectButton: TSpeedButton;
    function GetButton(Index: Integer): TSpeedButton;
    function GetButtonCount: Integer;
    procedure SetButtonCount(AButtonCount: Integer);
    procedure SetButtonWidth(AButtonWidth: Integer);
    procedure SetButtonHeight(AButtonHeight: Integer);
    procedure SetFirstVisible(AButton: Integer);
    procedure BtnClick(Sender: TObject);
    procedure BtnDblClick(Sender: TObject);
    procedure MoveClick(Sender: TObject);
    {$IFDEF VCL}
    procedure WMSetText(var Msg: TWMSetText); message WM_SETTEXT;
    {$ENDIF VCL}
  protected
    {$IFDEF VisualCLX}
    procedure SetText(const Value: TCaption); override;
    {$ENDIF VisualCLX}
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RecreateButtons;
    procedure SetMainButton;
    procedure Invalidate; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    property Buttons[Index: Integer]: TSpeedButton read GetButton; default;
    property FirstVisible: Integer read FFirstVisible write SetFirstVisible;
  published
    property Align;
    property OnClick: TButtonClick read FOnClick write FOnClick;
    property OnDblClick: TButtonClick read FOnDblClick write FOnDblClick;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth default 28;
    property ButtonHeight: Integer read FButtonHeight write SetButtonHeight default 28;
    property ButtonCount: Integer read GetButtonCount write SetButtonCount default 0;
    property Anchors;
    property Constraints;
    {$IFDEF VCL}
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
    {$ENDIF VCL}
    property OnConstrainedResize;
  end;

implementation

uses
  Controls;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvComponentPanel.res}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvComponentPanel.res}
{$ENDIF LINUX}

constructor TJvComponentPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelOuter := bvNone;
  FButtons := TList.Create;
  FFirstVisible := 0;
  FButtonWidth := 28;
  FButtonHeight := 28;
  FButtonLeft := TJvNoFrameButton.Create(Self);
  FButtonRight := TJvNoFrameButton.Create(Self);
  FButtonPointer := TSpeedButton.Create(Self);
  with FButtonLeft do
  begin
    Parent := Self;
    Tag := 0;
    Width := 12;
    Top := 0;
    Glyph.LoadFromResourceName(HInstance, 'RACPLEFT');
    NumGlyphs := 2;
    OnClick := MoveClick;
  end;
  with FButtonRight do
  begin
    Parent := Self;
    Tag := 1;
    Width := 12;
    Top := 0;
    Glyph.LoadFromResourceName(HInstance, 'RACPRIGHT');
    NumGlyphs := 2;
    OnClick := MoveClick;
  end;
  with FButtonPointer do
  begin
    Flat := True;
    Parent := Self;
    Top := 0;
    Glyph.LoadFromResourceName(HInstance, 'RACPPOINTER');
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

function TJvComponentPanel.GetButton(Index: Integer): TSpeedButton;
begin
  if (Index < 0) or (Index > FButtons.Count - 1) then
    Result := nil
  else
    Result := TSpeedButton(FButtons[Index]);
end;

function TJvComponentPanel.GetButtonCount: Integer;
begin
  Result := FButtons.Count;
end;

procedure TJvComponentPanel.SetButtonCount(AButtonCount: Integer);
var
  TmpButton: TSpeedButton;
begin
  // (rom) removed the exception and the limit of 100 buttons
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
      TmpButton := TSpeedButton.Create(Self);
      with TmpButton do
      begin
        Flat := True;
        Parent := Self;
        Top := 0;
        GroupIndex := 1;
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
      Dec(FFirstVisible);
    1:
      Inc(FFirstVisible);
  end;
  Resize;
end;

procedure TJvComponentPanel.BtnClick(Sender: TObject);
begin
  if FSelectButton <> Sender then
  begin
    FSelectButton := TSpeedButton(Sender);
    if Assigned(FOnClick) then
      FOnClick(Sender, FButtons.IndexOf(FSelectButton));
  end;
end;

procedure TJvComponentPanel.BtnDblClick(Sender: TObject);
begin
  if Assigned(FOnDblClick) then
    FOnDblClick(Sender, FButtons.IndexOf(Sender));
end;

{$IFDEF VCL}
procedure TJvComponentPanel.WMSetText(var Msg: TWMSetText);
begin
  inherited;
  Caption := '';
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}
procedure TJvComponentPanel.SetText(const Value: TCaption);
begin
  inherited SetText('');
  //Caption := '';
end;
{$ENDIF VisualCLX}

procedure TJvComponentPanel.Resize;
var
  VisibleCount: Integer;
  I: Integer;
begin
  Height := FButtonHeight;
  if FButtonPointer = nil then
    Exit; // asn: for visualclx
  FButtonPointer.Height := FButtonHeight;
  FButtonPointer.Width := FButtonWidth;
  FButtonLeft.Height := FButtonHeight;
  FButtonRight.Height := FButtonHeight;
  VisibleCount := (Width - (12 + 12 + FButtonWidth)) div FButtonWidth;
  FButtonPointer.Left := 0;
  FButtonLeft.Left := FButtonWidth + 6;
  FButtonRight.Left := (FButtonWidth + 12 + 6) + VisibleCount * FButtonWidth;
  if FFirstVisible = 0 then
    FButtonLeft.Enabled := False
  else
    FButtonLeft.Enabled := True;
  if FButtons.Count > FFirstVisible + VisibleCount then
    FButtonRight.Enabled := True
  else
    FButtonRight.Enabled := False;
  for I := 0 to FButtons.Count - 1 do
  begin
    TSpeedButton(FButtons[I]).Width := FButtonWidth;
    TSpeedButton(FButtons[I]).Height := FButtonHeight;
    if (I >= FFirstVisible) and (I < FFirstVisible + VisibleCount) then
      TSpeedButton(FButtons[I]).Left := (FButtonWidth + 12 + 6) + (I - FFirstVisible) * FButtonWidth
    else
      TSpeedButton(FButtons[I]).Left := -100;
  end;
end;

procedure TJvComponentPanel.SetFirstVisible(AButton: Integer);
begin
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
    EnableAlign;
  end;
end;

end.

