{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvComponentPanel.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

components : TJvComponentPanel
description: Component panel for GUI developers

Known Issues:
  Some russian comments were translated to english; these comments are marked
  with [translated]
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvComponentPanel;

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls,
  ExtCtrls, Buttons,
  JvButtons;

type
  TButtonClick = procedure(Sender: TObject; Button: Integer) of object;

  TJvComponentPanel = class(TCustomPanel)
  private
    FButtonW: Integer;
    FButtonH: Integer;
    FButtons: TList;
    FOnClick: TButtonClick;
    FOnDblClick: TButtonClick;
    FButtonPointer: TSpeedButton;
  {  FButtonLeft: TSpeedButton;
    FButtonRight: TSpeedButton; }
    FButtonLeft: TJvNoFrameButton;
    FButtonRight: TJvNoFrameButton;
    FFirstVisible: Integer;
    FLockUpdate: Integer;
    {***** For property the beginning [translated] *****}
    function GetButton(Index: Integer): TSpeedButton;
    function GetButtonCount: Integer;
    procedure SetButtonCount(AButtonCount: Integer);
    procedure SetButtonW(AButtonW: Integer);
    procedure SetButtonH(AButtonH: Integer);
    procedure SetFirstVisible(AButton: Integer);
    {##### For property the end [translated] #####}
  protected
    FSelectButton: TSpeedButton;
    procedure OnMoveClick(Sender: TObject);
    procedure OnBClick(Sender: TObject);
    procedure OnBDblClick(Sender: TObject);
    procedure WMSetText(var Msg: TWMSetText); message WM_SETTEXT;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
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
    property ButtonWidth: Integer read FButtonW write SetButtonW;
    property ButtonHeight: Integer read FButtonH write SetButtonH;
    property ButtonCount: Integer read GetButtonCount write SetButtonCount;
    {$IFDEF COMPILER4_UP}
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Constraints;
    property UseDockManager default True;
    property DockSite;
    property DragKind;
    property ParentBiDiMode;
    property OnCanResize;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnUnDock;
    {$ENDIF COMPILER4_UP}
  end;

implementation

{$R JvComponentPanel.res}

constructor TJvComponentPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelOuter := bvNone;
  FButtons := TList.Create;
  FFirstVisible := 0;
  FButtonW := 28;
  FButtonH := 28;
 { FButtonLeft    := TSpeedButton.Create(Self);
  FButtonRight   := TSpeedButton.Create(Self); }
  FButtonLeft := TJvNoFrameButton.Create(Self);
  FButtonRight := TJvNoFrameButton.Create(Self);
  FButtonPointer := TSpeedButton.Create(Self);
  with FButtonLeft do
  begin
    // Flat := True;
    Parent := Self;
    Tag := 0;
    Width := 12;
    Top := 0;
    Glyph.LoadFromResourceName(HInstance, 'RACPLEFT');
    NumGlyphs := 2;
    // Layout := blGlyphRight;
    OnClick := OnMoveClick;
  end;
  with FButtonRight do
  begin
  //  Flat   := True;
    Parent := Self;
    Tag := 1;
    Width := 12;
    Top := 0;
    Glyph.LoadFromResourceName(HInstance, 'RACPRIGHT');
    NumGlyphs := 2;
   // Layout := blGlyphLeft;
    OnClick := OnMoveClick;
  end;
  with FButtonPointer do
  begin
    {$IFDEF COMPILER3_UP}
    Flat := True;
    {$ENDIF COMPILER3_UP}
    Parent := Self;
    Top := 0;
    Glyph.LoadFromResourceName(HInstance, 'RACPPOINTER');
    GroupIndex := 1;
    OnClick := OnBClick;
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
  FButtonPointer.Free;
  FButtonRight.Free;
  FButtonLeft.Free;
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
  SetButtonCount(TmpNum);
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
  if (AButtonCount < 0) or (AButtonCount > 100) then
    raise Exception.Create('Invalid ButtonCount');
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
        {$IFDEF COMPILER3_UP}
        Flat := True;
        {$ENDIF COMPILER3_UP}
        Parent := Self;
        Top := 0;
        GroupIndex := 1;
        OnClick := OnBClick;
        OnDblClick := OnBDblClick;
      end;
      FButtons.Add(TmpButton);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TJvComponentPanel.SetButtonW(AButtonW: Integer);
begin
  if FButtonW <> AButtonW then
  begin
    FButtonW := AButtonW;
    Resize;
  end;
end;

procedure TJvComponentPanel.SetButtonH(AButtonH: Integer);
begin
  if FButtonH <> AButtonH then
  begin
    FButtonH := AButtonH;
    Resize;
  end;
end;

procedure TJvComponentPanel.OnMoveClick(Sender: TObject);
begin
  case TSpeedButton(Sender).Tag of
    0:
      Dec(FFirstVisible);
    1:
      Inc(FFirstVisible);
  end;
  Resize;
end;

procedure TJvComponentPanel.OnBClick(Sender: TObject);
begin
  if FSelectButton <> Sender then
  begin
    FSelectButton := TSpeedButton(Sender);
    if Assigned(FOnClick) then
      FOnClick(Sender, FButtons.IndexOf(FSelectButton));
  end;
end;

procedure TJvComponentPanel.OnBDblClick(Sender: TObject);
begin
  if Assigned(FOnDblClick) then
    FOnDblClick(Sender, FButtons.IndexOf(Sender));
end;

procedure TJvComponentPanel.WMSetText(var Msg: TWMSetText);
begin
  inherited;
  Caption := '';
end;

procedure TJvComponentPanel.WMSize(var Msg: TWMSize);
begin
  inherited;
  Resize;
end;

procedure TJvComponentPanel.Resize;
var
  VisibleCount: Integer;
  I: Integer;
begin
  Height := FButtonH;
  FButtonPointer.Height := FButtonH;
  FButtonPointer.Width := FButtonW;
  FButtonLeft.Height := FButtonH;
  FButtonRight.Height := FButtonH;
  VisibleCount := (Width - (12 + 12 + FButtonW)) div FButtonW;
  FButtonPointer.Left := 0;
  FButtonLeft.Left := FButtonW + 6;
  FButtonRight.Left := (FButtonW + 12 + 6) + VisibleCount * FButtonW;
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
    TSpeedButton(FButtons[I]).Width := FButtonW;
    TSpeedButton(FButtons[I]).Height := FButtonH;
    if (I >= FFirstVisible) and (I < FFirstVisible + VisibleCount) then
      TSpeedButton(FButtons[I]).Left := (FButtonW + 12 + 6) + (I - FFirstVisible) * FButtonW
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

