{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvControlBar.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2003-09-29

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvControlBar;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  ExtCtrls, Controls, Forms, Menus,
  JVCLVer, JvThemes, JvExControls, JvExExtCtrls;

type
  TPopupNames = (pnHint, pnName);
  TJvControlBar = class(TJvExControlBar, IJvDenySubClassing)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FHintColor: TColor;
    FSaved: TColor;
    FOver: Boolean;
    FOnParentColorChanged: TNotifyEvent;
    FPopupControl: Boolean;
    FPopup: TPopupMenu;
    FPopupNames: TPopupNames;
    FList: TList;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    {$IFDEF JVCLThemesEnabledD56}
    function GetParentBackground: Boolean;
    {$ENDIF JVCLThemesEnabledD56}
  protected
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure ParentColorChanged; override;
    {$IFDEF JVCLThemesEnabledD56}
    procedure SetParentBackground(Value: Boolean); virtual;
    {$ENDIF JVCLThemesEnabledD56}
    {$IFDEF VCL}
    procedure DoAddDockClient(Client: TControl; const ARect: TRect); override;
    {$ENDIF VCL}
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PopupMenuClick(Sender: TObject);
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function SavePositions: string;
    procedure LoadPositions(const Value: string);
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    {$IFDEF JVCLThemesEnabledD56}
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground default True;
    {$ENDIF JVCLThemesEnabledD56}
    property PopupControl: Boolean read FPopupControl write FPopupControl default True;
    property PopupNames: TPopupNames read FPopupNames write FPopupNames default pnHint;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  end;

implementation

const
  cFalse = 'false';
  cTrue = 'true';
  cUndocked = 'undocked';

constructor TJvControlBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FList := TList.Create;
  FHintColor := clInfoBk;
  FOver := False;
  FPopupControl := True;
  FPopupNames := pnHint;
  ControlStyle := ControlStyle + [csAcceptsControls];
  IncludeThemeStyle(Self, [csParentBackground]);
end;

destructor TJvControlBar.Destroy;
begin
  FList.Free;
  FPopup.Free;
  inherited Destroy;
end;

{$IFDEF JVCLThemesEnabledD56}

function TJvControlBar.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvControlBar.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;

{$ENDIF JVCLThemesEnabledD56}

procedure TJvControlBar.MouseEnter(AControl: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FHintColor;
    FOver := True;
  end;
  inherited MouseEnter(AControl);
end;

procedure TJvControlBar.MouseLeave(AControl: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if FOver then
  begin
    FOver := False;
    Application.HintColor := FSaved;
  end;
  inherited MouseLeave(AControl);
end;

procedure TJvControlBar.ParentColorChanged;
begin
  inherited ParentColorChanged;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvControlBar.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  if Picture.Graphic <> nil then
    inherited
  else
    DrawThemedBackground(Self, Msg.DC, ClientRect, Parent.Brush.Handle);
  Msg.Result := 1;
end;

procedure TJvControlBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;

  procedure DoAddControl(const AControl: TControl; const Index: Integer);
  var
    It: TMenuItem;
  begin
    It := TMenuItem.Create(FPopup);
    if PopupNames = pnHint then
      It.Caption := AControl.Hint
    else
      It.Caption := AControl.Name;
    {$IFDEF COMPILER6_UP}
    It.AutoCheck := True;
    {$ENDIF COMPILER6_UP}
    It.Tag := Index;
    It.OnClick := PopupMenuClick;
    It.Checked := AControl.Visible;
    FPopup.Items.Add(It);
  end;

begin
  inherited MouseUp(Button, Shift, X, Y);
  if PopupControl and (Button = mbRight) then
  begin
    if FPopup <> nil then
      FPopup.Items.Clear
    else
      FPopup := TPopupMenu.Create(Self);
    for I := 0 to FList.Count - 1 do
      DoAddControl(TControl(FList[I]), I);
    with ClientToScreen(Point(X, Y)) do
      FPopup.Popup(X, Y);
  end;
end;

procedure TJvControlBar.PopupMenuClick(Sender: TObject);
begin
  with Sender as TMenuItem do
  begin
    {$IFNDEF COMPILER6_UP}
    Checked := not Checked;
    {$ENDIF COMPILER6_UP}
    if (Tag >= 0) and (Tag < FList.Count) then
      TControl(FList[Tag]).Visible := Checked;
  end;
end;

procedure TJvControlBar.LoadPositions(const Value: string);
var
  St, St2: string;
  I, J: Integer;
  LLeft, LTop: Integer;
  LDocked: Boolean;
begin
  St := Value;
  J := 0;
  while (Length(St) > 1) and (J < FList.Count) do
  begin
    I := Pos(';', St);
    if I = 0 then
    begin
      St2 := St;
      St := '';
    end
    else
    begin
      St2 := Copy(St, 1, I - 1);
      St := Copy(St, I + 1, Length(St));
    end;

    I := Pos(',', St2);
    if I <> 0 then
    begin
      TControl(FList[J]).Visible := Pos(cTrue, St2) = 1;
      St2 := Copy(St2, I + 1, Length(St2));
      I := Pos(',', St2);
      if I <> 0 then
      begin
        LLeft := StrToIntDef(Copy(St2, 1, I - 1), TControl(FList[J]).Left);
        LDocked := True;
        St2 := Copy(St2, I + 1, Length(St2));
        I := Pos(',', St2);
        if I <> 0 then
        begin
          if Pos(cUndocked, St2) <> 0 then
            LDocked := False;
          St2 := Copy(St2, 1, I - 1);
        end;
        if LDocked and (TControl(FList[J]).Parent <> Self) then
          TControl(FList[J]).ManualDock(Self)
        else
        if (not LDocked) and (TControl(FList[J]).Parent = Self) then
          TControl(FList[J]).ManualDock(nil);
        LTop := StrToIntDef(St2, TControl(FList[J]).Top);

        if ControlAtPos(Point(LLeft, TControl(FList[J]).Top), True) <> nil then
        begin
          TControl(FList[J]).Left := LLeft;
          TControl(FList[J]).Top := LTop;
        end
        else
        begin
          TControl(FList[J]).Top := LTop;
          TControl(FList[J]).Left := LLeft;
        end;
      end;
    end;
    Inc(J);
  end;
end;

function TJvControlBar.SavePositions: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FList.Count - 1 do
  begin
    if TControl(FList[I]).Visible then
      Result := Result + cTrue + ','
    else
      Result := Result + cFalse + ',';
    Result := Result + IntToStr(TControl(FList[I]).Left) + ',' +
      IntToStr(TControl(FList[I]).Top);
    if TControl(FList[I]).Parent <> Self then
      Result := Result + ',' + cUndocked;
    Result := Result + ';';
  end;
end;

procedure TJvControlBar.Loaded;
var
  I: Integer;
begin
  inherited Loaded;
  for I := 0 to ControlCount - 1 do
    FList.Add(Controls[I]);
end;

{$IFDEF VCL}
procedure TJvControlBar.DoAddDockClient(Client: TControl; const ARect: TRect);
begin
  inherited DoAddDockClient(Client, ARect);
  if FList.IndexOf(Client) = -1 then
    FList.Add(Client);
end;
{$ENDIF VCL}

end.

