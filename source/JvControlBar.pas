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

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvControlBar;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Extctrls, Controls, Forms,
  Menus,
  JVCLVer;

type
  TPopupNames = (pnHint, pnName);
  TJvControlBar = class(TControlBar)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FHintColor: TColor;
    FSaved: TColor;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOver: Boolean;
    FPopupControl: Boolean;
    FPopup: TPopupMenu;
    FPopupNames: TPopupNames;
    FList: TList;
  protected
    procedure DoAddDockClient(Client: TControl; const ARect: TRect); override;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure PopupMenuClick(Sender: TObject);
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function SavePositions: string;
    procedure LoadPositions(const Value: string);
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property PopupControl: Boolean read FPopupControl write FPopupControl default True;
    property PopupNames: TPopupNames read FPopupNames write FPopupNames default pnHint;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  end;

implementation

constructor TJvControlBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FList := TList.Create;
  FHintColor := clInfoBk;
  FOver := False;
  FPopupControl := True;
  FPopupNames := pnHint;
  ControlStyle := ControlStyle + [csAcceptsControls];
end;

destructor TJvControlBar.Destroy;
begin
  FList.Free;
  if FPopup <> nil then
    FPopup.Free;
  inherited Destroy;
end;

procedure TJvControlBar.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvControlBar.CMMouseEnter(var Msg: TMessage);
begin
  if not FOver then
  begin
    FOver := True;
    FSaved := Application.HintColor;
    // for D7...
    if csDesigning in ComponentState then
      Exit;
    Application.HintColor := FHintColor;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvControlBar.CMMouseLeave(var Msg: TMessage);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    FOver := False;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
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
    {$ENDIF}
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
    {$ENDIF}
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
      TControl(FList[J]).Visible := Pos('true', St2) = 1;
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
          if Pos('undocked', St2) <> 0 then
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
      Result := Result + 'true,'
    else
      Result := Result + 'false,';
    Result := Result + IntToStr(TControl(FList[I]).Left) + ',' + IntToStr(TControl(Flist[I]).Top);
    if TControl(FList[I]).Parent <> Self then
      Result := Result + ',undocked';
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

procedure TJvControlBar.DoAddDockClient(Client: TControl;
  const ARect: TRect);
begin
  inherited DoAddDockClient(Client, ARect);
  if FList.IndexOf(Client) = -1 then
    FList.Add(Client);
end;

end.

