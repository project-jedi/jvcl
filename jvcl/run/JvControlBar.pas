{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvControlBar.PAS, released on 2001-02-28.

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

unit JvControlBar;

interface

uses
  Windows, Classes, Graphics, Controls, Menus,
  JvExControls, JvExExtCtrls;

type
  TPopupNames = (pnHint, pnName);
  TJvControlBar = class(TJvExControlBar, IJvDenySubClassing)
  private
    FPopupControl: Boolean;
    FPopup: TPopupMenu;
    FPopupNames: TPopupNames;
    FList: TList;
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; override;
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
    property HintColor;
    {$IFDEF JVCLThemesEnabled}
    property ParentBackground;
    {$ENDIF JVCLThemesEnabled}
    property PopupControl: Boolean read FPopupControl write FPopupControl default True;
    property PopupNames: TPopupNames read FPopupNames write FPopupNames default pnHint;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
  end;

implementation

uses
  SysUtils,
  JvThemes;

const
  cFalse = 'false';
  cTrue = 'true';
  cUndocked = 'undocked';

constructor TJvControlBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FList := TList.Create;
  FPopupControl := True;
  FPopupNames := pnHint;
  ControlStyle := ControlStyle + [csAcceptsControls];
  {$IFDEF VCL}
  IncludeThemeStyle(Self, [csParentBackground]);
  {$ENDIF VCL}
end;

destructor TJvControlBar.Destroy;
begin
  FList.Free;
  FPopup.Free;
  inherited Destroy;
end;

function TJvControlBar.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  if Picture.Graphic <> nil then
    Result := inherited DoPaintBackground(Canvas, Param)
  else
  begin
    DrawThemedBackground(Self, Canvas.Handle, ClientRect, Parent.Brush.Handle);
    Result := True;
  end;
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
    {$IFDEF VCL}
    {$IFDEF COMPILER6_UP}
    It.AutoCheck := True;
    {$ENDIF COMPILER6_UP}
    {$ENDIF VCL}
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
  {$IFDEF VCL}
  LDocked: Boolean;
  {$ENDIF VCL}
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
        {$IFDEF VCL}
        LDocked := True;
        {$ENDIF VCL}
        St2 := Copy(St2, I + 1, Length(St2));
        I := Pos(',', St2);
        if I <> 0 then
        begin
          {$IFDEF VCL}
          if Pos(cUndocked, St2) <> 0 then
            LDocked := False;
          {$ENDIF VCL}
          St2 := Copy(St2, 1, I - 1);
        end;
        {$IFDEF VCL}
        if LDocked and (TControl(FList[J]).Parent <> Self) then
          TControl(FList[J]).ManualDock(Self)
        else
        if (not LDocked) and (TControl(FList[J]).Parent = Self) then
          TControl(FList[J]).ManualDock(nil);
        {$ENDIF VCL}
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

