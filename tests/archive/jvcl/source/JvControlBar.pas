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
  Menus, JVCLVer;

type
  TPopupNames = (pnHint, pnName);
  TJvControlBar = class(TControlBar)
  private
    FColor: TColor;
    FSaved: TColor;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    Fover: Boolean;
    FAboutJVCL: TJVCLAboutInfo;
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
    procedure Loaded;override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    function SavePositions: string;
    procedure LoadPositions(const Value: string);
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;

    property PopupControl:Boolean read FPopupControl write FPopupControl default true;
    property PopupNames:TPopupNames read FPopupNames write FPopupNames default pnHint;

    property HintColor: TColor read FColor write FColor default clInfoBk;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  end;

implementation

{**************************************************}

procedure TJvControlBar.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

{**************************************************}

constructor TJvControlBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FList := TList.Create;
  FColor := clInfoBk;
  FOver := False;
  FPopupControl := true;
  FPopupNames := pnHint;
  ControlStyle := ControlStyle + [csAcceptsControls];
end;

{**************************************************}

procedure TJvControlBar.CMMouseEnter(var Msg: TMessage);
begin
  if not FOver then
  begin
    FOver := True;
    FSaved := Application.HintColor;
    Application.HintColor := FColor;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

{**************************************************}

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

{**************************************************}

procedure TJvControlBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
 i: Integer;

  procedure DoAddControl(const AControl: TControl; const Index: Integer);
  var
   it: TMenuItem;
  begin
    it := TMenuItem.Create(FPopup);
    if PopupNames=pnHint then
      it.Caption := AControl.Hint
    else
      it.Caption := AControl.Name;
    {$IFDEF COMPILER6_UP}
    it.AutoCheck := true;
    {$ENDIF}
    it.Tag := Index;
    it.OnClick := PopupMenuClick;
    it.Checked := AControl.Visible;
    FPopup.Items.Add(it);
  end;

begin
  inherited;
  if (PopupControl) and (Button = mbRight) then
  begin
    if FPopup<>nil then
      FPopup.Items.Clear
    else
      FPopup := TPopupMenu.Create(self);
    for i:=0 to FList.Count-1 do
      DoAddControl(TControl(FList[i]), i);
    with ClientToScreen(Point(X,Y)) do
      FPopup.Popup(X,Y);
  end;
end;

{**************************************************}

destructor TJvControlBar.Destroy;
begin
  FList.Free;
  if FPopup<>nil then
    FPopup.Free;
  inherited;
end;

{**************************************************}

procedure TJvControlBar.PopupMenuClick(Sender: TObject);
begin
  with Sender as TMenuItem do
  begin
    {$IFNDEF COMPILER6_UP}
    Checked := not Checked;
    {$ENDIF}
    if (Tag>=0) and (Tag<FList.Count) then
      TControl(FList[Tag]).Visible := Checked;
  end;
end;

{**************************************************}

procedure TJvControlBar.LoadPositions(const Value: string);
var
  st,st2: string;
  i,j: Integer;
  lLeft, lTop: Integer;
  lDocked: Boolean;
begin
  st := Value;
  j := 0;
  while (Length(st)>1) and (j<FList.Count) do
  begin
    i := Pos(';',st);
    if i=0 then
    begin
      st2 := st;
      st := '';
    end
    else
    begin
      st2 := Copy(st,1,i-1);
      st := Copy(st,i+1,Length(st));
    end;

    i := pos(',',st2);
    if i<>0 then
    begin
      TControl(FList[j]).Visible := pos('true',st2)=1;
      st2 := Copy(st2,i+1,Length(st2));
      i := pos(',',st2);
      if i<>0 then
      begin
        lLeft := StrToIntDef( Copy(st2,1,i-1), TControl(FList[j]).Left);
        lDocked := true;
        st2 := Copy(st2,i+1,Length(st2));
        i := pos(',',st2);
        if i<>0 then
        begin
          if pos('undocked',st2)<>0 then
            lDocked := false;
          st2 := Copy(st2,1,i-1);
        end;
        if (lDocked) and (TControl(FList[j]).Parent<>self) then
          TControl(FList[j]).ManualDock(self)
        else if (not lDocked) and (TControl(FList[j]).Parent=self) then
          TControl(FList[j]).ManualDock(nil);
        lTop := StrToIntDef(st2, TControl(FList[j]).Top);

        if ControlAtPos(Point(lLeft,TControl(FList[j]).Top),true)<>nil then
        begin
          TControl(FList[j]).Left := lLeft;
          TControl(FList[j]).Top := lTop;
        end
        else
        begin
          TControl(FList[j]).Top := lTop;
          TControl(FList[j]).Left := lLeft;
        end;
      end;
    end;
    inc(j);
  end;
end;

{**************************************************}

function TJvControlBar.SavePositions: string;
var
 i: Integer;
begin
  result := '';
  for i:=0 to FList.Count-1 do
  begin
    if TControl(FList[i]).Visible then
      result := result+'true,'
    else
      result := result+'false,';
    result := result + IntToStr(TControl(FList[i]).Left) + ',' + IntToStr(TControl(Flist[i]).Top);
    if TControl(FList[i]).Parent <> self then
      result := result + ',undocked';
    result := result + ';';
  end;
end;

{**************************************************}

procedure TJvControlBar.Loaded;
var
 i: Integer;
begin
  inherited;
  for i:=0 to ControlCount-1 do
    FList.Add(Controls[i]);
end;

procedure TJvControlBar.DoAddDockClient(Client: TControl;
  const ARect: TRect);
begin
  inherited;
  if FList.IndexOf(Client)=-1 then
    FList.Add(Client);
end;

end.
