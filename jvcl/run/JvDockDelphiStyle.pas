{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDockDelphiStyle.pas, released on 2003-12-31.

The Initial Developer of the Original Code is luxiaoban.
Portions created by luxiaoban are Copyright (C) 2002,2003 luxiaoban.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvDockDelphiStyle;

interface

uses
  Windows, Messages, Classes, Controls, Graphics,
  JvDockControlForm, JvDockSupportControl, JvDockTree;

type
  TJvDockDelphiStyle = class(TJvDockBasicStyle)
  protected
    procedure FormDockDrop(DockClient: TJvDockClient;
      Source: TJvDockDragDockObject; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    {$IFNDEF USEJVCL}
    function GetControlName: string; override;
    {$ENDIF USEJVCL}
  published
    property ConjoinServerOption;
    property TabServerOption;
  end;

  TJvDockDelphiSplitter = class(TJvDockSplitter);

  TJvDockDelphiPanel = class(TJvDockPanel);

  TJvDockDelphiConjoinPanel = class(TJvDockConjoinPanel);

  TJvDockDelphiTabPageControl = class(TJvDockTabPageControl)
  protected
    procedure CMDockClient(var Msg: TCMDockClient); message CM_DOCKCLIENT;
  end;

  TJvDockDelphiZone = class(TJvDockZone);

  TJvDockDelphiTree = class(TJvDockTree);

  TJvDockDelphiDragDockObject = class(TJvDockDragDockObject);

implementation

uses
  SysUtils, Forms,
  JvDockSupportProc, JvDockGlobals;

constructor TJvDockDelphiStyle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DockPanelClass := TJvDockDelphiPanel;
  DockSplitterClass := TJvDockDelphiSplitter;
  ConjoinPanelClass := TJvDockDelphiConjoinPanel;
  TabDockClass := TJvDockDelphiTabPageControl;
  DockPanelZoneClass := TJvDockDelphiZone;
  DockPanelTreeClass := TJvDockDelphiTree;
  ConjoinPanelZoneClass := TJvDockDelphiZone;
  ConjoinPanelTreeClass := TJvDockDelphiTree;
end;

procedure TJvDockDelphiStyle.FormDockDrop(DockClient: TJvDockClient;
  Source: TJvDockDragDockObject; X, Y: Integer);
var
  ARect, DRect: TRect;
  DockType: TAlign;
  Host: TForm;
  APanelDock: TWinControl;
  ADockClient: TJvDockClient;
begin
  if IsDockable(DockClient.ParentForm, Source.Control, Source.DropOnControl, Source.DropAlign) then
  begin
    Host := nil;

    if not JvGlobalDockIsLoading then
      JvDockLockWindow(nil);
    try
      with DockClient do
      begin
        DockType := ComputeDockingRect(DockClient.ParentForm, ARect, Point(X, Y));
        if ParentForm.HostDockSite is TJvDockPanel then
        begin
          if DockType = alClient then
          begin
            if Source.Control is TJvDockTabHostForm then
            begin
              APanelDock := ParentForm.HostDockSite;
              ARect := ParentForm.BoundsRect;
              ParentForm.ManualDock(TJvDockTabHostForm(Source.Control).PageControl, nil, alClient);
              TJvDockTabHostForm(Source.Control).PageControl.ActivePage.PageIndex := 0;
              Source.Control.BoundsRect := ARect;
              Source.Control.ManualDock(APanelDock, nil, alClient);
              if ParentForm.FormStyle = fsStayOnTop then
                TForm(Source.Control).FormStyle := fsStayOnTop;
            end
            else
            begin
              APanelDock := ParentForm.HostDockSite;
              DRect.TopLeft := ParentForm.HostDockSite.ClientToScreen(Point(0, 0));
              Host := CreateTabHostAndDockControl(ParentForm, Source.Control);
              SetDockSite(ParentForm, False);
              SetDockSite(TWinControl(Source.Control), False);
              Host.Top := DRect.Top;
              Host.Left := DRect.Left;
              Host.ManualDock(APanelDock, nil, alClient);
              Host.Visible := True;
            end;
          end
          else
          begin
            DRect := ParentForm.HostDockSite.BoundsRect;
            Source.Control.ManualDock(ParentForm.HostDockSite, nil, DockType);
            ParentForm.HostDockSite.BoundsRect := DRect;
          end;
          Exit;
        end;

        if DockType = alClient then
        begin
          if Source.Control is TJvDockTabHostForm then
          begin
            ARect := DockClient.ParentForm.BoundsRect;
            DockClient.ParentForm.ManualDock(TJvDockTabHostForm(Source.Control).PageControl, nil, alClient);
            TJvDockTabHostForm(Source.Control).PageControl.ActivePage.PageIndex := 0;
            Source.Control.BoundsRect := ARect;
            if DockClient.ParentForm.FormStyle = fsStayOnTop then
              TJvDockTabHostForm(Source.Control).FormStyle := fsStayOnTop;
            Exit;
          end
          else
          begin
            Host := DockClient.CreateTabHostAndDockControl(DockClient.ParentForm, Source.Control);
            Host.Visible := True;
          end;
        end
        else
        if DockType <> alNone then
        begin
          Host := CreateConjoinHostAndDockControl(ParentForm, Source.Control, DockType);
          ADockClient := FindDockClient(Host);
          if ADockClient <> nil then
            ADockClient.EnableDock := False;
          SetDockSite(ParentForm, False);
          SetDockSite(TWinControl(Source.Control), False);
          Host.Visible := True;
        end;

        if Host <> nil then
        begin
          Host.LRDockWidth := Source.Control.LRDockWidth;
          Host.TBDockHeight := Source.Control.TBDockHeight;
        end;
      end;
    finally
      if not JvGlobalDockIsLoading then
        JvDockUnLockWindow;
    end;
  end;
end;

{$IFNDEF USEJVCL}
function TJvDockDelphiStyle.GetControlName: string;
begin
  Result := Format(RsDockLikeDelphiStyle, [inherited GetControlName]);
end;
{$ENDIF USEJVCL}

procedure TJvDockDelphiTabPageControl.CMDockClient(var Msg: TCMDockClient);
var
  I: Integer;
  Control: TControl;
  Count: Integer;
begin
  if Msg.DockSource.Control is TJvDockTabHostForm then
    with TJvDockTabHostForm(Msg.DockSource.Control) do
    begin
      Count := Self.Count;
      for I := PageControl.DockClientCount - 1 downto 0 do
      begin
        Control := PageControl.DockClients[I];
        DoFloat(PageControl, Control);
        Control.ManualDock(Self, nil, alClient);
        Self.ActivePage.PageIndex := Count;
      end;
    end
  else
    inherited;
end;

end.

