{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvEnterTab.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            
You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

{ A unit that converts all Enter keypresses to Tab keypresses. }

unit JvQEnterTab;

interface

uses
  SysUtils, Classes,
  
  
  Qt, QGraphics, QControls, QForms, QWindows,
  
  JvQComponent;

type
  TJvEnterAsTab = class(TJvGraphicControl)
  private
    FEnterAsTab: Boolean;
    FAllowDefault: Boolean;
    FBmp: TBitmap;
  protected
    
    
    function TabKeyHook(Sender: QObjectH; Event: QEventH): Boolean; virtual;
    
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
  published
    property EnterAsTab: Boolean read FEnterAsTab write FEnterAsTab default True;
    property AllowDefault: Boolean read FAllowDefault write FAllowDefault default True;
  end;

implementation

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvEnterTab.res}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvEnterTab.res}
{$ENDIF LINUX}

constructor TJvEnterAsTab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoStdEvents, csFixedHeight, csFixedWidth];
  FEnterAsTab := True;
  FAllowDefault := True;
  if csDesigning in ComponentState then
  begin
    FBmp := TBitmap.Create;
    FBmp.LoadFromResourceName(hInstance, 'DESIGNENTERASTAB');
  end
  else
    Visible := False;
  
  InstallApplicationHook(TabKeyHook);
  
end;

destructor TJvEnterAsTab.Destroy;
begin
  
  UninstallApplicationHook(TabKeyHook);
  
  FBmp.Free;
  inherited Destroy;
end;



function TJvEnterAsTab.TabKeyHook(Sender: QObjectH; Event: QEventH): Boolean;
var
  ws: WideString;
begin
  Result := False;
  if QEvent_type(Event) = QEventType_KeyPress then
  begin
    if QObject_inherits(Sender, 'QButton') and AllowDefault then
      Exit;

    if ((QKeyEvent_key(QKeyEventH(Event)) = Key_Enter) or
        (QKeyEvent_key(QKeyEventH(Event)) = Key_Return) ) and EnterAsTab then
    begin
      ws := #9;

      QApplication_postEvent(GetParentForm(Self).Handle,
        QKeyEvent_create(QEventType_KeyPress, Key_Tab, 9, 0, @ws, False, 1));
      QApplication_postEvent(GetParentForm(Self).Handle,
        QKeyEvent_create(QEventType_KeyRelease, Key_Tab, 9, 0, @ws, False, 1));

      Result := True;
    end;
  end;
end;


procedure TJvEnterAsTab.Paint;
begin
  if not (csDesigning in ComponentState) then
    Exit;
  Canvas.Brush.Color := clBtnFace;
  
  
  FBmp.Transparent := True;
  FBmp.TransparentColor := clFuchsia;
  Canvas.StretchDraw(ClientRect, FBmp);
  
end;

procedure TJvEnterAsTab.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, 28, 28);
end;

end.

