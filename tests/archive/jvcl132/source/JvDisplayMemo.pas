{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAlignListbox.PAS, released on 2000-11-22.

The Initial Developer of the Original Code is Peter Below <100113.1101@compuserve.com>
Portions created by Peter Below are Copyright (C) 2000 Peter Below.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2000-mm-dd

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvDisplayMemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, JVCLVer;

type
  TJvDisplayMemo = class(TCustomMemo)
  private
    FAboutJVCL: TJVCLAboutInfo;
    { Private declarations }
    procedure WMSetFocus(var msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var msg: TWMKillFocus); message WM_KILLFOCUS;

  protected
    { Protected declarations }
    procedure WndProc(var Message: TMessage); override;
  public
    { Public declarations }
    constructor Create(aOwner: TComponent); override;
  published
    { Publish most of the stuff TMemo publishes, rest commented out }
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Align;
    property Alignment;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color default $C0FFFF;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    {property HideSelection;}
    property ImeMode;
    property ImeName;
    property Lines;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    {property ReadOnly;}
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    {property WantReturns;}
    {property WantTabs;}
    property WordWrap;
    property OnChange;
    {property OnClick;}
    {property OnDblClick;}
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {property OnKeyDown;}
    {property OnKeyPress;}
    {property OnKeyUp;}
    {property OnMouseDown;}
    {property OnMouseMove;}
    {property OnMouseUp;}
    property OnStartDock;
    property OnStartDrag;

  end;

implementation

{ TJvDisplayMemo }

constructor TJvDisplayMemo.Create(aOwner: TComponent);
begin
  inherited;
  ReadOnly := True;
  Color := $C0FFFF;
end;

procedure TJvDisplayMemo.WMKillFocus(var msg: TWMKillFocus);
begin
  ShowCaret(handle);
  inherited;
end;

procedure TJvDisplayMemo.WMSetFocus(var msg: TWMSetFocus);
begin
  inherited;
  HideCaret(handle);
end;

procedure TJvDisplayMemo.WndProc(var Message: TMessage);
  procedure Scroll(msg, scrollcode: Integer);
  begin
    Perform(msg, scrollcode, 0);
    Perform(msg, SB_ENDSCROLL, 0);
  end;
begin
  if not (csDesigning in ComponentState) then
    case Message.Msg of
      WM_LBUTTONDOWN, WM_LBUTTONUP, WM_MOUSEMOVE, WM_LBUTTONDBLCLK,
        WM_CHAR, WM_KEYUP:
        begin
          Message.Result := 0;
          if Message.Msg = WM_LBUTTONDOWN then
            if not Focused then
              SetFocus;
          Exit;
        end;
      WM_KEYDOWN:
        begin
          case Message.WParam of
            VK_DOWN: Scroll(WM_VSCROLL, SB_LINEDOWN);
            VK_UP: Scroll(WM_VSCROLL, SB_LINEUP);
            VK_LEFT: Scroll(WM_HSCROLL, SB_LINELEFT);
            VK_RIGHT: Scroll(WM_HSCROLL, SB_LINERIGHT);
            VK_NEXT: Scroll(WM_VSCROLL, SB_PAGEDOWN);
            VK_PRIOR: Scroll(WM_VSCROLL, SB_PAGEUP);
            VK_HOME: Scroll(WM_VSCROLL, SB_TOP);
            VK_END: Scroll(WM_VSCROLL, SB_BOTTOM);
          end;
          Message.Result := 0;
          Exit;
        end;
    end;
  inherited;
end;

end.
