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

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}


unit JvPopupMemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,  StdCtrls ,JVCLVer;


type
  TJvPopupMemo = class(TMemo)
  private
    { Private declarations }
    FDesigntimeHeight: Integer;
    FFocusedHeight: Integer;
    FMaximumHeight: Integer;
    FCanvas: TControlCanvas;
    FAboutJVCL: TJVCLAboutInfo;

    Procedure CMTextChanged(Var msg: TMessage); message CM_TEXTCHANGED;
    procedure SetFocusedHeight(const Value: Integer);
    procedure SetMaximumHeight(const Value: Integer);
    procedure UpdateHeight;
    procedure ChangeScrollbar( value: TScrollStyle );
  protected
    { Protected declarations }
    Procedure DoEnter; override;
    Procedure DoExit; override;
    Procedure Change; override;
    Procedure AdjustHeight;
    property Canvas: TControlCanvas read FCanvas;
  public
    { Public declarations }
    Constructor Create( aOwner: TComponent ); override;
    Destructor Destroy; override;
    property FocusedHeight: Integer read FFocusedHeight write SetFocusedHeight;
  published
    { Published declarations }
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL  stored False;
    property MaximumHeight: Integer read FMaximumHeight write SetMaximumHeight;
  end;


implementation


{ TJvPopupMemo }

procedure TJvPopupMemo.AdjustHeight;
const
  alignflags: Array [TAlignment] of DWORD =
    (DT_LEFT, DT_CENTER, DT_RIGHT);
var
  oldrect, newrect: TRect;
  newheight: Integer;
  S: String;
begin
  If not HandleAllocated Then Exit;

  Perform( EM_GETRECT, 0, lparam(@oldrect));
  S:= Text;

  { HACK ALERT! DrawText discards a trailing linebreak for measurement, so if
    the user hits return in the control and the new line would require a
    larger memo we do not get the correct value back. To fix that we add a
    blank just for the measurement if the last character is a linefeed. }
  If (Length(S) > 0) and (S[Length(S)] = #10) Then
    S:= S+' ';
  Canvas.Font:= Font;
  newrect:= oldrect;
  DrawText( Canvas.Handle, Pchar(S), Length(S), newrect,
            DT_CALCRECT or DT_EDITCONTROL or DT_WORDBREAK or DT_NOPREFIX or
            DT_EXPANDTABS or
            alignflags[ Alignment ] );
  If oldrect.bottom <> newrect.bottom Then Begin
    newHeight := Height - (oldrect.bottom-oldrect.top) +
                 (newrect.bottom - newrect.top );
    If newHeight > MaximumHeight Then
      ChangeScrollbar( ssVertical )
    Else
      ChangeScrollbar( ssNone );
    FocusedHeight := newHeight;
  End;
end;

procedure TJvPopupMemo.Change;
begin
  AdjustHeight;
  inherited;
end;

procedure TJvPopupMemo.ChangeScrollbar(value: TScrollStyle);
var
  oldpos : Integer;
begin
  If Scrollbars <> value Then Begin
    { Changing the scrollbar recreates the window and looses the caret
      position! }
    oldpos := SelStart;
    Scrollbars := value;
    SelStart := oldpos;
    Perform( EM_SCROLLCARET, 0, 0 );
  End;
end;

procedure TJvPopupMemo.CMTextChanged(var msg: TMessage);
begin
  AdjustHeight;
  inherited;
end;

constructor TJvPopupMemo.Create(aOwner: TComponent);
begin
  inherited;
  FFocusedHeight := Height;
  FMaximumHeight := 5*Height;
  FCanvas:= TControlCanvas.Create;
  FCanvas.Control := Self;
end;

destructor TJvPopupMemo.Destroy;
begin
  FCanvas.Free;
  inherited;
end;

procedure TJvPopupMemo.DoEnter;
begin
  inherited;
  FDesigntimeHeight := Height;
  UpdateHeight;
  // Height := FFocusedHeight;
end;

procedure TJvPopupMemo.DoExit;
begin
  inherited;
  Height := FDesigntimeHeight;
end;

procedure TJvPopupMemo.SetFocusedHeight(const Value: Integer);
begin
  If FFocusedHeight <> Value Then Begin
    If Value > MaximumHeight Then
      FFocusedHeight := MaximumHeight
    Else
      FFocusedHeight := value;
    If Focused Then
      UpdateHeight;
  End;
end;

procedure TJvPopupMemo.SetMaximumHeight(const Value: Integer);
begin
  If FMaximumHeight <> Value Then Begin
    FMaximumHeight := Value;
    If Value < FocusedHeight Then
      FocusedHeight := Value;
  End;
end;

procedure TJvPopupMemo.UpdateHeight;
var
  line: Integer;
begin
  If HandleAllocated and Focused Then Begin
    Height := FocusedHeight;
    If Scrollbars = ssNone Then Begin
      line := Perform( EM_GETFIRSTVISIBLELINE, 0, 0 );
      If line > 0 Then
        Perform( EM_LINESCROLL, 0, -line );
    End;
  End;
end;

end.
