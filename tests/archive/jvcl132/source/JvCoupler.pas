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


unit JvCoupler;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, stdctrls, JvComponent;

type
  TJvCoupler = class(TJvComponent)
  private
    FSpacing: Integer;
    FLabel: TCustomLabel;
    FControl: TWinControl;
    FOldWndProc: TWndMethod;

    procedure SetControl(const Value: TWinControl);
    procedure SetLabel(const Value: TCustomLabel);
    { Private declarations }
  protected
    { Protected declarations }
    procedure MessageHook( Var msg: TMessage );
    procedure AlignLabel;

    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    { Public declarations }
    Constructor Create( aOWner: TComponent ); override;
    DeStructor Destroy; override;
  published
    { Published declarations }
    property DisplayLabel: TCustomLabel read FLabel write SetLabel;
    property FocusControl: TWinControl read FControl write SetControl;
    property Spacing: Integer read FSpacing write FSpacing default 2;
  end;



implementation


{ TJvCoupler }

procedure TJvCoupler.AlignLabel;
begin
  If Assigned( FLabel ) Then
    With FLabel Do
      SetBounds( FControl.Left, FControl.Top - Height - FSpacing,
                 Width, Height );
end;

constructor TJvCoupler.Create(aOWner: TComponent);
begin
  inherited;
  FSpacing := 2;
end;

destructor TJvCoupler.Destroy;
begin
  FocusControl := Nil;
  inherited;
end;


procedure TJvCoupler.MessageHook(var msg: TMessage);
begin
  FOldWndProc( msg );
  If msg.Msg = WM_MOVE Then
    AlignLabel
  Else
    If msg.Msg = WM_DESTROY Then
      FocusControl := Nil;
end;

procedure TJvCoupler.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  If Operation = opRemove Then
    If FLabel = AComponent Then
      DisplayLabel := Nil
    Else
      If FControl = AComponent Then
        FocusControl := Nil;
  inherited;
end;

procedure TJvCoupler.SetControl(const Value: TWinControl);
begin
  If Fcontrol <> Value Then Begin
    If Assigned( FControl ) Then Begin
      FControl.WindowProc := FOldWndProc;
      If Assigned( FLabel ) Then
        TLabel(FLabel).FocusControl := Nil;
    End; { If }

    FControl := Value;
    If Assigned( FControl ) Then Begin
      FOldWndProc := FControl.WindowProc;
      FControl.WindowProc := MessageHook;
      If Assigned( FLabel ) Then Begin
        TLabel(FLabel).FocusControl := FControl;
        AlignLabel;
      End; { If }
    End; { If }
  End; { If }
end; { TJvCoupler.SetControl }

procedure TJvCoupler.SetLabel(const Value: TCustomLabel);
begin
  If FLabel <> value Then Begin
    If Assigned( FLabel ) Then
      TLabel(FLabel).FocusControl := Nil;
    FLabel := Value;
    If Assigned( FLabel ) Then Begin
      TLabel(FLabel).FocusControl := FControl;
      If Assigned( FControl ) Then
        AlignLabel;
    End; { If }
  End; { If }
end; { TJvCoupler.SetLabel }

end.
