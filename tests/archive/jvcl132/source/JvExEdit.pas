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


unit JvExEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,  StdCtrls, JVCLVer;

type
  TJvExEdit = class(Tedit)
  private
    FDisabledColor: TColor;
    FDisabledTextColor: TColor;
    FAboutJVCL: TJVCLAboutInfo;
    { Private declarations }
    Procedure WMPaint( Var msg: TWMPaint ); message WM_PAINT;
    Procedure WMEraseBkGnd( var msg: TWMEraseBkGnd ); message WM_ERASEBKGND;
    procedure SetDisabledColor(const Value: TColor); virtual;
    procedure SetDisabledTextColor(const Value: TColor); virtual;
  protected
    { Protected declarations }
  public
    { Public declarations }
    Constructor Create( aOwner: TComponent ); override;
  published
    { Published declarations }
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL  stored False;
    property DisabledTextColor: TColor
      read FDisabledTextColor
      write SetDisabledTextColor
      default clGrayText;
    property DisabledColor: TColor
      read FDisabledColor
      write SetDisabledColor
      default clWindow;
  end;

implementation


{ TJvExEdit }

constructor TJvExEdit.Create(aOwner: TComponent);
begin
  inherited;
  FDisabledColor := clWindow;
  FDisabledTextColor := clGrayText;
end;


procedure TJvExEdit.SetDisabledColor(const Value: TColor);
begin
  If FDisabledColor <> Value Then Begin
    FDisabledColor := Value;
    If not Enabled Then
      Invalidate;
  End;
end;

procedure TJvExEdit.SetDisabledTextColor(const Value: TColor);
begin
  If FDisabledTextColor <> Value Then Begin
    FDisabledTextColor := Value;
    If not Enabled Then
      Invalidate;
  End;
end;

procedure TJvExEdit.WMEraseBkGnd(var msg: TWMEraseBkGnd);
var
  canvas: TCanvas;
begin
  If Enabled Then
    inherited
  Else Begin
    canvas:= TCanvas.Create;
    try
      canvas.Handle := msg.DC;
      SaveDC( msg.DC );
      try
        canvas.Brush.Color := FDisabledColor;
        canvas.Brush.Style := bsSolid;
        canvas.Fillrect( clientrect );
        msg.result := 1;
      finally
        RestoreDC( msg.DC, -1 );
      end;
    finally
      canvas.free
    end;
  End; { Else }
end;

procedure TJvExEdit.WMPaint(var msg: TWMPaint);
var
  canvas: TCanvas;
  ps: TPaintStruct;
  callEndPaint: Boolean;
begin
  If Enabled Then
    inherited
  Else Begin
    callEndPaint := False;
    canvas:= TCanvas.Create;
    try
      If msg.DC <> 0 Then Begin
        canvas.Handle := msg.DC;
        ps.fErase := true;
      End
      Else Begin
        BeginPaint( handle, ps );
        callEndPaint:= true;
        canvas.handle := ps.hdc;
      End;

      If ps.fErase Then
        Perform( WM_ERASEBKGND, canvas.handle, 0 );

      SaveDC( canvas.handle );
      try
        canvas.Brush.Style := bsClear;
        canvas.Font := Font;
        canvas.Font.Color := FDisabledTextColor;
        canvas.TextOut( 1, 1, Text );
      finally
        RestoreDC( canvas.handle, -1 );
      end;
    finally
      If callEndPaint Then
        EndPaint( handle, ps );
      canvas.free
    end;
  End; { Else }
end;

end.
