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


unit JvMLButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,  StdCtrls ,JVCLVer;



type
  TJvMultilineButton = class(TButton)
  private
    FMultiline: Boolean;
    FAboutJVCL: TJVCLAboutInfo;
    function GetCaption: String;
    procedure SetCaption(const Value: String);
    procedure SetMultiline(const Value: Boolean);
  public
    Procedure CreateParams( Var params: TCreateParams ); override;
    Constructor Create( aOwner: TComponent ); override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL  stored False;
    property Multiline: Boolean read FMultiline write SetMultiline default True;
    property Caption: String read GetCaption write SetCaption;
  end;


implementation

{ TJvMultilineButton }

constructor TJvMultilineButton.Create(aOwner: TComponent);
begin
  inherited;
  FMultiline := True;
end;

procedure TJvMultilineButton.CreateParams(var params: TCreateParams);
begin
  inherited;
  If FMultiline Then
    params.Style := params.Style or BS_MULTILINE;
end;



function TJvMultilineButton.GetCaption: String;
begin
  Result := Stringreplace( inherited Caption, #13, '|', [rfReplaceAll] );
end;


procedure TJvMultilineButton.SetCaption(const Value: String);
begin
  If value <> Caption Then Begin
    inherited Caption := Stringreplace( value, '|', #13, [rfReplaceAll] );
    Invalidate;
  End;
end;

procedure TJvMultilineButton.SetMultiline(const Value: Boolean);
begin
  If FMultiline <> Value Then Begin
    FMultiline := Value;
    RecreateWnd;
  End;
end;

end.
