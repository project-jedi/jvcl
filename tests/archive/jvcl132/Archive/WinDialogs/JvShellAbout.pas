{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvShellAbout.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvShellAbout;

{$OBJEXPORTALL On}

interface

uses
  Windows, Classes, Graphics, Forms, ShellApi,
  JvBaseDlg;

type
  TJvShellAbout = class(TJvCommonDialogP)
  private
    FHandle: HWND;
    FIcon: TIcon;
    FText: string;
    FTitle: string;
    procedure SetIcon(Value: TIcon);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Handle: HWND read FHandle write FHandle;
    property Icon: TIcon read FIcon write SetIcon;
    property Text: string read FText write FText;
    property Title: string read FTitle write FTitle;
    procedure Execute; override;
    procedure ShowModal;
  end;

implementation

resourcestring
  RC_DefaultTitle = 'Project JEDI';
  RC_DefaultText = 'Place your text here';

  {**************************************************}

procedure TJvShellAbout.SetIcon(Value: TIcon);
begin
  FIcon.Assign(Value);
end;

{**************************************************}

constructor TJvShellAbout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTitle := RC_DefaultTitle;
  FText := RC_DefaultText;
  FHandle := (AOwner as TForm).Handle;
  FIcon := TIcon.Create;
end;

{**************************************************}

destructor TJvShellAbout.Destroy;
begin
  FIcon.Free;
  inherited Destroy;
end;

{**************************************************}

procedure TJvShellAbout.ShowModal;
begin
  ShellAbout(FHandle, PChar(FTitle), PChar(FText), FIcon.Handle);
end;

{**************************************************}

procedure TJvShellAbout.Execute;
begin
  ShowModal;
end;

end.
